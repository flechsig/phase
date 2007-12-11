      SUBROUTINE IODE(V, NV, TSTART, TSTOP, DT, D, ERRPAR, HANDLE)      
      INTEGER NV                                                        
      EXTERNAL D, HANDLE                                                
      REAL V(NV), TSTART, TSTOP, DT, ERRPAR(2)                          
      EXTERNAL IODEE, IODED, IODEN                                      
C THE FIRST LEVEL OF IODE.                                              
      CALL IODER(V, NV, TSTART, TSTOP, DT, D, IODEE, ERRPAR, IODEN,     
     1   IODED, HANDLE)                                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE IODES(V, NV, TSTART, TSTOP, DT, D, ERRPAR,             
     1   HANDLE)                                                        
      INTEGER NV                                                        
      EXTERNAL D, HANDLE                                                
      REAL V(NV), TSTART, TSTOP, DT, ERRPAR(2)                          
      EXTERNAL IODEE                                                    
      LOGICAL ERPUTS                                                    
C THE FIRST LEVEL OF IODES.                                             
      ERPUTS = .FALSE.                                                  
      CALL IODE1(V, NV, TSTART, TSTOP, DT, D, IODEE, ERRPAR, ERPUTS,    
     1   HANDLE)                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE IODE1(V, NV, TSTART, TSTOP, DT, D, ERROR, ERRPAR,      
     1   ERPUTS, HANDLE)                                                
      INTEGER NV                                                        
      EXTERNAL D, ERROR, HANDLE                                         
      REAL V(NV), TSTART, TSTOP, DT, ERRPAR(2)                          
      LOGICAL ERPUTS                                                    
      INTEGER KMAX, KINIT                                               
      LOGICAL EQUIL, XPOLY                                              
C THE SECOND LEVEL OF IODES.                                            
      KMAX = 10                                                         
      XPOLY = .FALSE.                                                   
      KINIT = 2                                                         
      EQUIL = .TRUE.                                                    
      CALL IODE2(V, NV, TSTART, TSTOP, DT, D, EQUIL, KMAX, XPOLY, KINIT,
     1   ERROR, ERRPAR, ERPUTS, HANDLE)                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE IODE2(V, NV, TSTART, TSTOP, DT, D, EQUIL, KMAX,        
     1   XPOLY, KINIT, ERROR, ERRPAR, ERPUTS, HANDLE)                   
      INTEGER NV                                                        
      EXTERNAL D, ERROR, HANDLE                                         
      INTEGER KMAX, KINIT                                               
      REAL V(NV), TSTART, TSTOP, DT, ERRPAR(2)                          
      LOGICAL EQUIL, XPOLY, ERPUTS                                      
      EXTERNAL IODED, IODEN                                             
      INTEGER KEEJAC, MINIT, MAXIT                                      
      REAL THETA                                                        
C THE THIRD LEVEL OF IODES.                                             
C CHECK THE INPUT FOR ERRORS.                                           
      IF (.NOT. EQUIL) GOTO 1                                           
         THETA = 1                                                      
         GOTO  2                                                        
   1     THETA = 0.5E0                                                  
   2  KEEJAC = 0                                                        
      MINIT = 10                                                        
      MAXIT = 15                                                        
      CALL IODE3(V, NV, TSTART, TSTOP, DT, D, THETA, KEEJAC, MINIT,     
     1   MAXIT, KMAX, XPOLY, KINIT, ERROR, ERRPAR, ERPUTS, IODEN, IODED,
     2   HANDLE)                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE IODE3(V, NV, TSTART, TSTOP, DT, D, THETA, KEEJAC,      
     1   MINIT, MAXIT, KMAX, XPOLY, KINIT, ERROR, ERRPAR, ERPUTS, INMI  
     2   , DA, HANDLE)                                                  
      INTEGER NV                                                        
      EXTERNAL D, ERROR, INMI, DA, HANDLE                               
      INTEGER KEEJAC, MINIT, MAXIT, KMAX, KINIT                         
      REAL V(NV), TSTART, TSTOP, DT, THETA, ERRPAR(2)                   
      LOGICAL XPOLY, ERPUTS                                             
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      EXTERNAL A1ODEE, A1ODEH, A1ODEP, A1ODEN                           
      INTEGER ISTKGT, I, IRCS, MMAX, IN, IS(1000)                       
      REAL HFRACT, BETA, GAMMA, DELTA, EGIVE, RS(1000)                  
      REAL WS(1000)                                                     
      LOGICAL USENFD, USENGJ, USENNS, LS(1000)                          
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      DATA USENGJ/.FALSE./                                              
      DATA USENNS/.FALSE./                                              
      DATA USENFD/.FALSE./                                              
C THE FOURTH LEVEL OF IODES.                                            
C SCRATCH SPACE ALLOCATED -                                             
C     S( IODE3) = S(A1ODES) +                                           
C REAL WORDS +                                                          
C                 MMAX                                                  
C INTEGER WORDS.                                                        
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      IF (TSTART .EQ. TSTOP) RETURN                                     
      CALL ENTER(1)                                                     
C CHECK THE INPUT FOR ERRORS.                                           
      MMAX = KMAX+5                                                     
      BETA = 1                                                          
      IF (THETA .EQ. 0.5) GOTO 1                                        
         GAMMA = 1                                                      
         GOTO  2                                                        
   1     GAMMA = 2                                                      
   2  IF (.NOT. ERPUTS) GOTO 3                                          
         DELTA = 1                                                      
         GOTO  4                                                        
   3     DELTA = 0                                                      
C GET N.                                                                
   4  IN = ISTKGT(MMAX, 2)                                              
      IS(IN) = 1                                                        
      IS(IN+1) = 2                                                      
      IS(IN+2) = 3                                                      
      I = 4                                                             
         GOTO  6                                                        
   5     I = I+1                                                        
   6     IF (I .GT. MMAX) GOTO  7                                       
         TEMP1 = IN+I                                                   
         TEMP = IN+I                                                    
         IS(TEMP1-1) = 2*IS(TEMP-3)                                     
         GOTO  5                                                        
   7  IF (THETA .NE. 0.5) GOTO 9                                        
         HFRACT = 0.5                                                   
         DO  8 I = 1, MMAX                                              
            TEMP = IN+I                                                 
            IS(TEMP-1) = 2*IS(TEMP-1)                                   
   8        CONTINUE                                                    
         GOTO  10                                                       
   9     HFRACT = 1                                                     
  10  EGIVE = 1E+2                                                      
      IRCS = 1                                                          
      CALL IODE4(V, NV, TSTART, TSTOP, DT, D, THETA, KEEJAC, MINIT,     
     1   MAXIT, USENGJ, USENNS, USENFD, BETA, GAMMA, DELTA, IS(IN),     
     2   MMAX, HFRACT, EGIVE, KMAX, XPOLY, KINIT, IRCS, ERROR, ERRPAR,  
     3   ERPUTS, INMI, DA, HANDLE)                                      
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION IODED(V, VT, NV, T, DT, D, DV, TJ, DTJ,          
     1   GETJAC, SEPATE, USENGJ, USENNS, USENFD, NES, NFS, FNUM, THETA  
     2   , IRCS, KEEJAC)                                                
      INTEGER NV                                                        
      EXTERNAL D                                                        
      INTEGER NES, NFS, FNUM, IRCS, KEEJAC                              
      REAL V(NV), VT(NV), T, DT, DV(NV), TJ                             
      REAL DTJ, THETA                                                   
      LOGICAL GETJAC, SEPATE, USENGJ, USENNS, USENFD                    
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A1ODES/ IMEM                                              
      INTEGER IMEM                                                      
      COMMON /IODEJ/ AJ, BJ, GETACC, NEEUMC                             
      REAL AJ, BJ                                                       
      LOGICAL GETACC, NEEUMC                                            
      COMMON /IODEF/ FAILED                                             
      LOGICAL FAILED                                                    
      INTEGER ID43, IDV, LDV, IDELVT, ISTKGT, NERROR                    
      INTEGER MIN0, MAX0, I, J, NERR, IDVT                              
      INTEGER LDVT, I0, I1, IS(1000), IDELV, ID4(2)                     
      INTEGER ID1                                                       
      REAL ABS, TEMP, AMAX1, RS(1000), WS(1000), DUMMY(100)             
      LOGICAL GETDVT, GETACT, NEESUM, LS(1000), IODEG                   
      INTEGER TEMP1, TEMP2, TEMP3, TEMP4                                
      LOGICAL TEMP5, TEMP6                                              
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C SCRATCH SPACE ALLOCATED - S( IODED) =                                 
C     NV**2 + NV*(2*NV+1) +                                             
C     MAX ( S(D), 2*NV + NV INTEGER )                                   
C REAL WORDS.                                                           
C A DUMMY ARRAY FOR VOID DV AND DVT.                                    
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      LDV = NV**2                                                       
C THE LENGTHS OF THE DV AND DVT ARRAYS.                                 
      LDVT = NV**2                                                      
      IF (IMEM .LE. 0) GOTO 1                                           
         ID4(1) = IS(IMEM+3)                                            
         ID4(2) = IS(IMEM+4)                                            
   1  TEMP5 = NFS .EQ. (-1)                                             
      IF (TEMP5) TEMP5 = KEEJAC .GT. 1                                  
      IF (TEMP5) GOTO 2                                                 
         TEMP6 = NFS .EQ. (-2)                                          
         IF (TEMP6) TEMP6 = KEEJAC .EQ. 1                               
         TEMP5 = TEMP6                                                  
   2  IF (.NOT. TEMP5) GOTO 3                                           
         IMEM = ISTKGT(10, 2)                                           
C INITIALIZE.                                                           
C D, DIAG, PIVOT, D41, D42, R, CA, RCA, CB AND RCB.                     
         IS(IMEM) = ISTKGT(LDV+3*NV+1, 3)                               
         IS(IMEM+1) = IS(IMEM)+LDV                                      
         IS(IMEM+3) = 0                                                 
         IS(IMEM+4) = 0                                                 
         IS(IMEM+5) = IS(IMEM+1)+NV                                     
         IS(IMEM+6) = IS(IMEM+5)+NV                                     
         IS(IMEM+8) = IS(IMEM+6)+NV                                     
         IS(IMEM+2) = ISTKGT(2*NV+1, 2)                                 
         IS(IMEM+7) = IS(IMEM+2)+NV                                     
         IS(IMEM+9) = IS(IMEM+7)+NV                                     
         GOTO  4                                                        
   3     IF (IMEM .EQ. 0) IMEM = 1                                      
C SIGNAL THAT KNOW ABOUT A1ODES.                                        
   4  TEMP5 = NFS .EQ. (-1)                                             
      IF (TEMP5) TEMP5 = KEEJAC .GT. 1                                  
      IF (.NOT. TEMP5) GOTO 5                                           
         IS(IMEM+3) = ISTKGT(LDV+LDVT, 3)                               
C THE JACOBIAN PARTS ARE GLOBAL.                                        
         IS(IMEM+4) = IS(IMEM+3)+LDV                                    
   5  IF (NFS .GE. 0) GOTO 6                                            
         IODED = .TRUE.                                                 
         RETURN                                                         
   6  CALL ENTER(1)                                                     
      NEESUM = DTJ .NE. DT                                              
      NEESUM = NEESUM .AND. SEPATE                                      
      IF (KEEJAC .NE. 0) GOTO 11                                        
         IMEM = ISTKGT(2*NV+11, 2)                                      
C NEED STACK FRAME.                                                     
         IF (USENNS) GOTO 7                                             
            ID4(1) = ISTKGT(LDV+LDVT+3*NV+1, 3)                         
            ID4(2) = ID4(1)+LDV                                         
            GOTO  8                                                     
   7        ID4(1) = ISTKGT(LDV+3*NV+1, 3)                              
            ID4(2) = 0                                                  
   8     IS(IMEM) = ID4(1)                                              
         ID43 = ID4(1)                                                  
         IF (USENNS) GOTO 9                                             
            IS(IMEM+1) = ID4(2)+LDVT                                    
            GOTO  10                                                    
   9        IS(IMEM+1) = ID4(1)+LDV                                     
  10     IS(IMEM+5) = IS(IMEM+1)+NV                                     
         IS(IMEM+6) = IS(IMEM+5)+NV                                     
         IS(IMEM+8) = IS(IMEM+6)+NV                                     
         IS(IMEM+2) = IMEM+10                                           
         IS(IMEM+7) = IS(IMEM+2)+NV                                     
         IS(IMEM+9) = IS(IMEM+7)+NV                                     
         GOTO  20                                                       
  11     IF (KEEJAC .NE. 1) GOTO 16                                     
            IF (.NOT. GETJAC) GOTO 14                                   
               ID4(1) = IS(IMEM)                                        
               ID43 = ID4(1)                                            
               IF (.NOT. USENNS) GOTO 12                                
                  ID4(2) = 0                                            
                  GOTO  13                                              
  12              ID4(2) = ISTKGT(LDVT, 3)                              
  13           CONTINUE                                                 
               GOTO  15                                                 
  14           ID4(1) = 0                                               
               ID4(2) = 0                                               
               ID43 = 0                                                 
  15        CONTINUE                                                    
            GOTO  19                                                    
  16        TEMP5 = GETJAC                                              
            IF (.NOT. TEMP5) TEMP5 = NEESUM                             
            IF (.NOT. TEMP5) GOTO 17                                    
               ID43 = IS(IMEM)                                          
               GOTO  18                                                 
  17           ID43 = 0                                                 
  18        CONTINUE                                                    
  19  CONTINUE                                                          
  20  NEESUM = DT .NE. DTJ                                              
      NEESUM = NEESUM .AND. SEPATE                                      
      TEMP5 = GETJAC                                                    
      IF (TEMP5) TEMP5 = .NOT. SEPATE                                   
      NEESUM = NEESUM .OR. TEMP5                                        
      IF ((.NOT. SEPATE) .OR. (.NOT. GETJAC)) GOTO 27                   
         AJ = 1                                                         
         BJ = 1                                                         
         GETACC = GETJAC                                                
         NEEUMC = .FALSE.                                               
C DEFAULT VALUES.                                                       
         CALL SETR(NV, 0E0, DV)                                         
         TEMP4 = ID4(1)                                                 
         CALL SETR(LDV, 0E0, WS(TEMP4))                                 
         TEMP4 = ID4(2)                                                 
         CALL SETR(LDVT, 0E0, WS(TEMP4))                                
         IF (USENFD) GOTO 21                                            
            FAILED = .FALSE.                                            
            NES = NES+1                                                 
            TEMP4 = ID4(1)                                              
            TEMP3 = ID4(2)                                              
            CALL D(TJ, V, VT, NV, DV, WS(TEMP4), WS(TEMP3))             
            GOTO  23                                                    
  21        GETACC = .FALSE.                                            
            IDELV = ISTKGT(3*NV, 3)                                     
            IDELVT = IDELV+NV                                           
            ID1 = IDELVT+NV                                             
            TEMP3 = ID4(1)                                              
            TEMP4 = ID4(2)                                              
            IF (IODEG(D, TJ, V, VT, NV, WS(IDELV), WS(IDELVT), DV, NES  
     1         , FNUM, NV, LDV, LDVT, WS(TEMP3), WS(TEMP4), .TRUE.,     
     2         DUMMY, 100, WS(ID1))) GOTO 22                            
               CALL LEAVE                                               
               IODED = .TRUE.                                           
               RETURN                                                   
  22        CALL ISTKRL(1)                                              
  23     IF (.NOT. FAILED) GOTO 24                                      
            FNUM = 1                                                    
            CALL LEAVE                                                  
            IODED = .TRUE.                                              
            RETURN                                                      
  24     IF (THETA .EQ. 1.) GOTO 26                                     
            TEMP4 = ID4(1)                                              
            TEMP3 = ID4(1)+LDV-1                                        
            DO  25 I = TEMP4, TEMP3                                     
               WS(I) = WS(I)*THETA                                      
  25           CONTINUE                                                 
  26     CONTINUE                                                       
  27  GETACT = GETJAC .AND. (.NOT. SEPATE)                              
      TEMP5 = USENNS                                                    
      IF (.NOT. TEMP5) TEMP5 = USENFD                                   
      IF (.NOT. TEMP5) GOTO 28                                          
         AJ = THETA                                                     
         BJ = DT                                                        
         GOTO  29                                                       
  28     AJ = 1                                                         
         BJ = 1                                                         
  29  GETACC = GETACT                                                   
      TEMP5 = NEESUM                                                    
      IF (TEMP5) TEMP5 = USENNS                                         
      NEEUMC = TEMP5                                                    
      IF (.NOT. USENFD) GOTO 30                                         
         GETDVT = KEEJAC .GT. 1                                         
         GOTO  31                                                       
  30     GETDVT = .TRUE.                                                
  31  CALL SETR(NV, 0E0, DV)                                            
      IF (.NOT. GETACT) GOTO 43                                         
         CALL SETR(LDV, 0E0, WS(ID43))                                  
         IF (ID4(2) .LE. 0) GOTO 32                                     
            TEMP3 = ID4(2)                                              
            CALL SETR(LDVT, 0E0, WS(TEMP3))                             
            GOTO  33                                                    
  32        CALL SETR(MIN0(100, LDVT), 0E0, DUMMY)                      
  33     IF (SEPATE) NEEUMC = .FALSE.                                   
         FAILED = .FALSE.                                               
         IF (ID4(2) .LE. 0) GOTO 37                                     
            IF (USENFD) GOTO 34                                         
               NES = NES+1                                              
               TEMP3 = ID4(2)                                           
               CALL D(T, V, VT, NV, DV, WS(ID43), WS(TEMP3))            
               GOTO  36                                                 
  34           GETACC = .FALSE.                                         
               IDELV = ISTKGT(3*NV, 3)                                  
               IDELVT = IDELV+NV                                        
               ID1 = IDELVT+NV                                          
               TEMP3 = ID4(2)                                           
               IF (IODEG(D, T, V, VT, NV, WS(IDELV), WS(IDELVT), DV,    
     1            NES, FNUM, NV, LDV, LDVT, WS(ID43), WS(TEMP3), GETDVT,
     2            DUMMY, 100, WS(ID1))) GOTO 35                         
                  CALL LEAVE                                            
                  IODED = .TRUE.                                        
                  RETURN                                                
  35           CALL ISTKRL(1)                                           
  36        CONTINUE                                                    
            GOTO  42                                                    
  37        IF (USENFD) GOTO 39                                         
               NES = NES+1                                              
               CALL D(T, V, VT, NV, DV, WS(ID43), DUMMY)                
               TEMP3 = MIN0(100, LDVT)                                  
               DO  38 I = 1, TEMP3                                      
C/6S                                                                    
C                 IF (DUMMY(I) .NE. 0.) CALL SETERR(                    
C    1               38H IODED - D USED DVT WHEN USENNS = TRUE, 38, 1, 2
C    2               )                                                  
C/7S                                                                    
                  IF (DUMMY(I) .NE. 0.) CALL SETERR(                    
     1               ' IODED - D USED DVT WHEN USENNS = TRUE', 38, 1, 2 
     2               )                                                  
C/                                                                      
  38              CONTINUE                                              
               GOTO  41                                                 
  39           GETACC = .FALSE.                                         
               IDELV = ISTKGT(2*NV, 3)                                  
               ID1 = IDELV+NV                                           
               IF (IODEG(D, T, V, VT, NV, WS(IDELV), DUMMY, DV, NES,    
     1            FNUM, NV, LDV, LDVT, WS(ID43), DUMMY, GETDVT, DUMMY,  
     2            100, WS(ID1))) GOTO 40                                
                  CALL LEAVE                                            
                  IODED = .TRUE.                                        
                  RETURN                                                
  40           CONTINUE                                                 
  41        CONTINUE                                                    
  42     CONTINUE                                                       
         GOTO  51                                                       
  43     IF (.NOT. USENGJ) GOTO 44                                      
            IDV = 0                                                     
            IDVT = 0                                                    
            CALL SETR(MIN0(100, MAX0(LDV, LDVT)), 0E0, DUMMY)           
            GOTO  47                                                    
  44        IF (USENNS) GOTO 45                                         
               IDV = ISTKGT(LDV+LDVT, 3)                                
               IDVT = IDV+LDV                                           
C DEFAULT VALUES.                                                       
               CALL SETR(LDV+LDVT, 0E0, WS(IDV))                        
               GOTO  46                                                 
  45           IDV = ISTKGT(MAX0(LDV, LDVT), 3)                         
               IDVT = IDV                                               
C DEFAULT VALUES.                                                       
               CALL SETR(MAX0(LDV, LDVT), 0E0, WS(IDV))                 
  46        CONTINUE                                                    
  47     FAILED = .FALSE.                                               
         NES = NES+1                                                    
         IF (IDV .LE. 0) GOTO 48                                        
            CALL D(T, V, VT, NV, DV, WS(IDV), WS(IDVT))                 
            GOTO  50                                                    
  48        CALL D(T, V, VT, NV, DV, DUMMY, DUMMY)                      
            TEMP3 = MIN0(100, MAX0(LDV, LDVT))                          
            DO  49 I = 1, TEMP3                                         
C/6S                                                                    
C              IF (DUMMY(I) .NE. 0.) CALL SETERR(                       
C    1            48H IODED - D USED DV AND/OR DVT WHEN USENGJ = TRUE,  
C    2            48, 2, 2)                                             
C/7S                                                                    
               IF (DUMMY(I) .NE. 0.) CALL SETERR(                       
     1            ' IODED - D USED DV AND/OR DVT WHEN USENGJ = TRUE',   
     2            48, 2, 2)                                             
C/                                                                      
  49           CONTINUE                                                 
  50     IF (.NOT. USENGJ) CALL ISTKRL(1)                               
  51  IF (.NOT. FAILED) GOTO 52                                         
         FNUM = 1                                                       
         CALL LEAVE                                                     
         IODED = .TRUE.                                                 
         RETURN                                                         
  52  DO  53 I = 1, NV                                                  
         DV(I) = -DV(I)                                                 
  53     CONTINUE                                                       
      TEMP5 = GETACT                                                    
      IF (TEMP5) TEMP5 = .NOT. USENNS                                   
      IF (TEMP5) TEMP5 = .NOT. USENFD                                   
      IF (TEMP5) TEMP5 = THETA .NE. 1.                                  
      IF (.NOT. TEMP5) GOTO 55                                          
         TEMP3 = ID43+LDV-1                                             
         DO  54 I = ID43, TEMP3                                         
            WS(I) = WS(I)*THETA                                         
  54        CONTINUE                                                    
  55  TEMP5 = NEESUM                                                    
      IF (.NOT. TEMP5) GOTO 56                                          
         TEMP6 = .NOT. USENNS                                           
         IF (TEMP6) TEMP6 = .NOT. USENFD                                
         IF (.NOT. TEMP6) TEMP6 = SEPATE                                
         TEMP5 = TEMP6                                                  
  56  IF (.NOT. TEMP5) GOTO 58                                          
         I0 = ID4(1)                                                    
         I1 = ID4(2)                                                    
         TEMP3 = ID43+LDV-1                                             
         DO  57 I = ID43, TEMP3                                         
            WS(I) = WS(I0)+WS(I1)/DT                                    
            I0 = I0+1                                                   
            I1 = I1+1                                                   
  57        CONTINUE                                                    
  58  IF ((.NOT. NEESUM) .AND. (.NOT. GETJAC)) GOTO 64                  
         NFS = NFS+1                                                    
         IF (IRCS .NE. 1) GOTO 62                                       
            DO  60 I = 1, NV                                            
C SCALE THE JACOBIAN.                                                   
C FIRST, GET THE ROW SCALE FACTORS.                                     
               TEMP = 0                                                 
               TEMP3 = IS(IMEM)+I-1                                     
               TEMP4 = IS(IMEM)+NV**2-1                                 
               DO  59 J = TEMP3, TEMP4, NV                              
                  TEMP = AMAX1(TEMP, ABS(WS(J)))                        
  59              CONTINUE                                              
               TEMP4 = IS(IMEM+5)+I                                     
               WS(TEMP4-1) = TEMP                                       
  60           CONTINUE                                                 
C COLUMN SCALE FACTORS.                                                 
            TEMP4 = IS(IMEM)                                            
            TEMP3 = IS(IMEM+5)                                          
            TEMP2 = IS(IMEM+6)                                          
            TEMP1 = IS(IMEM+7)                                          
            CALL RCSC(WS(TEMP4), NV, NV, WS(TEMP3), WS(TEMP2), IS(TEMP1)
     1         )                                                        
C SCALE THE JACBOBIAN.                                                  
            TEMP1 = IS(IMEM)                                            
            TEMP2 = IS(IMEM+5)                                          
            TEMP3 = IS(IMEM+6)                                          
            TEMP4 = IS(IMEM+7)                                          
            CALL RCSS(WS(TEMP1), NV, NV, WS(TEMP2), WS(TEMP3), IS(TEMP4)
     1         )                                                        
            IF (NERROR(NERR) .EQ. 0) GOTO 61                            
               CALL ERROFF                                              
C/6S                                                                    
C              CALL SETERR(                                             
C    1            43H IODED - MUST HAVE 1/(S*L) IN MACHINE RANGE, 43, 3,
C    2            2)                                                    
C/7S                                                                    
               CALL SETERR(                                             
     1            ' IODED - MUST HAVE 1/(S*L) IN MACHINE RANGE', 43, 3, 
     2            2)                                                    
C/                                                                      
  61        CONTINUE                                                    
C FACTOR JACOBIAN.                                                      
  62     TEMP4 = IS(IMEM)                                               
         TEMP3 = IS(IMEM+1)                                             
         TEMP2 = IS(IMEM+2)                                             
         CALL QRD(NV, NV, WS(TEMP4), WS(TEMP3), IS(TEMP2))              
         IF (NERROR(NERR) .EQ. 0) GOTO 63                               
            CALL ERROFF                                                 
            FNUM = 2                                                    
            CALL LEAVE                                                  
            IODED = .TRUE.                                              
            RETURN                                                      
  63     CONTINUE                                                       
  64  IF (IRCS .NE. 1) GOTO 65                                          
         TEMP2 = IS(IMEM+5)                                             
C SCALE FACTORS FOR THE RHS.                                            
         TEMP3 = IS(IMEM+8)                                             
         TEMP4 = IS(IMEM+9)                                             
         CALL RCSC(DV, NV, 1, WS(TEMP2), WS(TEMP3), IS(TEMP4))          
C SCALE THE RHS.                                                        
         TEMP4 = IS(IMEM+5)                                             
         TEMP3 = IS(IMEM+8)                                             
         TEMP2 = IS(IMEM+9)                                             
         CALL RCSB(DV, NV, 1, WS(TEMP4), WS(TEMP3), IS(TEMP2))          
C SOLVE JACOBIAN * DV = RHS.                                            
  65  TEMP2 = IS(IMEM)                                                  
      TEMP3 = IS(IMEM+1)                                                
      TEMP4 = IS(IMEM+2)                                                
      CALL QRQTB(NV, NV, WS(TEMP2), WS(TEMP3), IS(TEMP4), 1, DV, DV)    
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
C    1   32H IODED - SINGULAR DJAC IN  QRQTB, 32, 4, 2)                 
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
     1   ' IODED - SINGULAR DJAC IN  QRQTB', 32, 4, 2)                  
C/                                                                      
      IF (IRCS .NE. 1) GOTO 68                                          
         TEMP4 = IS(IMEM+6)                                             
C UN-SCALE THE SOLUTION.                                                
         TEMP3 = IS(IMEM+7)                                             
         TEMP2 = IS(IMEM+8)                                             
         TEMP1 = IS(IMEM+9)                                             
         CALL RCSX(DV, NV, 1, WS(TEMP4), IS(TEMP3), WS(TEMP2), IS(TEMP1)
     1      )                                                           
         IF (NERROR(NERR) .EQ. 0) GOTO 67                               
            IF (NERR .NE. 8) GOTO 66                                    
               CALL ERROFF                                              
C/6S                                                                    
C              CALL SETERR(26H IODED - DV HAS OVERFLOWED, 26, 5, 2)     
C/7S                                                                    
               CALL SETERR(' IODED - DV HAS OVERFLOWED', 26, 5, 2)      
C/                                                                      
  66        CONTINUE                                                    
  67     CONTINUE                                                       
  68  CALL LEAVE                                                        
      IODED = .FALSE.                                                   
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION IODEE(V, NV, T, DT, ERRPAR, ERPUTS, EV)          
      INTEGER NV                                                        
      REAL V(NV), T, DT, ERRPAR(2), EV(NV)                              
      LOGICAL ERPUTS                                                    
      INTEGER I                                                         
      REAL ABS, TEMP, DTPOW                                             
      LOGICAL CONGED                                                    
C THE STANDARD ERROR PROCEDURE FOR IODES.                               
C SCRATCH SPACE ALLOCATED - NONE.                                       
      IF (.NOT. ERPUTS) GOTO 1                                          
         DTPOW = ABS(DT)                                                
         GOTO  2                                                        
   1     DTPOW = 1                                                      
   2  CONGED = .TRUE.                                                   
C ERROR FOR V.                                                          
      DO  3 I = 1, NV                                                   
         TEMP = DTPOW*(ERRPAR(1)*ABS(V(I))+ERRPAR(2))                   
         IF (TEMP .LT. EV(I)) CONGED = .FALSE.                          
         EV(I) = TEMP                                                   
   3     CONTINUE                                                       
      IODEE = CONGED                                                    
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION IODEG(D, T, V, VT, NV, DELV, DELVT, D0,          
     1   NES, FNUM, LDIM, LDV, LDVT, DV, DVT, GETDVT, DUMMY, LDUMMY, D1)
      INTEGER LDUMMY, LDIM, NV                                          
      EXTERNAL D                                                        
      INTEGER NES, FNUM, LDV, LDVT                                      
      REAL T, V(NV), VT(NV), DELV(NV), DELVT(NV), D0(NV)                
      REAL DV(LDIM, 1), DVT(LDIM, 1), DUMMY(LDUMMY), D1(NV)             
      LOGICAL GETDVT                                                    
      COMMON /IODEJ/ AJ, BJ, GETJAC, NEESUM                             
      REAL AJ, BJ                                                       
      LOGICAL GETJAC, NEESUM                                            
      COMMON /IODEF/ FAILED                                             
      LOGICAL FAILED                                                    
      INTEGER MIN0, MAX0, I, J                                          
      REAL BIG, ABS, DELVTI, VTSAVE, SQRT, DELVI                        
      REAL VSAVE, SQRTR, R1MACH                                         
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      DATA SQRTR/0E0/                                                   
      DATA BIG/0E0/                                                     
C THE FINITE-DIFFERENCE JACOBIAN GETTER FOR IODE.                       
      CALL SETR(LDUMMY, 0E0, DUMMY)                                     
      IF (SQRTR .EQ. 0.) SQRTR = SQRT(R1MACH(4))                        
C SQRT( MACHINE PRECISION ).                                            
      IF (BIG .EQ. 0.) BIG = R1MACH(2)                                  
C THE BIGGEST FP NUMBER.                                                
C GET INITIAL D VALUE AND DELV, DELVT TERMS.                            
      CALL SETR(NV, 0E0, D0)                                            
      FAILED = .FALSE.                                                  
      NES = NES+1                                                       
      DO  1 I = 1, NV                                                   
         DELV(I) = SQRTR*ABS(V(I))                                      
         IF (DELV(I) .EQ. 0.) DELV(I) = SQRTR                           
   1     CONTINUE                                                       
      IF (.NOT. GETDVT) GOTO 3                                          
         DO  2 I = 1, NV                                                
            DELVT(I) = SQRTR*ABS(VT(I))                                 
            IF (DELVT(I) .EQ. 0.) DELVT(I) = SQRTR                      
   2        CONTINUE                                                    
   3  CALL D(T, V, VT, NV, D0, DELV, DELVT)                             
      DO  4 J = 1, NV                                                   
C/6S                                                                    
C        IF (DELV(J) .EQ. 0.) CALL SETERR(                              
C    1      50H IODEG - D RETURNED 0 VALUES FOR DELV AND/OR DELVT, 50, 1
C    2      , 2)                                                        
C/7S                                                                    
         IF (DELV(J) .EQ. 0.) CALL SETERR(                              
     1      ' IODEG - D RETURNED 0 VALUES FOR DELV AND/OR DELVT', 50, 1 
     2      , 2)                                                        
C/                                                                      
   4     CONTINUE                                                       
      IF (.NOT. GETDVT) GOTO 6                                          
         DO  5 J = 1, NV                                                
C/6S                                                                    
C           IF (DELVT(J) .EQ. 0.) CALL SETERR(                          
C    1         50H IODEG - D RETURNED 0 VALUES FOR DELV AND/OR DELVT,   
C    2         50, 1, 2)                                                
C/7S                                                                    
            IF (DELVT(J) .EQ. 0.) CALL SETERR(                          
     1         ' IODEG - D RETURNED 0 VALUES FOR DELV AND/OR DELVT',    
     2         50, 1, 2)                                                
C/                                                                      
   5        CONTINUE                                                    
         GOTO  8                                                        
   6     TEMP1 = MIN0(LDUMMY, LDVT)                                     
         DO  7 J = 1, TEMP1                                             
C/6S                                                                    
C           IF (DUMMY(J) .NE. 0.) CALL SETERR(                          
C    1         29H IODEG - D USED DV AND/OR DVT, 29, 2, 2)              
C/7S                                                                    
            IF (DUMMY(J) .NE. 0.) CALL SETERR(                          
     1         ' IODEG - D USED DV AND/OR DVT', 29, 2, 2)               
C/                                                                      
   7        CONTINUE                                                    
   8  IF (.NOT. FAILED) GOTO 9                                          
         FNUM = 1                                                       
         IODEG = .FALSE.                                                
         RETURN                                                         
   9  IF (.NOT. GETDVT) GOTO 22                                         
         DO  15 I = 1, NV                                               
C USE FDS TO GET DV AND DVT.                                            
            DELVI = ABS(DELV(I))                                        
            IF (V(I) .GT. 0.) DELVI = -DELVI                            
C GUARD AGAINST OVERFLOW.                                               
            VSAVE = V(I)                                                
            V(I) = V(I)+DELVI                                           
            CALL SETR(NV, 0E0, D1)                                      
            FAILED = .FALSE.                                            
            NES = NES+1                                                 
            CALL D(T, V, VT, NV, D1, DUMMY, DUMMY)                      
            TEMP1 = MIN0(LDUMMY, MAX0(LDV, LDVT))                       
            DO  10 J = 1, TEMP1                                         
C/6S                                                                    
C              IF (DUMMY(J) .NE. 0.) CALL SETERR(                       
C    1            29H IODEG - D USED DV AND/OR DVT, 29, 2, 2)           
C/7S                                                                    
               IF (DUMMY(J) .NE. 0.) CALL SETERR(                       
     1            ' IODEG - D USED DV AND/OR DVT', 29, 2, 2)            
C/                                                                      
  10           CONTINUE                                                 
            IF (.NOT. FAILED) GOTO 11                                   
               FNUM = 1                                                 
               IODEG = .FALSE.                                          
               RETURN                                                   
  11        DO  14 J = 1, NV                                            
               TEMP = ABS(DELVI) .LT. 1.                                
               IF (TEMP) TEMP = ABS(D1(J)-D0(J)) .GT. ABS(DELVI)*BIG    
               IF (.NOT. TEMP) GOTO 12                                  
                  DV(J, I) = BIG                                        
C OVERFLOW.                                                             
                  GOTO  13                                              
  12              DV(J, I) = (D1(J)-D0(J))/DELVI                        
  13           CONTINUE                                                 
  14           CONTINUE                                                 
            V(I) = VSAVE                                                
  15        CONTINUE                                                    
         DO  21 I = 1, NV                                               
            DELVTI = ABS(DELVT(I))                                      
            IF (VT(I) .GT. 0.) DELVTI = -DELVTI                         
C GUARD AGAINST OVERFLOW.                                               
            VTSAVE = VT(I)                                              
            VT(I) = VT(I)+DELVTI                                        
            CALL SETR(NV, 0E0, D1)                                      
            FAILED = .FALSE.                                            
            NES = NES+1                                                 
            CALL D(T, V, VT, NV, D1, DUMMY, DUMMY)                      
            TEMP1 = MIN0(LDUMMY, MAX0(LDV, LDVT))                       
            DO  16 J = 1, TEMP1                                         
C/6S                                                                    
C              IF (DUMMY(J) .NE. 0.) CALL SETERR(                       
C    1            29H IODEG - D USED DV AND/OR DVT, 29, 2, 2)           
C/7S                                                                    
               IF (DUMMY(J) .NE. 0.) CALL SETERR(                       
     1            ' IODEG - D USED DV AND/OR DVT', 29, 2, 2)            
C/                                                                      
  16           CONTINUE                                                 
            IF (.NOT. FAILED) GOTO 17                                   
               FNUM = 1                                                 
               IODEG = .FALSE.                                          
               RETURN                                                   
  17        DO  20 J = 1, NV                                            
               TEMP = ABS(DELVTI) .LT. 1.                               
               IF (TEMP) TEMP = ABS(D1(J)-D0(J)) .GT. ABS(DELVTI)*BIG   
               IF (.NOT. TEMP) GOTO 18                                  
                  DVT(J, I) = BIG                                       
C OVERFLOW.                                                             
                  GOTO  19                                              
  18              DVT(J, I) = (D1(J)-D0(J))/DELVTI                      
  19           CONTINUE                                                 
  20           CONTINUE                                                 
            VT(I) = VTSAVE                                              
  21        CONTINUE                                                    
         GOTO  29                                                       
  22     DO  28 I = 1, NV                                               
C NOT GETTING DVT.                                                      
            VSAVE = V(I)                                                
            VTSAVE = VT(I)                                              
            DELVI = ABS(DELV(I))                                        
            V(I) = V(I)+DELVI*AJ                                        
            VT(I) = VT(I)+DELVI/BJ                                      
            CALL SETR(NV, 0E0, D1)                                      
            FAILED = .FALSE.                                            
            NES = NES+1                                                 
            CALL D(T, V, VT, NV, D1, DUMMY, DUMMY)                      
            TEMP1 = MIN0(LDUMMY, MAX0(LDV, LDVT))                       
            DO  23 J = 1, TEMP1                                         
C/6S                                                                    
C              IF (DUMMY(J) .NE. 0.) CALL SETERR(                       
C    1            29H IODEG - D USED DV AND/OR DVT, 29, 2, 2)           
C/7S                                                                    
               IF (DUMMY(J) .NE. 0.) CALL SETERR(                       
     1            ' IODEG - D USED DV AND/OR DVT', 29, 2, 2)            
C/                                                                      
  23           CONTINUE                                                 
            IF (.NOT. FAILED) GOTO 24                                   
               FNUM = 1                                                 
               IODEG = .FALSE.                                          
               RETURN                                                   
  24        DO  27 J = 1, NV                                            
               TEMP = ABS(DELVI) .LT. 1.                                
               IF (TEMP) TEMP = ABS(D1(J)-D0(J)) .GT. BIG*ABS(DELVI)    
               IF (.NOT. TEMP) GOTO 25                                  
                  DV(J, I) = BIG                                        
C OVERFLOW.                                                             
                  GOTO  26                                              
  25              DV(J, I) = (D1(J)-D0(J))/DELVI                        
  26           CONTINUE                                                 
  27           CONTINUE                                                 
            V(I) = VSAVE                                                
            VT(I) = VTSAVE                                              
  28        CONTINUE                                                    
  29  IODEG = .TRUE.                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE IODEN(NV, T, DT, VOLD, V, VT)                          
      INTEGER NV                                                        
      REAL T, DT, VOLD(NV), V(NV), VT(NV)                               
C THE DEFAULT NEWTON ITERATION INITIALIZER FOR IODES.                   
      RETURN                                                            
      END                                                               
      SUBROUTINE IODER(V, NV, TSTART, TSTOP, DT, D, ERROR, ERRPAR,      
     1   INMI, DA, HANDLE)                                              
      INTEGER NV                                                        
      EXTERNAL D, ERROR, INMI, DA, HANDLE                               
      REAL V(NV), TSTART, TSTOP, DT, ERRPAR(2)                          
      INTEGER I, N(100), KEEJAC, KMAX, MMAX, IZAP                       
      INTEGER IRCS, KINIT, MINIT, MAXIT                                 
      REAL HFRACT, BETA, FZAP, RZAP, GAMMA, DELTA                       
      REAL EGIVE, THETA                                                 
      LOGICAL USENFD, USENGJ, USENNS, ERPUTS, LZAP, XPOLY               
C THE SECOND, ROUTINE, LEVEL OF IODE.                                   
C RETRIEVE THE VALUES TO BE USED.                                       
      CALL IODEV(-1, THETA, RZAP, IZAP, LZAP)                           
      CALL IODEV(-2, BETA, RZAP, IZAP, LZAP)                            
      CALL IODEV(-3, GAMMA, RZAP, IZAP, LZAP)                           
      CALL IODEV(-4, DELTA, RZAP, IZAP, LZAP)                           
      CALL IODEV(-1001, FZAP, HFRACT, IZAP, LZAP)                       
      CALL IODEV(-1002, FZAP, EGIVE, IZAP, LZAP)                        
      CALL IODEV(-2001, FZAP, RZAP, KEEJAC, LZAP)                       
      CALL IODEV(-2002, FZAP, RZAP, MINIT, LZAP)                        
      CALL IODEV(-2003, FZAP, RZAP, MAXIT, LZAP)                        
      CALL IODEV(-2004, FZAP, RZAP, KMAX, LZAP)                         
      CALL IODEV(-2005, FZAP, RZAP, KINIT, LZAP)                        
      CALL IODEV(-2006, FZAP, RZAP, MMAX, LZAP)                         
      CALL IODEV(-2007, FZAP, RZAP, IRCS, LZAP)                         
      CALL IODEV(-3001, FZAP, RZAP, IZAP, XPOLY)                        
      CALL IODEV(-3002, FZAP, RZAP, IZAP, ERPUTS)                       
      CALL IODEV(-3003, FZAP, RZAP, IZAP, USENGJ)                       
      CALL IODEV(-3004, FZAP, RZAP, IZAP, USENNS)                       
      CALL IODEV(-3005, FZAP, RZAP, IZAP, USENFD)                       
C TEST FOR ERRORS.                                                      
C/6S                                                                    
C     IF (KMAX .LT. 1) CALL SETERR(18H IODE4 - KMAX.LT.1, 18, 8, 2)     
C     IF (MMAX .LT. KMAX+2) CALL SETERR(23H IODE4 - MMAX.LT.KMAX+2, 23  
C    1   , 14, 2)                                                       
C/7S                                                                    
      IF (KMAX .LT. 1) CALL SETERR(' IODE4 - KMAX.LT.1', 18, 8, 2)      
      IF (MMAX .LT. KMAX+2) CALL SETERR(' IODE4 - MMAX.LT.KMAX+2', 23   
     1   , 14, 2)                                                       
C/                                                                      
      DO  1 I = 1, MMAX                                                 
         CALL IODEV(-(I+4000), FZAP, RZAP, N(I), LZAP)                  
   1     CONTINUE                                                       
C TEST N FOR MONOTONICITY.                                              
      DO  2 I = 2, MMAX                                                 
C/6S                                                                    
C        IF (N(I) .LE. N(I-1)) CALL SETERR(                             
C    1      37H IODE4 - N IS NOT MONOTONE INCREASING, 37, 16, 2)        
C/7S                                                                    
         IF (N(I) .LE. N(I-1)) CALL SETERR(                             
     1      ' IODE4 - N IS NOT MONOTONE INCREASING', 37, 16, 2)         
C/                                                                      
   2     CONTINUE                                                       
      CALL IODE4(V, NV, TSTART, TSTOP, DT, D, THETA, KEEJAC, MINIT,     
     1   MAXIT, USENGJ, USENNS, USENFD, BETA, GAMMA, DELTA, N, MMAX,    
     2   HFRACT, EGIVE, KMAX, XPOLY, KINIT, IRCS, ERROR, ERRPAR, ERPUTS,
     3   INMI, DA, HANDLE)                                              
      RETURN                                                            
      END                                                               
      SUBROUTINE IODEV(J, F, R, I, L)                                   
      INTEGER J, I                                                      
      REAL F, R                                                         
      LOGICAL L                                                         
      INTEGER MAX0, K, M, N(100), IABS, KEEJAC                          
      INTEGER KMAX, IRCS, MMAX, KINIT, MINIT, MAXIT                     
      REAL HFRACT, BETA, SQRT, GAMMA, DELTA, EGIVE                      
      REAL THETA, FLOAT                                                 
      LOGICAL USENFD, USENGJ, USENNS, ERPUTS, XPOLY                     
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
      DATA THETA/1E0/                                                   
      DATA BETA/1E0/                                                    
      DATA GAMMA/1E0/                                                   
      DATA DELTA/0E0/                                                   
      DATA HFRACT/1E0/                                                  
      DATA EGIVE/1E+2/                                                  
      DATA KEEJAC/0/                                                    
      DATA MINIT/10/                                                    
      DATA MAXIT/50/                                                    
      DATA KMAX/10/                                                     
      DATA KINIT/4/                                                     
      DATA MMAX/15/                                                     
      DATA IRCS/1/                                                      
      DATA XPOLY/.FALSE./                                               
      DATA ERPUTS/.FALSE./                                              
      DATA USENGJ/.FALSE./                                              
      DATA USENNS/.FALSE./                                              
      DATA USENFD/.FALSE./                                              
      DATA N(1)/1/, N(2)/0/, N(3)/0/                                    
C THE PARAMETER SETTING ROUTINE FOR IODE.                               
C THE VARIABLES ARE                                                     
C J = 1.                                                                
C J = 2.                                                                
C J = 3.                                                                
C J = 4.                                                                
C J = 1001.                                                             
C J = 1002.                                                             
C J = 2001.                                                             
C J = 2002.                                                             
C J = 2003.                                                             
C J = 2004.                                                             
C J = 2005.                                                             
C J = 2006.                                                             
C J = 2007. 0 DO NOT SCALE, +1 SCALE (DEFAULT).                         
C J = 3001.                                                             
C J = 3002.                                                             
C J = 3003.                                                             
C J = 3004.                                                             
C J = 3005.                                                             
C J = 4001, ... , 4100.                                                 
      GOTO  58                                                          
C   EXPORT THE VARIABLES.                                               
   1     F = THETA                                                      
         GOTO  59                                                       
   2     F = BETA                                                       
         GOTO  59                                                       
   3     F = GAMMA                                                      
         GOTO  59                                                       
   4     F = DELTA                                                      
         GOTO  59                                                       
   5     R = HFRACT                                                     
         GOTO  59                                                       
   6     R = EGIVE                                                      
         GOTO  59                                                       
   7     I = KEEJAC                                                     
         GOTO  59                                                       
   8     I = MINIT                                                      
         GOTO  59                                                       
   9     I = MAXIT                                                      
         GOTO  59                                                       
  10     I = KMAX                                                       
         GOTO  59                                                       
  11     I = KINIT                                                      
         GOTO  59                                                       
  12     I = MMAX                                                       
         GOTO  59                                                       
  13     I = IRCS                                                       
         GOTO  59                                                       
  14     L = XPOLY                                                      
         GOTO  59                                                       
  15     L = ERPUTS                                                     
         GOTO  59                                                       
  16     L = USENGJ                                                     
         GOTO  59                                                       
  17     L = USENNS                                                     
         GOTO  59                                                       
  18     L = USENFD                                                     
         GOTO  59                                                       
C IODE VERSION NUMBER.                                                  
  19     F = 3E0                                                        
         GOTO  59                                                       
C SET THE VARIABLES TO THE DEFAULTS.                                    
  20     THETA = 1E0                                                    
         BETA = 1                                                       
         GAMMA = 1                                                      
         DELTA = 0                                                      
         HFRACT = 1                                                     
         EGIVE = 1E+2                                                   
         KEEJAC = 0                                                     
         MINIT = 10                                                     
         MAXIT = 50                                                     
         KMAX = 10                                                      
         KINIT = 4                                                      
         MMAX = 15                                                      
         IRCS = 1                                                       
         XPOLY = .FALSE.                                                
         ERPUTS = .FALSE.                                               
         USENGJ = .FALSE.                                               
         USENNS = .FALSE.                                               
         USENFD = .FALSE.                                               
         CALL SETI(100, 0, N)                                           
         N(1) = 1                                                       
C   IMPORT THE VARIABLES.                                               
         GOTO  59                                                       
  21     THETA = F                                                      
         IF (THETA .EQ. 0.5) GOTO 22                                    
            GAMMA = 1                                                   
            HFRACT = 1                                                  
            GOTO  26                                                    
  22        GAMMA = 2                                                   
            HFRACT = 0.5                                                
            N(1) = 2                                                    
            N(2) = 4                                                    
            N(3) = 6                                                    
            M = 4                                                       
               GOTO  24                                                 
  23           M = M+1                                                  
  24           IF (M .GT. MMAX) GOTO  25                                
               N(M) = 2*N(M-2)                                          
               GOTO  23                                                 
  25        CONTINUE                                                    
  26     GOTO  59                                                       
  27     BETA = F                                                       
         GOTO  59                                                       
  28     GAMMA = F                                                      
         GOTO  59                                                       
  29     DELTA = F                                                      
         GOTO  59                                                       
  30     HFRACT = R                                                     
         GOTO  59                                                       
  31     EGIVE = R                                                      
         GOTO  59                                                       
  32     KEEJAC = I                                                     
         GOTO  59                                                       
  33     MINIT = I                                                      
         GOTO  59                                                       
  34     MAXIT = I                                                      
         GOTO  59                                                       
  35     KMAX = I                                                       
         MMAX = KMAX+5                                                  
         GOTO  59                                                       
  36     KINIT = I                                                      
         GOTO  59                                                       
  37     MMAX = I                                                       
         GOTO  59                                                       
  38     IRCS = I                                                       
         GOTO  59                                                       
  39     XPOLY = L                                                      
         GOTO  59                                                       
  40     ERPUTS = L                                                     
         IF (.NOT. ERPUTS) GOTO 41                                      
            DELTA = 1                                                   
            GOTO  42                                                    
  41        DELTA = 0                                                   
  42     GOTO  59                                                       
  43     USENGJ = L                                                     
         GOTO  59                                                       
  44     USENNS = L                                                     
         GOTO  59                                                       
  45     USENFD = L                                                     
         GOTO  59                                                       
  46     TEMP1 = IABS(J) .GT. 4100                                      
         IF (.NOT. TEMP1) TEMP1 = IABS(J) .LT. 4001                     
C/6S                                                                    
C        IF (TEMP1) CALL SETERR(24H IODEV - J OUT OF BOUNDS, 24, 1, 2)  
C/7S                                                                    
         IF (TEMP1) CALL SETERR(' IODEV - J OUT OF BOUNDS', 24, 1, 2)   
C/                                                                      
         IF (J .GE. 0) GOTO 56                                          
            IF (N(2) .NE. 0) GOTO 47                                    
               N(2) = SQRT(2E0)*FLOAT(N(1))                             
C EXPORT N(ABS(J)-4000)                                                 
C ONLY N(1) IS GIVEN, USE SQRT(2) INCREASE.                             
               IF (N(2) .EQ. N(1)) N(2) = N(2)+1                        
               N(3) = SQRT(2E0)*FLOAT(N(2))                             
               IF (N(3) .EQ. N(2)) N(3) = N(3)+1                        
               N(4) = 0                                                 
  47        TEMP = IABS(J)                                              
            IF (N(TEMP-4000) .NE. 0) GOTO 55                            
               DO  53 K = 1, MMAX                                       
C FILL IN THE MISSING N(M).                                             
                  IF (N(K) .NE. 0) GOTO 52                              
                     IF (K .NE. 3) GOTO 49                              
                        DO  48 M = K, MMAX                              
                           N(M) = (N(2)*N(M-1))/MAX0(1, N(1))           
  48                       CONTINUE                                     
                        GOTO  51                                        
  49                    DO  50 M = K, MMAX                              
                           N(M) = 2*N(M-2)                              
  50                       CONTINUE                                     
  51                 GOTO  54                                           
  52              CONTINUE                                              
  53              CONTINUE                                              
  54           CONTINUE                                                 
  55        TEMP = IABS(J)                                              
            I = N(TEMP-4000)                                            
            GOTO  57                                                    
  56        N(J-4000) = I                                               
C IMPORT N(J-4000)                                                      
            IF (J-4000 .LT. 100) N(J-3999) = 0                          
  57     CONTINUE                                                       
         GOTO  59                                                       
  58     IF (J .EQ. 3005) GOTO  45                                      
         IF (J .EQ. 3004) GOTO  44                                      
         IF (J .EQ. 3003) GOTO  43                                      
         IF (J .EQ. 3002) GOTO  40                                      
         IF (J .EQ. 3001) GOTO  39                                      
         IF (J .EQ. 2007) GOTO  38                                      
         IF (J .EQ. 2006) GOTO  37                                      
         IF (J .EQ. 2005) GOTO  36                                      
         IF (J .EQ. 2004) GOTO  35                                      
         IF (J .EQ. 2003) GOTO  34                                      
         IF (J .EQ. 2002) GOTO  33                                      
         IF (J .EQ. 2001) GOTO  32                                      
         IF (J .EQ. 1002) GOTO  31                                      
         IF (J .EQ. 1001) GOTO  30                                      
         IF (J .EQ. 4) GOTO  29                                         
         IF (J .EQ. 3) GOTO  28                                         
         IF (J .EQ. 2) GOTO  27                                         
         IF (J .EQ. 1) GOTO  21                                         
         IF (J .EQ. 0) GOTO  20                                         
         IF (J .EQ. (-6000)) GOTO  19                                   
         IF (J .EQ. (-3005)) GOTO  18                                   
         IF (J .EQ. (-3004)) GOTO  17                                   
         IF (J .EQ. (-3003)) GOTO  16                                   
         IF (J .EQ. (-3002)) GOTO  15                                   
         IF (J .EQ. (-3001)) GOTO  14                                   
         IF (J .EQ. (-2007)) GOTO  13                                   
         IF (J .EQ. (-2006)) GOTO  12                                   
         IF (J .EQ. (-2005)) GOTO  11                                   
         IF (J .EQ. (-2004)) GOTO  10                                   
         IF (J .EQ. (-2003)) GOTO  9                                    
         IF (J .EQ. (-2002)) GOTO  8                                    
         IF (J .EQ. (-2001)) GOTO  7                                    
         IF (J .EQ. (-1002)) GOTO  6                                    
         IF (J .EQ. (-1001)) GOTO  5                                    
         IF (J .EQ. (-4)) GOTO  4                                       
         IF (J .EQ. (-3)) GOTO  3                                       
         IF (J .EQ. (-2)) GOTO  2                                       
         IF (J .EQ. (-1)) GOTO  1                                       
         GOTO  46                                                       
  59  RETURN                                                            
      END                                                               
      SUBROUTINE QRD(M, N, A, ALFA, PIVOT)                              
      INTEGER M, N                                                      
      INTEGER PIVOT(N)                                                  
      REAL A(M, N), ALFA(N)                                             
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, MIN0, I, J, K, JBAR                               
      INTEGER ISUM, IS(1000)                                            
      REAL AKK, EPS, RNDING, BETA, SDOT, SQRT                           
      REAL ALFAK, SIGMA, RS(1000), FLOAT, WS(1000), R1MACH              
      LOGICAL LS(1000)                                                  
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      DATA RNDING/0E0/                                                  
C TO OBTAIN THE QR DECOMPOSITION OF A MATRIX.                           
C MNEMONIC - QR DECOMPOSITION OF A MATRIX.                              
C INPUT -                                                               
C   M - THE NUMBER OF ROWS IN THE MATRIX A.                             
C   N - THE NUMBER OF COLUMNS IN THE MATRIX A.                          
C   A - THE MATRIX.                                                     
C OUTPUT -                                                              
C   A     - THE UPPER TRIANGULAR PORTION OF A ABOVE THE DIAGONAL        
C           IS THE R OF THE QR DECOMPOSITION, WITH THE DIAGONAL         
C           ELEMENTS OF R IN ALFA.                                      
C           THE LOWER TRIANGULAR PORTION OF A STORES THE Q IN           
C           FACTORED HOUSEHOLDER FORM. Q*A = R.                         
C   ALFA  - THE DIAGONAL OF R.                                          
C   PIVOT - PIVOT(J) IS THE POSITION OF THE J-TH VARIABLE, J = 1,...,N, 
C           CHOSEN SO THAT THE MAXIMAL COLUMN IS ELIMINATED AT EACH     
C           STEP.                                                       
C SCRATCH SPACE ALLOCATED - N*MU WORDS.                                 
C ERROR STATES -                                                        
C   1 - M.LT.1.                                                         
C   2 - N.LT.1.                                                         
C   3 - SINGULAR MATRIX. (RECOVERABLE)                                  
C ALFA(MIN(M,N)).                                                       
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
C DEFINE SUM(J) WS(ISUM-1+J)                                            
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(13H QRD - M.LT.1, 13, 1, 2)             
C     IF (N .LT. 1) CALL SETERR(13H QRD - N.LT.1, 13, 2, 2)             
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' QRD - M.LT.1', 13, 1, 2)              
      IF (N .LT. 1) CALL SETERR(' QRD - N.LT.1', 13, 2, 2)              
C/                                                                      
      IF (RNDING .EQ. 0.) RNDING = R1MACH(4)                            
      EPS = (RNDING*FLOAT(MIN0(M, N)**3))**2                            
      ISUM = ISTKGT(N, 3)                                               
      J = 1                                                             
         GOTO  2                                                        
   1     J = J+1                                                        
   2     IF (J .GT. N) GOTO  3                                          
C GET THE L2 NORM OF THE J-TH COLUMN.                                   
         TEMP1 = ISUM-1+J                                               
         WS(TEMP1) = SDOT(M, A(1, J), 1, A(1, J), 1)                    
         PIVOT(J) = J                                                   
         GOTO  1                                                        
   3  K = 1                                                             
         GOTO  5                                                        
   4     K = K+1                                                        
   5     IF (K .GT. MIN0(M, N)) GOTO  19                                
C ELIMINATE K-TH COLUMN.                                                
         TEMP1 = ISUM-1+K                                               
         SIGMA = WS(TEMP1)                                              
C FIND THE PIVOT COLUMN.                                                
         JBAR = K                                                       
         J = K+1                                                        
            GOTO  7                                                     
   6        J = J+1                                                     
   7        IF (J .GT. N) GOTO  9                                       
            TEMP1 = ISUM-1+J                                            
            IF (SIGMA .GE. WS(TEMP1)) GOTO 8                            
               TEMP = ISUM-1+J                                          
               SIGMA = WS(TEMP)                                         
               JBAR = J                                                 
   8        CONTINUE                                                    
            GOTO  6                                                     
   9     IF (JBAR .EQ. K) GOTO 11                                       
            I = PIVOT(K)                                                
C NEED TO INTERCHANGE THE COLUMNS.                                      
            PIVOT(K) = PIVOT(JBAR)                                      
            PIVOT(JBAR) = I                                             
            TEMP1 = ISUM-1+JBAR                                         
            TEMP = ISUM-1+K                                             
            WS(TEMP1) = WS(TEMP)                                        
            TEMP = ISUM-1+K                                             
            WS(TEMP) = SIGMA                                            
            DO  10 I = 1, M                                             
               SIGMA = A(I, K)                                          
               A(I, K) = A(I, JBAR)                                     
               A(I, JBAR) = SIGMA                                       
  10           CONTINUE                                                 
  11     SIGMA = SDOT(M-K+1, A(K, K), 1, A(K, K), 1)                    
         IF (SIGMA .GT. EPS*WS(ISUM)) GOTO 12                           
C/6S                                                                    
C           CALL SETERR(22H QRD - SINGULAR MATRIX, 22, 3, 1)            
C/7S                                                                    
            CALL SETERR(' QRD - SINGULAR MATRIX', 22, 3, 1)             
C/                                                                      
            GOTO  19                                                    
  12     AKK = A(K, K)                                                  
         IF (AKK .GE. 0.) GOTO 13                                       
            ALFAK = SQRT(SIGMA)                                         
            GOTO  14                                                    
  13        ALFAK = -SQRT(SIGMA)                                        
  14     ALFA(K) = ALFAK                                                
         BETA = 1./(SIGMA-AKK*ALFAK)                                    
         A(K, K) = AKK-ALFAK                                            
         J = K+1                                                        
            GOTO  16                                                    
  15        J = J+1                                                     
  16        IF (J .GT. N) GOTO  18                                      
            SIGMA = BETA*SDOT(M+1-K, A(K, K), 1, A(K, J), 1)            
            DO  17 I = K, M                                             
               A(I, J) = A(I, J)-A(I, K)*SIGMA                          
  17           CONTINUE                                                 
            TEMP = ISUM-1+J                                             
            WS(TEMP) = WS(TEMP)-A(K, J)**2                              
            GOTO  15                                                    
  18     CONTINUE                                                       
         GOTO  4                                                        
  19  CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE QRQTB(M, N, QR, ALFA, PIVOT, NB, B, X)                 
      INTEGER M, N, NB                                                  
      INTEGER PIVOT(N)                                                  
      REAL QR(M, N), ALFA(N), B(M, NB), X(N, NB)                        
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, MIN0, I, J, JB, IS(1000)                          
      INTEGER IZ                                                        
      REAL SDOT, GAMMA, RS(1000), WS(1000)                              
      LOGICAL LS(1000)                                                  
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO SOLVE R*X = Q*B.                                                   
C MNEMONIC - QR Q*B AND BACK-SOLVE.                                     
C INPUT -                                                               
C   M     - THE NUMBER OF ROWS IN THE MATRIX.                           
C   N     - THE NUMBER OF COLUMNS IN THE MATRIX.                        
C   QR    - THE QR FACTORIZATION OF A MATRIX, AS DESCRIBED IN  QRD.     
C   ALFA  - THE DIAGONAL OF R, AS DESCRIBED IN  QRD.                    
C   PIVOT - THE PIVOTING ARRAY, AS DESCRIBED IN  QRD.                   
C   NB    - THE NUMBER OF RIGHT-HAND-SIDES.                             
C   B     - THE RIGHT-HAND-SIDES.                                       
C OUTPUT -                                                              
C   B - HAS BEEN CLOBBERED.                                             
C   X - THE SOLUTION VECTORS.                                           
C SCRATCH SPACE ALLOCATED - N*MU WORDS.                                 
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - M.LT.N.                                                         
C   3 - NB.LT.1.                                                        
C   4 - ALFA(J)=0.                                                      
C   5 - QR(J,J)=0.                                                      
C   6 - PIVOT(I) NOT ONE OF 1,...,N.                                    
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
C DEFINE Z(J) WS(IZ-1+J)                                                
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15H QRQTB - N.LT.1, 15, 1, 2)           
C     IF (M .LT. N) CALL SETERR(15H QRQTB - M.LT.N, 15, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(16H QRQTB - NB.LT.1, 16, 3, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' QRQTB - N.LT.1', 15, 1, 2)            
      IF (M .LT. N) CALL SETERR(' QRQTB - M.LT.N', 15, 2, 2)            
      IF (NB .LT. 1) CALL SETERR(' QRQTB - NB.LT.1', 16, 3, 2)          
C/                                                                      
      DO  1 J = 1, N                                                    
C/6S                                                                    
C        IF (ALFA(J) .EQ. 0.) CALL SETERR(18H QRQTB - ALFA(J)=0, 18, 4  
C    1      , 2)                                                        
C/7S                                                                    
         IF (ALFA(J) .EQ. 0.) CALL SETERR(' QRQTB - ALFA(J)=0', 18, 4   
     1      , 2)                                                        
C/                                                                      
   1     CONTINUE                                                       
      TEMP = MIN0(M, N)                                                 
      DO  2 J = 1, TEMP                                                 
C/6S                                                                    
C        IF (QR(J, J) .EQ. 0.) CALL SETERR(18H QRQTB - QR(J,J)=0, 18, 5,
C    1      2)                                                          
C/7S                                                                    
         IF (QR(J, J) .EQ. 0.) CALL SETERR(' QRQTB - QR(J,J)=0', 18, 5, 
     1      2)                                                          
C/                                                                      
   2     CONTINUE                                                       
      DO  3 I = 1, N                                                    
C/6S                                                                    
C        IF (PIVOT(I) .LT. 1 .OR. PIVOT(I) .GT. N) CALL SETERR(         
C    1      36H QRQTB - PIVOT(I) NOT ONE OF 1,...,N, 36, 6, 2)          
C/7S                                                                    
         IF (PIVOT(I) .LT. 1 .OR. PIVOT(I) .GT. N) CALL SETERR(         
     1      ' QRQTB - PIVOT(I) NOT ONE OF 1,...,N', 36, 6, 2)           
C/                                                                      
   3     CONTINUE                                                       
C FORM Q*B.                                                             
C MULTIPLY ALL THE VECTORS.                                             
      DO  6 JB = 1, NB                                                  
C APPLY THE J-TH TRANSFORMATION.                                        
         TEMP = MIN0(M, N)                                              
         DO  5 J = 1, TEMP                                              
            GAMMA = SDOT(M-J+1, QR(J, J), 1, B(J, JB), 1)/(ALFA(J)*QR(J,
     1         J))                                                      
            DO  4 I = J, M                                              
               B(I, JB) = B(I, JB)+GAMMA*QR(I, J)                       
   4           CONTINUE                                                 
   5        CONTINUE                                                    
   6     CONTINUE                                                       
C SOLVE R*X = Q*B.                                                      
      IZ = ISTKGT(N, 3)                                                 
C DO ALL THE RIGHT-HAND-SIDES.                                          
      DO  11 JB = 1, NB                                                 
         TEMP = IZ+N                                                    
         WS(TEMP-1) = B(N, JB)/ALFA(N)                                  
         I = N-1                                                        
            GOTO  8                                                     
   7        I = I-1                                                     
   8        IF (I .LT. 1) GOTO  9                                       
            TEMP = IZ-1+I                                               
            TEMP1 = IZ+I                                                
            WS(TEMP) = (-(SDOT(N-I, QR(I, I+1), M, WS(TEMP1), 1)-B(I,   
     1         JB)))/ALFA(I)                                            
            GOTO  7                                                     
   9     DO  10 I = 1, N                                                
            TEMP1 = PIVOT(I)                                            
            TEMP = IZ+I                                                 
            X(TEMP1, JB) = WS(TEMP-1)                                   
  10        CONTINUE                                                    
  11     CONTINUE                                                       
      CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSB(B, M, N, R, C, RC)                                
      INTEGER M, N                                                      
      INTEGER RC(N)                                                     
      REAL B(M, N), R(M), C(N)                                          
      INTEGER NERROR, I, NERR, IROLD                                    
      REAL L, S, R1MACH                                                 
      DATA S/0E0/                                                       
      DATA L/0E0/                                                       
C TO GET THE COLUMN SCALED MATRIX (1/R)*B*(1/C),                        
C GIVEN THE ROW SCALE FACTOR, AND RETURN THE COLUMN FACTOR.             
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16H RCSB - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16H RCSB - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' RCSB - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR(' RCSB - N .LT. 1', 16, 2, 2)           
C/                                                                      
      IF (S .NE. 0.) GOTO 1                                             
         S = R1MACH(1)                                                  
         L = R1MACH(2)                                                  
   1  DO  2 I = 1, M                                                    
         IF (R(I) .EQ. 0.) GOTO  2                                      
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      36H RCSB - MUST HAVE S .LE. R(I) .LE. L, 36, 3, 2)          
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      ' RCSB - MUST HAVE S .LE. R(I) .LE. L', 36, 3, 2)           
C/                                                                      
   2     CONTINUE                                                       
C TURN ERROR RECOVERY ON AND SAVE OLD VALUE.                            
      CALL ENTSRC(IROLD, 1)                                             
C GET COLUMN SCALE FACTOR.                                              
      CALL RCSC(B, M, N, R, C, RC)                                      
C APPLY THEM.                                                           
      CALL RCSS(B, M, N, R, C, RC)                                      
      IF (NERROR(NERR) .EQ. 0) GOTO 3                                   
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(42H RCSB - MUST HAVE 1/(S*L) IN MACHINE RANGE, 42  
C    1      , 4, 1)                                                     
C/7S                                                                    
         CALL SETERR(' RCSB - MUST HAVE 1/(S*L) IN MACHINE RANGE', 42   
     1      , 4, 1)                                                     
C/                                                                      
         RETURN                                                         
C RESTORE OLD RECOVERY VALUE.                                           
   3  CALL RETSRC(IROLD)                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSC(A, M, N, R, C, RC)                                
      INTEGER M, N                                                      
      INTEGER RC(N)                                                     
      REAL A(M, N), R(M), C(N)                                          
      INTEGER I, J, RD2                                                 
      REAL AIJ, ABS, AAIJ, L, S, D1                                     
      REAL D2, AMAX1, R1MACH                                            
      DATA S/0E0/                                                       
      DATA L/0E0/                                                       
C TO GET THE COLUMN SCALE FACTOR FOR (1/R)*A.                           
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16H RCSC - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16H RCSC - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' RCSC - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR(' RCSC - N .LT. 1', 16, 2, 2)           
C/                                                                      
      IF (S .NE. 0.) GOTO 1                                             
         S = R1MACH(1)                                                  
         L = R1MACH(2)                                                  
   1  DO  2 I = 1, M                                                    
         IF (R(I) .EQ. 0.) GOTO  2                                      
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      36H RCSC - MUST HAVE S .LE. R(I) .LE. L, 36, 3, 2)          
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      ' RCSC - MUST HAVE S .LE. R(I) .LE. L', 36, 3, 2)           
C/                                                                      
   2     CONTINUE                                                       
      DO  18 J = 1, N                                                   
         D2 = 0                                                         
C -1 = UNDERFLOW, 0 = IN-RANGE, +1 = OVERFLOW.                          
         RD2 = -1                                                       
         DO  17 I = 1, M                                                
            AIJ = A(I, J)                                               
            AAIJ = ABS(AIJ)                                             
            D1 = R(I)                                                   
            IF (AIJ .EQ. 0. .OR. D1 .EQ. 0.) GOTO  17                   
            IF (D1 .GE. 1.) GOTO 9                                      
               IF (AAIJ .LE. D1*L) GOTO 3                               
                  IF (RD2 .LT. 1) D2 = 0                                
C CHECK FOR OVERFLOW.                                                   
C OVERFLOW.                                                             
                  RD2 = 1                                               
                  D2 = AMAX1(D2, AAIJ*(S/D1))                           
                  GOTO  8                                               
   3              IF (RD2 .LE. 0) GOTO 4                                
                     GOTO  17                                           
C THIS ELEMENT IS IN-RANGE.                                             
C ALREADY OVERFLOWED, NO EFFECT.                                        
   4                 IF (RD2 .NE. 0) GOTO 5                             
                        D2 = AMAX1(D2, AAIJ/D1)                         
                        GOTO  6                                         
   5                    RD2 = 0                                         
C RD2 = -1.                                                             
                        D2 = AAIJ/D1                                    
   6              CONTINUE                                              
   7              CONTINUE                                              
   8           CONTINUE                                                 
               GOTO  16                                                 
   9           IF (AAIJ .GE. D1*S) GOTO 10                              
                  IF (RD2 .GE. 0) GOTO  17                              
C ELEMENT UNDERFLOW, D1 >= 1.                                           
C NO-EFFECT.                                                            
                  D2 = AMAX1(D2, AAIJ*(L/D1))                           
                  GOTO  15                                              
  10              IF (RD2 .LE. 0) GOTO 11                               
                     GOTO  17                                           
C IN-RANGE.                                                             
C NO-EFFECT.                                                            
  11                 IF (RD2 .NE. 0) GOTO 12                            
                        D2 = AMAX1(D2, AAIJ/D1)                         
                        GOTO  13                                        
  12                    RD2 = 0                                         
C UNDERFLOWED SO FAR.                                                   
                        D2 = AAIJ/D1                                    
  13              CONTINUE                                              
  14              CONTINUE                                              
  15        CONTINUE                                                    
  16        CONTINUE                                                    
  17        CONTINUE                                                    
         C(J) = D2                                                      
         RC(J) = RD2                                                    
  18     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSS(A, M, N, R, C, RC)                                
      INTEGER M, N                                                      
      INTEGER RC(N)                                                     
      REAL A(M, N), R(M), C(N)                                          
      INTEGER I, J, RD2                                                 
      REAL L, S, D1, D2, R1MACH                                         
      LOGICAL BADNGE                                                    
      DATA S/0E0/                                                       
      DATA L/0E0/                                                       
C TO SCALE ((1/R)*A)*(1/C).                                             
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16H RCSS - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16H RCSS - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' RCSS - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR(' RCSS - N .LT. 1', 16, 2, 2)           
C/                                                                      
      IF (S .NE. 0.) GOTO 1                                             
         S = R1MACH(1)                                                  
         L = R1MACH(2)                                                  
   1  DO  2 I = 1, M                                                    
         IF (R(I) .EQ. 0.) GOTO  2                                      
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      36H RCSS - MUST HAVE S .LE. R(I) .LE. L, 36, 3, 2)          
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      ' RCSS - MUST HAVE S .LE. R(I) .LE. L', 36, 3, 2)           
C/                                                                      
   2     CONTINUE                                                       
      DO  3 I = 1, N                                                    
         IF (C(I) .EQ. 0.) GOTO  3                                      
C/6S                                                                    
C        IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
C    1      36H RCSS - MUST HAVE S .LE. C(I) .LE. L, 36, 4, 2)          
C        IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
C    1      36H RCSS - MUST HAVE RC(I) IN (-1,0,+1), 36, 5, 2)          
C/7S                                                                    
         IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
     1      ' RCSS - MUST HAVE S .LE. C(I) .LE. L', 36, 4, 2)           
         IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
     1      ' RCSS - MUST HAVE RC(I) IN (-1,0,+1)', 36, 5, 2)           
C/                                                                      
   3     CONTINUE                                                       
C CHECK 1/(S*L) RANGE.                                                  
      BADNGE = .FALSE.                                                  
      IF (S*L .GT. 1.) GOTO 4                                           
         IF (1./L .GT. S*L) BADNGE = .TRUE.                             
         GOTO  5                                                        
   4     IF (S*L .GT. 1./S) BADNGE = .TRUE.                             
C S*L > 1.                                                              
C/6S                                                                    
C  5  IF (BADNGE) CALL SETERR(                                          
C    1   42H RCSX - MUST HAVE 1/(S*L) IN MACHINE RANGE, 42, 6, 1)       
C/7S                                                                    
   5  IF (BADNGE) CALL SETERR(                                          
     1   ' RCSX - MUST HAVE 1/(S*L) IN MACHINE RANGE', 42, 6, 1)        
C/                                                                      
      DO  33 I = 1, M                                                   
         D1 = R(I)                                                      
         IF (D1 .EQ. 0.) GOTO  33                                       
         DO  32 J = 1, N                                                
            D2 = C(J)                                                   
            RD2 = RC(J)                                                 
            IF (A(I, J) .NE. 0. .AND. D2 .NE. 0.) GOTO 6                
               GOTO  32                                                 
   6           IF (D1 .LT. 1.) GOTO 19                                  
                  IF (RD2 .LE. 0) GOTO 11                               
                     IF (D2 .LT. 1.) GOTO 7                             
                        A(I, J) = S*((A(I, J)/D1)/D2)                   
C D2 OVERFLOWED.                                                        
                        GOTO  10                                        
   7                    IF (D1*D2 .LT. 1.) GOTO 8                       
                           A(I, J) = S*(A(I, J)/(D1*D2))                
C D2 < 1.                                                               
                           GOTO  9                                      
   8                       A(I, J) = A(I, J)*(S/(D1*D2))                
   9                    CONTINUE                                        
  10                 CONTINUE                                           
                     GOTO  18                                           
  11                 IF (D2 .LT. 1.) GOTO 12                            
                        A(I, J) = (A(I, J)/D1)/D2                       
                        GOTO  17                                        
  12                    IF (RD2 .GE. 0) GOTO 15                         
                           IF (D2 .LT. 1./D1) GOTO 13                   
                              A(I, J) = A(I, J)*((L/D1)/D2)             
C D2 UNDERFLOWED.                                                       
                              GOTO  14                                  
  13                          A(I, J) = L*(A(I, J)/(D1*D2))             
  14                       CONTINUE                                     
                           GOTO  16                                     
  15                       A(I, J) = A(I, J)/(D1*D2)                    
C D2 < 1.                                                               
  16                 CONTINUE                                           
  17              CONTINUE                                              
  18              CONTINUE                                              
                  GOTO  30                                              
  19              IF (RD2 .LE. 0) GOTO 22                               
                     IF (D1*D2 .LT. 1.) GOTO 20                         
                        A(I, J) = S*(A(I, J)/(D1*D2))                   
C D1 < 1.                                                               
C D2 OVERFLOWED.                                                        
                        GOTO  21                                        
  20                    A(I, J) = A(I, J)*((S/D1)/D2)                   
  21                 CONTINUE                                           
                     GOTO  29                                           
  22                 IF (D2 .LT. 1.) GOTO 23                            
                        A(I, J) = A(I, J)/(D1*D2)                       
                        GOTO  28                                        
  23                    IF (RD2 .GE. 0) GOTO 26                         
                           IF (D1*D2 .GT. 1.) GOTO 24                   
                              A(I, J) = L*(A(I, J)/(D1*D2))             
C D2 UNDERFLOWED.                                                       
                              GOTO  25                                  
  24                          A(I, J) = A(I, J)*(L/(D1*D2))             
  25                       CONTINUE                                     
                           GOTO  27                                     
  26                       A(I, J) = (A(I, J)/D1)/D2                    
C D2 < 1.                                                               
  27                 CONTINUE                                           
  28              CONTINUE                                              
  29              CONTINUE                                              
  30        CONTINUE                                                    
  31        CONTINUE                                                    
  32        CONTINUE                                                    
  33     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSX(X, N, M, R, RR, C, RC)                            
      INTEGER M, N                                                      
      INTEGER RR(N), RC(M)                                              
      REAL X(N, M), R(N), C(M)                                          
      INTEGER I, J, RD2, RD3                                            
      REAL ABS, AXX, L, S, D2, D3                                       
      REAL AMIN1, AMAX1, SL, XX, R1MACH                                 
      LOGICAL BADNGE, OVELOW                                            
      DATA S/0E0/                                                       
      DATA L/0E0/                                                       
C TO SCALE                                                              
C   X = (1/R) * X * C.                                                  
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(16H RCSX - N .LT. 1, 16, 1, 2)          
C     IF (M .LT. 1) CALL SETERR(16H RCSX - M .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' RCSX - N .LT. 1', 16, 1, 2)           
      IF (M .LT. 1) CALL SETERR(' RCSX - M .LT. 1', 16, 2, 2)           
C/                                                                      
      IF (S .NE. 0.) GOTO 1                                             
         S = R1MACH(1)                                                  
         L = R1MACH(2)                                                  
   1  CONTINUE                                                          
      SL = S*L                                                          
      DO  2 I = 1, N                                                    
         IF (R(I) .EQ. 0.) GOTO  2                                      
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      36H RCSX - MUST HAVE S .LE. R(I) .LE. L, 36, 3, 2)          
C        IF (RR(I) .LT. (-1) .OR. RR(I) .GT. 1) CALL SETERR(            
C    1      36H RCSX - MUST HAVE RR(I) IN (-1,0,+1), 36, 4, 2)          
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      ' RCSX - MUST HAVE S .LE. R(I) .LE. L', 36, 3, 2)           
         IF (RR(I) .LT. (-1) .OR. RR(I) .GT. 1) CALL SETERR(            
     1      ' RCSX - MUST HAVE RR(I) IN (-1,0,+1)', 36, 4, 2)           
C/                                                                      
   2     CONTINUE                                                       
      DO  3 I = 1, M                                                    
         IF (C(I) .EQ. 0.) GOTO  3                                      
C/6S                                                                    
C        IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
C    1      36H RCSX - MUST HAVE S .LE. C(I) .LE. L, 36, 5, 2)          
C        IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
C    1      36H RCSX - MUST HAVE RC(I) IN (-1,0,+1), 36, 6, 2)          
C/7S                                                                    
         IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
     1      ' RCSX - MUST HAVE S .LE. C(I) .LE. L', 36, 5, 2)           
         IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
     1      ' RCSX - MUST HAVE RC(I) IN (-1,0,+1)', 36, 6, 2)           
C/                                                                      
   3     CONTINUE                                                       
C CHECK 1/(S*L) RANGE.                                                  
      BADNGE = .FALSE.                                                  
      IF (SL .GT. 1.) GOTO 4                                            
         IF (1./L .GT. SL) BADNGE = .TRUE.                              
         GOTO  5                                                        
   4     IF (SL .GT. 1./S) BADNGE = .TRUE.                              
C S*L > 1.                                                              
C/6S                                                                    
C  5  IF (BADNGE) CALL SETERR(                                          
C    1   42H RCSX - MUST HAVE 1/(S*L) IN MACHINE RANGE, 42, 7, 1)       
C/7S                                                                    
   5  IF (BADNGE) CALL SETERR(                                          
     1   ' RCSX - MUST HAVE 1/(S*L) IN MACHINE RANGE', 42, 7, 1)        
C/                                                                      
      OVELOW = .FALSE.                                                  
      DO  86 J = 1, M                                                   
         D3 = C(J)                                                      
         RD3 = RC(J)                                                    
C DO THE SCALING.                                                       
         DO  85 I = 1, N                                                
            D2 = R(I)                                                   
            RD2 = RR(I)                                                 
            XX = X(I, J)                                                
            AXX = ABS(XX)                                               
            IF (D2 .EQ. 0.) GOTO  85                                    
            BADNGE = .FALSE.                                            
            IF (XX .NE. 0. .AND. D3 .NE. 0.) GOTO 6                     
               X(I, J) = 0                                              
               GOTO  85                                                 
   6        IF (RD2 .NE. RD3) GOTO 26                                   
               IF (D3 .LT. D2) GOTO 16                                  
                  IF (D3 .GT. 1.) GOTO 7                                
                     IF (D3*AXX .GT. D2*L) BADNGE = .TRUE.              
C WORRY ABOUT OVERFLOW.                                                 
                     GOTO  10                                           
   7                 IF (D2 .GT. 1.) GOTO 8                             
                        IF (AXX .GT. D2*(L/D3)) BADNGE = .TRUE.         
C D3 > 1.                                                               
                        GOTO  9                                         
   8                    IF (AXX/D2 .GT. L/D3) BADNGE = .TRUE.           
C D2 > 1 & D3 > 1.                                                      
   9              CONTINUE                                              
  10              IF (BADNGE) GOTO 15                                   
                     IF (D3 .GT. AMIN1(D2, 1E0)*L) GOTO 11              
                        X(I, J) = X(I, J)*(D3/D2)                       
C IN-RANGE.                                                             
                        GOTO  14                                        
  11                    IF (D3 .LT. 1. .AND. AXX .GT. D2*L) GOTO 12     
                           X(I, J) = D3*(XX/D2)                         
C D3/D2 OVERFLOWS, IE, D3 > D2*L AND D2 < 1.                            
                           GOTO  13                                     
  12                       X(I, J) = (D3*XX)/D2                         
  13                    CONTINUE                                        
  14                 CONTINUE                                           
  15              CONTINUE                                              
                  GOTO  25                                              
  16              IF (D3 .LT. 1.) GOTO 17                               
                     IF (AXX .LT. (D2*S)/D3) X(I, J) = 0                
C D3 < D2, WORRY ABOUT UNDERFLOW.                                       
C D2 > D3 >= 1.                                                         
                     GOTO  20                                           
  17                 IF (D2 .LT. 1.) GOTO 18                            
                        IF (D3*AXX .LT. D2*S) X(I, J) = 0               
C D3 < 1.                                                               
                        GOTO  19                                        
  18                    IF (AXX .LT. (S/D3)*D2) X(I, J) = 0             
C D2, D3 < 1.                                                           
  19              CONTINUE                                              
  20              IF (X(I, J) .EQ. 0.) GOTO  85                         
C       IN-RANGE.                                                       
                  IF (D3 .LT. AMAX1(D2, 1E0)*S) GOTO 21                 
                     X(I, J) = X(I, J)*(D3/D2)                          
                     GOTO  24                                           
  21                 IF (D3 .GT. 1. .AND. AXX .LT. D2*S) GOTO 22        
                        X(I, J) = D3*(XX/D2)                            
C D3/D2 UNDERFLOWS, I.E., D3 < D2*S, D2 > 1 .                           
                        GOTO  23                                        
  22                    X(I, J) = (D3*XX)/D2                            
  23                 CONTINUE                                           
  24              CONTINUE                                              
  25           CONTINUE                                                 
               GOTO  83                                                 
  26           IF (RD2 .LE. 0 .OR. RD3 .GE. 0) GOTO 27                  
                  X(I, J) = 0                                           
                  GOTO  85                                              
C UNDERFLOW.                                                            
  27              IF (RD2 .GE. 0 .OR. RD3 .LE. 0) GOTO 28               
                     BADNGE = .TRUE.                                    
C OVERFLOW.                                                             
                     GOTO  81                                           
  28                 IF (RD2 .GE. 0 .OR. RD3 .NE. 0) GOTO 42            
                        IF (D3 .GT. 1.) GOTO 29                         
                           IF (D3*AXX .GT. D2) BADNGE = .TRUE.          
C WORRY ABOUT OVERFLOW.                                                 
                           GOTO  30                                     
  29                       IF (AXX .GT. D2/D3) BADNGE = .TRUE.          
C D3 > 1.                                                               
  30                    IF (BADNGE) GOTO 41                             
                           IF (D3 .GT. D2) GOTO 33                      
                              IF (D3 .GT. 1.) GOTO 31                   
                                 X(I, J) = ((D3*L)/D2)*XX               
C IN-RANGE.                                                             
                                 GOTO  32                               
  31                             X(I, J) = ((D3/D2)*L)*XX               
  32                          CONTINUE                                  
                              GOTO  40                                  
  33                          IF (D3 .LT. 1. .AND. AXX .GT. D2) GOTO 36 
                                 IF (AXX .LT. D2*S) GOTO 34             
                                    X(I, J) = D3*(L*(XX/D2))            
C D3 > D2.                                                              
C D3*(X*L/D2).                                                          
                                    GOTO  35                            
  34                                X(I, J) = D3*((XX*L)/D2)            
  35                             CONTINUE                               
                                 GOTO  39                               
  36                             IF (AXX .GT. D2*L) GOTO 37             
                                    X(I, J) = (D3*(XX/D2))*L            
C D3 < 1 & |X| > D2.                                                    
                                    GOTO  38                            
  37                                X(I, J) = ((D3/D2)*XX)*L            
  38                             CONTINUE                               
  39                          CONTINUE                                  
  40                       CONTINUE                                     
  41                    CONTINUE                                        
                        GOTO  80                                        
  42                    IF (RD2 .LE. 0 .OR. RD3 .NE. 0) GOTO 55         
                           IF (D3 .LT. 1.) GOTO 43                      
                              IF (AXX .LT. D2/D3) X(I, J) = 0           
C WORRY ABOUT UNDERFLOW.                                                
                              GOTO  44                                  
  43                          IF (D3*AXX .LT. D2) X(I, J) = 0           
  44                       IF (X(I, J) .EQ. 0.) GOTO  85                
C       IN-RANGE.                                                       
                           IF (D3 .LT. D2) GOTO 47                      
                              IF (D2 .GT. 1.) GOTO 45                   
                                 X(I, J) = ((S/D2)*D3)*XX               
                                 GOTO  46                               
  45                             X(I, J) = ((D3/D2)*S)*XX               
  46                          CONTINUE                                  
                              GOTO  54                                  
  47                          IF (D3 .GT. 1. .AND. AXX .LT. D2) GOTO 50 
                                 IF (D2 .GT. 1.) GOTO 48                
                                    X(I, J) = D3*(XX*(S/D2))            
C D3 < D2.                                                              
                                    GOTO  49                            
  48                                X(I, J) = D3*((XX/D2)*S)            
  49                             CONTINUE                               
                                 GOTO  53                               
  50                             IF (D3 .LT. S*D2) GOTO 51              
                                    X(I, J) = ((D3/D2)*XX)*S            
C D2 > D3 > 1 AND |X| < D2.                                             
                                    GOTO  52                            
  51                                X(I, J) = ((XX/D2)*D3)*S            
  52                             CONTINUE                               
  53                          CONTINUE                                  
  54                       CONTINUE                                     
                           GOTO  79                                     
  55                       IF (RD2 .NE. 0 .OR. RD3 .GE. 0) GOTO 66      
                              IF (D2 .LT. 1.) GOTO 56                   
                                 IF (AXX/D2 .LT. SL/D3) X(I, J) = 0     
C WORRY ABOUT UNDERFLOW.                                                
                                 GOTO  57                               
  56                             IF (AXX .LT. (SL/D3)*D2) X(I, J) = 0   
  57                          IF (X(I, J) .EQ. 0.) GOTO  85             
C       IN-RANGE.                                                       
                              IF (D2 .GT. 1.) GOTO 62                   
                                 IF (D3 .LT. D2*SL) GOTO 58             
                                    X(I, J) = ((D3/D2)/L)*XX            
                                    GOTO  61                            
  58                                IF (AXX .LT. SL) GOTO 59            
                                       X(I, J) = (D3/D2)*(XX/L)         
                                       GOTO  60                         
  59                                   X(I, J) = ((D3/D2)*XX)/L         
  60                             CONTINUE                               
  61                             CONTINUE                               
                                 GOTO  65                               
  62                             IF (AXX/D2 .LT. SL) GOTO 63            
                                    X(I, J) = ((XX/D2)/L)*D3            
C D2 > 1.                                                               
                                    GOTO  64                            
  63                                X(I, J) = ((XX/D2)*D3)/L            
  64                             CONTINUE                               
  65                          CONTINUE                                  
                              GOTO  78                                  
  66                          IF (D2 .LT. 1.) GOTO 67                   
                                 IF (AXX/D2 .GT. SL/D3) BADNGE = .TRUE. 
C RD2 == 0 & RD3 > 0, WORRY ABOUT OVERFLOW.                             
                                 GOTO  68                               
  67                             IF (AXX .GT. D2*(SL/D3)) BADNGE =      
     1                              .TRUE.                              
  68                          IF (BADNGE) GOTO 77                       
                                 IF (D2 .LT. 1.) GOTO 73                
                                    IF (D3/D2 .GT. SL) GOTO 69          
                                       X(I, J) = ((D3/D2)/S)*XX         
C IN-RANGE.                                                             
                                       GOTO  72                         
  69                                   IF (AXX .GT. SL) GOTO 70         
                                          X(I, J) = (D3/D2)*(XX/S)      
                                          GOTO  71                      
  70                                      X(I, J) = ((D3/D2)*XX)/S      
  71                                CONTINUE                            
  72                                CONTINUE                            
                                    GOTO  76                            
  73                                IF (AXX .GT. D2*SL) GOTO 74         
                                       X(I, J) = ((XX/D2)/S)*D3         
C D2 < 1.                                                               
                                       GOTO  75                         
  74                                   X(I, J) = ((XX/D2)*D3)/S         
  75                                CONTINUE                            
  76                             CONTINUE                               
  77                          CONTINUE                                  
  78                    CONTINUE                                        
  79                 CONTINUE                                           
  80              CONTINUE                                              
  81           CONTINUE                                                 
  82        CONTINUE                                                    
  83        IF (.NOT. BADNGE) GOTO 84                                   
               X(I, J) = L*(XX/AXX)                                     
               OVELOW = .TRUE.                                          
  84        CONTINUE                                                    
  85        CONTINUE                                                    
  86     CONTINUE                                                       
      IF (.NOT. OVELOW) GOTO 87                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(33H RCSX - SOLUTION X HAS OVERFLOWED, 33, 8, 1)    
C/7S                                                                    
         CALL SETERR(' RCSX - SOLUTION X HAS OVERFLOWED', 33, 8, 1)     
C/                                                                      
  87  RETURN                                                            
      END                                                               
      SUBROUTINE IODE4(V, NV, TSTART, TSTOP, DT, D, THETA, KEEJAC,      
     1   MINIT, MAXIT, USENGJ, USENNS, USENFD, BETA, GAMMA, DELTA, N,   
     2   MMAX, HFRACT, EGIVE, KMAX, XPOLY, KINIT, IRCS, ERROR, ERRPAR,  
     3   ERPUTS, INMI, DA, HANDLE)                                      
      INTEGER MMAX, NV                                                  
      EXTERNAL D, ERROR, INMI, DA, HANDLE                               
      INTEGER KEEJAC, MINIT, MAXIT, N(MMAX), KMAX, KINIT                
      INTEGER IRCS                                                      
      REAL V(NV), TSTART, TSTOP, DT, THETA, BETA                        
      REAL GAMMA, DELTA, HFRACT, EGIVE, ERRPAR(2)                       
      LOGICAL USENGJ, USENNS, USENFD, XPOLY, ERPUTS, DA                 
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A10DEY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL WV(30), RV(30)                                               
      LOGICAL LV(20)                                                    
      COMMON /A10DET/ TGOOD                                             
      REAL TGOOD                                                        
      COMMON /A1ODES/ IMEM                                              
      INTEGER IMEM                                                      
      COMMON /A10DER/ STATS                                             
      INTEGER STATS(9)                                                  
      COMMON /A1ODEM/ THETAC, EGIVEC, MINITC, MAXITC, KEEACC, IRCSC     
      INTEGER MINITC, MAXITC, KEEACC, IRCSC                             
      REAL THETAC, EGIVEC                                               
      COMMON /A1ODEL/ ERPTSC                                            
      LOGICAL ERPTSC                                                    
      COMMON /IODEJ/ AJ, BJ, GETACC, NEEUMC                             
      REAL AJ, BJ                                                       
      LOGICAL GETACC, NEEUMC                                            
      COMMON /A1ODEG/ TJ, DTJ, GETJAC, SEPATE, USEGJC, USENSC, USEFDC   
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE, USEGJC, USENSC, USEFDC                    
      COMMON /A1ODEF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /IODEF/ FAILED                                             
      LOGICAL FAILED                                                    
      EXTERNAL A1ODEE, A1ODEH, A1ODEP, A1ODEN                           
      INTEGER ISTKGT, NERROR, I, NERR, IS(1000), IVOLD                  
      REAL ABS, RS(1000), WS(1000)                                      
      LOGICAL LS(1000)                                                  
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE BOTTOM LEVEL OF IODES.                                            
C SCRATCH SPACE ALLOCATED -                                             
C     S( IODE3) = S(A1ODES) +                                           
C REAL WORDS +                                                          
C                 MMAX                                                  
C INTEGER WORDS.                                                        
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      IF (TSTART .EQ. TSTOP) RETURN                                     
      CALL ENTER(1)                                                     
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (NV .LE. 0) CALL SETERR(16H IODE4 - NV.LE.0, 16, 1, 2)         
C     IF (TSTART+DT .EQ. TSTART) CALL SETERR(                           
C    1   31H IODE4 - INPUT VALUE OF DT IS 0, 31, 2, 2)                  
C     IF ((DT/ABS(DT))*(TSTOP-TSTART) .LE. 0.) CALL SETERR(             
C    1   45H IODE4 - INPUT VALUE OF DT HAS THE WRONG SIGN, 45, 3, 2)    
C     IF (THETA .LT. 0. .OR. THETA .GT. 1.) CALL SETERR(                
C    1   27H IODE4 - THETA NOT IN (0,1), 27, 4, 2)                      
C     IF (KEEJAC .LT. 0 .OR. KEEJAC .GT. 5) CALL SETERR(                
C    1   37H IODE4 - KEEPJAC NOT ONE OF (0,...,5), 37, 5, 2)            
C     IF (MINIT .LT. 1) CALL SETERR(19H IODE4 - MINIT.LT.1, 19, 6, 2)   
C     IF (MAXIT .LT. 1) CALL SETERR(19H IODE4 - MAXIT.LT.1, 19, 7, 2)   
C     IF (KMAX .LT. 1) CALL SETERR(18H IODE4 - KMAX.LT.1, 18, 8, 2)     
C/7S                                                                    
      IF (NV .LE. 0) CALL SETERR(' IODE4 - NV.LE.0', 16, 1, 2)          
      IF (TSTART+DT .EQ. TSTART) CALL SETERR(                           
     1   ' IODE4 - INPUT VALUE OF DT IS 0', 31, 2, 2)                   
      IF ((DT/ABS(DT))*(TSTOP-TSTART) .LE. 0.) CALL SETERR(             
     1   ' IODE4 - INPUT VALUE OF DT HAS THE WRONG SIGN', 45, 3, 2)     
      IF (THETA .LT. 0. .OR. THETA .GT. 1.) CALL SETERR(                
     1   ' IODE4 - THETA NOT IN (0,1)', 27, 4, 2)                       
      IF (KEEJAC .LT. 0 .OR. KEEJAC .GT. 5) CALL SETERR(                
     1   ' IODE4 - KEEPJAC NOT ONE OF (0,...,5)', 37, 5, 2)             
      IF (MINIT .LT. 1) CALL SETERR(' IODE4 - MINIT.LT.1', 19, 6, 2)    
      IF (MAXIT .LT. 1) CALL SETERR(' IODE4 - MAXIT.LT.1', 19, 7, 2)    
      IF (KMAX .LT. 1) CALL SETERR(' IODE4 - KMAX.LT.1', 18, 8, 2)      
C/                                                                      
      TEMP = KINIT .LT. 1                                               
      IF (.NOT. TEMP) TEMP = KINIT .GT. KMAX                            
C/6S                                                                    
C     IF (TEMP) CALL SETERR(34H IODE4 - KINIT NOT IN (1,...,KMAX), 34, 9
C    1   , 2)                                                           
C     IF (BETA .LE. 0.) CALL SETERR(19H IODE4 - BETA .LE.0, 19, 10, 2)  
C     IF (GAMMA .LE. 0.) CALL SETERR(20H IODE4 - GAMMA .LE.0, 20, 11, 2)
C     IF (DELTA .LT. 0.) CALL SETERR(20H IODE4 - DELTA .LT.0, 20, 12, 2)
C     IF (BETA+GAMMA-DELTA .LE. 0.) CALL SETERR(                        
C    1   30H IODE4 - BETA+GAMMA-DELTA.LE.0, 30, 13, 2)                  
C     IF (MMAX .LT. KMAX+2) CALL SETERR(23H IODE4 - MMAX.LT.KMAX+2, 23  
C    1   , 14, 2)                                                       
C     IF (N(1) .LT. 1) CALL SETERR(18H IODE4 - N(1).LT.1, 18, 15, 2)    
C/7S                                                                    
      IF (TEMP) CALL SETERR(' IODE4 - KINIT NOT IN (1,...,KMAX)', 34, 9 
     1   , 2)                                                           
      IF (BETA .LE. 0.) CALL SETERR(' IODE4 - BETA .LE.0', 19, 10, 2)   
      IF (GAMMA .LE. 0.) CALL SETERR(' IODE4 - GAMMA .LE.0', 20, 11, 2) 
      IF (DELTA .LT. 0.) CALL SETERR(' IODE4 - DELTA .LT.0', 20, 12, 2) 
      IF (BETA+GAMMA-DELTA .LE. 0.) CALL SETERR(                        
     1   ' IODE4 - BETA+GAMMA-DELTA.LE.0', 30, 13, 2)                   
      IF (MMAX .LT. KMAX+2) CALL SETERR(' IODE4 - MMAX.LT.KMAX+2', 23   
     1   , 14, 2)                                                       
      IF (N(1) .LT. 1) CALL SETERR(' IODE4 - N(1).LT.1', 18, 15, 2)     
C/                                                                      
      DO  1 I = 2, MMAX                                                 
C/6S                                                                    
C        IF (N(I) .LE. N(I-1)) CALL SETERR(                             
C    1      37H IODE4 - N IS NOT MONOTONE INCREASING, 37, 16, 2)        
C/7S                                                                    
         IF (N(I) .LE. N(I-1)) CALL SETERR(                             
     1      ' IODE4 - N IS NOT MONOTONE INCREASING', 37, 16, 2)         
C/                                                                      
   1     CONTINUE                                                       
C/6S                                                                    
C     IF (HFRACT .LE. 0.) CALL SETERR(20H IODE4 - HFRACT.LE.0, 20, 17, 2
C    1   )                                                              
C     IF (EGIVE .LT. 1.) CALL SETERR(21H IODE4 - EGIVE .LT. 1, 21, 18, 2
C    1   )                                                              
C/7S                                                                    
      IF (HFRACT .LE. 0.) CALL SETERR(' IODE4 - HFRACT.LE.0', 20, 17, 2 
     1   )                                                              
      IF (EGIVE .LT. 1.) CALL SETERR(' IODE4 - EGIVE .LT. 1', 21, 18, 2 
     1   )                                                              
C/                                                                      
      ERPTSC = ERPUTS                                                   
      EGIVEC = EGIVE                                                    
      IRCSC = IRCS                                                      
      THETAC = THETA                                                    
      MINITC = MINIT                                                    
      MAXITC = MAXIT                                                    
      KEEACC = KEEJAC                                                   
      IF (KEEJAC .EQ. 1 .AND. MAXIT .EQ. 1) KEEACC = 0                  
C SAME AS L.B.E.                                                        
      SEPATE = KEEACC .GT. 1                                            
      USEGJC = USENGJ                                                   
      USENSC = USENNS                                                   
      USEFDC = USENFD                                                   
      IF (KEEACC .LT. 3) GOTO 2                                         
         GETJAC = .TRUE.                                                
         TJ = TSTART                                                    
         GOTO  5                                                        
   2     IF (KEEJAC .NE. 2) GOTO 3                                      
            TJ = TSTOP                                                  
C CANNOT BE TSTART.                                                     
            GOTO  4                                                     
   3        TJ = TSTART                                                 
C CANNOT BE TSTART+THETA*DT/N.                                          
   4     CONTINUE                                                       
   5  DTJ = 0                                                           
C START WITH NO ERROR STATES.                                           
      FNUM = 0                                                          
C FLAG DA WORK-SPACE AS UN-ALLOCATED.                                   
      IMEM = 0                                                          
C GET SPACE FOR DA.                                                     
      IF (DA(V, V, NV, TSTART, DT, D, V, TJ, DTJ, GETJAC, SEPATE,       
     1   USENGJ, USENNS, USENFD, 0, -1, FNUM, THETA, IRCS, KEEACC))     
     2   CONTINUE                                                       
C/6S                                                                    
C     IF (IMEM .LE. 0) CALL SETERR(                                     
C    1   51H IODE4 - IODED FAILED TO INITIALIZE COMMON /A1ODES/, 52, 19,
C    2   2)                                                             
C/7S                                                                    
      IF (IMEM .LE. 0) CALL SETERR(                                     
     1   ' IODE4 - IODED FAILED TO INITIALIZE COMMON /A1ODES/', 52, 19, 
     2   2)                                                             
C/                                                                      
      TGOOD = TSTART                                                    
      IVOLD = ISTKGT(NV, 3)                                             
      CALL MOVEFR(NV, V, WS(IVOLD))                                     
C TELL STATS ROUTINE IN IODE.                                           
      CALL A1ODEX(STATS, 1)                                             
      CALL A10DEX(TSTART, TSTOP, A1ODEP, A1ODEN, DA, D, BETA, GAMMA,    
     1   DELTA, WS(IVOLD), NV, DT, N, KMAX, MMAX, XPOLY, A1ODEE, ERROR  
     2   , ERRPAR, INMI, A1ODEH, HANDLE, 0.9E0, HFRACT, KINIT)          
C TELL STATS ROUTINE OUT OF IODE.                                       
      CALL A1ODEX(STATS, -1)                                            
      CALL MOVEFR(NV, WS(IVOLD), V)                                     
      TSTOP = TGOOD                                                     
C CAPTURE THE ERROR NUMBER, IF ANY.                                     
      NERR = NERROR(NERR)                                               
      IF (NERR .NE. 15) GOTO 6                                          
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(13H IODE4 - DT=0, 13, 1000, 1)                     
C/7S                                                                    
         CALL SETERR(' IODE4 - DT=0', 13, 1000, 1)                      
C/                                                                      
   6  IF (NERR .NE. 16) GOTO 7                                          
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(32H IODE4 - DT=0 RETURNED BY HANDLE, 32, 1001, 1)  
C/7S                                                                    
         CALL SETERR(' IODE4 - DT=0 RETURNED BY HANDLE', 32, 1001, 1)   
C/                                                                      
   7  IF (NERR .NE. 17) GOTO 8                                          
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(45H IODE4 - DT RETURNED BY HANDLE HAS WRONG SIGN,  
C    1      45, 1002, 1)                                                
C/7S                                                                    
         CALL SETERR(' IODE4 - DT RETURNED BY HANDLE HAS WRONG SIGN',   
     1      45, 1002, 1)                                                
C/                                                                      
   8  IF (NERR .NE. 18) GOTO 9                                          
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(46H IODE4 - CANNOT RAISE DT IN HANDLE WHEN FAILED  
C    1      , 46, 1003, 1)                                              
C/7S                                                                    
         CALL SETERR(' IODE4 - CANNOT RAISE DT IN HANDLE WHEN FAILED'   
     1      , 46, 1003, 1)                                              
C/                                                                      
   9  IF (NERR .NE. 19) GOTO 10                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(36H IODE4 - E(I).LE.0 RETURNED BY ERROR, 36, 1004  
C    1      , 1)                                                        
C/7S                                                                    
         CALL SETERR(' IODE4 - E(I).LE.0 RETURNED BY ERROR', 36, 1004   
     1      , 1)                                                        
C/                                                                      
  10  IF (NERR .NE. 15) GOTO 15                                         
         IF (FNUM .NE. 1) GOTO 11                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(18H IODE4 - D FAILURE, 18, 1005, 1)             
C/7S                                                                    
            CALL SETERR(' IODE4 - D FAILURE', 18, 1005, 1)              
C/                                                                      
  11     IF (FNUM .NE. 2) GOTO 12                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(26H IODE4 - SINGULAR JACOBIAN, 26, 1006, 1)     
C/7S                                                                    
            CALL SETERR(' IODE4 - SINGULAR JACOBIAN', 26, 1006, 1)      
C/                                                                      
  12     IF (FNUM .NE. 3) GOTO 13                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(                                                
C    1         45H IODE4 - TOO MANY NEWTON ITERATIONS PREDICTED, 45,    
C    2         1007, 1)                                                 
C/7S                                                                    
            CALL SETERR(                                                
     1         ' IODE4 - TOO MANY NEWTON ITERATIONS PREDICTED', 45,     
     2         1007, 1)                                                 
C/                                                                      
  13     IF (FNUM .NE. 4) GOTO 14                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(42H IODE4 - TOO MANY NEWTON ITERATIONS NEEDED,  
C    1         42, 1008, 1)                                             
C/7S                                                                    
            CALL SETERR(' IODE4 - TOO MANY NEWTON ITERATIONS NEEDED',   
     1         42, 1008, 1)                                             
C/                                                                      
  14     CONTINUE                                                       
  15  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE A10DEX(TSTART, TSTOP, XA, F, DA, D, BETA, GAMMA        
     1   , DELTA, X, NX, DT, N, KMAX, MMAX, XPOLY, SERROR, ERROR,       
     2   ERRPAR, INMI, SOUT, OUTPUT, PESPAR, HFRACT, KINIT)             
      INTEGER MMAX, NX                                                  
      EXTERNAL XA, F, DA, D, SERROR, ERROR                              
      EXTERNAL INMI, SOUT, OUTPUT                                       
      INTEGER N(MMAX), KMAX, KINIT                                      
      REAL TSTART, TSTOP, BETA, GAMMA, DELTA, X(NX)                     
      REAL DT, ERRPAR(2), PESPAR, HFRACT                                
      LOGICAL XPOLY, SERROR                                             
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A10DEY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL WV(30), RV(30)                                               
      LOGICAL LV(20)                                                    
      COMMON /A1ODEG/ TJ, DTJ, GETJAC, SEPATE, USENGJ, USENNS, USENFD   
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE, USENGJ, USENNS, USENFD                    
      COMMON /A1ODEM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC, IRCS          
      INTEGER MINIT, MAXIT, KEEJAC, IRCS                                
      REAL THETA, EGIVE                                                 
      INTEGER NERROR, M, NERR, IE, IS(1000), IX1                        
      REAL T0, T1, RS(1000), WS(1000)                                   
      LOGICAL A4SSOR, A4SSOX, DONE, OK, LS(1000), A4SSOE                
      LOGICAL A4SSOI, A4SSOM                                            
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      EQUIVALENCE (WV(10), T0)                                          
      EQUIVALENCE (WV(11), T1)                                          
      EQUIVALENCE (IV(12), IX1)                                         
      EQUIVALENCE (IV(22), IE)                                          
      EQUIVALENCE (LV(2), OK)                                           
      EQUIVALENCE (LV(7), DONE)                                         
C SCRATCH SPACE ALLOCATED -                                             
C     S(A10DEX) <= 2*MMAX + 1 + NX*(KMAX+1) +                           
C     ( 5*KMAX + 2*MMAX + 3 ) INTEGER +                                 
C     MAX ( S(XA), NX*(KMAX+1) REAL +                                   
C           MAX ( KMAX + KMAX INTEGER, S(SERROR) ),                     
C           NX REAL + S(SOUT) )                                         
C REAL.                                                                 
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      CALL ENTER(1)                                                     
      IF (.NOT. A4SSOI(WV, RV, IV, LV, TSTART, TSTOP, BETA, GAMMA,      
     1   DELTA, NX, DT, N, KMAX, MMAX, XPOLY, ERRPAR, PESPAR, HFRACT,   
     2   KINIT)) GOTO 1                                                 
         CALL LEAVE                                                     
         RETURN                                                         
   1  IF (T0 .EQ. TSTOP) GOTO  7                                        
C TAKE THE TIME-STEPS.                                                  
         IF (KEEJAC .NE. 2) GOTO 2                                      
            GETJAC = T0 .NE. TJ                                         
            TJ = T0                                                     
C BUILD THE EXTRAPLOATION LOZENGE.                                      
   2     DO  5 M = 1, MMAX                                              
C GET XA((T1-T0)/N(M)).                                                 
            OK = .TRUE.                                                 
            CALL XA(T0, X, T1, WS(IX1), NX, N(M), F, D, DA, OK, ERROR,  
     1         ERRPAR, INMI)                                            
            IF (OK) GOTO 4                                              
               IF (NERROR(NERR) .EQ. 0) GOTO 3                          
                  CALL LEAVE                                            
                  RETURN                                                
   3           CONTINUE                                                 
C     EXTRAPOLATE THE RESULTS.                                          
   4        IF (A4SSOX(WV, RV, IV, LV, N, M)) GOTO  6                   
            IF (M .GT. 1) DONE = SERROR(WS(IX1), NX, T1, DT, ERRPAR,    
     1         DELTA, RS(IE), ERROR)                                    
C CHECK FOR CONVERGENCE.                                                
C     CHECK FOR A RESTART.                                              
            IF (A4SSOR(WV, RV, IV, LV, ERRPAR)) GOTO  6                 
   5        CONTINUE                                                    
C   GET OPTIMAL DT AND ORDER ( LOZENGE SIZE ).                          
   6     IF (A4SSOM(WV, RV, IV, LV, DT)) GOTO  7                        
C   OUTPUT THE RESULTS FOR THIS TIME-STEP.                              
         CALL SOUT(T0, X, T1, WS(IX1), NX, DT, TSTOP, RS(IE), OK,       
     1      OUTPUT)                                                     
C   WIND-UP THIS TIME-STEP.                                             
         IF (A4SSOE(WV, RV, IV, LV, X, TSTOP, DT)) GOTO  7              
         GOTO  1                                                        
   7  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A1ODEE(V, NV, T, DT, ERRPAR, DELTA, EV,          
     1   ERROR)                                                         
      INTEGER NV                                                        
      EXTERNAL ERROR                                                    
      REAL V(NV), T, DT, ERRPAR(2), DELTA, EV(NV)                       
      LOGICAL ERROR                                                     
      LOGICAL ERPUTS                                                    
C THE ERROR FILTER FOR NLEQS.                                           
C SCRATCH SPACE ALLOCATED - S(A1ODEE) = S(ERROR).                       
      ERPUTS = DELTA .EQ. 1.                                            
      A1ODEE = ERROR(V, NV, T, DT, ERRPAR, ERPUTS, EV)                  
      RETURN                                                            
      END                                                               
      SUBROUTINE A1ODEH(T0, V0, T1, V1, NV, DT, TSTOP, EV, OK,          
     1   HANDLE)                                                        
      INTEGER NV                                                        
      EXTERNAL HANDLE                                                   
      REAL T0, V0(NV), T1, V1(NV), DT, TSTOP                            
      REAL EV(NV)                                                       
      LOGICAL OK                                                        
      COMMON /A1ODEF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /A10DER/ NJS, NFS, NTSS, NSSS, NES, NNITS, NNDS, NNFS, NRS 
      INTEGER NJS, NFS, NTSS, NSSS, NES, NNITS                          
      INTEGER NNDS, NNFS, NRS                                           
      COMMON /A1ODEM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC, IRCS          
      INTEGER MINIT, MAXIT, KEEJAC, IRCS                                
      REAL THETA, EGIVE                                                 
      COMMON /A1ODEG/ TJ, DTJ, GETJAC, SEPATE, USENGJ, USENNS, USENFD   
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE, USENGJ, USENNS, USENFD                    
      COMMON /A10DET/ TGOOD                                             
      REAL TGOOD                                                        
C OUTPUT FILTER FOR IODES.                                              
C SCRATCH SPACE ALLOCATED - S(A1ODEH) = S(HANDLE).                      
      IF (T0 .EQ. T1) GOTO 1                                            
         FNUM = 0                                                       
         TGOOD = T1                                                     
         GOTO  2                                                        
   1     NRS = NRS+1                                                    
   2  IF (T0 .NE. T1 .OR. KEEJAC .NE. 3) GOTO 3                         
         GETJAC = T0 .NE. TJ                                            
         TJ = T0                                                        
   3  NTSS = NTSS+1                                                     
      CALL HANDLE(T0, V0, T1, V1, NV, DT, TSTOP)                        
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A1ODEN(V, NV, T, DT, D, DA, ERROR, INMI,         
     1   ERRPAR)                                                        
      INTEGER NV                                                        
      EXTERNAL D, DA, ERROR, INMI                                       
      REAL V(NV), T, DT, ERRPAR(2)                                      
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A1ODEM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC, IRCS          
      INTEGER MINIT, MAXIT, KEEJAC, IRCS                                
      REAL THETA, EGIVE                                                 
      INTEGER IDV, IEV, IVT, ISTKGT, IEV1, IEV2                         
      INTEGER IVTETA, IS(1000), IVOLD                                   
      REAL RS(1000), WS(1000)                                           
      LOGICAL DONE, LS(1000), A1ODEO                                    
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C NONLINEAR EQUATION SOLVER FOR IODES.                                  
C SCRATCH SPACE ALLOCATED -                                             
C     S(A1ODEN) =  4*NV +                                               
C                 3*NV REAL +                                           
C                 MAX ( S(DA), S(ERROR) )                               
C REAL WORDS.                                                           
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      CALL ENTER(1)                                                     
      IVOLD = ISTKGT(4*NV, 3)                                           
      IVTETA = IVOLD+NV                                                 
      IVT = IVTETA+NV                                                   
      IDV = IVT+NV                                                      
      IEV = ISTKGT(3*NV, 3)                                             
      IEV1 = IEV+NV                                                     
      IEV2 = IEV1+NV                                                    
      DONE = A1ODEO(V, NV, T, DT, D, DA, ERROR, INMI, ERRPAR, WS(IVTETA)
     1   , WS(IVT), WS(IVOLD), RS(IEV), RS(IEV1), RS(IEV2), WS(IDV),    
     2   THETA, MINIT, MAXIT, KEEJAC, IRCS, EGIVE)                      
      CALL LEAVE                                                        
      A1ODEN = DONE                                                     
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A1ODEO(V, NV, T, DT, D, DA, ERROR, INMI,         
     1   ERRPAR, VTHETA, VT, VOLD, EV, EV1, EV2, DV, THETA, MINIT,      
     2   MAXIT, KEEJAC, IRCS, EGIVE)                                    
      INTEGER NV                                                        
      EXTERNAL D, DA, ERROR, INMI                                       
      INTEGER MINIT, MAXIT, KEEJAC, IRCS                                
      REAL V(NV), T, DT, ERRPAR(2), VTHETA(NV), VT(NV)                  
      REAL VOLD(NV), EV(NV), EV1(NV), EV2(NV), DV(NV), THETA            
      REAL EGIVE                                                        
      LOGICAL DA, ERROR                                                 
      COMMON /A10DER/ NJS, NFS, NTSS, NSSS, NES, NNITS, NNDS, NNFS, NRS 
      INTEGER NJS, NFS, NTSS, NSSS, NES, NNITS                          
      INTEGER NNDS, NNFS, NRS                                           
      COMMON /A1ODEG/ TJ, DTJ, GETJAC, SEPATE, USENGJ, USENNS, USENFD   
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE, USENGJ, USENNS, USENFD                    
      COMMON /A1ODEL/ ERPUTS                                            
      LOGICAL ERPUTS                                                    
      COMMON /A1ODEF/ FNUM                                              
      INTEGER FNUM                                                      
      INTEGER I, J, ITER                                                
      REAL ABS, RHO, PROD, TEMP, POWER                                  
      LOGICAL DONE                                                      
      INTEGER TEMP1                                                     
      LOGICAL TEMP2                                                     
      CALL MOVEFR(NV, V, VOLD)                                          
      CALL SETR(NV, 0E0, VT)                                            
C GET INITIAL NEWTON METHOD GUESS.                                      
      CALL INMI(NV, T, DT, VOLD, V, VT)                                 
      DO  23 ITER = 1, MAXIT                                            
         IF (KEEJAC .NE. 0) GOTO 1                                      
            GETJAC = .TRUE.                                             
            TJ = T                                                      
   1     IF (GETJAC) NJS = NJS+1                                        
         NNITS = NNITS+1                                                
         DO  2 I = 1, NV                                                
            VTHETA(I) = THETA*(V(I)-VOLD(I))+VOLD(I)                    
   2        CONTINUE                                                    
         IF (GETJAC) DTJ = 0                                            
         DONE = DA(VTHETA, VT, NV, T, DT, D, DV, TJ, DTJ, GETJAC,       
     1      SEPATE, USENGJ, USENNS, USENFD, NES, NFS, FNUM, THETA, IRCS,
     2      KEEJAC)                                                     
         IF (.NOT. DONE) GOTO 3                                         
            DONE = .FALSE.                                              
            GOTO  24                                                    
   3     GETJAC = .FALSE.                                               
         DTJ = DT                                                       
         DO  4 I = 1, NV                                                
            V(I) = V(I)+DV(I)                                           
            EV(I) = EGIVE*ABS(DV(I))                                    
   4        CONTINUE                                                    
         IF (MAXIT .NE. 1) GOTO 5                                       
            DONE = .TRUE.                                               
            GOTO  24                                                    
   5     CALL MOVEFR(NV, EV, EV2)                                       
         DONE = ERROR(V, NV, T, DT, ERRPAR, ERPUTS, EV)                 
C CHECK FOR NEGATIVE ERROR REQUESTS.                                    
         DO  7 I = 1, NV                                                
            TEMP2 = EV(I) .EQ. 0.                                       
            IF (TEMP2) TEMP2 = EV2(I) .NE. 0.                           
            IF (.NOT. TEMP2) TEMP2 = EV(I) .LT. 0.                      
            IF (.NOT. TEMP2) GOTO 6                                     
C/6S                                                                    
C              CALL SETERR(37H ESSOM - E(I).LE.0 RETURNED BY SERROR, 37,
C    1            19, 1)                                                
C/7S                                                                    
               CALL SETERR(' ESSOM - E(I).LE.0 RETURNED BY SERROR', 37, 
     1            19, 1)                                                
C/                                                                      
               A1ODEO = .FALSE.                                         
               RETURN                                                   
   6        CONTINUE                                                    
   7        CONTINUE                                                    
         IF (.NOT. DONE) GOTO 8                                         
            GOTO  24                                                    
   8        IF (ITER .NE. MAXIT) GOTO 9                                 
               FNUM = 4                                                 
               NNFS = NNFS+1                                            
               GOTO  24                                                 
   9     CONTINUE                                                       
  10     DO  21 I = 1, NV                                               
            IF (ITER .LE. MINIT .OR. EV(I) .GE. EV2(I)) GOTO 20         
               IF (EV1(I) .LE. EV2(I)) GOTO 11                          
                  RHO = EV2(I)/EV1(I)                                   
C CAN CHECK CONVERGENCE RATE.                                           
                  GOTO  12                                              
  11              RHO = 1                                               
  12           IF (RHO .LT. 1.) GOTO 13                                 
                  NNDS = NNDS+1                                         
C DIVERGING.                                                            
                  FNUM = 3                                              
                  A1ODEO = DONE                                         
                  RETURN                                                
  13           IF (KEEJAC .NE. 0) GOTO 17                               
                  PROD = 1                                              
C CHECK QUADRATIC CONVERGENCE RATE.                                     
                  POWER = RHO**2                                        
C < 1.                                                                  
                  TEMP = EV(I)/EV2(I)                                   
                  TEMP1 = MAXIT-ITER                                    
                  DO  14 J = 1, TEMP1                                   
                     PROD = PROD*POWER                                  
                     POWER = POWER**2                                   
                     IF (PROD .LE. TEMP) GOTO  15                       
  14                 CONTINUE                                           
  15              IF (PROD .LE. TEMP) GOTO 16                           
                     NNDS = NNDS+1                                      
C SLOW CONVERGENCE.                                                     
                     FNUM = 3                                           
                     A1ODEO = DONE                                      
                     RETURN                                             
  16              CONTINUE                                              
                  GOTO  19                                              
  17              IF (RHO**(MAXIT-ITER)*EV2(I) .LE. EV(I)) GOTO 18      
                     NNDS = NNDS+1                                      
C CHECK LINEAR CONVERGENCE RATE.                                        
C SLOW CONVERGENCE.                                                     
                     FNUM = 3                                           
                     A1ODEO = DONE                                      
                     RETURN                                             
  18              CONTINUE                                              
  19           CONTINUE                                                 
  20        EV1(I) = EV2(I)                                             
  21        CONTINUE                                                    
         DO  22 I = 1, NV                                               
            VT(I) = VT(I)+DV(I)/DT                                      
  22        CONTINUE                                                    
  23     CONTINUE                                                       
  24  A1ODEO = DONE                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE A1ODEP(T0, V0, T1, V1, NV, N, NLEQS, D, DA, OK,        
     1   ERROR, ERRPAR, INMI)                                           
      INTEGER NV                                                        
      EXTERNAL NLEQS, D, DA, ERROR, INMI                                
      INTEGER N                                                         
      REAL T0, V0(NV), T1, V1(NV), ERRPAR(2)                            
      LOGICAL NLEQS, DA, OK                                             
      COMMON /A10DEY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL WV(30), RV(30)                                               
      LOGICAL LV(20)                                                    
      COMMON /A1ODEG/ TJ, DTJ, GETJAC, SEPATE, USENGJ, USENNS, USENFD   
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE, USENGJ, USENNS, USENFD                    
      COMMON /A1ODEM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC, IRCS          
      INTEGER MINIT, MAXIT, KEEJAC, IRCS                                
      REAL THETA, EGIVE                                                 
      COMMON /A1ODES/ IMEM                                              
      INTEGER IMEM                                                      
      COMMON /A10DER/ NJS, NFS, NTSS, NSSS, NES, NNITS, NNDS, NNFS, NRS 
      INTEGER NJS, NFS, NTSS, NSSS, NES, NNITS                          
      INTEGER NNDS, NNFS, NRS                                           
      COMMON /A1ODEF/ FNUM                                              
      INTEGER FNUM                                                      
      INTEGER ISTEP                                                     
      REAL TSTART, T, DT, FLOAT                                         
      EQUIVALENCE (TSTART, WV(1))                                       
C TIME-STEPPING SCHEME FOR IODES.                                       
C SCRATCH SPACE ALLOCATED -                                             
C     S(A1ODEP) = NV**2 + S(NLEQS).                                     
C REAL WORDS.                                                           
      CALL ENTER(1)                                                     
      DT = (T1-T0)/FLOAT(N)                                             
C INITIAL APPROXIMATION FOR V1.                                         
      CALL MOVEFR(NV, V0, V1)                                           
      IF (DA(V0, V1, NV, T1, DT, D, V1, TJ, DTJ, GETJAC, SEPATE, USENGJ,
     1   USENNS, USENFD, 0, -2, FNUM, THETA, IRCS, KEEJAC)) CONTINUE    
      DO  3 ISTEP = 1, N                                                
         T = T0+(FLOAT(ISTEP-1)+THETA)*DT                               
         NSSS = NSSS+1                                                  
         IF (KEEJAC .NE. 1) GOTO 1                                      
            GETJAC = .TRUE.                                             
            TJ = T                                                      
   1     IF (DT .GT. 0. .AND. T .GT. T1 .OR. DT .LT. 0. .AND. T .LT. T1)
     1       T = T1                                                     
         OK = NLEQS(V1, NV, T, DT, D, DA, ERROR, INMI, ERRPAR)          
         IF (FNUM .LT. 3 .OR. KEEJAC .NE. 4) GOTO 2                     
            GETJAC = T0 .NE. TJ                                         
            TJ = T0                                                     
   2     IF (.NOT. OK) GOTO  4                                          
   3     CONTINUE                                                       
   4  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE IODEX                                                  
      COMMON /A10DER/ STATS                                             
      INTEGER STATS(9)                                                  
      INTEGER I1MACH                                                    
      INTEGER TEMP                                                      
C TO PRINT THE RUN-TIME STATISTICS FOR IODE.                            
      CALL A1ODEX(STATS, 0)                                             
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  1) STATS                                            
   1  FORMAT (32H IODE(J,F,TS,SS,E,NIT,ND,NF,R) =, 9(I5))               
      RETURN                                                            
      END                                                               
      SUBROUTINE A1ODEX(XSTATS, IFLAG)                                  
      INTEGER XSTATS(9), IFLAG                                          
      INTEGER STATS(9)                                                  
      LOGICAL INPOST                                                    
      DATA STATS(1)/0/                                                  
      DATA STATS(2)/0/                                                  
      DATA STATS(3)/0/                                                  
      DATA STATS(4)/0/                                                  
      DATA STATS(5)/0/                                                  
      DATA STATS(6)/0/                                                  
      DATA STATS(7)/0/                                                  
      DATA STATS(8)/0/                                                  
      DATA STATS(9)/0/                                                  
      DATA INPOST/.FALSE./                                              
C INTERNAL SAVING OF STATISTICS FOR IODE.                               
C FOR IFLAG = 0, THE STATS ARE SIMPLY REPORTED.                         
C FOR IFLAG > 0, IT ENTERS IODE.                                        
C FOR IFLAG < 0, IT EXITS IODE.                                         
      IF (IFLAG .NE. 0) GOTO 1                                          
         IF (.NOT. INPOST) CALL MOVEFI(9, STATS, XSTATS)                
         GOTO  4                                                        
   1     IF (IFLAG .LE. 0) GOTO 2                                       
            INPOST = .TRUE.                                             
            CALL SETI(9, 0, STATS)                                      
            CALL MOVEFI(9, STATS, XSTATS)                               
            GOTO  3                                                     
   2        INPOST = .FALSE.                                            
C IFLAG < 0.                                                            
            CALL MOVEFI(9, XSTATS, STATS)                               
   3  CONTINUE                                                          
   4  RETURN                                                            
      END                                                               
      SUBROUTINE IODEH(T0, V0, T1, V1, NV, DT, TSTOP)                   
      INTEGER NV                                                        
      REAL T0, V0(NV), T1, V1(NV), DT, TSTOP                            
C DEFAULT HANDLE PROCEDURE FOR IODES.                                   
C SCRATCH SPACE ALLOCATED - NONE.                                       
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A4SSOE(WV, RV, IV, LV, X, TSTOPE, DTE)           
      INTEGER IV(40)                                                    
      REAL WV(30), RV(30), X(1), TSTOPE, DTE                            
      LOGICAL LV(20)                                                    
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER MIN0, I, K, I1, I2, IS(1000)                              
      REAL ABS, TEMP, AMIN1, RS(1000), WS(500), RTEMP                   
      LOGICAL LS(1000)                                                  
      INTEGER TEMP2                                                     
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C WIND THINGS UP AT THE END OF THE TIME-STEP.                           
      WV(2) = TSTOPE                                                    
      WV(6) = DTE                                                       
      A4SSOE = .FALSE.                                                  
C RELEASE E.                                                            
      CALL ISTKRL(1)                                                    
      IF (WV(11)+WV(6) .NE. WV(11)) GOTO 1                              
C/6S                                                                    
C        CALL SETERR(30H ESSOM - DT=0 RETURNED BY SOUT, 30, 16, 1)      
C/7S                                                                    
         CALL SETERR(' ESSOM - DT=0 RETURNED BY SOUT', 30, 16, 1)       
C/                                                                      
         A4SSOE = .TRUE.                                                
         RETURN                                                         
   1  IF ((WV(6)/ABS(WV(6)))*(WV(2)-WV(11)) .GE. 0.) GOTO 2             
C/6S                                                                    
C        CALL SETERR(47H ESSOM - DT RETURNED BY SOUT HAS THE WRONG SIGN,
C    1      47, 17, 1)                                                  
C/7S                                                                    
         CALL SETERR(' ESSOM - DT RETURNED BY SOUT HAS THE WRONG SIGN', 
     1      47, 17, 1)                                                  
C/                                                                      
         A4SSOE = .TRUE.                                                
         RETURN                                                         
   2  IF (.NOT. LV(2)) GOTO 9                                           
         IF (WV(10) .NE. WV(11)) GOTO 3                                 
            IV(17) = MIN0(IV(16), IV(17))                               
            GOTO  8                                                     
   3        TEMP2 = IV(1)                                               
C CONVERGED, SO UPDATE X0, HOPT, HUP, ETC.                              
            DO  4 I = 1, TEMP2                                          
               I1 = IV(12)+I-1                                          
               X(I) = WS(I1)                                            
   4           CONTINUE                                                 
C X=X1.                                                                 
            K = 1                                                       
               GOTO  6                                                  
   5           K = K+1                                                  
   6           IF (K .GT. IV(15)) GOTO  7                               
C HOPTO = HOPT.                                                         
               I1 = IV(7)+K-1                                           
               I2 = IV(6)+K-1                                           
               RS(I1) = RS(I2)                                          
               GOTO  5                                                  
   7        RV(7) = AMIN1(2.*RV(7), RV(6))                              
            IV(19) = IV(18)                                             
            IV(17) = IV(16)                                             
            LV(3) = .FALSE.                                             
   8     CONTINUE                                                       
         GOTO  12                                                       
   9     IF (ABS(WV(9)) .GE. ABS(WV(6))) GOTO 10                        
C/6S                                                                    
C           CALL SETERR(42H ESSOM - DT RAISED BY SOUT WHEN OK = FALSE,  
C    1         42, 18, 1)                                               
C/7S                                                                    
            CALL SETERR(' ESSOM - DT RAISED BY SOUT WHEN OK = FALSE',   
     1         42, 18, 1)                                               
C/                                                                      
            A4SSOE = .TRUE.                                             
            RETURN                                                      
C   THE DEFAULT RESPONSE IS TO LOWER DT BY 10**3.                       
  10     IF (ABS(WV(6)) .EQ. ABS(WV(9))) WV(6) = WV(6)/1E+3             
         IF (WV(11)+WV(6) .NE. WV(11)) GOTO 11                          
C/6S                                                                    
C           CALL SETERR(13H ESSOM - DT=0, 13, 15, 1)                    
C/7S                                                                    
            CALL SETERR(' ESSOM - DT=0', 13, 15, 1)                     
C/                                                                      
            A4SSOE = .TRUE.                                             
            RETURN                                                      
  11     CONTINUE                                                       
  12  WV(10) = WV(11)                                                   
      WV(11) = WV(10)+WV(6)                                             
      TEMP1 = (WV(6)/ABS(WV(6)))*(WV(11)-WV(2)) .GE. 0.                 
      IF (.NOT. TEMP1) TEMP1 = ABS(WV(11)-WV(2)) .LE. WV(7)*ABS(WV(6))  
      IF (TEMP1) WV(11) = WV(2)                                         
      IF (WV(10) .NE. WV(2)) WV(6) = WV(11)-WV(10)                      
      DTE = WV(6)                                                       
      IF (WV(11)+WV(6) .NE. WV(11)) GOTO 13                             
C/6S                                                                    
C        CALL SETERR(13H ESSOM - DT=0, 13, 15, 1)                       
C/7S                                                                    
         CALL SETERR(' ESSOM - DT=0', 13, 15, 1)                        
C/                                                                      
         A4SSOE = .TRUE.                                                
  13  RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A4SSOI(WV, RV, IV, LV, TSTART, TSTOP, BETA,      
     1   GAMMA, DELTA, NX, DT, N, KMAX, MMAX, XPOLY, ERRPAR, PESPAR,    
     2   HFRACT, KINIT)                                                 
      INTEGER MMAX                                                      
      INTEGER IV(40), NX, N(MMAX), KMAX, KINIT                          
      REAL WV(30), RV(30), TSTART, TSTOP, BETA, GAMMA                   
      REAL DELTA, DT, ERRPAR(2), PESPAR, HFRACT                         
      LOGICAL LV(20), XPOLY                                             
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IFA, ING, ILW, ISLOGN, IHOPTO, ILOZNG                     
      INTEGER ISTKGT, ILWORK, I, J, IPOW, IW                            
      INTEGER IS(1000), MUSED, IRCNT, ICOST, IHOPT, MRCNT               
      INTEGER IWORK, KOPTO, IX1                                         
      REAL ABS, HUP, RNDING, LOGRND, HUPMAX, HUP0                       
      REAL ALOG, T0, T1, RS(1000), LOGHI, CLOSE                         
      REAL FLOAT, WS(500), LOGLO, R1MACH                                
      LOGICAL PRSTRT, LS(1000)                                          
      LOGICAL TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      DATA CLOSE/1E-2/                                                  
      DATA MRCNT/3/                                                     
      DATA HUP0/1E-1/                                                   
C INITIALIZATION.                                                       
      IF (TSTART .NE. TSTOP) GOTO 1                                     
         A4SSOI = .TRUE.                                                
         RETURN                                                         
   1     A4SSOI = .FALSE.                                               
C CHECK THE INPUT.                                                      
C/6S                                                                    
C  2  IF (BETA .LE. 0.) CALL SETERR(18H ESSOM - BETA.LE.0, 18, 1, 2)    
C     IF (GAMMA .LE. 0.) CALL SETERR(19H ESSOM - GAMMA.LE.0, 19, 2, 2)  
C     IF (DELTA .LT. 0.) CALL SETERR(19H ESSOM - DELTA.LT.0, 19, 3, 2)  
C     IF (NX .LT. 1) CALL SETERR(16H ESSOM - NX.LT.1, 16, 4, 2)         
C     IF (TSTART+DT .EQ. TSTART) CALL SETERR(22H ESSOM - DT=0 ON INPUT  
C    1   , 22, 5, 2)                                                    
C     IF (N(1) .LT. 1) CALL SETERR(18H ESSOM - N(1).LT.1, 18, 6, 2)     
C     IF (KMAX .LT. 1) CALL SETERR(18H ESSOM - KMAX.LT.1, 18, 7, 2)     
C/7S                                                                    
   2  IF (BETA .LE. 0.) CALL SETERR(' ESSOM - BETA.LE.0', 18, 1, 2)     
      IF (GAMMA .LE. 0.) CALL SETERR(' ESSOM - GAMMA.LE.0', 19, 2, 2)   
      IF (DELTA .LT. 0.) CALL SETERR(' ESSOM - DELTA.LT.0', 19, 3, 2)   
      IF (NX .LT. 1) CALL SETERR(' ESSOM - NX.LT.1', 16, 4, 2)          
      IF (TSTART+DT .EQ. TSTART) CALL SETERR(' ESSOM - DT=0 ON INPUT'   
     1   , 22, 5, 2)                                                    
      IF (N(1) .LT. 1) CALL SETERR(' ESSOM - N(1).LT.1', 18, 6, 2)      
      IF (KMAX .LT. 1) CALL SETERR(' ESSOM - KMAX.LT.1', 18, 7, 2)      
C/                                                                      
      TEMP = MMAX .LT. KMAX+2                                           
      IF (.NOT. TEMP) GOTO 3                                            
         TEMP1 = KMAX .GT. 1                                            
         IF (.NOT. TEMP1) TEMP1 = MMAX .NE. 1                           
         TEMP = TEMP1                                                   
C/6S                                                                    
C  3  IF (TEMP) CALL SETERR(23H ESSOM - MMAX.LT.KMAX+2, 23, 8, 2)       
C/7S                                                                    
   3  IF (TEMP) CALL SETERR(' ESSOM - MMAX.LT.KMAX+2', 23, 8, 2)        
C/                                                                      
      TEMP = PESPAR .LE. 0.                                             
      IF (.NOT. TEMP) TEMP = PESPAR .GT. 1.                             
C/6S                                                                    
C     IF (TEMP) CALL SETERR(28H ESSOM - PESPAR NOT IN (0,1), 28, 9, 2)  
C     IF (HFRACT .LE. 0.) CALL SETERR(20H ESSOM - HFRACT.LE.0, 20, 10, 2
C    1   )                                                              
C/7S                                                                    
      IF (TEMP) CALL SETERR(' ESSOM - PESPAR NOT IN (0,1)', 28, 9, 2)   
      IF (HFRACT .LE. 0.) CALL SETERR(' ESSOM - HFRACT.LE.0', 20, 10, 2 
     1   )                                                              
C/                                                                      
      TEMP = KINIT .LT. 1                                               
      IF (.NOT. TEMP) TEMP = KINIT .GT. KMAX                            
C/6S                                                                    
C     IF (TEMP) CALL SETERR(34H ESSOM - KINIT NOT IN (1,...,KMAX), 34,  
C    1   11, 2)                                                         
C     IF (BETA-DELTA+GAMMA .LE. 0.) CALL SETERR(                        
C    1   30H ESSOM - BETA-DELTA+GAMMA.LE.0, 30, 12, 2)                  
C/7S                                                                    
      IF (TEMP) CALL SETERR(' ESSOM - KINIT NOT IN (1,...,KMAX)', 34,   
     1   11, 2)                                                         
      IF (BETA-DELTA+GAMMA .LE. 0.) CALL SETERR(                        
     1   ' ESSOM - BETA-DELTA+GAMMA.LE.0', 30, 12, 2)                   
C/                                                                      
C ALLOCATE AND LOAD THE ARRAY SLOGN WITH LOG(N(1))+ ... +LOG(N(I-1)).   
      ISLOGN = ISTKGT(MMAX+1, 3)                                        
      WS(ISLOGN) = 0                                                    
      J = 2                                                             
         GOTO  5                                                        
   4     J = J+1                                                        
   5     IF (J .GT. MMAX) GOTO  6                                       
C/6S                                                                    
C        IF (N(J-1) .GE. N(J)) CALL SETERR(                             
C    1      37H ESSOM - N IS NOT MONOTONE INCREASING, 37, 13, 2)        
C/7S                                                                    
         IF (N(J-1) .GE. N(J)) CALL SETERR(                             
     1      ' ESSOM - N IS NOT MONOTONE INCREASING', 37, 13, 2)         
C/                                                                      
         I = ISLOGN+J-1                                                 
         WS(I) = WS(I-1)+ALOG(FLOAT(N(J-1)))                            
         GOTO  4                                                        
   6  I = ISLOGN+MMAX                                                   
      WS(I) = WS(I-1)+ALOG(FLOAT(N(MMAX)))                              
C/6S                                                                    
C     IF (DT/ABS(DT)*(TSTOP-TSTART) .LE. 0.) CALL SETERR(               
C    1   30H ESSOM - DT HAS THE WRONG SIGN, 30, 14, 2)                  
C/7S                                                                    
      IF (DT/ABS(DT)*(TSTOP-TSTART) .LE. 0.) CALL SETERR(               
     1   ' ESSOM - DT HAS THE WRONG SIGN', 30, 14, 2)                   
C/                                                                      
C ALLOCATE CURRENT AND OLD OPTIMAL STEP-SIZE ARRAYS.                    
      IHOPT = ISTKGT(KMAX+1, 3)                                         
      IHOPTO = ISTKGT(KMAX+1, 3)                                        
C ALLOCATE AND LOAD THE ARRAY NG WITH N(J)**GAMMA.                      
      ING = ISTKGT(MMAX, 3)                                             
      DO  7 J = 1, MMAX                                                 
         I = ING+J-1                                                    
         WS(I) = FLOAT(N(J))**GAMMA                                     
   7     CONTINUE                                                       
C ALLOCATE SPACE FOR X1 (THE SOLUTION AT TIME T1) AND A SCRATCH ARRAY F.
      IX1 = ISTKGT(NX, 3)                                               
      IFA = ISTKGT(KMAX, 3)                                             
C ALLOCATE AND LOAD POW(J) WITH 1/(BETA-DELTA+J*GAMMA).                 
      IPOW = ISTKGT(KMAX, 3)                                            
      DO  8 J = 1, KMAX                                                 
         I = IPOW+J-1                                                   
         RS(I) = 1./(BETA-DELTA+FLOAT(J)*GAMMA)                         
   8     CONTINUE                                                       
C ALLOCATE AND LOAD ARRAYS WORK AND LWORK WITH                          
C SUM(I=1,...,J)(N(I)) AND LOG(WORK(J)), RESPECTIVELY.                  
      IWORK = ISTKGT(MMAX, 3)                                           
      ILWORK = ISTKGT(MMAX, 3)                                          
      RS(IWORK) = N(1)                                                  
      RS(ILWORK) = ALOG(RS(IWORK))                                      
      J = 2                                                             
         GOTO  10                                                       
   9     J = J+1                                                        
  10     IF (J .GT. MMAX) GOTO  11                                      
         IW = IWORK+J-1                                                 
         RS(IW) = RS(IW-1)+FLOAT(N(J))                                  
         ILW = ILWORK+J-1                                               
         RS(ILW) = ALOG(RS(IW))                                         
         GOTO  9                                                        
C ALLOCATE THE COST/UNIT TIME-STEP ARRAY.                               
  11  ICOST = ISTKGT(KMAX+1, 3)                                         
C ALLOCATE THE EXTRAPOLATION LOZENGE LAST SO THAT ISTKMD CAN            
C BE USED TO LET IT GROW ONLY AS NEEDED.                                
      ILOZNG = ISTKGT(1, 3)                                             
C GET THE LOGARITHMS OF THE LARGEST AND SMALLEST NUMBERS,               
C AS WELL AS THE ROUNDING ERROR LEVEL, OF THE MACHINE.                  
      LOGLO = ALOG(R1MACH(1))                                           
      LOGHI = ALOG(R1MACH(2))                                           
      RNDING = R1MACH(4)                                                
      LOGRND = ALOG(RNDING)                                             
      HUPMAX = (-(LOGRND+4.6))/(BETA+GAMMA)                             
      MUSED = 0                                                         
      IRCNT = 0                                                         
      KOPTO = KINIT                                                     
      HUP = HUPMAX                                                      
      PRSTRT = .FALSE.                                                  
      T0 = TSTART                                                       
      T1 = T0+DT                                                        
      TEMP = (DT/ABS(DT))*(T1-TSTOP) .GE. 0.                            
      IF (.NOT. TEMP) TEMP = ABS(T1-TSTOP) .LE. CLOSE*ABS(DT)           
      IF (TEMP) T1 = TSTOP                                              
      DT = T1-T0                                                        
C LOAD THE VARIABLE ARRAYS WITH THE APPROPRIATE VALUES.                 
      WV(1) = TSTART                                                    
      WV(2) = TSTOP                                                     
      WV(3) = BETA                                                      
      WV(4) = GAMMA                                                     
      WV(5) = DELTA                                                     
      WV(6) = DT                                                        
      WV(7) = CLOSE                                                     
      WV(9) = DT                                                        
      WV(10) = T0                                                       
      WV(11) = T1                                                       
      RV(1) = ERRPAR(1)                                                 
      RV(2) = ERRPAR(2)                                                 
      RV(3) = PESPAR                                                    
      RV(4) = HFRACT                                                    
      RV(5) = HUP0                                                      
      RV(6) = HUPMAX                                                    
      RV(7) = HUP                                                       
      RV(8) = LOGLO                                                     
      RV(9) = LOGHI                                                     
      RV(10) = LOGRND                                                   
      RV(11) = RNDING                                                   
      RV(12) = 1E-2                                                     
      IV(1) = NX                                                        
      IV(2) = KMAX                                                      
      IV(3) = MMAX                                                      
      IV(4) = KINIT                                                     
      IV(5) = ICOST                                                     
      IV(6) = IHOPT                                                     
      IV(7) = IHOPTO                                                    
      IV(8) = IWORK                                                     
      IV(9) = ILWORK                                                    
      IV(10) = IPOW                                                     
      IV(11) = ISLOGN                                                   
      IV(12) = IX1                                                      
      IV(13) = ILOZNG                                                   
      IV(14) = MRCNT                                                    
      IV(15) = 0                                                        
      IV(16) = KOPTO                                                    
      IV(17) = KOPTO                                                    
      IV(20) = MUSED                                                    
      IV(25) = IRCNT                                                    
      IV(26) = IFA                                                      
      IV(27) = ING                                                      
      LV(1) = XPOLY                                                     
      LV(3) = PRSTRT                                                    
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A4SSOM(WV, RV, IV, LV, DTE)                      
      INTEGER IV(40)                                                    
      REAL WV(30), RV(30), DTE                                          
      LOGICAL LV(20)                                                    
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IDX, NERROR, MIN0, K, NERR, I1                            
      INTEGER I2, I3, IS(1000)                                          
      REAL ABS, EXP, TEMP, SQRT, AMIN1, AMAX1                           
      REAL RS(1000), WS(500), RTEMP                                     
      LOGICAL LS(1000), A4SSOL                                          
      INTEGER TEMP2, TEMP3, TEMP4, TEMP5, TEMP6, TEMP7                  
      INTEGER TEMP8                                                     
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE BASIC STEP-SIZE AND ORDER MONITOR.                                
      IF (NERROR(NERR) .EQ. 0) GOTO 1                                   
         A4SSOM = .TRUE.                                                
         RETURN                                                         
   1     A4SSOM = .FALSE.                                               
   2  IF (IV(3) .NE. 1) GOTO 3                                          
         IF (LV(2)) CALL ISTKRL(1)                                      
         A4SSOM = .FALSE.                                               
         RETURN                                                         
   3  IF (.NOT. LV(7)) WV(11) = WV(10)                                  
C SIGNAL A RESTART.                                                     
      TEMP1 = WV(10) .EQ. WV(11)                                        
      IF (TEMP1) TEMP1 = WV(10) .NE. WV(1)                              
      IF (.NOT. TEMP1) GOTO 4                                           
         IV(25) = IV(14)                                                
         WV(12) = WV(10)+WV(6)                                          
   4  TEMP1 = LV(2)                                                     
      IF (TEMP1) TEMP1 = .NOT. LV(4)                                    
      IF (.NOT. TEMP1) GOTO 7                                           
         IF (LV(5)) GOTO 6                                              
            TEMP6 = IV(21)                                              
C FIND THE OPTIMAL DT AND M FOR                                         
C THE NEXT TIME-STEP.                                                   
            TEMP7 = IV(22)                                              
            TEMP8 = IV(10)                                              
            IF (.NOT. A4SSOL(RS(TEMP6), RS(TEMP7), IV(1), IV(18), IV(2),
     1         RS(TEMP8), RV(10))) GOTO 5                               
               A4SSOM = .TRUE.                                          
               RETURN                                                   
   5        CONTINUE                                                    
   6     TEMP8 = IV(21)                                                 
         TEMP7 = IV(11)                                                 
         TEMP6 = IV(26)                                                 
         TEMP5 = IV(10)                                                 
         TEMP4 = IV(6)                                                  
         TEMP3 = IV(9)                                                  
         TEMP2 = IV(5)                                                  
         CALL A4SSOO(RS(TEMP8), IV(1), IV(18), IV(3), IV(2), WS(TEMP7)  
     1      , WV(4), RS(TEMP6), RS(TEMP5), IV(16), WV(6), RS(TEMP4), RS(
     2      TEMP3), RS(TEMP2), RV(8), RV(9), RV(12))                    
C RELEASE THE LOZENGE ERROR ESTIMATES E2.                               
         CALL ISTKRL(1)                                                 
   7  IF (.NOT. LV(2)) GOTO 15                                          
         IV(16) = MIN0(IV(16), IV(17)+1)                                
C GET THE DT FOR THE NEXT TIME-STEP.                                    
         IF (WV(10) .NE. WV(11)) GOTO 8                                 
            IV(28) = MIN0(IV(16), IV(17))                               
            GOTO  9                                                     
   8        IV(28) = IV(16)                                             
   9     IDX = IV(6)+IV(28)                                             
C ABS(HOPT(KOPTM+1))).                                                  
         WV(8) = (WV(6)/ABS(WV(6)))*RV(3)*AMIN1(ABS(WV(6))*EXP(RV(7)),  
     1      ABS(RS(IDX)))                                               
C   TWO RESTARTS IN A ROW CAUSE DT TO DECREASE BY AT LEAST A            
C   FACTOR OF 10**3.                                                    
         TEMP1 = LV(3)                                                  
         IF (TEMP1) TEMP1 = WV(10) .EQ. WV(11)                          
         IF (TEMP1) WV(8) = (WV(6)/ABS(WV(6)))*AMIN1(ABS(WV(8)), 1E-3*  
     1      ABS(WV(6)))                                                 
         IV(15) = MIN0(IV(18), IV(2)+1)                                 
C   COMPUTE THE COST/UNIT TIME-STEP FOR EACH LOZENGE SIZE.              
C COST(K) = WORK(K+1)/ABS(HOPT(K)).                                     
         TEMP2 = IV(15)                                                 
         DO  12 K = 1, TEMP2                                            
            I1 = IV(5)+K-1                                              
            I2 = IV(8)+K                                                
            I3 = IV(6)+K-1                                              
            IF (RS(I3) .NE. 0.) GOTO 10                                 
               RS(I1) = -1                                              
               GOTO  11                                                 
  10           RS(I1) = RS(I2)/ABS(RS(I3))                              
  11        CONTINUE                                                    
  12        CONTINUE                                                    
         TEMP1 = WV(10) .NE. WV(1)                                      
         IF (TEMP1) TEMP1 = WV(10) .NE. WV(11)                          
         IF (.NOT. TEMP1) GOTO 14                                       
            IDX = MIN0(IV(16)+1, IV(19)-1, IV(18)-1)                    
C SEE IF SHOULD BE CAUTIOUS.                                            
C     IF ABS(HOPT(NEW)) < ABS(HOPT(OLD)), BE CONSERVATIVE.              
            I1 = IV(6)+IDX-1                                            
            I2 = IV(7)+IDX-1                                            
            IF (ABS(WV(8)) .GT. 1E-2*ABS(RS(I1)) .AND. ABS(RS(I1))      
     1          .LT. ABS(RS(I2))) WV(8) = WV(8)*AMAX1(ABS(RS(I1)/RS(I2))
     2         , 1E-2)                                                  
            IF (IV(25) .LE. 0) GOTO 13                                  
               IV(25) = IV(25)-1                                        
C LOGARITHMIC BISECTION.                                                
               IF (IV(25) .EQ. 0) RV(7) = 0.5*RV(5)                     
               TEMP = (WV(12)-WV(11))/WV(6)                             
               IF (TEMP .GT. 0.99) WV(8) = (WV(6)/ABS(WV(6)))*AMIN1(0.5*
     1            ABS(WV(12)-WV(11)), ABS(WV(8)), SQRT(TEMP)*ABS(WV(6)))
  13        CONTINUE                                                    
  14     CONTINUE                                                       
  15  IF (WV(10) .EQ. WV(11)) LV(3) = .TRUE.                            
      WV(9) = WV(6)                                                     
      IF (LV(2)) WV(6) = WV(8)                                          
      DTE = WV(6)                                                       
      IF (WV(11)+WV(6) .NE. WV(11)) GOTO 16                             
C/6S                                                                    
C        CALL SETERR(13H ESSOM - DT=0, 13, 15, 1)                       
C/7S                                                                    
         CALL SETERR(' ESSOM - DT=0', 13, 15, 1)                        
C/                                                                      
         A4SSOM = .TRUE.                                                
  16  RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A4SSOR(WV, RV, IV, LV, ERRARE)                   
      INTEGER IV(40)                                                    
      REAL WV(30), RV(30), ERRARE(2)                                    
      LOGICAL LV(20)                                                    
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IDX, IFIX, I1, I2, I3, IS(1000)                           
      REAL TEMP, RS(1000), WS(500), RTEMP                               
      LOGICAL LS(1000), A4SSOD, A4SSOL                                  
      INTEGER TEMP1, TEMP2, TEMP3, TEMP4, TEMP5, TEMP6                  
      INTEGER TEMP7                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C SEE IF A RESTART IS IN ORDER.                                         
      RV(1) = ERRARE(1)                                                 
C UPDATE ERRPAR.                                                        
      RV(2) = ERRARE(2)                                                 
      IF (.NOT. LV(7)) GOTO 1                                           
         A4SSOR = .TRUE.                                                
         RETURN                                                         
   1     A4SSOR = .FALSE.                                               
   2  IF (IV(18) .LE. IV(17)) GOTO 8                                    
         LV(5) = .TRUE.                                                 
C SEE IF A RE-START IS NECESSARY.                                       
         TEMP6 = IV(21)                                                 
         TEMP5 = IV(22)                                                 
         TEMP4 = IV(10)                                                 
         IF (.NOT. A4SSOL(RS(TEMP6), RS(TEMP5), IV(1), IV(18), IV(2),   
     1      RS(TEMP4), RV(10))) GOTO 3                                  
            A4SSOR = .TRUE.                                             
            RETURN                                                      
C   IF WILL NOT CONVERGE IN THIS LOZENGE, RESTART.                      
   3     TEMP4 = IV(21)                                                 
         TEMP5 = IV(11)                                                 
         TEMP6 = IV(26)                                                 
         TEMP7 = IV(10)                                                 
         LV(6) = A4SSOD(RS(TEMP4), IV(1), IV(18), IV(3), IV(2), WS(     
     1      TEMP5), WV(3), WV(4), WV(5), RS(TEMP6), RS(TEMP7), IV(24),  
     2      IV(23), IV(17))                                             
         IF (.NOT. LV(6)) GOTO 4                                        
            A4SSOR = .TRUE.                                             
            RETURN                                                      
   4     IF (IV(18) .LE. IV(17)+1) GOTO 7                               
            TEMP7 = IV(21)                                              
C SEE IF A RE-START WOULD BE MORE EFFICIENT.                            
            TEMP6 = IV(11)                                              
            TEMP5 = IV(26)                                              
            TEMP4 = IV(10)                                              
            TEMP3 = IV(6)                                               
            TEMP2 = IV(9)                                               
            TEMP1 = IV(5)                                               
            CALL A4SSOO(RS(TEMP7), IV(1), IV(18), IV(3), IV(2), WS(     
     1         TEMP6), WV(4), RS(TEMP5), RS(TEMP4), IV(16), WV(6), RS(  
     2         TEMP3), RS(TEMP2), RS(TEMP1), RV(8), RV(9), RV(12))      
            IDX = IV(6)+IV(16)                                          
            IF (WV(11)+RS(IDX) .NE. WV(11)) GOTO 5                      
C/6S                                                                    
C              CALL SETERR(13H ESSOM - DT=0, 13, 15, 1)                 
C/7S                                                                    
               CALL SETERR(' ESSOM - DT=0', 13, 15, 1)                  
C/                                                                      
               A4SSOR = .TRUE.                                          
               RETURN                                                   
C = SNGL(DT/HOPT(KOPT+1)).                                              
   5        RTEMP = WV(6)/RS(IDX)                                       
            IF (RTEMP .LE. 1E+3) RTEMP = IFIX(RTEMP+0.99)               
C     IF ( WORK(M) + WORK(KOPT+2)*RTEMP <= WORK(L) )                    
            I1 = IV(8)+IV(18)-1                                         
            I2 = IV(8)+IV(16)+1                                         
            I3 = IV(8)+IV(24)-1                                         
            IF (RS(I1)+RS(I2)*RTEMP .GT. RS(I3)) GOTO 6                 
               CALL ISTKRL(1)                                           
               LV(4) = .TRUE.                                           
               A4SSOR = .TRUE.                                          
               RETURN                                                   
   6        CONTINUE                                                    
   7     CONTINUE                                                       
   8  IF (IV(18) .LT. IV(3)) CALL ISTKRL(2)                             
      RETURN                                                            
      END                                                               
      SUBROUTINE A4SSOO(E, NX, M, MMAX, KMAX, SLOGN, GAMMA, F,          
     1   POW, KOPT, DT, HOPT, LNWORK, COST, LOGLO, LOGHI, COSTOL)       
      INTEGER KMAX, MMAX, NX                                            
      INTEGER M, KOPT                                                   
      REAL E(NX, KMAX), SLOGN(MMAX), GAMMA, F(KMAX), POW(KMAX), DT      
      REAL HOPT(1), LNWORK(MMAX), COST(1), LOGLO, LOGHI, COSTOL         
      INTEGER JHI, KHI, KMJ, MMJ, MIN0, I                               
      INTEGER J, K                                                      
      REAL EXP, RHOMAX, MINOST, AMIN1, AMAX1, HOPTK                     
      INTEGER TEMP                                                      
C COMPUTE THE OPTIMAL K AND H.                                          
C REAL HOPT(KMAX+1),COST(KMAX+1)                                        
      KOPT = 1                                                          
      KHI = MIN0(M-1, KMAX)                                             
C COMPUTE HOPT(K), K=1,...,MIN(M,KMAX+1).                               
      TEMP = KHI+1                                                      
      DO  6 K = 1, TEMP                                                 
         JHI = MIN0(K, KHI)                                             
C   COMPUTE THE FACTORS WHICH CONVERT ERRORS INTO STEP-SIZES.           
         DO  1 J = 1, JHI                                               
            KMJ = K-J                                                   
            MMJ = M-J                                                   
            F(J) = GAMMA*POW(J)*((SLOGN(K+1)-SLOGN(M))-(SLOGN(KMJ+1)-   
     1         SLOGN(MMJ)))                                             
   1        CONTINUE                                                    
C   HOPTK IS THE OPTIMAL STEP-SIZE FOR THE K-COLUMN LOZENGE.            
         HOPTK = LOGHI                                                  
         DO  3 I = 1, NX                                                
            RHOMAX = LOGLO                                              
            DO  2 J = 1, JHI                                            
               RHOMAX = AMAX1(RHOMAX, F(J)+E(I, J))                     
   2           CONTINUE                                                 
            HOPTK = AMIN1(HOPTK, RHOMAX)                                
   3        CONTINUE                                                    
         COST(K) = LNWORK(K+1)-HOPTK                                    
         IF (K .NE. 1) GOTO 4                                           
            MINOST = COST(1)                                            
            GOTO  5                                                     
   4        IF (K .LE. KHI) MINOST = AMIN1(MINOST, COST(K))             
   5     HOPT(K) = EXP(HOPTK)*DT                                        
   6     CONTINUE                                                       
C OPTIMIZE THE COST TO WITHIN A RELATIVE TOLERANCE OF COSTTOL.          
      DO  8 K = 1, KHI                                                  
         IF (COST(K) .GT. MINOST+COSTOL) GOTO 7                         
            KOPT = K                                                    
            GOTO  9                                                     
   7     CONTINUE                                                       
   8     CONTINUE                                                       
   9  RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A4SSOX(WV, RV, IV, LV, N, ME)                    
      INTEGER ME                                                        
      INTEGER IV(40), N(ME)                                             
      REAL WV(30), RV(30)                                               
      LOGICAL LV(20)                                                    
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKMD, ISTKGT, MIN0, MAX0, IS(1000), ITEMP               
      REAL ABS, TEMP, AMAX1, RS(1000), WS(500), FLOAT                   
      REAL RTEMP                                                        
      LOGICAL LS(1000)                                                  
      INTEGER TEMP1, TEMP2, TEMP3, TEMP4, TEMP5                         
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C DO THE EXTRAPOLATION.                                                 
      LV(6) = .FALSE.                                                   
      LV(7) = .FALSE.                                                   
      LV(4) = .FALSE.                                                   
      LV(5) = .FALSE.                                                   
      A4SSOX = .FALSE.                                                  
      IV(18) = ME                                                       
      TEMP5 = IV(18)                                                    
      IF (10.*AMAX1(ABS(WV(10)), ABS(WV(11)))*RV(11) .LT. ABS(WV(11)-WV(
     1   10))*RV(4)/FLOAT(N(TEMP5))) GOTO 1                             
C/6S                                                                    
C        CALL SETERR(13H ESSOM - DT=0, 13, 15, 1)                       
C/7S                                                                    
         CALL SETERR(' ESSOM - DT=0', 13, 15, 1)                        
C/                                                                      
         A4SSOX = .TRUE.                                                
         RETURN                                                         
   1  IF (LV(2)) GOTO 2                                                 
         IV(22) = ISTKGT(IV(1), 3)                                      
         A4SSOX = .TRUE.                                                
         RETURN                                                         
   2  IF (MIN0(IV(18), IV(2)) .LE. IV(20)) GOTO 3                       
         ITEMP = ISTKMD(IV(1)*MIN0(IV(18), IV(2)))                      
C EXPAND THE EXTRAPOLATION LOZENGE.                                     
C/6S                                                                    
C        IF (IV(13) .LT. ITEMP) CALL SETERR(                            
C    1      47H ESSOM - SOMEBODY IS LEAVING STUFF ON THE STACK, 47, 20  
C    2      , 2)                                                        
C        IF (IV(13) .GT. ITEMP) CALL SETERR(                            
C    1      50H ESSOM - SOMEBODY IS REMOVING STUFF FROM THE STACK, 50,  
C    2      21, 2)                                                      
C/7S                                                                    
         IF (IV(13) .LT. ITEMP) CALL SETERR(                            
     1      ' ESSOM - SOMEBODY IS LEAVING STUFF ON THE STACK', 47, 20   
     2      , 2)                                                        
         IF (IV(13) .GT. ITEMP) CALL SETERR(                            
     1      ' ESSOM - SOMEBODY IS REMOVING STUFF FROM THE STACK', 50,   
     2      21, 2)                                                      
C/                                                                      
   3  IV(20) = MAX0(IV(18), IV(20))                                     
C THE BEST ERROR IN THE LOZENGE.                                        
      IV(22) = ISTKGT(IV(1), 3)                                         
C THE LOZENGE ERROR.                                                    
      IV(21) = ISTKGT(MAX0(1, IV(1)*MIN0(IV(18)-1, IV(2))), 3)          
      TEMP5 = IV(12)                                                    
      TEMP4 = IV(27)                                                    
      TEMP3 = IV(13)                                                    
      TEMP2 = IV(21)                                                    
      TEMP1 = IV(22)                                                    
      CALL XTRAP(WS(TEMP5), IV(18), IV(1), WS(TEMP4), IV(2), LV(1), WS( 
     1   TEMP3), RS(TEMP2), RS(TEMP1))                                  
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A4SSOD(E, NX, M, MMAX, KMAX, SLOGN, BETA,        
     1   GAMMA, DELTA, F, POW, LDONE, ILDONE, KOPTO)                    
      INTEGER KMAX, MMAX, NX                                            
      INTEGER M, LDONE, ILDONE, KOPTO                                   
      REAL E(NX, KMAX), SLOGN(MMAX), BETA, GAMMA, DELTA, F(KMAX)        
      REAL POW(KMAX)                                                    
      INTEGER JHI, LMJ, MMJ, LDONEO, MIN0, I                            
      INTEGER J, L, LMAX, LGIVE, JSAVE                                  
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      DATA LGIVE/1/                                                     
C RETURN LDONE = THE LEVEL WHERE CONVERGENCE IS EXPECTED.               
C                                                                       
C IF M = KOPTO+1, RETURN ILDONE=LDONE.                                  
C IF M > KOPTO+1, DO NOT LET LDONE > ILDONE+1 HAPPEN IN THE FIRST       
C KOPTO COLUMNS.                                                        
C                                                                       
C A4SSOD = TRUE IF WILL NOT CONVERGE IN THIS LOZENGE.                   
C A4SSOD = FALSE IF WILL CONVERGE.                                      
      A4SSOD = .TRUE.                                                   
C INITIALLY, FLAG NOT CONVERGENT.                                       
      LDONE = 0                                                         
      IF (M .LE. KOPTO+1) GOTO 1                                        
         LMAX = MIN0(ILDONE+LGIVE, MMAX)                                
         GOTO  2                                                        
   1     LMAX = MMAX                                                    
   2  IF (M .GE. LMAX) GOTO 10                                          
         TEMP1 = M+1                                                    
         DO  9 L = TEMP1, LMAX                                          
            IF (LDONE .GT. 0 .AND. (L .LT. LMAX .OR. M .LE. KOPTO+1))   
     1         GOTO  9                                                  
C     LDONEO DETERMINES IF WILL CONVERGE IN THE FIRST KOPTO             
C     COLUMNS. SET TRUE INITIALLY.                                      
            LDONEO = L                                                  
            JHI = MIN0(M-1, KMAX)                                       
C COMPUTE THE FACTORS USED FOR CONVERGENCE CHECK.                       
            DO  3 J = 1, JHI                                            
               MMJ = M-J                                                
               LMJ = L-J                                                
               F(J) = POW(J)*GAMMA*((SLOGN(M)-SLOGN(L))-(SLOGN(MMJ)-    
     1            SLOGN(LMJ)))                                          
   3           CONTINUE                                                 
C SEE IF THE I-TH VARIABLE WILL CONVERGE AT M=L.                        
            DO  7 I = 1, NX                                             
C FLAG NOT CONVERGENT.                                                  
               JSAVE = 0                                                
C CHECK EACH COLUMN FOR CONVERGENCE.                                    
               DO  5 J = 1, JHI                                         
                  IF (E(I, J) .LT. F(J)) GOTO 4                         
                     JSAVE = J                                          
                     GOTO  6                                            
   4              CONTINUE                                              
   5              CONTINUE                                              
   6           IF (JSAVE .EQ. 0) GOTO  8                                
C NO CONVERGENCE.                                                       
               IF (JSAVE .GT. KOPTO) LDONEO = 0                         
C NO CONVERGENCE IN                                                     
C COLUMNS 1,...,KOPTO.                                                  
   7           CONTINUE                                                 
   8        IF (JSAVE .EQ. 0) GOTO  9                                   
C NO CONVERGENCE, TRY THE NEXT L.                                       
            IF (LDONE .EQ. 0) LDONE = L                                 
   9        CONTINUE                                                    
  10  TEMP = LDONE .NE. 0                                               
      IF (TEMP) TEMP = LDONEO .NE. 0                                    
      IF (.NOT. TEMP) GOTO 11                                           
         IF (M .EQ. KOPTO+1) ILDONE = LDONE                             
C WILL CONVERGE AT M = LDONE.                                           
         A4SSOD = .FALSE.                                               
  11  RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A4SSOL(E2, E, NX, M, KMAX, POW, LOGRND)          
      INTEGER KMAX, NX                                                  
      INTEGER M                                                         
      REAL E2(NX, KMAX), E(NX), POW(KMAX), LOGRND                       
      INTEGER JHI, MIN0, I, J                                           
      REAL ABS, V, ALOG, LOGE, AMIN1                                    
      LOGICAL TEMP                                                      
C TO RETURN POW(J) TIMES THE LOGARITHM OF THE RATIO OF THE DESIRED      
C TO THE ATTAINED ERROR, FOR EACH ELEMENT IN THE LOZENGE.               
C                                                                       
C A4SSOL = TRUE IF NOT SUCCESSFUL.                                      
C A4SSOL = FALSE IF SUCCESSFUL.                                         
      A4SSOL = .FALSE.                                                  
      JHI = MIN0(M-1, KMAX)                                             
      DO  5 I = 1, NX                                                   
         TEMP = E(I) .EQ. 0.                                            
         IF (TEMP) TEMP = E2(I, 1) .NE. 0.                              
         IF (.NOT. TEMP) TEMP = E(I) .LT. 0.                            
         IF (.NOT. TEMP) GOTO 1                                         
C/6S                                                                    
C           CALL SETERR(37H ESSOM - E(I).LE.0 RETURNED BY SERROR, 37,   
C    1         19, 1)                                                   
C/7S                                                                    
            CALL SETERR(' ESSOM - E(I).LE.0 RETURNED BY SERROR', 37,    
     1         19, 1)                                                   
C/                                                                      
            A4SSOL = .TRUE.                                             
            GOTO  6                                                     
   1     IF (E(I) .GT. 0.) LOGE = ALOG(E(I))                            
         DO  4 J = 1, JHI                                               
            TEMP = E2(I, J) .NE. 0.                                     
            IF (TEMP) TEMP = E2(I, 1) .NE. 0.                           
            IF (.NOT. TEMP) GOTO 2                                      
               V = AMIN1(LOGE-ALOG(ABS(E2(I, J))), (-LOGRND)-4.6)       
               GOTO  3                                                  
   2           V = (-LOGRND)-4.6                                        
   3        E2(I, J) = POW(J)*V                                         
   4        CONTINUE                                                    
   5     CONTINUE                                                       
   6  RETURN                                                            
      END                                                               
      SUBROUTINE DIODE(V, NV, TSTART, TSTOP, DT, D, ERRPAR,             
     1   HANDLE)                                                        
      INTEGER NV                                                        
      EXTERNAL D, HANDLE                                                
      REAL ERRPAR(2)                                                    
      DOUBLE PRECISION V(NV), TSTART, TSTOP, DT                         
      EXTERNAL DIODED, DIODEE, DIODEN                                   
C THE FIRST LEVEL OF DIODE.                                             
      CALL DIODER(V, NV, TSTART, TSTOP, DT, D, DIODEE, ERRPAR, DIODEN,  
     1   DIODED, HANDLE)                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE DIODES(V, NV, TSTART, TSTOP, DT, D, ERRPAR,            
     1   HANDLE)                                                        
      INTEGER NV                                                        
      EXTERNAL D, HANDLE                                                
      REAL ERRPAR(2)                                                    
      DOUBLE PRECISION V(NV), TSTART, TSTOP, DT                         
      EXTERNAL DIODEE                                                   
      LOGICAL ERPUTS                                                    
C THE FIRST LEVEL OF DIODES.                                            
      ERPUTS = .FALSE.                                                  
      CALL DIODE1(V, NV, TSTART, TSTOP, DT, D, DIODEE, ERRPAR, ERPUTS,  
     1   HANDLE)                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DIODE1(V, NV, TSTART, TSTOP, DT, D, ERROR,             
     1   ERRPAR, ERPUTS, HANDLE)                                        
      INTEGER NV                                                        
      EXTERNAL D, ERROR, HANDLE                                         
      REAL ERRPAR(2)                                                    
      LOGICAL ERPUTS                                                    
      DOUBLE PRECISION V(NV), TSTART, TSTOP, DT                         
      INTEGER KMAX, KINIT                                               
      LOGICAL EQUIL, XPOLY                                              
C THE SECOND LEVEL OF DIODES.                                           
      KMAX = 10                                                         
      XPOLY = .FALSE.                                                   
      KINIT = 2                                                         
      EQUIL = .TRUE.                                                    
      CALL DIODE2(V, NV, TSTART, TSTOP, DT, D, EQUIL, KMAX, XPOLY,      
     1   KINIT, ERROR, ERRPAR, ERPUTS, HANDLE)                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DIODE2(V, NV, TSTART, TSTOP, DT, D, EQUIL, KMAX        
     1   , XPOLY, KINIT, ERROR, ERRPAR, ERPUTS, HANDLE)                 
      INTEGER NV                                                        
      EXTERNAL D, ERROR, HANDLE                                         
      INTEGER KMAX, KINIT                                               
      REAL ERRPAR(2)                                                    
      LOGICAL EQUIL, XPOLY, ERPUTS                                      
      DOUBLE PRECISION V(NV), TSTART, TSTOP, DT                         
      EXTERNAL DIODED, DIODEN                                           
      INTEGER KEEJAC, MINIT, MAXIT                                      
      DOUBLE PRECISION THETA                                            
C THE THIRD LEVEL OF DIODES.                                            
C CHECK THE INPUT FOR ERRORS.                                           
      IF (.NOT. EQUIL) GOTO 1                                           
         THETA = 1                                                      
         GOTO  2                                                        
   1     THETA = 0.5D0                                                  
   2  KEEJAC = 0                                                        
      MINIT = 10                                                        
      MAXIT = 15                                                        
      CALL DIODE3(V, NV, TSTART, TSTOP, DT, D, THETA, KEEJAC, MINIT,    
     1   MAXIT, KMAX, XPOLY, KINIT, ERROR, ERRPAR, ERPUTS, DIODEN,      
     2   DIODED, HANDLE)                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE DIODE3(V, NV, TSTART, TSTOP, DT, D, THETA,             
     1   KEEJAC, MINIT, MAXIT, KMAX, XPOLY, KINIT, ERROR, ERRPAR,       
     2   ERPUTS, INMI, DA, HANDLE)                                      
      INTEGER NV                                                        
      EXTERNAL D, ERROR, INMI, DA, HANDLE                               
      INTEGER KEEJAC, MINIT, MAXIT, KMAX, KINIT                         
      REAL ERRPAR(2)                                                    
      LOGICAL XPOLY, ERPUTS                                             
      DOUBLE PRECISION V(NV), TSTART, TSTOP, DT, THETA                  
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      EXTERNAL D1ODEE, D1ODEH, D1ODEP, D1ODEN                           
      INTEGER ISTKGT, I, IRCS, MMAX, IN, IS(1000)                       
      REAL HFRACT, EGIVE, RS(1000)                                      
      LOGICAL USENFD, USENGJ, USENNS, LS(1000)                          
      DOUBLE PRECISION BETA, GAMMA, DELTA, WS(500)                      
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      DATA USENGJ/.FALSE./                                              
      DATA USENNS/.FALSE./                                              
      DATA USENFD/.FALSE./                                              
C THE FOURTH LEVEL OF IODES.                                            
C SCRATCH SPACE ALLOCATED -                                             
C     S(DIODE3) = S(D1ODES) +                                           
C LONG REAL WORDS +                                                     
C                 MMAX                                                  
C INTEGER WORDS.                                                        
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      IF (TSTART .EQ. TSTOP) RETURN                                     
      CALL ENTER(1)                                                     
C CHECK THE INPUT FOR ERRORS.                                           
      MMAX = KMAX+5                                                     
      BETA = 1                                                          
      IF (THETA .EQ. 0.5) GOTO 1                                        
         GAMMA = 1                                                      
         GOTO  2                                                        
   1     GAMMA = 2                                                      
   2  IF (.NOT. ERPUTS) GOTO 3                                          
         DELTA = 1                                                      
         GOTO  4                                                        
   3     DELTA = 0                                                      
C GET N.                                                                
   4  IN = ISTKGT(MMAX, 2)                                              
      IS(IN) = 1                                                        
      IS(IN+1) = 2                                                      
      IS(IN+2) = 3                                                      
      I = 4                                                             
         GOTO  6                                                        
   5     I = I+1                                                        
   6     IF (I .GT. MMAX) GOTO  7                                       
         TEMP1 = IN+I                                                   
         TEMP = IN+I                                                    
         IS(TEMP1-1) = 2*IS(TEMP-3)                                     
         GOTO  5                                                        
   7  IF (THETA .NE. 0.5) GOTO 9                                        
         HFRACT = 0.5                                                   
         DO  8 I = 1, MMAX                                              
            TEMP = IN+I                                                 
            IS(TEMP-1) = 2*IS(TEMP-1)                                   
   8        CONTINUE                                                    
         GOTO  10                                                       
   9     HFRACT = 1                                                     
  10  EGIVE = 1E+2                                                      
      IRCS = 1                                                          
      CALL DIODE4(V, NV, TSTART, TSTOP, DT, D, THETA, KEEJAC, MINIT,    
     1   MAXIT, USENGJ, USENNS, USENFD, BETA, GAMMA, DELTA, IS(IN),     
     2   MMAX, HFRACT, EGIVE, KMAX, XPOLY, KINIT, IRCS, ERROR, ERRPAR,  
     3   ERPUTS, INMI, DA, HANDLE)                                      
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION DIODED(V, VT, NV, T, DT, D, DV, TJ, DTJ,         
     1   GETJAC, SEPATE, USENGJ, USENNS, USENFD, NES, NFS, FNUM, THETA  
     2   , IRCS, KEEJAC)                                                
      INTEGER NV                                                        
      EXTERNAL D                                                        
      INTEGER NES, NFS, FNUM, IRCS, KEEJAC                              
      LOGICAL GETJAC, SEPATE, USENGJ, USENNS, USENFD                    
      DOUBLE PRECISION V(NV), VT(NV), T, DT, DV(NV), TJ                 
      DOUBLE PRECISION DTJ, THETA                                       
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D1ODES/ IMEM                                              
      INTEGER IMEM                                                      
      COMMON /DIODEJ/ AJ, BJ, GETACC, NEEUMC                            
      LOGICAL GETACC, NEEUMC                                            
      DOUBLE PRECISION AJ, BJ                                           
      COMMON /DIODEF/ FAILED                                            
      LOGICAL FAILED                                                    
      INTEGER ID43, IDV, LDV, IDELVT, ISTKGT, NERROR                    
      INTEGER MIN0, MAX0, I, J, NERR, IDVT                              
      INTEGER LDVT, I0, I1, IS(1000), IDELV, ID4(2)                     
      INTEGER ID1                                                       
      REAL RS(1000)                                                     
      LOGICAL DIODEG, GETDVT, GETACT, NEESUM, LS(1000)                  
      DOUBLE PRECISION DABS, TEMP, DMAX1, WS(500), DUMMY(100)           
      INTEGER TEMP1, TEMP2, TEMP3, TEMP4                                
      LOGICAL TEMP5, TEMP6                                              
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C SCRATCH SPACE ALLOCATED - S(DIODED) =                                 
C     NV**2 + NV*(2*NV+1) +                                             
C     MAX ( S(D), 2*NV + NV INTEGER )                                   
C LONG REAL WORDS.                                                      
C A DUMMY ARRAY FOR VOID DV AND DVT.                                    
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      LDV = NV**2                                                       
C THE LENGTHS OF THE DV AND DVT ARRAYS.                                 
      LDVT = NV**2                                                      
      IF (IMEM .LE. 0) GOTO 1                                           
         ID4(1) = IS(IMEM+3)                                            
         ID4(2) = IS(IMEM+4)                                            
   1  TEMP5 = NFS .EQ. (-1)                                             
      IF (TEMP5) TEMP5 = KEEJAC .GT. 1                                  
      IF (TEMP5) GOTO 2                                                 
         TEMP6 = NFS .EQ. (-2)                                          
         IF (TEMP6) TEMP6 = KEEJAC .EQ. 1                               
         TEMP5 = TEMP6                                                  
   2  IF (.NOT. TEMP5) GOTO 3                                           
         IMEM = ISTKGT(10, 2)                                           
C INITIALIZE.                                                           
C D, DIAG, PIVOT, D41, D42, R, CA, RCA, CB AND RCB.                     
         IS(IMEM) = ISTKGT(LDV+3*NV+1, 4)                               
         IS(IMEM+1) = IS(IMEM)+LDV                                      
         IS(IMEM+3) = 0                                                 
         IS(IMEM+4) = 0                                                 
         IS(IMEM+5) = IS(IMEM+1)+NV                                     
         IS(IMEM+6) = IS(IMEM+5)+NV                                     
         IS(IMEM+8) = IS(IMEM+6)+NV                                     
         IS(IMEM+2) = ISTKGT(2*NV+1, 2)                                 
         IS(IMEM+7) = IS(IMEM+2)+NV                                     
         IS(IMEM+9) = IS(IMEM+7)+NV                                     
         GOTO  4                                                        
   3     IF (IMEM .EQ. 0) IMEM = 1                                      
C SIGNAL THAT KNOW ABOUT D1ODES.                                        
   4  TEMP5 = NFS .EQ. (-1)                                             
      IF (TEMP5) TEMP5 = KEEJAC .GT. 1                                  
      IF (.NOT. TEMP5) GOTO 5                                           
         IS(IMEM+3) = ISTKGT(LDV+LDVT, 4)                               
C THE JACOBIAN PARTS ARE GLOBAL.                                        
         IS(IMEM+4) = IS(IMEM+3)+LDV                                    
   5  IF (NFS .GE. 0) GOTO 6                                            
         DIODED = .TRUE.                                                
         RETURN                                                         
   6  CALL ENTER(1)                                                     
      NEESUM = DTJ .NE. DT                                              
      NEESUM = NEESUM .AND. SEPATE                                      
      IF (KEEJAC .NE. 0) GOTO 11                                        
         IMEM = ISTKGT(2*NV+11, 2)                                      
C NEED STACK FRAME.                                                     
         IF (USENNS) GOTO 7                                             
            ID4(1) = ISTKGT(LDV+LDVT+3*NV+1, 4)                         
            ID4(2) = ID4(1)+LDV                                         
            GOTO  8                                                     
   7        ID4(1) = ISTKGT(LDV+3*NV+1, 4)                              
            ID4(2) = 0                                                  
   8     IS(IMEM) = ID4(1)                                              
         ID43 = ID4(1)                                                  
         IF (USENNS) GOTO 9                                             
            IS(IMEM+1) = ID4(2)+LDVT                                    
            GOTO  10                                                    
   9        IS(IMEM+1) = ID4(1)+LDV                                     
  10     IS(IMEM+5) = IS(IMEM+1)+NV                                     
         IS(IMEM+6) = IS(IMEM+5)+NV                                     
         IS(IMEM+8) = IS(IMEM+6)+NV                                     
         IS(IMEM+2) = IMEM+10                                           
         IS(IMEM+7) = IS(IMEM+2)+NV                                     
         IS(IMEM+9) = IS(IMEM+7)+NV                                     
         GOTO  20                                                       
  11     IF (KEEJAC .NE. 1) GOTO 16                                     
            IF (.NOT. GETJAC) GOTO 14                                   
               ID4(1) = IS(IMEM)                                        
               ID43 = ID4(1)                                            
               IF (.NOT. USENNS) GOTO 12                                
                  ID4(2) = 0                                            
                  GOTO  13                                              
  12              ID4(2) = ISTKGT(LDVT, 4)                              
  13           CONTINUE                                                 
               GOTO  15                                                 
  14           ID4(1) = 0                                               
               ID4(2) = 0                                               
               ID43 = 0                                                 
  15        CONTINUE                                                    
            GOTO  19                                                    
  16        TEMP5 = GETJAC                                              
            IF (.NOT. TEMP5) TEMP5 = NEESUM                             
            IF (.NOT. TEMP5) GOTO 17                                    
               ID43 = IS(IMEM)                                          
               GOTO  18                                                 
  17           ID43 = 0                                                 
  18        CONTINUE                                                    
  19  CONTINUE                                                          
  20  NEESUM = DT .NE. DTJ                                              
      NEESUM = NEESUM .AND. SEPATE                                      
      TEMP5 = GETJAC                                                    
      IF (TEMP5) TEMP5 = .NOT. SEPATE                                   
      NEESUM = NEESUM .OR. TEMP5                                        
      IF ((.NOT. SEPATE) .OR. (.NOT. GETJAC)) GOTO 27                   
         AJ = 1                                                         
         BJ = 1                                                         
         GETACC = GETJAC                                                
         NEEUMC = .FALSE.                                               
C DEFAULT VALUES.                                                       
         CALL SETD(NV, 0D0, DV)                                         
         TEMP4 = ID4(1)                                                 
         CALL SETD(LDV, 0D0, WS(TEMP4))                                 
         TEMP4 = ID4(2)                                                 
         CALL SETD(LDVT, 0D0, WS(TEMP4))                                
         IF (USENFD) GOTO 21                                            
            FAILED = .FALSE.                                            
            NES = NES+1                                                 
            TEMP4 = ID4(1)                                              
            TEMP3 = ID4(2)                                              
            CALL D(TJ, V, VT, NV, DV, WS(TEMP4), WS(TEMP3))             
            GOTO  23                                                    
  21        GETACC = .FALSE.                                            
            IDELV = ISTKGT(3*NV, 4)                                     
            IDELVT = IDELV+NV                                           
            ID1 = IDELVT+NV                                             
            TEMP3 = ID4(1)                                              
            TEMP4 = ID4(2)                                              
            IF (DIODEG(D, TJ, V, VT, NV, WS(IDELV), WS(IDELVT), DV, NES,
     1         FNUM, NV, LDV, LDVT, WS(TEMP3), WS(TEMP4), .TRUE., DUMMY,
     2         100, WS(ID1))) GOTO 22                                   
               CALL LEAVE                                               
               DIODED = .TRUE.                                          
               RETURN                                                   
  22        CALL ISTKRL(1)                                              
  23     IF (.NOT. FAILED) GOTO 24                                      
            FNUM = 1                                                    
            CALL LEAVE                                                  
            DIODED = .TRUE.                                             
            RETURN                                                      
  24     IF (THETA .EQ. 1D0) GOTO 26                                    
            TEMP4 = ID4(1)                                              
            TEMP3 = ID4(1)+LDV-1                                        
            DO  25 I = TEMP4, TEMP3                                     
               WS(I) = WS(I)*THETA                                      
  25           CONTINUE                                                 
  26     CONTINUE                                                       
  27  GETACT = GETJAC .AND. (.NOT. SEPATE)                              
      TEMP5 = USENNS                                                    
      IF (.NOT. TEMP5) TEMP5 = USENFD                                   
      IF (.NOT. TEMP5) GOTO 28                                          
         AJ = THETA                                                     
         BJ = DT                                                        
         GOTO  29                                                       
  28     AJ = 1                                                         
         BJ = 1                                                         
  29  GETACC = GETACT                                                   
      TEMP5 = NEESUM                                                    
      IF (TEMP5) TEMP5 = USENNS                                         
      NEEUMC = TEMP5                                                    
      IF (.NOT. USENFD) GOTO 30                                         
         GETDVT = KEEJAC .GT. 1                                         
         GOTO  31                                                       
  30     GETDVT = .TRUE.                                                
  31  CALL SETD(NV, 0D0, DV)                                            
      IF (.NOT. GETACT) GOTO 43                                         
         CALL SETD(LDV, 0D0, WS(ID43))                                  
         IF (ID4(2) .LE. 0) GOTO 32                                     
            TEMP3 = ID4(2)                                              
            CALL SETD(LDVT, 0D0, WS(TEMP3))                             
            GOTO  33                                                    
  32        CALL SETD(MIN0(100, LDVT), 0D0, DUMMY)                      
  33     IF (SEPATE) NEEUMC = .FALSE.                                   
         FAILED = .FALSE.                                               
         IF (ID4(2) .LE. 0) GOTO 37                                     
            IF (USENFD) GOTO 34                                         
               NES = NES+1                                              
               TEMP3 = ID4(2)                                           
               CALL D(T, V, VT, NV, DV, WS(ID43), WS(TEMP3))            
               GOTO  36                                                 
  34           GETACC = .FALSE.                                         
               IDELV = ISTKGT(3*NV, 4)                                  
               IDELVT = IDELV+NV                                        
               ID1 = IDELVT+NV                                          
               TEMP3 = ID4(2)                                           
               IF (DIODEG(D, T, V, VT, NV, WS(IDELV), WS(IDELVT), DV,   
     1            NES, FNUM, NV, LDV, LDVT, WS(ID43), WS(TEMP3), GETDVT,
     2            DUMMY, 100, WS(ID1))) GOTO 35                         
                  CALL LEAVE                                            
                  DIODED = .TRUE.                                       
                  RETURN                                                
  35           CALL ISTKRL(1)                                           
  36        CONTINUE                                                    
            GOTO  42                                                    
  37        IF (USENFD) GOTO 39                                         
               NES = NES+1                                              
               CALL D(T, V, VT, NV, DV, WS(ID43), DUMMY)                
               TEMP3 = MIN0(100, LDVT)                                  
               DO  38 I = 1, TEMP3                                      
C/6S                                                                    
C                 IF (DUMMY(I) .NE. 0D0) CALL SETERR(                   
C    1               38HDIODED - D USED DVT WHEN USENNS = TRUE, 38, 1, 2
C    2               )                                                  
C/7S                                                                    
                  IF (DUMMY(I) .NE. 0D0) CALL SETERR(                   
     1               'DIODED - D USED DVT WHEN USENNS = TRUE', 38, 1, 2 
     2               )                                                  
C/                                                                      
  38              CONTINUE                                              
               GOTO  41                                                 
  39           GETACC = .FALSE.                                         
               IDELV = ISTKGT(2*NV, 4)                                  
               ID1 = IDELV+NV                                           
               IF (DIODEG(D, T, V, VT, NV, WS(IDELV), DUMMY, DV, NES,   
     1            FNUM, NV, LDV, LDVT, WS(ID43), DUMMY, GETDVT, DUMMY,  
     2            100, WS(ID1))) GOTO 40                                
                  CALL LEAVE                                            
                  DIODED = .TRUE.                                       
                  RETURN                                                
  40           CONTINUE                                                 
  41        CONTINUE                                                    
  42     CONTINUE                                                       
         GOTO  51                                                       
  43     IF (.NOT. USENGJ) GOTO 44                                      
            IDV = 0                                                     
            IDVT = 0                                                    
            CALL SETD(MIN0(100, MAX0(LDV, LDVT)), 0D0, DUMMY)           
            GOTO  47                                                    
  44        IF (USENNS) GOTO 45                                         
               IDV = ISTKGT(LDV+LDVT, 4)                                
               IDVT = IDV+LDV                                           
C DEFAULT VALUES.                                                       
               CALL SETD(LDV+LDVT, 0D0, WS(IDV))                        
               GOTO  46                                                 
  45           IDV = ISTKGT(MAX0(LDV, LDVT), 4)                         
               IDVT = IDV                                               
C DEFAULT VALUES.                                                       
               CALL SETD(MAX0(LDV, LDVT), 0D0, WS(IDV))                 
  46        CONTINUE                                                    
  47     FAILED = .FALSE.                                               
         NES = NES+1                                                    
         IF (IDV .LE. 0) GOTO 48                                        
            CALL D(T, V, VT, NV, DV, WS(IDV), WS(IDVT))                 
            GOTO  50                                                    
  48        CALL D(T, V, VT, NV, DV, DUMMY, DUMMY)                      
            TEMP3 = MIN0(100, MAX0(LDV, LDVT))                          
            DO  49 I = 1, TEMP3                                         
C/6S                                                                    
C              IF (DUMMY(I) .NE. 0D0) CALL SETERR(                      
C    1            48HDIODED - D USED DV AND/OR DVT WHEN USENGJ = TRUE,  
C    2            48, 2, 2)                                             
C/7S                                                                    
               IF (DUMMY(I) .NE. 0D0) CALL SETERR(                      
     1            'DIODED - D USED DV AND/OR DVT WHEN USENGJ = TRUE',   
     2            48, 2, 2)                                             
C/                                                                      
  49           CONTINUE                                                 
  50     IF (.NOT. USENGJ) CALL ISTKRL(1)                               
  51  IF (.NOT. FAILED) GOTO 52                                         
         FNUM = 1                                                       
         CALL LEAVE                                                     
         DIODED = .TRUE.                                                
         RETURN                                                         
  52  DO  53 I = 1, NV                                                  
         DV(I) = -DV(I)                                                 
  53     CONTINUE                                                       
      TEMP5 = GETACT                                                    
      IF (TEMP5) TEMP5 = .NOT. USENNS                                   
      IF (TEMP5) TEMP5 = .NOT. USENFD                                   
      IF (TEMP5) TEMP5 = THETA .NE. 1D0                                 
      IF (.NOT. TEMP5) GOTO 55                                          
         TEMP3 = ID43+LDV-1                                             
         DO  54 I = ID43, TEMP3                                         
            WS(I) = WS(I)*THETA                                         
  54        CONTINUE                                                    
  55  TEMP5 = NEESUM                                                    
      IF (.NOT. TEMP5) GOTO 56                                          
         TEMP6 = .NOT. USENNS                                           
         IF (TEMP6) TEMP6 = .NOT. USENFD                                
         IF (.NOT. TEMP6) TEMP6 = SEPATE                                
         TEMP5 = TEMP6                                                  
  56  IF (.NOT. TEMP5) GOTO 58                                          
         I0 = ID4(1)                                                    
         I1 = ID4(2)                                                    
         TEMP3 = ID43+LDV-1                                             
         DO  57 I = ID43, TEMP3                                         
            WS(I) = WS(I0)+WS(I1)/DT                                    
            I0 = I0+1                                                   
            I1 = I1+1                                                   
  57        CONTINUE                                                    
  58  IF ((.NOT. NEESUM) .AND. (.NOT. GETJAC)) GOTO 64                  
         NFS = NFS+1                                                    
         IF (IRCS .NE. 1) GOTO 62                                       
            DO  60 I = 1, NV                                            
C SCALE THE JACOBIAN.                                                   
C FIRST, GET THE ROW SCALE FACTORS.                                     
               TEMP = 0                                                 
               TEMP3 = IS(IMEM)+I-1                                     
               TEMP4 = IS(IMEM)+NV**2-1                                 
               DO  59 J = TEMP3, TEMP4, NV                              
                  TEMP = DMAX1(TEMP, DABS(WS(J)))                       
  59              CONTINUE                                              
               TEMP4 = IS(IMEM+5)+I                                     
               WS(TEMP4-1) = TEMP                                       
  60           CONTINUE                                                 
C COLUMN SCALE FACTORS.                                                 
            TEMP4 = IS(IMEM)                                            
            TEMP3 = IS(IMEM+5)                                          
            TEMP2 = IS(IMEM+6)                                          
            TEMP1 = IS(IMEM+7)                                          
            CALL DRCSC(WS(TEMP4), NV, NV, WS(TEMP3), WS(TEMP2), IS(     
     1         TEMP1))                                                  
C SCALE THE JACBOBIAN.                                                  
            TEMP1 = IS(IMEM)                                            
            TEMP2 = IS(IMEM+5)                                          
            TEMP3 = IS(IMEM+6)                                          
            TEMP4 = IS(IMEM+7)                                          
            CALL DRCSS(WS(TEMP1), NV, NV, WS(TEMP2), WS(TEMP3), IS(     
     1         TEMP4))                                                  
            IF (NERROR(NERR) .EQ. 0) GOTO 61                            
               CALL ERROFF                                              
C/6S                                                                    
C              CALL SETERR(                                             
C    1            43HDIODED - MUST HAVE 1/(S*L) IN MACHINE RANGE, 43, 3,
C    2            2)                                                    
C/7S                                                                    
               CALL SETERR(                                             
     1            'DIODED - MUST HAVE 1/(S*L) IN MACHINE RANGE', 43, 3, 
     2            2)                                                    
C/                                                                      
  61        CONTINUE                                                    
C FACTOR JACOBIAN.                                                      
  62     TEMP4 = IS(IMEM)                                               
         TEMP3 = IS(IMEM+1)                                             
         TEMP2 = IS(IMEM+2)                                             
         CALL DQRD(NV, NV, WS(TEMP4), WS(TEMP3), IS(TEMP2))             
         IF (NERROR(NERR) .EQ. 0) GOTO 63                               
            CALL ERROFF                                                 
            FNUM = 2                                                    
            CALL LEAVE                                                  
            DIODED = .TRUE.                                             
            RETURN                                                      
  63     CONTINUE                                                       
  64  IF (IRCS .NE. 1) GOTO 65                                          
         TEMP2 = IS(IMEM+5)                                             
C SCALE FACTORS FOR THE RHS.                                            
         TEMP3 = IS(IMEM+8)                                             
         TEMP4 = IS(IMEM+9)                                             
         CALL DRCSC(DV, NV, 1, WS(TEMP2), WS(TEMP3), IS(TEMP4))         
C SCALE THE RHS.                                                        
         TEMP4 = IS(IMEM+5)                                             
         TEMP3 = IS(IMEM+8)                                             
         TEMP2 = IS(IMEM+9)                                             
         CALL DRCSB(DV, NV, 1, WS(TEMP4), WS(TEMP3), IS(TEMP2))         
C SOLVE JACOBIAN * DV = RHS.                                            
  65  TEMP2 = IS(IMEM)                                                  
      TEMP3 = IS(IMEM+1)                                                
      TEMP4 = IS(IMEM+2)                                                
      CALL DQRQTB(NV, NV, WS(TEMP2), WS(TEMP3), IS(TEMP4), 1, DV, DV)   
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
C    1   32HDIODED - SINGULAR DJAC IN DQRQTB, 32, 4, 2)                 
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
     1   'DIODED - SINGULAR DJAC IN DQRQTB', 32, 4, 2)                  
C/                                                                      
      IF (IRCS .NE. 1) GOTO 68                                          
         TEMP4 = IS(IMEM+6)                                             
C UN-SCALE THE SOLUTION.                                                
         TEMP3 = IS(IMEM+7)                                             
         TEMP2 = IS(IMEM+8)                                             
         TEMP1 = IS(IMEM+9)                                             
         CALL DRCSX(DV, NV, 1, WS(TEMP4), IS(TEMP3), WS(TEMP2), IS(     
     1      TEMP1))                                                     
         IF (NERROR(NERR) .EQ. 0) GOTO 67                               
            IF (NERR .NE. 8) GOTO 66                                    
               CALL ERROFF                                              
C/6S                                                                    
C              CALL SETERR(26HDIODED - DV HAS OVERFLOWED, 26, 5, 2)     
C/7S                                                                    
               CALL SETERR('DIODED - DV HAS OVERFLOWED', 26, 5, 2)      
C/                                                                      
  66        CONTINUE                                                    
  67     CONTINUE                                                       
  68  CALL LEAVE                                                        
      DIODED = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION DIODEE(V, NV, T, DT, ERRPAR, ERPUTS, EV)         
      INTEGER NV                                                        
      REAL ERRPAR(2), EV(NV)                                            
      LOGICAL ERPUTS                                                    
      DOUBLE PRECISION V(NV), T, DT                                     
      INTEGER I                                                         
      REAL TEMP, DTPOW                                                  
      LOGICAL CONGED                                                    
      DOUBLE PRECISION DABS                                             
C THE STANDARD ERROR PROCEDURE FOR DIODES.                              
C SCRATCH SPACE ALLOCATED - NONE.                                       
      IF (.NOT. ERPUTS) GOTO 1                                          
         DTPOW = DABS(DT)                                               
         GOTO  2                                                        
   1     DTPOW = 1                                                      
   2  CONGED = .TRUE.                                                   
C ERROR FOR V.                                                          
      DO  3 I = 1, NV                                                   
         TEMP = DTPOW*(ERRPAR(1)*DABS(V(I))+ERRPAR(2))                  
         IF (TEMP .LT. EV(I)) CONGED = .FALSE.                          
         EV(I) = TEMP                                                   
   3     CONTINUE                                                       
      DIODEE = CONGED                                                   
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION DIODEG(D, T, V, VT, NV, DELV, DELVT, D0,         
     1   NES, FNUM, LDIM, LDV, LDVT, DV, DVT, GETDVT, DUMMY, LDUMMY, D1)
      INTEGER LDUMMY, LDIM, NV                                          
      EXTERNAL D                                                        
      INTEGER NES, FNUM, LDV, LDVT                                      
      LOGICAL GETDVT                                                    
      DOUBLE PRECISION T, V(NV), VT(NV), DELV(NV), DELVT(NV), D0(NV)    
      DOUBLE PRECISION DV(LDIM, 1), DVT(LDIM, 1), DUMMY(LDUMMY), D1(NV) 
      COMMON /DIODEJ/ AJ, BJ, GETJAC, NEESUM                            
      LOGICAL GETJAC, NEESUM                                            
      DOUBLE PRECISION AJ, BJ                                           
      COMMON /DIODEF/ FAILED                                            
      LOGICAL FAILED                                                    
      INTEGER MIN0, MAX0, I, J                                          
      DOUBLE PRECISION BIG, DELVTI, VTSAVE, DABS, DELVI, VSAVE          
      DOUBLE PRECISION D1MACH, DSQRT, SQRTR                             
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      DATA SQRTR/0D0/                                                   
      DATA BIG/0D0/                                                     
C THE FINITE-DIFFERENCE JACOBIAN GETTER FOR IODE.                       
      CALL SETD(LDUMMY, 0D0, DUMMY)                                     
      IF (SQRTR .EQ. 0D0) SQRTR = DSQRT(D1MACH(4))                      
C SQRT( MACHINE PRECISION ).                                            
      IF (BIG .EQ. 0D0) BIG = D1MACH(2)                                 
C THE BIGGEST FP NUMBER.                                                
C GET INITIAL D VALUE AND DELV, DELVT TERMS.                            
      CALL SETD(NV, 0D0, D0)                                            
      FAILED = .FALSE.                                                  
      NES = NES+1                                                       
      DO  1 I = 1, NV                                                   
         DELV(I) = SQRTR*DABS(V(I))                                     
         IF (DELV(I) .EQ. 0D0) DELV(I) = SQRTR                          
   1     CONTINUE                                                       
      IF (.NOT. GETDVT) GOTO 3                                          
         DO  2 I = 1, NV                                                
            DELVT(I) = SQRTR*DABS(VT(I))                                
            IF (DELVT(I) .EQ. 0D0) DELVT(I) = SQRTR                     
   2        CONTINUE                                                    
   3  CALL D(T, V, VT, NV, D0, DELV, DELVT)                             
      DO  4 J = 1, NV                                                   
C/6S                                                                    
C        IF (DELV(J) .EQ. 0D0) CALL SETERR(                             
C    1      50HDIODEG - D RETURNED 0 VALUES FOR DELV AND/OR DELVT, 50, 1
C    2      , 2)                                                        
C/7S                                                                    
         IF (DELV(J) .EQ. 0D0) CALL SETERR(                             
     1      'DIODEG - D RETURNED 0 VALUES FOR DELV AND/OR DELVT', 50, 1 
     2      , 2)                                                        
C/                                                                      
   4     CONTINUE                                                       
      IF (.NOT. GETDVT) GOTO 6                                          
         DO  5 J = 1, NV                                                
C/6S                                                                    
C           IF (DELVT(J) .EQ. 0D0) CALL SETERR(                         
C    1         50HDIODEG - D RETURNED 0 VALUES FOR DELV AND/OR DELVT,   
C    2         50, 1, 2)                                                
C/7S                                                                    
            IF (DELVT(J) .EQ. 0D0) CALL SETERR(                         
     1         'DIODEG - D RETURNED 0 VALUES FOR DELV AND/OR DELVT',    
     2         50, 1, 2)                                                
C/                                                                      
   5        CONTINUE                                                    
         GOTO  8                                                        
   6     TEMP1 = MIN0(LDUMMY, LDVT)                                     
         DO  7 J = 1, TEMP1                                             
C/6S                                                                    
C           IF (DUMMY(J) .NE. 0D0) CALL SETERR(                         
C    1         29HDIODEG - D USED DV AND/OR DVT, 29, 2, 2)              
C/7S                                                                    
            IF (DUMMY(J) .NE. 0D0) CALL SETERR(                         
     1         'DIODEG - D USED DV AND/OR DVT', 29, 2, 2)               
C/                                                                      
   7        CONTINUE                                                    
   8  IF (.NOT. FAILED) GOTO 9                                          
         FNUM = 1                                                       
         DIODEG = .FALSE.                                               
         RETURN                                                         
   9  IF (.NOT. GETDVT) GOTO 22                                         
         DO  15 I = 1, NV                                               
C USE FDS TO GET DV AND DVT.                                            
            DELVI = DABS(DELV(I))                                       
            IF (V(I) .GT. 0D0) DELVI = -DELVI                           
C GUARD AGAINST OVERFLOW.                                               
            VSAVE = V(I)                                                
            V(I) = V(I)+DELVI                                           
            CALL SETD(NV, 0D0, D1)                                      
            FAILED = .FALSE.                                            
            NES = NES+1                                                 
            CALL D(T, V, VT, NV, D1, DUMMY, DUMMY)                      
            TEMP1 = MIN0(LDUMMY, MAX0(LDV, LDVT))                       
            DO  10 J = 1, TEMP1                                         
C/6S                                                                    
C              IF (DUMMY(J) .NE. 0D0) CALL SETERR(                      
C    1            29HDIODEG - D USED DV AND/OR DVT, 29, 2, 2)           
C/7S                                                                    
               IF (DUMMY(J) .NE. 0D0) CALL SETERR(                      
     1            'DIODEG - D USED DV AND/OR DVT', 29, 2, 2)            
C/                                                                      
  10           CONTINUE                                                 
            IF (.NOT. FAILED) GOTO 11                                   
               FNUM = 1                                                 
               DIODEG = .FALSE.                                         
               RETURN                                                   
  11        DO  14 J = 1, NV                                            
               TEMP = DABS(DELVI) .LT. 1D0                              
               IF (TEMP) TEMP = DABS(D1(J)-D0(J)) .GT. DABS(DELVI)*BIG  
               IF (.NOT. TEMP) GOTO 12                                  
                  DV(J, I) = BIG                                        
C OVERFLOW.                                                             
                  GOTO  13                                              
  12              DV(J, I) = (D1(J)-D0(J))/DELVI                        
  13           CONTINUE                                                 
  14           CONTINUE                                                 
            V(I) = VSAVE                                                
  15        CONTINUE                                                    
         DO  21 I = 1, NV                                               
            DELVTI = DABS(DELVT(I))                                     
            IF (VT(I) .GT. 0D0) DELVTI = -DELVTI                        
C GUARD AGAINST OVERFLOW.                                               
            VTSAVE = VT(I)                                              
            VT(I) = VT(I)+DELVTI                                        
            CALL SETD(NV, 0D0, D1)                                      
            FAILED = .FALSE.                                            
            NES = NES+1                                                 
            CALL D(T, V, VT, NV, D1, DUMMY, DUMMY)                      
            TEMP1 = MIN0(LDUMMY, MAX0(LDV, LDVT))                       
            DO  16 J = 1, TEMP1                                         
C/6S                                                                    
C              IF (DUMMY(J) .NE. 0D0) CALL SETERR(                      
C    1            29HDIODEG - D USED DV AND/OR DVT, 29, 2, 2)           
C/7S                                                                    
               IF (DUMMY(J) .NE. 0D0) CALL SETERR(                      
     1            'DIODEG - D USED DV AND/OR DVT', 29, 2, 2)            
C/                                                                      
  16           CONTINUE                                                 
            IF (.NOT. FAILED) GOTO 17                                   
               FNUM = 1                                                 
               DIODEG = .FALSE.                                         
               RETURN                                                   
  17        DO  20 J = 1, NV                                            
               TEMP = DABS(DELVTI) .LT. 1D0                             
               IF (TEMP) TEMP = DABS(D1(J)-D0(J)) .GT. DABS(DELVTI)*BIG 
               IF (.NOT. TEMP) GOTO 18                                  
                  DVT(J, I) = BIG                                       
C OVERFLOW.                                                             
                  GOTO  19                                              
  18              DVT(J, I) = (D1(J)-D0(J))/DELVTI                      
  19           CONTINUE                                                 
  20           CONTINUE                                                 
            VT(I) = VTSAVE                                              
  21        CONTINUE                                                    
         GOTO  29                                                       
  22     DO  28 I = 1, NV                                               
C NOT GETTING DVT.                                                      
            VSAVE = V(I)                                                
            VTSAVE = VT(I)                                              
            DELVI = DABS(DELV(I))                                       
            V(I) = V(I)+DELVI*AJ                                        
            VT(I) = VT(I)+DELVI/BJ                                      
            CALL SETD(NV, 0D0, D1)                                      
            FAILED = .FALSE.                                            
            NES = NES+1                                                 
            CALL D(T, V, VT, NV, D1, DUMMY, DUMMY)                      
            TEMP1 = MIN0(LDUMMY, MAX0(LDV, LDVT))                       
            DO  23 J = 1, TEMP1                                         
C/6S                                                                    
C              IF (DUMMY(J) .NE. 0D0) CALL SETERR(                      
C    1            29HDIODEG - D USED DV AND/OR DVT, 29, 2, 2)           
C/7S                                                                    
               IF (DUMMY(J) .NE. 0D0) CALL SETERR(                      
     1            'DIODEG - D USED DV AND/OR DVT', 29, 2, 2)            
C/                                                                      
  23           CONTINUE                                                 
            IF (.NOT. FAILED) GOTO 24                                   
               FNUM = 1                                                 
               DIODEG = .FALSE.                                         
               RETURN                                                   
  24        DO  27 J = 1, NV                                            
               TEMP = DABS(DELVI) .LT. 1D0                              
               IF (TEMP) TEMP = DABS(D1(J)-D0(J)) .GT. BIG*DABS(DELVI)  
               IF (.NOT. TEMP) GOTO 25                                  
                  DV(J, I) = BIG                                        
C OVERFLOW.                                                             
                  GOTO  26                                              
  25              DV(J, I) = (D1(J)-D0(J))/DELVI                        
  26           CONTINUE                                                 
  27           CONTINUE                                                 
            V(I) = VSAVE                                                
            VT(I) = VTSAVE                                              
  28        CONTINUE                                                    
  29  DIODEG = .TRUE.                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE DIODEN(NV, T, DT, VOLD, V, VT)                         
      INTEGER NV                                                        
      DOUBLE PRECISION T, DT, VOLD(NV), V(NV), VT(NV)                   
C THE DEFAULT NEWTON ITERATION INITIALIZER FOR IODES.                   
      RETURN                                                            
      END                                                               
      SUBROUTINE DIODER(V, NV, TSTART, TSTOP, DT, D, ERROR,             
     1   ERRPAR, INMI, DA, HANDLE)                                      
      INTEGER NV                                                        
      EXTERNAL D, ERROR, INMI, DA, HANDLE                               
      REAL ERRPAR(2)                                                    
      DOUBLE PRECISION V(NV), TSTART, TSTOP, DT                         
      INTEGER I, N(100), KEEJAC, KMAX, MMAX, IZAP                       
      INTEGER IRCS, KINIT, MINIT, MAXIT                                 
      REAL HFRACT, RZAP, EGIVE                                          
      LOGICAL USENFD, USENGJ, USENNS, ERPUTS, LZAP, XPOLY               
      DOUBLE PRECISION BETA, FZAP, GAMMA, DELTA, THETA                  
C THE SECOND, ROUTINE, LEVEL OF DIODE.                                  
C RETRIEVE THE VALUES TO BE USED.                                       
      CALL DIODEV(-1, THETA, RZAP, IZAP, LZAP)                          
      CALL DIODEV(-2, BETA, RZAP, IZAP, LZAP)                           
      CALL DIODEV(-3, GAMMA, RZAP, IZAP, LZAP)                          
      CALL DIODEV(-4, DELTA, RZAP, IZAP, LZAP)                          
      CALL DIODEV(-1001, FZAP, HFRACT, IZAP, LZAP)                      
      CALL DIODEV(-1002, FZAP, EGIVE, IZAP, LZAP)                       
      CALL DIODEV(-2001, FZAP, RZAP, KEEJAC, LZAP)                      
      CALL DIODEV(-2002, FZAP, RZAP, MINIT, LZAP)                       
      CALL DIODEV(-2003, FZAP, RZAP, MAXIT, LZAP)                       
      CALL DIODEV(-2004, FZAP, RZAP, KMAX, LZAP)                        
      CALL DIODEV(-2005, FZAP, RZAP, KINIT, LZAP)                       
      CALL DIODEV(-2006, FZAP, RZAP, MMAX, LZAP)                        
      CALL DIODEV(-2007, FZAP, RZAP, IRCS, LZAP)                        
      CALL DIODEV(-3001, FZAP, RZAP, IZAP, XPOLY)                       
      CALL DIODEV(-3002, FZAP, RZAP, IZAP, ERPUTS)                      
      CALL DIODEV(-3003, FZAP, RZAP, IZAP, USENGJ)                      
      CALL DIODEV(-3004, FZAP, RZAP, IZAP, USENNS)                      
      CALL DIODEV(-3005, FZAP, RZAP, IZAP, USENFD)                      
C TEST FOR ERRORS.                                                      
C/6S                                                                    
C     IF (KMAX .LT. 1) CALL SETERR(18HDIODE4 - KMAX.LT.1, 18, 8, 2)     
C     IF (MMAX .LT. KMAX+2) CALL SETERR(23HDIODE4 - MMAX.LT.KMAX+2, 23  
C    1   , 14, 2)                                                       
C/7S                                                                    
      IF (KMAX .LT. 1) CALL SETERR('DIODE4 - KMAX.LT.1', 18, 8, 2)      
      IF (MMAX .LT. KMAX+2) CALL SETERR('DIODE4 - MMAX.LT.KMAX+2', 23   
     1   , 14, 2)                                                       
C/                                                                      
      DO  1 I = 1, MMAX                                                 
         CALL DIODEV(-(I+4000), FZAP, RZAP, N(I), LZAP)                 
   1     CONTINUE                                                       
C TEST N FOR MONOTONICITY.                                              
      DO  2 I = 2, MMAX                                                 
C/6S                                                                    
C        IF (N(I) .LE. N(I-1)) CALL SETERR(                             
C    1      37HDIODE4 - N IS NOT MONOTONE INCREASING, 37, 16, 2)        
C/7S                                                                    
         IF (N(I) .LE. N(I-1)) CALL SETERR(                             
     1      'DIODE4 - N IS NOT MONOTONE INCREASING', 37, 16, 2)         
C/                                                                      
   2     CONTINUE                                                       
      CALL DIODE4(V, NV, TSTART, TSTOP, DT, D, THETA, KEEJAC, MINIT,    
     1   MAXIT, USENGJ, USENNS, USENFD, BETA, GAMMA, DELTA, N, MMAX,    
     2   HFRACT, EGIVE, KMAX, XPOLY, KINIT, IRCS, ERROR, ERRPAR, ERPUTS,
     3   INMI, DA, HANDLE)                                              
      RETURN                                                            
      END                                                               
      SUBROUTINE DIODEV(J, F, R, I, L)                                  
      INTEGER J, I                                                      
      REAL R                                                            
      LOGICAL L                                                         
      DOUBLE PRECISION F                                                
      INTEGER MAX0, K, M, N(100), IABS, KEEJAC                          
      INTEGER KMAX, IRCS, MMAX, KINIT, MINIT, MAXIT                     
      REAL HFRACT, EGIVE, FLOAT                                         
      LOGICAL USENFD, USENGJ, USENNS, ERPUTS, XPOLY                     
      DOUBLE PRECISION BETA, DBLE, GAMMA, DELTA, THETA, DSQRT           
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
      DATA THETA/1D0/                                                   
      DATA BETA/1D0/                                                    
      DATA GAMMA/1D0/                                                   
      DATA DELTA/0D0/                                                   
      DATA HFRACT/1E0/                                                  
      DATA EGIVE/1E+2/                                                  
      DATA KEEJAC/0/                                                    
      DATA MINIT/10/                                                    
      DATA MAXIT/50/                                                    
      DATA KMAX/10/                                                     
      DATA KINIT/4/                                                     
      DATA MMAX/15/                                                     
      DATA IRCS/1/                                                      
      DATA XPOLY/.FALSE./                                               
      DATA ERPUTS/.FALSE./                                              
      DATA USENGJ/.FALSE./                                              
      DATA USENNS/.FALSE./                                              
      DATA USENFD/.FALSE./                                              
      DATA N(1)/1/, N(2)/0/, N(3)/0/                                    
C THE PARAMETER SETTING ROUTINE FOR IODE.                               
C THE VARIABLES ARE                                                     
C J = 1.                                                                
C J = 2.                                                                
C J = 3.                                                                
C J = 4.                                                                
C J = 1001.                                                             
C J = 1002.                                                             
C J = 2001.                                                             
C J = 2002.                                                             
C J = 2003.                                                             
C J = 2004.                                                             
C J = 2005.                                                             
C J = 2006.                                                             
C J = 2007. 0 DO NOT SCALE, +1 SCALE (DEFAULT).                         
C J = 3001.                                                             
C J = 3002.                                                             
C J = 3003.                                                             
C J = 3004.                                                             
C J = 3005.                                                             
C J = 4001, ... , 4100.                                                 
      GOTO  58                                                          
C   EXPORT THE VARIABLES.                                               
   1     F = THETA                                                      
         GOTO  59                                                       
   2     F = BETA                                                       
         GOTO  59                                                       
   3     F = GAMMA                                                      
         GOTO  59                                                       
   4     F = DELTA                                                      
         GOTO  59                                                       
   5     R = HFRACT                                                     
         GOTO  59                                                       
   6     R = EGIVE                                                      
         GOTO  59                                                       
   7     I = KEEJAC                                                     
         GOTO  59                                                       
   8     I = MINIT                                                      
         GOTO  59                                                       
   9     I = MAXIT                                                      
         GOTO  59                                                       
  10     I = KMAX                                                       
         GOTO  59                                                       
  11     I = KINIT                                                      
         GOTO  59                                                       
  12     I = MMAX                                                       
         GOTO  59                                                       
  13     I = IRCS                                                       
         GOTO  59                                                       
  14     L = XPOLY                                                      
         GOTO  59                                                       
  15     L = ERPUTS                                                     
         GOTO  59                                                       
  16     L = USENGJ                                                     
         GOTO  59                                                       
  17     L = USENNS                                                     
         GOTO  59                                                       
  18     L = USENFD                                                     
         GOTO  59                                                       
C IODE VERSION NUMBER.                                                  
  19     F = 3D0                                                        
         GOTO  59                                                       
C SET THE VARIABLES TO THE DEFAULTS.                                    
  20     THETA = 1D0                                                    
         BETA = 1                                                       
         GAMMA = 1                                                      
         DELTA = 0                                                      
         HFRACT = 1                                                     
         EGIVE = 1E+2                                                   
         KEEJAC = 0                                                     
         MINIT = 10                                                     
         MAXIT = 50                                                     
         KMAX = 10                                                      
         KINIT = 4                                                      
         MMAX = 15                                                      
         IRCS = 1                                                       
         XPOLY = .FALSE.                                                
         ERPUTS = .FALSE.                                               
         USENGJ = .FALSE.                                               
         USENNS = .FALSE.                                               
         USENFD = .FALSE.                                               
         CALL SETI(100, 0, N)                                           
         N(1) = 1                                                       
C   IMPORT THE VARIABLES.                                               
         GOTO  59                                                       
  21     THETA = F                                                      
         IF (THETA .EQ. 0.5) GOTO 22                                    
            GAMMA = 1                                                   
            HFRACT = 1                                                  
            GOTO  26                                                    
  22        GAMMA = 2                                                   
            HFRACT = 0.5                                                
            N(1) = 2                                                    
            N(2) = 4                                                    
            N(3) = 6                                                    
            M = 4                                                       
               GOTO  24                                                 
  23           M = M+1                                                  
  24           IF (M .GT. MMAX) GOTO  25                                
               N(M) = 2*N(M-2)                                          
               GOTO  23                                                 
  25        CONTINUE                                                    
  26     GOTO  59                                                       
  27     BETA = F                                                       
         GOTO  59                                                       
  28     GAMMA = F                                                      
         GOTO  59                                                       
  29     DELTA = F                                                      
         GOTO  59                                                       
  30     HFRACT = R                                                     
         GOTO  59                                                       
  31     EGIVE = R                                                      
         GOTO  59                                                       
  32     KEEJAC = I                                                     
         GOTO  59                                                       
  33     MINIT = I                                                      
         GOTO  59                                                       
  34     MAXIT = I                                                      
         GOTO  59                                                       
  35     KMAX = I                                                       
         MMAX = KMAX+5                                                  
         GOTO  59                                                       
  36     KINIT = I                                                      
         GOTO  59                                                       
  37     MMAX = I                                                       
         GOTO  59                                                       
  38     IRCS = I                                                       
         GOTO  59                                                       
  39     XPOLY = L                                                      
         GOTO  59                                                       
  40     ERPUTS = L                                                     
         IF (.NOT. ERPUTS) GOTO 41                                      
            DELTA = 1                                                   
            GOTO  42                                                    
  41        DELTA = 0                                                   
  42     GOTO  59                                                       
  43     USENGJ = L                                                     
         GOTO  59                                                       
  44     USENNS = L                                                     
         GOTO  59                                                       
  45     USENFD = L                                                     
         GOTO  59                                                       
  46     TEMP1 = IABS(J) .GT. 4100                                      
         IF (.NOT. TEMP1) TEMP1 = IABS(J) .LT. 4001                     
C/6S                                                                    
C        IF (TEMP1) CALL SETERR(24HDIODEV - J OUT OF BOUNDS, 24, 1, 2)  
C/7S                                                                    
         IF (TEMP1) CALL SETERR('DIODEV - J OUT OF BOUNDS', 24, 1, 2)   
C/                                                                      
         IF (J .GE. 0) GOTO 56                                          
            IF (N(2) .NE. 0) GOTO 47                                    
               N(2) = DSQRT(2D0)*DBLE(FLOAT(N(1)))                      
C EXPORT N(ABS(J)-4000)                                                 
C ONLY N(1) IS GIVEN, USE SQRT(2) INCREASE.                             
               IF (N(2) .EQ. N(1)) N(2) = N(2)+1                        
               N(3) = DSQRT(2D0)*DBLE(FLOAT(N(2)))                      
               IF (N(3) .EQ. N(2)) N(3) = N(3)+1                        
               N(4) = 0                                                 
  47        TEMP = IABS(J)                                              
            IF (N(TEMP-4000) .NE. 0) GOTO 55                            
               DO  53 K = 1, MMAX                                       
C FILL IN THE MISSING N(M).                                             
                  IF (N(K) .NE. 0) GOTO 52                              
                     IF (K .NE. 3) GOTO 49                              
                        DO  48 M = K, MMAX                              
                           N(M) = (N(2)*N(M-1))/MAX0(1, N(1))           
  48                       CONTINUE                                     
                        GOTO  51                                        
  49                    DO  50 M = K, MMAX                              
                           N(M) = 2*N(M-2)                              
  50                       CONTINUE                                     
  51                 GOTO  54                                           
  52              CONTINUE                                              
  53              CONTINUE                                              
  54           CONTINUE                                                 
  55        TEMP = IABS(J)                                              
            I = N(TEMP-4000)                                            
            GOTO  57                                                    
  56        N(J-4000) = I                                               
C IMPORT N(J-4000)                                                      
            IF (J-4000 .LT. 100) N(J-3999) = 0                          
  57     CONTINUE                                                       
         GOTO  59                                                       
  58     IF (J .EQ. 3005) GOTO  45                                      
         IF (J .EQ. 3004) GOTO  44                                      
         IF (J .EQ. 3003) GOTO  43                                      
         IF (J .EQ. 3002) GOTO  40                                      
         IF (J .EQ. 3001) GOTO  39                                      
         IF (J .EQ. 2007) GOTO  38                                      
         IF (J .EQ. 2006) GOTO  37                                      
         IF (J .EQ. 2005) GOTO  36                                      
         IF (J .EQ. 2004) GOTO  35                                      
         IF (J .EQ. 2003) GOTO  34                                      
         IF (J .EQ. 2002) GOTO  33                                      
         IF (J .EQ. 2001) GOTO  32                                      
         IF (J .EQ. 1002) GOTO  31                                      
         IF (J .EQ. 1001) GOTO  30                                      
         IF (J .EQ. 4) GOTO  29                                         
         IF (J .EQ. 3) GOTO  28                                         
         IF (J .EQ. 2) GOTO  27                                         
         IF (J .EQ. 1) GOTO  21                                         
         IF (J .EQ. 0) GOTO  20                                         
         IF (J .EQ. (-6000)) GOTO  19                                   
         IF (J .EQ. (-3005)) GOTO  18                                   
         IF (J .EQ. (-3004)) GOTO  17                                   
         IF (J .EQ. (-3003)) GOTO  16                                   
         IF (J .EQ. (-3002)) GOTO  15                                   
         IF (J .EQ. (-3001)) GOTO  14                                   
         IF (J .EQ. (-2007)) GOTO  13                                   
         IF (J .EQ. (-2006)) GOTO  12                                   
         IF (J .EQ. (-2005)) GOTO  11                                   
         IF (J .EQ. (-2004)) GOTO  10                                   
         IF (J .EQ. (-2003)) GOTO  9                                    
         IF (J .EQ. (-2002)) GOTO  8                                    
         IF (J .EQ. (-2001)) GOTO  7                                    
         IF (J .EQ. (-1002)) GOTO  6                                    
         IF (J .EQ. (-1001)) GOTO  5                                    
         IF (J .EQ. (-4)) GOTO  4                                       
         IF (J .EQ. (-3)) GOTO  3                                       
         IF (J .EQ. (-2)) GOTO  2                                       
         IF (J .EQ. (-1)) GOTO  1                                       
         GOTO  46                                                       
  59  RETURN                                                            
      END                                                               
      SUBROUTINE DQRD(M, N, A, ALFA, PIVOT)                             
      INTEGER M, N                                                      
      INTEGER PIVOT(N)                                                  
      DOUBLE PRECISION A(M, N), ALFA(N)                                 
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, MIN0, I, J, K, JBAR                               
      INTEGER ISUM, IS(1000)                                            
      REAL RS(1000), FLOAT                                              
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION AKK, EPS, RNDING, DBLE, BETA, DDOT               
      DOUBLE PRECISION ALFAK, SIGMA, WS(500), D1MACH, DSQRT             
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      DATA RNDING/0D0/                                                  
C TO OBTAIN THE QR DECOMPOSITION OF A MATRIX.                           
C MNEMONIC - DOUBLE PRECISION QR DECOMPOSITION OF A MATRIX.             
C INPUT -                                                               
C   M - THE NUMBER OF ROWS IN THE MATRIX A.                             
C   N - THE NUMBER OF COLUMNS IN THE MATRIX A.                          
C   A - THE MATRIX.                                                     
C OUTPUT -                                                              
C   A     - THE UPPER TRIANGULAR PORTION OF A ABOVE THE DIAGONAL        
C           IS THE R OF THE QR DECOMPOSITION, WITH THE DIAGONAL         
C           ELEMENTS OF R IN ALFA.                                      
C           THE LOWER TRIANGULAR PORTION OF A STORES THE Q IN           
C           FACTORED HOUSEHOLDER FORM. Q*A = R.                         
C   ALFA  - THE DIAGONAL OF R.                                          
C   PIVOT - PIVOT(J) IS THE POSITION OF THE J-TH VARIABLE, J = 1,...,N, 
C           CHOSEN SO THAT THE MAXIMAL COLUMN IS ELIMINATED AT EACH     
C           STEP.                                                       
C SCRATCH SPACE ALLOCATED - N*MU WORDS.                                 
C ERROR STATES -                                                        
C   1 - M.LT.1.                                                         
C   2 - N.LT.1.                                                         
C   3 - SINGULAR MATRIX. (RECOVERABLE)                                  
C ALFA(MIN(M,N)).                                                       
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
C DEFINE SUM(J) WS(ISUM-1+J)                                            
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(13HDQRD - M.LT.1, 13, 1, 2)             
C     IF (N .LT. 1) CALL SETERR(13HDQRD - N.LT.1, 13, 2, 2)             
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DQRD - M.LT.1', 13, 1, 2)              
      IF (N .LT. 1) CALL SETERR('DQRD - N.LT.1', 13, 2, 2)              
C/                                                                      
      IF (RNDING .EQ. 0D0) RNDING = D1MACH(4)                           
      EPS = (RNDING*DBLE(FLOAT(MIN0(M, N)**3)))**2                      
      ISUM = ISTKGT(N, 4)                                               
      J = 1                                                             
         GOTO  2                                                        
   1     J = J+1                                                        
   2     IF (J .GT. N) GOTO  3                                          
C GET THE L2 NORM OF THE J-TH COLUMN.                                   
         TEMP1 = ISUM-1+J                                               
         WS(TEMP1) = DDOT(M, A(1, J), 1, A(1, J), 1)                    
         PIVOT(J) = J                                                   
         GOTO  1                                                        
   3  K = 1                                                             
         GOTO  5                                                        
   4     K = K+1                                                        
   5     IF (K .GT. MIN0(M, N)) GOTO  19                                
C ELIMINATE K-TH COLUMN.                                                
         TEMP1 = ISUM-1+K                                               
         SIGMA = WS(TEMP1)                                              
C FIND THE PIVOT COLUMN.                                                
         JBAR = K                                                       
         J = K+1                                                        
            GOTO  7                                                     
   6        J = J+1                                                     
   7        IF (J .GT. N) GOTO  9                                       
            TEMP1 = ISUM-1+J                                            
            IF (SIGMA .GE. WS(TEMP1)) GOTO 8                            
               TEMP = ISUM-1+J                                          
               SIGMA = WS(TEMP)                                         
               JBAR = J                                                 
   8        CONTINUE                                                    
            GOTO  6                                                     
   9     IF (JBAR .EQ. K) GOTO 11                                       
            I = PIVOT(K)                                                
C NEED TO INTERCHANGE THE COLUMNS.                                      
            PIVOT(K) = PIVOT(JBAR)                                      
            PIVOT(JBAR) = I                                             
            TEMP1 = ISUM-1+JBAR                                         
            TEMP = ISUM-1+K                                             
            WS(TEMP1) = WS(TEMP)                                        
            TEMP = ISUM-1+K                                             
            WS(TEMP) = SIGMA                                            
            DO  10 I = 1, M                                             
               SIGMA = A(I, K)                                          
               A(I, K) = A(I, JBAR)                                     
               A(I, JBAR) = SIGMA                                       
  10           CONTINUE                                                 
  11     SIGMA = DDOT(M-K+1, A(K, K), 1, A(K, K), 1)                    
         IF (SIGMA .GT. EPS*WS(ISUM)) GOTO 12                           
C/6S                                                                    
C           CALL SETERR(22HDQRD - SINGULAR MATRIX, 22, 3, 1)            
C/7S                                                                    
            CALL SETERR('DQRD - SINGULAR MATRIX', 22, 3, 1)             
C/                                                                      
            GOTO  19                                                    
  12     AKK = A(K, K)                                                  
         IF (AKK .GE. 0D0) GOTO 13                                      
            ALFAK = DSQRT(SIGMA)                                        
            GOTO  14                                                    
  13        ALFAK = -DSQRT(SIGMA)                                       
  14     ALFA(K) = ALFAK                                                
         BETA = 1D0/(SIGMA-AKK*ALFAK)                                   
         A(K, K) = AKK-ALFAK                                            
         J = K+1                                                        
            GOTO  16                                                    
  15        J = J+1                                                     
  16        IF (J .GT. N) GOTO  18                                      
            SIGMA = BETA*DDOT(M+1-K, A(K, K), 1, A(K, J), 1)            
            DO  17 I = K, M                                             
               A(I, J) = A(I, J)-A(I, K)*SIGMA                          
  17           CONTINUE                                                 
            TEMP = ISUM-1+J                                             
            WS(TEMP) = WS(TEMP)-A(K, J)**2                              
            GOTO  15                                                    
  18     CONTINUE                                                       
         GOTO  4                                                        
  19  CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE DQRQTB(M, N, QR, ALFA, PIVOT, NB, B, X)                
      INTEGER M, N, NB                                                  
      INTEGER PIVOT(N)                                                  
      DOUBLE PRECISION QR(M, N), ALFA(N), B(M, NB), X(N, NB)            
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, MIN0, I, J, JB, IS(1000)                          
      INTEGER IZ                                                        
      REAL RS(1000)                                                     
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION DDOT, GAMMA, WS(500)                             
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO SOLVE R*X = Q*B.                                                   
C MNEMONIC - DOUBLE PRECISION QR Q*B AND BACK-SOLVE.                    
C INPUT -                                                               
C   M     - THE NUMBER OF ROWS IN THE MATRIX.                           
C   N     - THE NUMBER OF COLUMNS IN THE MATRIX.                        
C   QR    - THE QR FACTORIZATION OF A MATRIX, AS DESCRIBED IN DQRD.     
C   ALFA  - THE DIAGONAL OF R, AS DESCRIBED IN DQRD.                    
C   PIVOT - THE PIVOTING ARRAY, AS DESCRIBED IN DQRD.                   
C   NB    - THE NUMBER OF RIGHT-HAND-SIDES.                             
C   B     - THE RIGHT-HAND-SIDES.                                       
C OUTPUT -                                                              
C   B - HAS BEEN CLOBBERED.                                             
C   X - THE SOLUTION VECTORS.                                           
C SCRATCH SPACE ALLOCATED - N*MU WORDS.                                 
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - M.LT.N.                                                         
C   3 - NB.LT.1.                                                        
C   4 - ALFA(J)=0.                                                      
C   5 - QR(J,J)=0.                                                      
C   6 - PIVOT(I) NOT ONE OF 1,...,N.                                    
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
C DEFINE Z(J) WS(IZ-1+J)                                                
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15HDQRQTB - N.LT.1, 15, 1, 2)           
C     IF (M .LT. N) CALL SETERR(15HDQRQTB - M.LT.N, 15, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(16HDQRQTB - NB.LT.1, 16, 3, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DQRQTB - N.LT.1', 15, 1, 2)            
      IF (M .LT. N) CALL SETERR('DQRQTB - M.LT.N', 15, 2, 2)            
      IF (NB .LT. 1) CALL SETERR('DQRQTB - NB.LT.1', 16, 3, 2)          
C/                                                                      
      DO  1 J = 1, N                                                    
C/6S                                                                    
C        IF (ALFA(J) .EQ. 0D0) CALL SETERR(18HDQRQTB - ALFA(J)=0, 18, 4,
C    1      2)                                                          
C/7S                                                                    
         IF (ALFA(J) .EQ. 0D0) CALL SETERR('DQRQTB - ALFA(J)=0', 18, 4, 
     1      2)                                                          
C/                                                                      
   1     CONTINUE                                                       
      TEMP = MIN0(M, N)                                                 
      DO  2 J = 1, TEMP                                                 
C/6S                                                                    
C        IF (QR(J, J) .EQ. 0D0) CALL SETERR(18HDQRQTB - QR(J,J)=0, 18, 5
C    1      , 2)                                                        
C/7S                                                                    
         IF (QR(J, J) .EQ. 0D0) CALL SETERR('DQRQTB - QR(J,J)=0', 18, 5 
     1      , 2)                                                        
C/                                                                      
   2     CONTINUE                                                       
      DO  3 I = 1, N                                                    
C/6S                                                                    
C        IF (PIVOT(I) .LT. 1 .OR. PIVOT(I) .GT. N) CALL SETERR(         
C    1      36HDQRQTB - PIVOT(I) NOT ONE OF 1,...,N, 36, 6, 2)          
C/7S                                                                    
         IF (PIVOT(I) .LT. 1 .OR. PIVOT(I) .GT. N) CALL SETERR(         
     1      'DQRQTB - PIVOT(I) NOT ONE OF 1,...,N', 36, 6, 2)           
C/                                                                      
   3     CONTINUE                                                       
C FORM Q*B.                                                             
C MULTIPLY ALL THE VECTORS.                                             
      DO  6 JB = 1, NB                                                  
C APPLY THE J-TH TRANSFORMATION.                                        
         TEMP = MIN0(M, N)                                              
         DO  5 J = 1, TEMP                                              
            GAMMA = DDOT(M-J+1, QR(J, J), 1, B(J, JB), 1)/(ALFA(J)*QR(J,
     1         J))                                                      
            DO  4 I = J, M                                              
               B(I, JB) = B(I, JB)+GAMMA*QR(I, J)                       
   4           CONTINUE                                                 
   5        CONTINUE                                                    
   6     CONTINUE                                                       
C SOLVE R*X = Q*B.                                                      
      IZ = ISTKGT(N, 4)                                                 
C DO ALL THE RIGHT-HAND-SIDES.                                          
      DO  11 JB = 1, NB                                                 
         TEMP = IZ+N                                                    
         WS(TEMP-1) = B(N, JB)/ALFA(N)                                  
         I = N-1                                                        
            GOTO  8                                                     
   7        I = I-1                                                     
   8        IF (I .LT. 1) GOTO  9                                       
            TEMP = IZ-1+I                                               
            TEMP1 = IZ+I                                                
            WS(TEMP) = (-(DDOT(N-I, QR(I, I+1), M, WS(TEMP1), 1)-B(I,   
     1         JB)))/ALFA(I)                                            
            GOTO  7                                                     
   9     DO  10 I = 1, N                                                
            TEMP1 = PIVOT(I)                                            
            TEMP = IZ+I                                                 
            X(TEMP1, JB) = WS(TEMP-1)                                   
  10        CONTINUE                                                    
  11     CONTINUE                                                       
      CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSB(B, M, N, R, C, RC)                               
      INTEGER M, N                                                      
      INTEGER RC(N)                                                     
      DOUBLE PRECISION B(M, N), R(M), C(N)                              
      INTEGER NERROR, I, NERR, IROLD                                    
      DOUBLE PRECISION L, S, D1MACH                                     
      DATA S/0D0/                                                       
      DATA L/0D0/                                                       
C TO GET THE COLUMN SCALED MATRIX (1/R)*B*(1/C),                        
C GIVEN THE ROW SCALE FACTOR, AND RETURN THE COLUMN FACTOR.             
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16HDRCSB - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16HDRCSB - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DRCSB - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR('DRCSB - N .LT. 1', 16, 2, 2)           
C/                                                                      
      IF (S .NE. 0D0) GOTO 1                                            
         S = D1MACH(1)                                                  
         L = D1MACH(2)                                                  
   1  DO  2 I = 1, M                                                    
         IF (R(I) .EQ. 0D0) GOTO  2                                     
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      36HDRCSB - MUST HAVE S .LE. R(I) .LE. L, 36, 3, 2)          
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      'DRCSB - MUST HAVE S .LE. R(I) .LE. L', 36, 3, 2)           
C/                                                                      
   2     CONTINUE                                                       
C TURN ERROR RECOVERY ON AND SAVE OLD VALUE.                            
      CALL ENTSRC(IROLD, 1)                                             
C GET COLUMN SCALE FACTOR.                                              
      CALL DRCSC(B, M, N, R, C, RC)                                     
C APPLY THEM.                                                           
      CALL DRCSS(B, M, N, R, C, RC)                                     
      IF (NERROR(NERR) .EQ. 0) GOTO 3                                   
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(42HDRCSB - MUST HAVE 1/(S*L) IN MACHINE RANGE, 42  
C    1      , 4, 1)                                                     
C/7S                                                                    
         CALL SETERR('DRCSB - MUST HAVE 1/(S*L) IN MACHINE RANGE', 42   
     1      , 4, 1)                                                     
C/                                                                      
         RETURN                                                         
C RESTORE OLD RECOVERY VALUE.                                           
   3  CALL RETSRC(IROLD)                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSC(A, M, N, R, C, RC)                               
      INTEGER M, N                                                      
      INTEGER RC(N)                                                     
      DOUBLE PRECISION A(M, N), R(M), C(N)                              
      INTEGER I, J, RD2                                                 
      DOUBLE PRECISION AIJ, AAIJ, L, S, DABS, D1                        
      DOUBLE PRECISION D2, DMAX1, D1MACH                                
      DATA S/0D0/                                                       
      DATA L/0D0/                                                       
C TO GET THE COLUMN SCALE FACTOR FOR (1/R)*A.                           
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16HDRCSC - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16HDRCSC - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DRCSC - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR('DRCSC - N .LT. 1', 16, 2, 2)           
C/                                                                      
      IF (S .NE. 0D0) GOTO 1                                            
         S = D1MACH(1)                                                  
         L = D1MACH(2)                                                  
   1  DO  2 I = 1, M                                                    
         IF (R(I) .EQ. 0D0) GOTO  2                                     
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      36HDRCSC - MUST HAVE S .LE. R(I) .LE. L, 36, 3, 2)          
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      'DRCSC - MUST HAVE S .LE. R(I) .LE. L', 36, 3, 2)           
C/                                                                      
   2     CONTINUE                                                       
      DO  18 J = 1, N                                                   
         D2 = 0                                                         
C -1 = UNDERFLOW, 0 = IN-RANGE, +1 = OVERFLOW.                          
         RD2 = -1                                                       
         DO  17 I = 1, M                                                
            AIJ = A(I, J)                                               
            AAIJ = DABS(AIJ)                                            
            D1 = R(I)                                                   
            IF (AIJ .EQ. 0D0 .OR. D1 .EQ. 0D0) GOTO  17                 
            IF (D1 .GE. 1D0) GOTO 9                                     
               IF (AAIJ .LE. D1*L) GOTO 3                               
                  IF (RD2 .LT. 1) D2 = 0                                
C CHECK FOR OVERFLOW.                                                   
C OVERFLOW.                                                             
                  RD2 = 1                                               
                  D2 = DMAX1(D2, AAIJ*(S/D1))                           
                  GOTO  8                                               
   3              IF (RD2 .LE. 0) GOTO 4                                
                     GOTO  17                                           
C THIS ELEMENT IS IN-RANGE.                                             
C ALREADY OVERFLOWED, NO EFFECT.                                        
   4                 IF (RD2 .NE. 0) GOTO 5                             
                        D2 = DMAX1(D2, AAIJ/D1)                         
                        GOTO  6                                         
   5                    RD2 = 0                                         
C RD2 = -1.                                                             
                        D2 = AAIJ/D1                                    
   6              CONTINUE                                              
   7              CONTINUE                                              
   8           CONTINUE                                                 
               GOTO  16                                                 
   9           IF (AAIJ .GE. D1*S) GOTO 10                              
                  IF (RD2 .GE. 0) GOTO  17                              
C ELEMENT UNDERFLOW, D1 >= 1.                                           
C NO-EFFECT.                                                            
                  D2 = DMAX1(D2, AAIJ*(L/D1))                           
                  GOTO  15                                              
  10              IF (RD2 .LE. 0) GOTO 11                               
                     GOTO  17                                           
C IN-RANGE.                                                             
C NO-EFFECT.                                                            
  11                 IF (RD2 .NE. 0) GOTO 12                            
                        D2 = DMAX1(D2, AAIJ/D1)                         
                        GOTO  13                                        
  12                    RD2 = 0                                         
C UNDERFLOWED SO FAR.                                                   
                        D2 = AAIJ/D1                                    
  13              CONTINUE                                              
  14              CONTINUE                                              
  15        CONTINUE                                                    
  16        CONTINUE                                                    
  17        CONTINUE                                                    
         C(J) = D2                                                      
         RC(J) = RD2                                                    
  18     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSS(A, M, N, R, C, RC)                               
      INTEGER M, N                                                      
      INTEGER RC(N)                                                     
      DOUBLE PRECISION A(M, N), R(M), C(N)                              
      INTEGER I, J, RD2                                                 
      LOGICAL BADNGE                                                    
      DOUBLE PRECISION L, S, D1, D2, D1MACH                             
      DATA S/0D0/                                                       
      DATA L/0D0/                                                       
C TO SCALE ((1/R)*A)*(1/C).                                             
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16HDRCSS - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16HDRCSS - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DRCSS - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR('DRCSS - N .LT. 1', 16, 2, 2)           
C/                                                                      
      IF (S .NE. 0D0) GOTO 1                                            
         S = D1MACH(1)                                                  
         L = D1MACH(2)                                                  
   1  DO  2 I = 1, M                                                    
         IF (R(I) .EQ. 0D0) GOTO  2                                     
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      36HDRCSS - MUST HAVE S .LE. R(I) .LE. L, 36, 3, 2)          
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      'DRCSS - MUST HAVE S .LE. R(I) .LE. L', 36, 3, 2)           
C/                                                                      
   2     CONTINUE                                                       
      DO  3 I = 1, N                                                    
         IF (C(I) .EQ. 0D0) GOTO  3                                     
C/6S                                                                    
C        IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
C    1      36HDRCSS - MUST HAVE S .LE. C(I) .LE. L, 36, 4, 2)          
C        IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
C    1      36HDRCSS - MUST HAVE RC(I) IN (-1,0,+1), 36, 5, 2)          
C/7S                                                                    
         IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
     1      'DRCSS - MUST HAVE S .LE. C(I) .LE. L', 36, 4, 2)           
         IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
     1      'DRCSS - MUST HAVE RC(I) IN (-1,0,+1)', 36, 5, 2)           
C/                                                                      
   3     CONTINUE                                                       
C CHECK 1/(S*L) RANGE.                                                  
      BADNGE = .FALSE.                                                  
      IF (S*L .GT. 1D0) GOTO 4                                          
         IF (1D0/L .GT. S*L) BADNGE = .TRUE.                            
         GOTO  5                                                        
   4     IF (S*L .GT. 1D0/S) BADNGE = .TRUE.                            
C S*L > 1.                                                              
C/6S                                                                    
C  5  IF (BADNGE) CALL SETERR(                                          
C    1   42HDRCSX - MUST HAVE 1/(S*L) IN MACHINE RANGE, 42, 6, 1)       
C/7S                                                                    
   5  IF (BADNGE) CALL SETERR(                                          
     1   'DRCSX - MUST HAVE 1/(S*L) IN MACHINE RANGE', 42, 6, 1)        
C/                                                                      
      DO  33 I = 1, M                                                   
         D1 = R(I)                                                      
         IF (D1 .EQ. 0D0) GOTO  33                                      
         DO  32 J = 1, N                                                
            D2 = C(J)                                                   
            RD2 = RC(J)                                                 
            IF (A(I, J) .NE. 0D0 .AND. D2 .NE. 0D0) GOTO 6              
               GOTO  32                                                 
   6           IF (D1 .LT. 1D0) GOTO 19                                 
                  IF (RD2 .LE. 0) GOTO 11                               
                     IF (D2 .LT. 1D0) GOTO 7                            
                        A(I, J) = S*((A(I, J)/D1)/D2)                   
C D2 OVERFLOWED.                                                        
                        GOTO  10                                        
   7                    IF (D1*D2 .LT. 1D0) GOTO 8                      
                           A(I, J) = S*(A(I, J)/(D1*D2))                
C D2 < 1.                                                               
                           GOTO  9                                      
   8                       A(I, J) = A(I, J)*(S/(D1*D2))                
   9                    CONTINUE                                        
  10                 CONTINUE                                           
                     GOTO  18                                           
  11                 IF (D2 .LT. 1D0) GOTO 12                           
                        A(I, J) = (A(I, J)/D1)/D2                       
                        GOTO  17                                        
  12                    IF (RD2 .GE. 0) GOTO 15                         
                           IF (D2 .LT. 1D0/D1) GOTO 13                  
                              A(I, J) = A(I, J)*((L/D1)/D2)             
C D2 UNDERFLOWED.                                                       
                              GOTO  14                                  
  13                          A(I, J) = L*(A(I, J)/(D1*D2))             
  14                       CONTINUE                                     
                           GOTO  16                                     
  15                       A(I, J) = A(I, J)/(D1*D2)                    
C D2 < 1.                                                               
  16                 CONTINUE                                           
  17              CONTINUE                                              
  18              CONTINUE                                              
                  GOTO  30                                              
  19              IF (RD2 .LE. 0) GOTO 22                               
                     IF (D1*D2 .LT. 1D0) GOTO 20                        
                        A(I, J) = S*(A(I, J)/(D1*D2))                   
C D1 < 1.                                                               
C D2 OVERFLOWED.                                                        
                        GOTO  21                                        
  20                    A(I, J) = A(I, J)*((S/D1)/D2)                   
  21                 CONTINUE                                           
                     GOTO  29                                           
  22                 IF (D2 .LT. 1D0) GOTO 23                           
                        A(I, J) = A(I, J)/(D1*D2)                       
                        GOTO  28                                        
  23                    IF (RD2 .GE. 0) GOTO 26                         
                           IF (D1*D2 .GT. 1D0) GOTO 24                  
                              A(I, J) = L*(A(I, J)/(D1*D2))             
C D2 UNDERFLOWED.                                                       
                              GOTO  25                                  
  24                          A(I, J) = A(I, J)*(L/(D1*D2))             
  25                       CONTINUE                                     
                           GOTO  27                                     
  26                       A(I, J) = (A(I, J)/D1)/D2                    
C D2 < 1.                                                               
  27                 CONTINUE                                           
  28              CONTINUE                                              
  29              CONTINUE                                              
  30        CONTINUE                                                    
  31        CONTINUE                                                    
  32        CONTINUE                                                    
  33     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSX(X, N, M, R, RR, C, RC)                           
      INTEGER M, N                                                      
      INTEGER RR(N), RC(M)                                              
      DOUBLE PRECISION X(N, M), R(N), C(M)                              
      INTEGER I, J, RD2, RD3                                            
      LOGICAL BADNGE, OVELOW                                            
      DOUBLE PRECISION AXX, L, S, DABS, D2, D3                          
      DOUBLE PRECISION DMIN1, DMAX1, SL, XX, D1MACH                     
      DATA S/0D0/                                                       
      DATA L/0D0/                                                       
C TO SCALE                                                              
C   X = (1/R) * X * C.                                                  
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(16HDRCSX - N .LT. 1, 16, 1, 2)          
C     IF (M .LT. 1) CALL SETERR(16HDRCSX - M .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DRCSX - N .LT. 1', 16, 1, 2)           
      IF (M .LT. 1) CALL SETERR('DRCSX - M .LT. 1', 16, 2, 2)           
C/                                                                      
      IF (S .NE. 0D0) GOTO 1                                            
         S = D1MACH(1)                                                  
         L = D1MACH(2)                                                  
   1  CONTINUE                                                          
      SL = S*L                                                          
      DO  2 I = 1, N                                                    
         IF (R(I) .EQ. 0D0) GOTO  2                                     
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      36HDRCSX - MUST HAVE S .LE. R(I) .LE. L, 36, 3, 2)          
C        IF (RR(I) .LT. (-1) .OR. RR(I) .GT. 1) CALL SETERR(            
C    1      36HDRCSX - MUST HAVE RR(I) IN (-1,0,+1), 36, 4, 2)          
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      'DRCSX - MUST HAVE S .LE. R(I) .LE. L', 36, 3, 2)           
         IF (RR(I) .LT. (-1) .OR. RR(I) .GT. 1) CALL SETERR(            
     1      'DRCSX - MUST HAVE RR(I) IN (-1,0,+1)', 36, 4, 2)           
C/                                                                      
   2     CONTINUE                                                       
      DO  3 I = 1, M                                                    
         IF (C(I) .EQ. 0D0) GOTO  3                                     
C/6S                                                                    
C        IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
C    1      36HDRCSX - MUST HAVE S .LE. C(I) .LE. L, 36, 5, 2)          
C        IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
C    1      36HDRCSX - MUST HAVE RC(I) IN (-1,0,+1), 36, 6, 2)          
C/7S                                                                    
         IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
     1      'DRCSX - MUST HAVE S .LE. C(I) .LE. L', 36, 5, 2)           
         IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
     1      'DRCSX - MUST HAVE RC(I) IN (-1,0,+1)', 36, 6, 2)           
C/                                                                      
   3     CONTINUE                                                       
C CHECK 1/(S*L) RANGE.                                                  
      BADNGE = .FALSE.                                                  
      IF (SL .GT. 1D0) GOTO 4                                           
         IF (1D0/L .GT. SL) BADNGE = .TRUE.                             
         GOTO  5                                                        
   4     IF (SL .GT. 1D0/S) BADNGE = .TRUE.                             
C S*L > 1.                                                              
C/6S                                                                    
C  5  IF (BADNGE) CALL SETERR(                                          
C    1   42HDRCSX - MUST HAVE 1/(S*L) IN MACHINE RANGE, 42, 7, 1)       
C/7S                                                                    
   5  IF (BADNGE) CALL SETERR(                                          
     1   'DRCSX - MUST HAVE 1/(S*L) IN MACHINE RANGE', 42, 7, 1)        
C/                                                                      
      OVELOW = .FALSE.                                                  
      DO  86 J = 1, M                                                   
         D3 = C(J)                                                      
         RD3 = RC(J)                                                    
C DO THE SCALING.                                                       
         DO  85 I = 1, N                                                
            D2 = R(I)                                                   
            RD2 = RR(I)                                                 
            XX = X(I, J)                                                
            AXX = DABS(XX)                                              
            IF (D2 .EQ. 0D0) GOTO  85                                   
            BADNGE = .FALSE.                                            
            IF (XX .NE. 0D0 .AND. D3 .NE. 0D0) GOTO 6                   
               X(I, J) = 0                                              
               GOTO  85                                                 
   6        IF (RD2 .NE. RD3) GOTO 26                                   
               IF (D3 .LT. D2) GOTO 16                                  
                  IF (D3 .GT. 1D0) GOTO 7                               
                     IF (D3*AXX .GT. D2*L) BADNGE = .TRUE.              
C WORRY ABOUT OVERFLOW.                                                 
                     GOTO  10                                           
   7                 IF (D2 .GT. 1D0) GOTO 8                            
                        IF (AXX .GT. D2*(L/D3)) BADNGE = .TRUE.         
C D3 > 1.                                                               
                        GOTO  9                                         
   8                    IF (AXX/D2 .GT. L/D3) BADNGE = .TRUE.           
C D2 > 1 & D3 > 1.                                                      
   9              CONTINUE                                              
  10              IF (BADNGE) GOTO 15                                   
                     IF (D3 .GT. DMIN1(D2, 1D0)*L) GOTO 11              
                        X(I, J) = X(I, J)*(D3/D2)                       
C IN-RANGE.                                                             
                        GOTO  14                                        
  11                    IF (D3 .LT. 1D0 .AND. AXX .GT. D2*L) GOTO 12    
                           X(I, J) = D3*(XX/D2)                         
C D3/D2 OVERFLOWS, IE, D3 > D2*L AND D2 < 1.                            
                           GOTO  13                                     
  12                       X(I, J) = (D3*XX)/D2                         
  13                    CONTINUE                                        
  14                 CONTINUE                                           
  15              CONTINUE                                              
                  GOTO  25                                              
  16              IF (D3 .LT. 1D0) GOTO 17                              
                     IF (AXX .LT. (D2*S)/D3) X(I, J) = 0                
C D3 < D2, WORRY ABOUT UNDERFLOW.                                       
C D2 > D3 >= 1.                                                         
                     GOTO  20                                           
  17                 IF (D2 .LT. 1D0) GOTO 18                           
                        IF (D3*AXX .LT. D2*S) X(I, J) = 0               
C D3 < 1.                                                               
                        GOTO  19                                        
  18                    IF (AXX .LT. (S/D3)*D2) X(I, J) = 0             
C D2, D3 < 1.                                                           
  19              CONTINUE                                              
  20              IF (X(I, J) .EQ. 0D0) GOTO  85                        
C       IN-RANGE.                                                       
                  IF (D3 .LT. DMAX1(D2, 1D0)*S) GOTO 21                 
                     X(I, J) = X(I, J)*(D3/D2)                          
                     GOTO  24                                           
  21                 IF (D3 .GT. 1D0 .AND. AXX .LT. D2*S) GOTO 22       
                        X(I, J) = D3*(XX/D2)                            
C D3/D2 UNDERFLOWS, I.E., D3 < D2*S, D2 > 1 .                           
                        GOTO  23                                        
  22                    X(I, J) = (D3*XX)/D2                            
  23                 CONTINUE                                           
  24              CONTINUE                                              
  25           CONTINUE                                                 
               GOTO  83                                                 
  26           IF (RD2 .LE. 0 .OR. RD3 .GE. 0) GOTO 27                  
                  X(I, J) = 0                                           
                  GOTO  85                                              
C UNDERFLOW.                                                            
  27              IF (RD2 .GE. 0 .OR. RD3 .LE. 0) GOTO 28               
                     BADNGE = .TRUE.                                    
C OVERFLOW.                                                             
                     GOTO  81                                           
  28                 IF (RD2 .GE. 0 .OR. RD3 .NE. 0) GOTO 42            
                        IF (D3 .GT. 1D0) GOTO 29                        
                           IF (D3*AXX .GT. D2) BADNGE = .TRUE.          
C WORRY ABOUT OVERFLOW.                                                 
                           GOTO  30                                     
  29                       IF (AXX .GT. D2/D3) BADNGE = .TRUE.          
C D3 > 1.                                                               
  30                    IF (BADNGE) GOTO 41                             
                           IF (D3 .GT. D2) GOTO 33                      
                              IF (D3 .GT. 1D0) GOTO 31                  
                                 X(I, J) = ((D3*L)/D2)*XX               
C IN-RANGE.                                                             
                                 GOTO  32                               
  31                             X(I, J) = ((D3/D2)*L)*XX               
  32                          CONTINUE                                  
                              GOTO  40                                  
  33                          IF (D3 .LT. 1D0 .AND. AXX .GT. D2) GOTO   
     1                           36                                     
                                 IF (AXX .LT. D2*S) GOTO 34             
                                    X(I, J) = D3*(L*(XX/D2))            
C D3 > D2.                                                              
C D3*(X*L/D2).                                                          
                                    GOTO  35                            
  34                                X(I, J) = D3*((XX*L)/D2)            
  35                             CONTINUE                               
                                 GOTO  39                               
  36                             IF (AXX .GT. D2*L) GOTO 37             
                                    X(I, J) = (D3*(XX/D2))*L            
C D3 < 1 & |X| > D2.                                                    
                                    GOTO  38                            
  37                                X(I, J) = ((D3/D2)*XX)*L            
  38                             CONTINUE                               
  39                          CONTINUE                                  
  40                       CONTINUE                                     
  41                    CONTINUE                                        
                        GOTO  80                                        
  42                    IF (RD2 .LE. 0 .OR. RD3 .NE. 0) GOTO 55         
                           IF (D3 .LT. 1D0) GOTO 43                     
                              IF (AXX .LT. D2/D3) X(I, J) = 0           
C WORRY ABOUT UNDERFLOW.                                                
                              GOTO  44                                  
  43                          IF (D3*AXX .LT. D2) X(I, J) = 0           
  44                       IF (X(I, J) .EQ. 0D0) GOTO  85               
C       IN-RANGE.                                                       
                           IF (D3 .LT. D2) GOTO 47                      
                              IF (D2 .GT. 1D0) GOTO 45                  
                                 X(I, J) = ((S/D2)*D3)*XX               
                                 GOTO  46                               
  45                             X(I, J) = ((D3/D2)*S)*XX               
  46                          CONTINUE                                  
                              GOTO  54                                  
  47                          IF (D3 .GT. 1D0 .AND. AXX .LT. D2) GOTO   
     1                           50                                     
                                 IF (D2 .GT. 1D0) GOTO 48               
                                    X(I, J) = D3*(XX*(S/D2))            
C D3 < D2.                                                              
                                    GOTO  49                            
  48                                X(I, J) = D3*((XX/D2)*S)            
  49                             CONTINUE                               
                                 GOTO  53                               
  50                             IF (D3 .LT. S*D2) GOTO 51              
                                    X(I, J) = ((D3/D2)*XX)*S            
C D2 > D3 > 1 AND |X| < D2.                                             
                                    GOTO  52                            
  51                                X(I, J) = ((XX/D2)*D3)*S            
  52                             CONTINUE                               
  53                          CONTINUE                                  
  54                       CONTINUE                                     
                           GOTO  79                                     
  55                       IF (RD2 .NE. 0 .OR. RD3 .GE. 0) GOTO 66      
                              IF (D2 .LT. 1D0) GOTO 56                  
                                 IF (AXX/D2 .LT. SL/D3) X(I, J) = 0     
C WORRY ABOUT UNDERFLOW.                                                
                                 GOTO  57                               
  56                             IF (AXX .LT. (SL/D3)*D2) X(I, J) = 0   
  57                          IF (X(I, J) .EQ. 0D0) GOTO  85            
C       IN-RANGE.                                                       
                              IF (D2 .GT. 1D0) GOTO 62                  
                                 IF (D3 .LT. D2*SL) GOTO 58             
                                    X(I, J) = ((D3/D2)/L)*XX            
                                    GOTO  61                            
  58                                IF (AXX .LT. SL) GOTO 59            
                                       X(I, J) = (D3/D2)*(XX/L)         
                                       GOTO  60                         
  59                                   X(I, J) = ((D3/D2)*XX)/L         
  60                             CONTINUE                               
  61                             CONTINUE                               
                                 GOTO  65                               
  62                             IF (AXX/D2 .LT. SL) GOTO 63            
                                    X(I, J) = ((XX/D2)/L)*D3            
C D2 > 1.                                                               
                                    GOTO  64                            
  63                                X(I, J) = ((XX/D2)*D3)/L            
  64                             CONTINUE                               
  65                          CONTINUE                                  
                              GOTO  78                                  
  66                          IF (D2 .LT. 1D0) GOTO 67                  
                                 IF (AXX/D2 .GT. SL/D3) BADNGE = .TRUE. 
C RD2 == 0 & RD3 > 0, WORRY ABOUT OVERFLOW.                             
                                 GOTO  68                               
  67                             IF (AXX .GT. D2*(SL/D3)) BADNGE =      
     1                              .TRUE.                              
  68                          IF (BADNGE) GOTO 77                       
                                 IF (D2 .LT. 1D0) GOTO 73               
                                    IF (D3/D2 .GT. SL) GOTO 69          
                                       X(I, J) = ((D3/D2)/S)*XX         
C IN-RANGE.                                                             
                                       GOTO  72                         
  69                                   IF (AXX .GT. SL) GOTO 70         
                                          X(I, J) = (D3/D2)*(XX/S)      
                                          GOTO  71                      
  70                                      X(I, J) = ((D3/D2)*XX)/S      
  71                                CONTINUE                            
  72                                CONTINUE                            
                                    GOTO  76                            
  73                                IF (AXX .GT. D2*SL) GOTO 74         
                                       X(I, J) = ((XX/D2)/S)*D3         
C D2 < 1.                                                               
                                       GOTO  75                         
  74                                   X(I, J) = ((XX/D2)*D3)/S         
  75                                CONTINUE                            
  76                             CONTINUE                               
  77                          CONTINUE                                  
  78                    CONTINUE                                        
  79                 CONTINUE                                           
  80              CONTINUE                                              
  81           CONTINUE                                                 
  82        CONTINUE                                                    
  83        IF (.NOT. BADNGE) GOTO 84                                   
               X(I, J) = L*(XX/AXX)                                     
               OVELOW = .TRUE.                                          
  84        CONTINUE                                                    
  85        CONTINUE                                                    
  86     CONTINUE                                                       
      IF (.NOT. OVELOW) GOTO 87                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(33HDRCSX - SOLUTION X HAS OVERFLOWED, 33, 8, 1)    
C/7S                                                                    
         CALL SETERR('DRCSX - SOLUTION X HAS OVERFLOWED', 33, 8, 1)     
C/                                                                      
  87  RETURN                                                            
      END                                                               
      SUBROUTINE DIODE4(V, NV, TSTART, TSTOP, DT, D, THETA,             
     1   KEEJAC, MINIT, MAXIT, USENGJ, USENNS, USENFD, BETA, GAMMA,     
     2   DELTA, N, MMAX, HFRACT, EGIVE, KMAX, XPOLY, KINIT, IRCS, ERROR,
     3   ERRPAR, ERPUTS, INMI, DA, HANDLE)                              
      INTEGER MMAX, NV                                                  
      EXTERNAL D, ERROR, INMI, DA, HANDLE                               
      INTEGER KEEJAC, MINIT, MAXIT, N(MMAX), KMAX, KINIT                
      INTEGER IRCS                                                      
      REAL HFRACT, EGIVE, ERRPAR(2)                                     
      LOGICAL USENGJ, USENNS, USENFD, XPOLY, ERPUTS, DA                 
      DOUBLE PRECISION V(NV), TSTART, TSTOP, DT, THETA, BETA            
      DOUBLE PRECISION GAMMA, DELTA                                     
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D10DEY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL RV(30)                                                       
      LOGICAL LV(20)                                                    
      DOUBLE PRECISION WV(30)                                           
      COMMON /D10DET/ TGOOD                                             
      DOUBLE PRECISION TGOOD                                            
      COMMON /D1ODES/ IMEM                                              
      INTEGER IMEM                                                      
      COMMON /D10DER/ STATS                                             
      INTEGER STATS(9)                                                  
      COMMON /D1ODEM/ THETAC, EGIVEC, MINITC, MAXITC, KEEACC, IRCSC     
      INTEGER MINITC, MAXITC, KEEACC, IRCSC                             
      REAL EGIVEC                                                       
      DOUBLE PRECISION THETAC                                           
      COMMON /D1ODEL/ ERPTSC                                            
      LOGICAL ERPTSC                                                    
      COMMON /DIODEJ/ AJ, BJ, GETACC, NEEUMC                            
      LOGICAL GETACC, NEEUMC                                            
      DOUBLE PRECISION AJ, BJ                                           
      COMMON /D1ODEG/ TJ, DTJ, GETJAC, SEPATE, USEGJC, USENSC, USEFDC   
      LOGICAL GETJAC, SEPATE, USEGJC, USENSC, USEFDC                    
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D1ODEF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /DIODEF/ FAILED                                            
      LOGICAL FAILED                                                    
      EXTERNAL D1ODEE, D1ODEH, D1ODEP, D1ODEN                           
      INTEGER ISTKGT, NERROR, I, NERR, IS(1000), IVOLD                  
      REAL RS(1000)                                                     
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION DABS, WS(500)                                    
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE BOTTOM LEVEL OF IODES.                                            
C SCRATCH SPACE ALLOCATED -                                             
C     S(DIODE3) = S(D1ODES) +                                           
C LONG REAL WORDS +                                                     
C                 MMAX                                                  
C INTEGER WORDS.                                                        
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      IF (TSTART .EQ. TSTOP) RETURN                                     
      CALL ENTER(1)                                                     
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (NV .LE. 0) CALL SETERR(16HDIODE4 - NV.LE.0, 16, 1, 2)         
C     IF (TSTART+DT .EQ. TSTART) CALL SETERR(                           
C    1   31HDIODE4 - INPUT VALUE OF DT IS 0, 31, 2, 2)                  
C     IF ((DT/DABS(DT))*(TSTOP-TSTART) .LE. 0D0) CALL SETERR(           
C    1   45HDIODE4 - INPUT VALUE OF DT HAS THE WRONG SIGN, 45, 3, 2)    
C     IF (THETA .LT. 0D0 .OR. THETA .GT. 1D0) CALL SETERR(              
C    1   27HDIODE4 - THETA NOT IN (0,1), 27, 4, 2)                      
C     IF (KEEJAC .LT. 0 .OR. KEEJAC .GT. 5) CALL SETERR(                
C    1   37HDIODE4 - KEEPJAC NOT ONE OF (0,...,5), 37, 5, 2)            
C     IF (MINIT .LT. 1) CALL SETERR(19HDIODE4 - MINIT.LT.1, 19, 6, 2)   
C     IF (MAXIT .LT. 1) CALL SETERR(19HDIODE4 - MAXIT.LT.1, 19, 7, 2)   
C     IF (KMAX .LT. 1) CALL SETERR(18HDIODE4 - KMAX.LT.1, 18, 8, 2)     
C/7S                                                                    
      IF (NV .LE. 0) CALL SETERR('DIODE4 - NV.LE.0', 16, 1, 2)          
      IF (TSTART+DT .EQ. TSTART) CALL SETERR(                           
     1   'DIODE4 - INPUT VALUE OF DT IS 0', 31, 2, 2)                   
      IF ((DT/DABS(DT))*(TSTOP-TSTART) .LE. 0D0) CALL SETERR(           
     1   'DIODE4 - INPUT VALUE OF DT HAS THE WRONG SIGN', 45, 3, 2)     
      IF (THETA .LT. 0D0 .OR. THETA .GT. 1D0) CALL SETERR(              
     1   'DIODE4 - THETA NOT IN (0,1)', 27, 4, 2)                       
      IF (KEEJAC .LT. 0 .OR. KEEJAC .GT. 5) CALL SETERR(                
     1   'DIODE4 - KEEPJAC NOT ONE OF (0,...,5)', 37, 5, 2)             
      IF (MINIT .LT. 1) CALL SETERR('DIODE4 - MINIT.LT.1', 19, 6, 2)    
      IF (MAXIT .LT. 1) CALL SETERR('DIODE4 - MAXIT.LT.1', 19, 7, 2)    
      IF (KMAX .LT. 1) CALL SETERR('DIODE4 - KMAX.LT.1', 18, 8, 2)      
C/                                                                      
      TEMP = KINIT .LT. 1                                               
      IF (.NOT. TEMP) TEMP = KINIT .GT. KMAX                            
C/6S                                                                    
C     IF (TEMP) CALL SETERR(34HDIODE4 - KINIT NOT IN (1,...,KMAX), 34, 9
C    1   , 2)                                                           
C     IF (BETA .LE. 0D0) CALL SETERR(19HDIODE4 - BETA .LE.0, 19, 10, 2) 
C     IF (GAMMA .LE. 0D0) CALL SETERR(20HDIODE4 - GAMMA .LE.0, 20, 11, 2
C    1   )                                                              
C     IF (DELTA .LT. 0D0) CALL SETERR(20HDIODE4 - DELTA .LT.0, 20, 12, 2
C    1   )                                                              
C     IF (BETA+GAMMA-DELTA .LE. 0D0) CALL SETERR(                       
C    1   30HDIODE4 - BETA+GAMMA-DELTA.LE.0, 30, 13, 2)                  
C     IF (MMAX .LT. KMAX+2) CALL SETERR(23HDIODE4 - MMAX.LT.KMAX+2, 23  
C    1   , 14, 2)                                                       
C     IF (N(1) .LT. 1) CALL SETERR(18HDIODE4 - N(1).LT.1, 18, 15, 2)    
C/7S                                                                    
      IF (TEMP) CALL SETERR('DIODE4 - KINIT NOT IN (1,...,KMAX)', 34, 9 
     1   , 2)                                                           
      IF (BETA .LE. 0D0) CALL SETERR('DIODE4 - BETA .LE.0', 19, 10, 2)  
      IF (GAMMA .LE. 0D0) CALL SETERR('DIODE4 - GAMMA .LE.0', 20, 11, 2 
     1   )                                                              
      IF (DELTA .LT. 0D0) CALL SETERR('DIODE4 - DELTA .LT.0', 20, 12, 2 
     1   )                                                              
      IF (BETA+GAMMA-DELTA .LE. 0D0) CALL SETERR(                       
     1   'DIODE4 - BETA+GAMMA-DELTA.LE.0', 30, 13, 2)                   
      IF (MMAX .LT. KMAX+2) CALL SETERR('DIODE4 - MMAX.LT.KMAX+2', 23   
     1   , 14, 2)                                                       
      IF (N(1) .LT. 1) CALL SETERR('DIODE4 - N(1).LT.1', 18, 15, 2)     
C/                                                                      
      DO  1 I = 2, MMAX                                                 
C/6S                                                                    
C        IF (N(I) .LE. N(I-1)) CALL SETERR(                             
C    1      37HDIODE4 - N IS NOT MONOTONE INCREASING, 37, 16, 2)        
C/7S                                                                    
         IF (N(I) .LE. N(I-1)) CALL SETERR(                             
     1      'DIODE4 - N IS NOT MONOTONE INCREASING', 37, 16, 2)         
C/                                                                      
   1     CONTINUE                                                       
C/6S                                                                    
C     IF (HFRACT .LE. 0.) CALL SETERR(20HDIODE4 - HFRACT.LE.0, 20, 17, 2
C    1   )                                                              
C     IF (EGIVE .LT. 1.) CALL SETERR(21HDIODE4 - EGIVE .LT. 1, 21, 18, 2
C    1   )                                                              
C/7S                                                                    
      IF (HFRACT .LE. 0.) CALL SETERR('DIODE4 - HFRACT.LE.0', 20, 17, 2 
     1   )                                                              
      IF (EGIVE .LT. 1.) CALL SETERR('DIODE4 - EGIVE .LT. 1', 21, 18, 2 
     1   )                                                              
C/                                                                      
      ERPTSC = ERPUTS                                                   
      EGIVEC = EGIVE                                                    
      IRCSC = IRCS                                                      
      THETAC = THETA                                                    
      MINITC = MINIT                                                    
      MAXITC = MAXIT                                                    
      KEEACC = KEEJAC                                                   
      IF (KEEJAC .EQ. 1 .AND. MAXIT .EQ. 1) KEEACC = 0                  
C SAME AS L.B.E.                                                        
      SEPATE = KEEACC .GT. 1                                            
      USEGJC = USENGJ                                                   
      USENSC = USENNS                                                   
      USEFDC = USENFD                                                   
      IF (KEEACC .LT. 3) GOTO 2                                         
         GETJAC = .TRUE.                                                
         TJ = TSTART                                                    
         GOTO  5                                                        
   2     IF (KEEJAC .NE. 2) GOTO 3                                      
            TJ = TSTOP                                                  
C CANNOT BE TSTART.                                                     
            GOTO  4                                                     
   3        TJ = TSTART                                                 
C CANNOT BE TSTART+THETA*DT/N.                                          
   4     CONTINUE                                                       
   5  DTJ = 0                                                           
C START WITH NO ERROR STATES.                                           
      FNUM = 0                                                          
C FLAG DA WORK-SPACE AS UN-ALLOCATED.                                   
      IMEM = 0                                                          
C GET SPACE FOR DA.                                                     
      IF (DA(V, V, NV, TSTART, DT, D, V, TJ, DTJ, GETJAC, SEPATE,       
     1   USENGJ, USENNS, USENFD, 0, -1, FNUM, THETA, IRCS, KEEACC))     
     2   CONTINUE                                                       
C/6S                                                                    
C     IF (IMEM .LE. 0) CALL SETERR(                                     
C    1   52HDIODE4 - DIODED FAILED TO INITIALIZE COMMON /D1ODES/, 52,   
C    2   19, 2)                                                         
C/7S                                                                    
      IF (IMEM .LE. 0) CALL SETERR(                                     
     1   'DIODE4 - DIODED FAILED TO INITIALIZE COMMON /D1ODES/', 52,    
     2   19, 2)                                                         
C/                                                                      
      TGOOD = TSTART                                                    
      IVOLD = ISTKGT(NV, 4)                                             
      CALL MOVEFD(NV, V, WS(IVOLD))                                     
C TELL STATS ROUTINE IN IODE.                                           
      CALL D1ODEX(STATS, 1)                                             
      CALL D10DEX(TSTART, TSTOP, D1ODEP, D1ODEN, DA, D, BETA, GAMMA,    
     1   DELTA, WS(IVOLD), NV, DT, N, KMAX, MMAX, XPOLY, D1ODEE, ERROR  
     2   , ERRPAR, INMI, D1ODEH, HANDLE, 0.9E0, HFRACT, KINIT)          
C TELL STATS ROUTINE OUT OF IODE.                                       
      CALL D1ODEX(STATS, -1)                                            
      CALL MOVEFD(NV, WS(IVOLD), V)                                     
      TSTOP = TGOOD                                                     
C CAPTURE THE ERROR NUMBER, IF ANY.                                     
      NERR = NERROR(NERR)                                               
      IF (NERR .NE. 15) GOTO 6                                          
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(13HDIODE4 - DT=0, 13, 1000, 1)                     
C/7S                                                                    
         CALL SETERR('DIODE4 - DT=0', 13, 1000, 1)                      
C/                                                                      
   6  IF (NERR .NE. 16) GOTO 7                                          
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(32HDIODE4 - DT=0 RETURNED BY HANDLE, 32, 1001, 1)  
C/7S                                                                    
         CALL SETERR('DIODE4 - DT=0 RETURNED BY HANDLE', 32, 1001, 1)   
C/                                                                      
   7  IF (NERR .NE. 17) GOTO 8                                          
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(45HDIODE4 - DT RETURNED BY HANDLE HAS WRONG SIGN,  
C    1      45, 1002, 1)                                                
C/7S                                                                    
         CALL SETERR('DIODE4 - DT RETURNED BY HANDLE HAS WRONG SIGN',   
     1      45, 1002, 1)                                                
C/                                                                      
   8  IF (NERR .NE. 18) GOTO 9                                          
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(46HDIODE4 - CANNOT RAISE DT IN HANDLE WHEN FAILED  
C    1      , 46, 1003, 1)                                              
C/7S                                                                    
         CALL SETERR('DIODE4 - CANNOT RAISE DT IN HANDLE WHEN FAILED'   
     1      , 46, 1003, 1)                                              
C/                                                                      
   9  IF (NERR .NE. 19) GOTO 10                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(36HDIODE4 - E(I).LE.0 RETURNED BY ERROR, 36, 1004  
C    1      , 1)                                                        
C/7S                                                                    
         CALL SETERR('DIODE4 - E(I).LE.0 RETURNED BY ERROR', 36, 1004   
     1      , 1)                                                        
C/                                                                      
  10  IF (NERR .NE. 15) GOTO 15                                         
         IF (FNUM .NE. 1) GOTO 11                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(18HDIODE4 - D FAILURE, 18, 1005, 1)             
C/7S                                                                    
            CALL SETERR('DIODE4 - D FAILURE', 18, 1005, 1)              
C/                                                                      
  11     IF (FNUM .NE. 2) GOTO 12                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(26HDIODE4 - SINGULAR JACOBIAN, 26, 1006, 1)     
C/7S                                                                    
            CALL SETERR('DIODE4 - SINGULAR JACOBIAN', 26, 1006, 1)      
C/                                                                      
  12     IF (FNUM .NE. 3) GOTO 13                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(                                                
C    1         45HDIODE4 - TOO MANY NEWTON ITERATIONS PREDICTED, 45,    
C    2         1007, 1)                                                 
C/7S                                                                    
            CALL SETERR(                                                
     1         'DIODE4 - TOO MANY NEWTON ITERATIONS PREDICTED', 45,     
     2         1007, 1)                                                 
C/                                                                      
  13     IF (FNUM .NE. 4) GOTO 14                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(42HDIODE4 - TOO MANY NEWTON ITERATIONS NEEDED,  
C    1         42, 1008, 1)                                             
C/7S                                                                    
            CALL SETERR('DIODE4 - TOO MANY NEWTON ITERATIONS NEEDED',   
     1         42, 1008, 1)                                             
C/                                                                      
  14     CONTINUE                                                       
  15  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE D10DEX(TSTART, TSTOP, XA, F, DA, D, BETA, GAMMA        
     1   , DELTA, X, NX, DT, N, KMAX, MMAX, XPOLY, SERROR, ERROR,       
     2   ERRPAR, INMI, SOUT, OUTPUT, PESPAR, HFRACT, KINIT)             
      INTEGER MMAX, NX                                                  
      EXTERNAL XA, F, DA, D, SERROR, ERROR                              
      EXTERNAL INMI, SOUT, OUTPUT                                       
      INTEGER N(MMAX), KMAX, KINIT                                      
      REAL ERRPAR(2), PESPAR, HFRACT                                    
      LOGICAL XPOLY, SERROR                                             
      DOUBLE PRECISION TSTART, TSTOP, BETA, GAMMA, DELTA, X(NX)         
      DOUBLE PRECISION DT                                               
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D10DEY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL RV(30)                                                       
      LOGICAL LV(20)                                                    
      DOUBLE PRECISION WV(30)                                           
      COMMON /D1ODEG/ TJ, DTJ, GETJAC, SEPATE, USENGJ, USENNS, USENFD   
      LOGICAL GETJAC, SEPATE, USENGJ, USENNS, USENFD                    
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D1ODEM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC, IRCS          
      INTEGER MINIT, MAXIT, KEEJAC, IRCS                                
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      INTEGER NERROR, M, NERR, IE, IS(1000), IX1                        
      REAL RS(1000)                                                     
      LOGICAL D4SSOM, D4SSOR, D4SSOX, DONE, OK, LS(1000)                
      LOGICAL D4SSOE, D4SSOI                                            
      DOUBLE PRECISION T0, T1, WS(500)                                  
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      EQUIVALENCE (WV(10), T0)                                          
      EQUIVALENCE (WV(11), T1)                                          
      EQUIVALENCE (IV(12), IX1)                                         
      EQUIVALENCE (IV(22), IE)                                          
      EQUIVALENCE (LV(2), OK)                                           
      EQUIVALENCE (LV(7), DONE)                                         
C SCRATCH SPACE ALLOCATED -                                             
C     S(D10DEX) <= 2*MMAX + 1 + NX*(KMAX+1) +                           
C     ( 5*KMAX + 2*MMAX + 3 ) INTEGER +                                 
C     MAX ( S(XA), NX*(KMAX+1) REAL +                                   
C           MAX ( KMAX + KMAX INTEGER, S(SERROR) ),                     
C           NX REAL + S(SOUT) )                                         
C LONG REAL.                                                            
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      CALL ENTER(1)                                                     
      IF (.NOT. D4SSOI(WV, RV, IV, LV, TSTART, TSTOP, BETA, GAMMA,      
     1   DELTA, NX, DT, N, KMAX, MMAX, XPOLY, ERRPAR, PESPAR, HFRACT,   
     2   KINIT)) GOTO 1                                                 
         CALL LEAVE                                                     
         RETURN                                                         
   1  IF (T0 .EQ. TSTOP) GOTO  7                                        
C TAKE THE TIME-STEPS.                                                  
         IF (KEEJAC .NE. 2) GOTO 2                                      
            GETJAC = T0 .NE. TJ                                         
            TJ = T0                                                     
C BUILD THE EXTRAPLOATION LOZENGE.                                      
   2     DO  5 M = 1, MMAX                                              
C GET XA((T1-T0)/N(M)).                                                 
            OK = .TRUE.                                                 
            CALL XA(T0, X, T1, WS(IX1), NX, N(M), F, D, DA, OK, ERROR,  
     1         ERRPAR, INMI)                                            
            IF (OK) GOTO 4                                              
               IF (NERROR(NERR) .EQ. 0) GOTO 3                          
                  CALL LEAVE                                            
                  RETURN                                                
   3           CONTINUE                                                 
C     EXTRAPOLATE THE RESULTS.                                          
   4        IF (D4SSOX(WV, RV, IV, LV, N, M)) GOTO  6                   
            IF (M .GT. 1) DONE = SERROR(WS(IX1), NX, T1, DT, ERRPAR,    
     1         DELTA, RS(IE), ERROR)                                    
C CHECK FOR CONVERGENCE.                                                
C     CHECK FOR A RESTART.                                              
            IF (D4SSOR(WV, RV, IV, LV, ERRPAR)) GOTO  6                 
   5        CONTINUE                                                    
C   GET OPTIMAL DT AND ORDER ( LOZENGE SIZE ).                          
   6     IF (D4SSOM(WV, RV, IV, LV, DT)) GOTO  7                        
C   OUTPUT THE RESULTS FOR THIS TIME-STEP.                              
         CALL SOUT(T0, X, T1, WS(IX1), NX, DT, TSTOP, RS(IE), OK,       
     1      OUTPUT)                                                     
C   WIND-UP THIS TIME-STEP.                                             
         IF (D4SSOE(WV, RV, IV, LV, X, TSTOP, DT)) GOTO  7              
         GOTO  1                                                        
   7  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D1ODEE(V, NV, T, DT, ERRPAR, DELTA, EV,          
     1   ERROR)                                                         
      INTEGER NV                                                        
      EXTERNAL ERROR                                                    
      REAL ERRPAR(2), EV(NV)                                            
      LOGICAL ERROR                                                     
      DOUBLE PRECISION V(NV), T, DT, DELTA                              
      LOGICAL ERPUTS                                                    
C THE ERROR FILTER FOR NLEQS.                                           
C SCRATCH SPACE ALLOCATED - S(D1ODEE) = S(ERROR).                       
      ERPUTS = DELTA .EQ. 1D0                                           
      D1ODEE = ERROR(V, NV, T, DT, ERRPAR, ERPUTS, EV)                  
      RETURN                                                            
      END                                                               
      SUBROUTINE D1ODEH(T0, V0, T1, V1, NV, DT, TSTOP, EV, OK,          
     1   HANDLE)                                                        
      INTEGER NV                                                        
      EXTERNAL HANDLE                                                   
      REAL EV(NV)                                                       
      LOGICAL OK                                                        
      DOUBLE PRECISION T0, V0(NV), T1, V1(NV), DT, TSTOP                
      COMMON /D1ODEF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /D10DER/ NJS, NFS, NTSS, NSSS, NES, NNITS, NNDS, NNFS, NRS 
      INTEGER NJS, NFS, NTSS, NSSS, NES, NNITS                          
      INTEGER NNDS, NNFS, NRS                                           
      COMMON /D1ODEM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC, IRCS          
      INTEGER MINIT, MAXIT, KEEJAC, IRCS                                
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      COMMON /D1ODEG/ TJ, DTJ, GETJAC, SEPATE, USENGJ, USENNS, USENFD   
      LOGICAL GETJAC, SEPATE, USENGJ, USENNS, USENFD                    
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D10DET/ TGOOD                                             
      DOUBLE PRECISION TGOOD                                            
C OUTPUT FILTER FOR IODES.                                              
C SCRATCH SPACE ALLOCATED - S(D1ODEH) = S(HANDLE).                      
      IF (T0 .EQ. T1) GOTO 1                                            
         FNUM = 0                                                       
         TGOOD = T1                                                     
         GOTO  2                                                        
   1     NRS = NRS+1                                                    
   2  IF (T0 .NE. T1 .OR. KEEJAC .NE. 3) GOTO 3                         
         GETJAC = T0 .NE. TJ                                            
         TJ = T0                                                        
   3  NTSS = NTSS+1                                                     
      CALL HANDLE(T0, V0, T1, V1, NV, DT, TSTOP)                        
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D1ODEN(V, NV, T, DT, D, DA, ERROR, INMI,         
     1   ERRPAR)                                                        
      INTEGER NV                                                        
      EXTERNAL D, DA, ERROR, INMI                                       
      REAL ERRPAR(2)                                                    
      DOUBLE PRECISION V(NV), T, DT                                     
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D1ODEM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC, IRCS          
      INTEGER MINIT, MAXIT, KEEJAC, IRCS                                
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      INTEGER IDV, IEV, IVT, ISTKGT, IEV1, IEV2                         
      INTEGER IVTETA, IS(1000), IVOLD                                   
      REAL RS(1000)                                                     
      LOGICAL DONE, LS(1000), D1ODEO                                    
      DOUBLE PRECISION WS(500)                                          
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C NONLINEAR EQUATION SOLVER FOR IODES.                                  
C SCRATCH SPACE ALLOCATED -                                             
C     S(D1ODEN) =  4*NV +                                               
C                 3*NV REAL +                                           
C                 MAX ( S(DA), S(ERROR) )                               
C LONG REAL WORDS.                                                      
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      CALL ENTER(1)                                                     
      IVOLD = ISTKGT(4*NV, 4)                                           
      IVTETA = IVOLD+NV                                                 
      IVT = IVTETA+NV                                                   
      IDV = IVT+NV                                                      
      IEV = ISTKGT(3*NV, 3)                                             
      IEV1 = IEV+NV                                                     
      IEV2 = IEV1+NV                                                    
      DONE = D1ODEO(V, NV, T, DT, D, DA, ERROR, INMI, ERRPAR, WS(IVTETA)
     1   , WS(IVT), WS(IVOLD), RS(IEV), RS(IEV1), RS(IEV2), WS(IDV),    
     2   THETA, MINIT, MAXIT, KEEJAC, IRCS, EGIVE)                      
      CALL LEAVE                                                        
      D1ODEN = DONE                                                     
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D1ODEO(V, NV, T, DT, D, DA, ERROR, INMI,         
     1   ERRPAR, VTHETA, VT, VOLD, EV, EV1, EV2, DV, THETA, MINIT,      
     2   MAXIT, KEEJAC, IRCS, EGIVE)                                    
      INTEGER NV                                                        
      EXTERNAL D, DA, ERROR, INMI                                       
      INTEGER MINIT, MAXIT, KEEJAC, IRCS                                
      REAL ERRPAR(2), EV(NV), EV1(NV), EV2(NV), EGIVE                   
      LOGICAL DA, ERROR                                                 
      DOUBLE PRECISION V(NV), T, DT, VTHETA(NV), VT(NV), VOLD(NV)       
      DOUBLE PRECISION DV(NV), THETA                                    
      COMMON /D10DER/ NJS, NFS, NTSS, NSSS, NES, NNITS, NNDS, NNFS, NRS 
      INTEGER NJS, NFS, NTSS, NSSS, NES, NNITS                          
      INTEGER NNDS, NNFS, NRS                                           
      COMMON /D1ODEG/ TJ, DTJ, GETJAC, SEPATE, USENGJ, USENNS, USENFD   
      LOGICAL GETJAC, SEPATE, USENGJ, USENNS, USENFD                    
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D1ODEL/ ERPUTS                                            
      LOGICAL ERPUTS                                                    
      COMMON /D1ODEF/ FNUM                                              
      INTEGER FNUM                                                      
      INTEGER I, J, ITER                                                
      REAL RHO, PROD, TEMP, POWER                                       
      LOGICAL DONE                                                      
      DOUBLE PRECISION DABS                                             
      INTEGER TEMP1                                                     
      LOGICAL TEMP2                                                     
      CALL MOVEFD(NV, V, VOLD)                                          
      CALL SETD(NV, 0D0, VT)                                            
C GET INITIAL NEWTON METHOD GUESS.                                      
      CALL INMI(NV, T, DT, VOLD, V, VT)                                 
      DO  23 ITER = 1, MAXIT                                            
         IF (KEEJAC .NE. 0) GOTO 1                                      
            GETJAC = .TRUE.                                             
            TJ = T                                                      
   1     IF (GETJAC) NJS = NJS+1                                        
         NNITS = NNITS+1                                                
         DO  2 I = 1, NV                                                
            VTHETA(I) = THETA*(V(I)-VOLD(I))+VOLD(I)                    
   2        CONTINUE                                                    
         IF (GETJAC) DTJ = 0                                            
         DONE = DA(VTHETA, VT, NV, T, DT, D, DV, TJ, DTJ, GETJAC,       
     1      SEPATE, USENGJ, USENNS, USENFD, NES, NFS, FNUM, THETA, IRCS,
     2      KEEJAC)                                                     
         IF (.NOT. DONE) GOTO 3                                         
            DONE = .FALSE.                                              
            GOTO  24                                                    
   3     GETJAC = .FALSE.                                               
         DTJ = DT                                                       
         DO  4 I = 1, NV                                                
            V(I) = V(I)+DV(I)                                           
            EV(I) = EGIVE*DABS(DV(I))                                   
   4        CONTINUE                                                    
         IF (MAXIT .NE. 1) GOTO 5                                       
            DONE = .TRUE.                                               
            GOTO  24                                                    
   5     CALL MOVEFR(NV, EV, EV2)                                       
         DONE = ERROR(V, NV, T, DT, ERRPAR, ERPUTS, EV)                 
C CHECK FOR NEGATIVE ERROR REQUESTS.                                    
         DO  7 I = 1, NV                                                
            TEMP2 = EV(I) .EQ. 0.                                       
            IF (TEMP2) TEMP2 = EV2(I) .NE. 0.                           
            IF (.NOT. TEMP2) TEMP2 = EV(I) .LT. 0.                      
            IF (.NOT. TEMP2) GOTO 6                                     
C/6S                                                                    
C              CALL SETERR(37HDESSOM - E(I).LE.0 RETURNED BY SERROR, 37,
C    1            19, 1)                                                
C/7S                                                                    
               CALL SETERR('DESSOM - E(I).LE.0 RETURNED BY SERROR', 37, 
     1            19, 1)                                                
C/                                                                      
               D1ODEO = .FALSE.                                         
               RETURN                                                   
   6        CONTINUE                                                    
   7        CONTINUE                                                    
         IF (.NOT. DONE) GOTO 8                                         
            GOTO  24                                                    
   8        IF (ITER .NE. MAXIT) GOTO 9                                 
               FNUM = 4                                                 
               NNFS = NNFS+1                                            
               GOTO  24                                                 
   9     CONTINUE                                                       
  10     DO  21 I = 1, NV                                               
            IF (ITER .LE. MINIT .OR. EV(I) .GE. EV2(I)) GOTO 20         
               IF (EV1(I) .LE. EV2(I)) GOTO 11                          
                  RHO = EV2(I)/EV1(I)                                   
C CAN CHECK CONVERGENCE RATE.                                           
                  GOTO  12                                              
  11              RHO = 1                                               
  12           IF (RHO .LT. 1.) GOTO 13                                 
                  NNDS = NNDS+1                                         
C DIVERGING.                                                            
                  FNUM = 3                                              
                  D1ODEO = DONE                                         
                  RETURN                                                
  13           IF (KEEJAC .NE. 0) GOTO 17                               
                  PROD = 1                                              
C CHECK QUADRATIC CONVERGENCE RATE.                                     
                  POWER = RHO**2                                        
C < 1.                                                                  
                  TEMP = EV(I)/EV2(I)                                   
                  TEMP1 = MAXIT-ITER                                    
                  DO  14 J = 1, TEMP1                                   
                     PROD = PROD*POWER                                  
                     POWER = POWER**2                                   
                     IF (PROD .LE. TEMP) GOTO  15                       
  14                 CONTINUE                                           
  15              IF (PROD .LE. TEMP) GOTO 16                           
                     NNDS = NNDS+1                                      
C SLOW CONVERGENCE.                                                     
                     FNUM = 3                                           
                     D1ODEO = DONE                                      
                     RETURN                                             
  16              CONTINUE                                              
                  GOTO  19                                              
  17              IF (RHO**(MAXIT-ITER)*EV2(I) .LE. EV(I)) GOTO 18      
                     NNDS = NNDS+1                                      
C CHECK LINEAR CONVERGENCE RATE.                                        
C SLOW CONVERGENCE.                                                     
                     FNUM = 3                                           
                     D1ODEO = DONE                                      
                     RETURN                                             
  18              CONTINUE                                              
  19           CONTINUE                                                 
  20        EV1(I) = EV2(I)                                             
  21        CONTINUE                                                    
         DO  22 I = 1, NV                                               
            VT(I) = VT(I)+DV(I)/DT                                      
  22        CONTINUE                                                    
  23     CONTINUE                                                       
  24  D1ODEO = DONE                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE D1ODEP(T0, V0, T1, V1, NV, N, NLEQS, D, DA, OK,        
     1   ERROR, ERRPAR, INMI)                                           
      INTEGER NV                                                        
      EXTERNAL NLEQS, D, DA, ERROR, INMI                                
      INTEGER N                                                         
      REAL ERRPAR(2)                                                    
      LOGICAL NLEQS, DA, OK                                             
      DOUBLE PRECISION T0, V0(NV), T1, V1(NV)                           
      COMMON /D10DEY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL RV(30)                                                       
      LOGICAL LV(20)                                                    
      DOUBLE PRECISION WV(30)                                           
      COMMON /D1ODEG/ TJ, DTJ, GETJAC, SEPATE, USENGJ, USENNS, USENFD   
      LOGICAL GETJAC, SEPATE, USENGJ, USENNS, USENFD                    
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D1ODEM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC, IRCS          
      INTEGER MINIT, MAXIT, KEEJAC, IRCS                                
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      COMMON /D1ODES/ IMEM                                              
      INTEGER IMEM                                                      
      COMMON /D10DER/ NJS, NFS, NTSS, NSSS, NES, NNITS, NNDS, NNFS, NRS 
      INTEGER NJS, NFS, NTSS, NSSS, NES, NNITS                          
      INTEGER NNDS, NNFS, NRS                                           
      COMMON /D1ODEF/ FNUM                                              
      INTEGER FNUM                                                      
      INTEGER ISTEP                                                     
      REAL FLOAT                                                        
      DOUBLE PRECISION TSTART, DBLE, T, DT                              
      EQUIVALENCE (TSTART, WV(1))                                       
C TIME-STEPPING SCHEME FOR IODES.                                       
C SCRATCH SPACE ALLOCATED -                                             
C     S(D1ODEP) = NV**2 + S(NLEQS).                                     
C LONG REAL WORDS.                                                      
      CALL ENTER(1)                                                     
      DT = (T1-T0)/DBLE(FLOAT(N))                                       
C INITIAL APPROXIMATION FOR V1.                                         
      CALL MOVEFD(NV, V0, V1)                                           
      IF (DA(V0, V1, NV, T1, DT, D, V1, TJ, DTJ, GETJAC, SEPATE, USENGJ,
     1   USENNS, USENFD, 0, -2, FNUM, THETA, IRCS, KEEJAC)) CONTINUE    
      DO  3 ISTEP = 1, N                                                
         T = T0+(DBLE(FLOAT(ISTEP-1))+THETA)*DT                         
         NSSS = NSSS+1                                                  
         IF (KEEJAC .NE. 1) GOTO 1                                      
            GETJAC = .TRUE.                                             
            TJ = T                                                      
   1     IF (DT .GT. 0D0 .AND. T .GT. T1 .OR. DT .LT. 0D0 .AND. T .LT.  
     1      T1) T = T1                                                  
         OK = NLEQS(V1, NV, T, DT, D, DA, ERROR, INMI, ERRPAR)          
         IF (FNUM .LT. 3 .OR. KEEJAC .NE. 4) GOTO 2                     
            GETJAC = T0 .NE. TJ                                         
            TJ = T0                                                     
   2     IF (.NOT. OK) GOTO  4                                          
   3     CONTINUE                                                       
   4  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DIODEX                                                 
      COMMON /D10DER/ STATS                                             
      INTEGER STATS(9)                                                  
      INTEGER I1MACH                                                    
      INTEGER TEMP                                                      
C TO PRINT THE RUN-TIME STATISTICS FOR IODE.                            
      CALL D1ODEX(STATS, 0)                                             
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  1) STATS                                            
   1  FORMAT (33H DIODE(J,F,TS,SS,E,NIT,ND,NF,R) =, 9(I5))              
      RETURN                                                            
      END                                                               
      SUBROUTINE D1ODEX(XSTATS, IFLAG)                                  
      INTEGER XSTATS(9), IFLAG                                          
      INTEGER STATS(9)                                                  
      LOGICAL INPOST                                                    
      DATA STATS(1)/0/                                                  
      DATA STATS(2)/0/                                                  
      DATA STATS(3)/0/                                                  
      DATA STATS(4)/0/                                                  
      DATA STATS(5)/0/                                                  
      DATA STATS(6)/0/                                                  
      DATA STATS(7)/0/                                                  
      DATA STATS(8)/0/                                                  
      DATA STATS(9)/0/                                                  
      DATA INPOST/.FALSE./                                              
C INTERNAL SAVING OF STATISTICS FOR IODE.                               
C FOR IFLAG = 0, THE STATS ARE SIMPLY REPORTED.                         
C FOR IFLAG > 0, IT ENTERS IODE.                                        
C FOR IFLAG < 0, IT EXITS IODE.                                         
      IF (IFLAG .NE. 0) GOTO 1                                          
         IF (.NOT. INPOST) CALL MOVEFI(9, STATS, XSTATS)                
         GOTO  4                                                        
   1     IF (IFLAG .LE. 0) GOTO 2                                       
            INPOST = .TRUE.                                             
            CALL SETI(9, 0, STATS)                                      
            CALL MOVEFI(9, STATS, XSTATS)                               
            GOTO  3                                                     
   2        INPOST = .FALSE.                                            
C IFLAG < 0.                                                            
            CALL MOVEFI(9, XSTATS, STATS)                               
   3  CONTINUE                                                          
   4  RETURN                                                            
      END                                                               
      SUBROUTINE DIODEH(T0, V0, T1, V1, NV, DT, TSTOP)                  
      INTEGER NV                                                        
      DOUBLE PRECISION T0, V0(NV), T1, V1(NV), DT, TSTOP                
C DEFAULT HANDLE PROCEDURE FOR IODES.                                   
C SCRATCH SPACE ALLOCATED - NONE.                                       
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D4SSOE(WV, RV, IV, LV, X, TSTOPE, DTE)           
      INTEGER IV(40)                                                    
      REAL RV(30)                                                       
      LOGICAL LV(20)                                                    
      DOUBLE PRECISION WV(30), X(1), TSTOPE, DTE                        
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER MIN0, I, K, I1, I2, IS(1000)                              
      REAL AMIN1, RS(1000), RTEMP                                       
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION DABS, TEMP, WS(500)                              
      INTEGER TEMP2                                                     
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C WIND THINGS UP AT THE END OF THE TIME-STEP.                           
      WV(2) = TSTOPE                                                    
      WV(6) = DTE                                                       
      D4SSOE = .FALSE.                                                  
C RELEASE E.                                                            
      CALL ISTKRL(1)                                                    
      IF (WV(11)+WV(6) .NE. WV(11)) GOTO 1                              
C/6S                                                                    
C        CALL SETERR(30HDESSOM - DT=0 RETURNED BY SOUT, 30, 16, 1)      
C/7S                                                                    
         CALL SETERR('DESSOM - DT=0 RETURNED BY SOUT', 30, 16, 1)       
C/                                                                      
         D4SSOE = .TRUE.                                                
         RETURN                                                         
   1  IF ((WV(6)/DABS(WV(6)))*(WV(2)-WV(11)) .GE. 0D0) GOTO 2           
C/6S                                                                    
C        CALL SETERR(47HDESSOM - DT RETURNED BY SOUT HAS THE WRONG SIGN,
C    1      47, 17, 1)                                                  
C/7S                                                                    
         CALL SETERR('DESSOM - DT RETURNED BY SOUT HAS THE WRONG SIGN', 
     1      47, 17, 1)                                                  
C/                                                                      
         D4SSOE = .TRUE.                                                
         RETURN                                                         
   2  IF (.NOT. LV(2)) GOTO 9                                           
         IF (WV(10) .NE. WV(11)) GOTO 3                                 
            IV(17) = MIN0(IV(16), IV(17))                               
            GOTO  8                                                     
   3        TEMP2 = IV(1)                                               
C CONVERGED, SO UPDATE X0, HOPT, HUP, ETC.                              
            DO  4 I = 1, TEMP2                                          
               I1 = IV(12)+I-1                                          
               X(I) = WS(I1)                                            
   4           CONTINUE                                                 
C X=X1.                                                                 
            K = 1                                                       
               GOTO  6                                                  
   5           K = K+1                                                  
   6           IF (K .GT. IV(15)) GOTO  7                               
C HOPTO = HOPT.                                                         
               I1 = IV(7)+K-1                                           
               I2 = IV(6)+K-1                                           
               RS(I1) = RS(I2)                                          
               GOTO  5                                                  
   7        RV(7) = AMIN1(2.*RV(7), RV(6))                              
            IV(19) = IV(18)                                             
            IV(17) = IV(16)                                             
            LV(3) = .FALSE.                                             
   8     CONTINUE                                                       
         GOTO  12                                                       
   9     IF (DABS(WV(9)) .GE. DABS(WV(6))) GOTO 10                      
C/6S                                                                    
C           CALL SETERR(42HDESSOM - DT RAISED BY SOUT WHEN OK = FALSE,  
C    1         42, 18, 1)                                               
C/7S                                                                    
            CALL SETERR('DESSOM - DT RAISED BY SOUT WHEN OK = FALSE',   
     1         42, 18, 1)                                               
C/                                                                      
            D4SSOE = .TRUE.                                             
            RETURN                                                      
C   THE DEFAULT RESPONSE IS TO LOWER DT BY 10**3.                       
  10     IF (DABS(WV(6)) .EQ. DABS(WV(9))) WV(6) = WV(6)/1D+3           
         IF (WV(11)+WV(6) .NE. WV(11)) GOTO 11                          
C/6S                                                                    
C           CALL SETERR(13HDESSOM - DT=0, 13, 15, 1)                    
C/7S                                                                    
            CALL SETERR('DESSOM - DT=0', 13, 15, 1)                     
C/                                                                      
            D4SSOE = .TRUE.                                             
            RETURN                                                      
  11     CONTINUE                                                       
  12  WV(10) = WV(11)                                                   
      WV(11) = WV(10)+WV(6)                                             
      TEMP1 = (WV(6)/DABS(WV(6)))*(WV(11)-WV(2)) .GE. 0D0               
      IF (.NOT. TEMP1) TEMP1 = DABS(WV(11)-WV(2)) .LE. WV(7)*DABS(WV(6))
      IF (TEMP1) WV(11) = WV(2)                                         
      IF (WV(10) .NE. WV(2)) WV(6) = WV(11)-WV(10)                      
      DTE = WV(6)                                                       
      IF (WV(11)+WV(6) .NE. WV(11)) GOTO 13                             
C/6S                                                                    
C        CALL SETERR(13HDESSOM - DT=0, 13, 15, 1)                       
C/7S                                                                    
         CALL SETERR('DESSOM - DT=0', 13, 15, 1)                        
C/                                                                      
         D4SSOE = .TRUE.                                                
  13  RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D4SSOI(WV, RV, IV, LV, TSTART, TSTOP, BETA,      
     1   GAMMA, DELTA, NX, DT, N, KMAX, MMAX, XPOLY, ERRPAR, PESPAR,    
     2   HFRACT, KINIT)                                                 
      INTEGER MMAX                                                      
      INTEGER IV(40), NX, N(MMAX), KMAX, KINIT                          
      REAL RV(30), ERRPAR(2), PESPAR, HFRACT                            
      LOGICAL LV(20), XPOLY                                             
      DOUBLE PRECISION WV(30), TSTART, TSTOP, BETA, GAMMA, DELTA        
      DOUBLE PRECISION DT                                               
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IFA, ING, ILW, ISLOGN, IHOPTO, ILOZNG                     
      INTEGER ISTKGT, ILWORK, I, J, IPOW, IW                            
      INTEGER IS(1000), MUSED, IRCNT, ICOST, IHOPT, MRCNT               
      INTEGER IWORK, KOPTO, IX1                                         
      REAL HUP, LOGRND, HUPMAX, HUP0, ALOG, RS(1000)                    
      REAL LOGHI, FLOAT, LOGLO                                          
      LOGICAL PRSTRT, LS(1000)                                          
      DOUBLE PRECISION DFLOAT, RNDING, DBLE, DABS, DLOG, T0             
      DOUBLE PRECISION T1, CLOSE, WS(500), D1MACH                       
      LOGICAL TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      DATA CLOSE/1D-2/                                                  
      DATA MRCNT/3/                                                     
      DATA HUP0/1E-1/                                                   
C INITIALIZATION.                                                       
      IF (TSTART .NE. TSTOP) GOTO 1                                     
         D4SSOI = .TRUE.                                                
         RETURN                                                         
   1     D4SSOI = .FALSE.                                               
C CHECK THE INPUT.                                                      
C/6S                                                                    
C  2  IF (BETA .LE. 0D0) CALL SETERR(18HDESSOM - BETA.LE.0, 18, 1, 2)   
C     IF (GAMMA .LE. 0D0) CALL SETERR(19HDESSOM - GAMMA.LE.0, 19, 2, 2) 
C     IF (DELTA .LT. 0D0) CALL SETERR(19HDESSOM - DELTA.LT.0, 19, 3, 2) 
C     IF (NX .LT. 1) CALL SETERR(16HDESSOM - NX.LT.1, 16, 4, 2)         
C     IF (TSTART+DT .EQ. TSTART) CALL SETERR(22HDESSOM - DT=0 ON INPUT  
C    1   , 22, 5, 2)                                                    
C     IF (N(1) .LT. 1) CALL SETERR(18HDESSOM - N(1).LT.1, 18, 6, 2)     
C     IF (KMAX .LT. 1) CALL SETERR(18HDESSOM - KMAX.LT.1, 18, 7, 2)     
C/7S                                                                    
   2  IF (BETA .LE. 0D0) CALL SETERR('DESSOM - BETA.LE.0', 18, 1, 2)    
      IF (GAMMA .LE. 0D0) CALL SETERR('DESSOM - GAMMA.LE.0', 19, 2, 2)  
      IF (DELTA .LT. 0D0) CALL SETERR('DESSOM - DELTA.LT.0', 19, 3, 2)  
      IF (NX .LT. 1) CALL SETERR('DESSOM - NX.LT.1', 16, 4, 2)          
      IF (TSTART+DT .EQ. TSTART) CALL SETERR('DESSOM - DT=0 ON INPUT'   
     1   , 22, 5, 2)                                                    
      IF (N(1) .LT. 1) CALL SETERR('DESSOM - N(1).LT.1', 18, 6, 2)      
      IF (KMAX .LT. 1) CALL SETERR('DESSOM - KMAX.LT.1', 18, 7, 2)      
C/                                                                      
      TEMP = MMAX .LT. KMAX+2                                           
      IF (.NOT. TEMP) GOTO 3                                            
         TEMP1 = KMAX .GT. 1                                            
         IF (.NOT. TEMP1) TEMP1 = MMAX .NE. 1                           
         TEMP = TEMP1                                                   
C/6S                                                                    
C  3  IF (TEMP) CALL SETERR(23HDESSOM - MMAX.LT.KMAX+2, 23, 8, 2)       
C/7S                                                                    
   3  IF (TEMP) CALL SETERR('DESSOM - MMAX.LT.KMAX+2', 23, 8, 2)        
C/                                                                      
      TEMP = PESPAR .LE. 0.                                             
      IF (.NOT. TEMP) TEMP = PESPAR .GT. 1.                             
C/6S                                                                    
C     IF (TEMP) CALL SETERR(28HDESSOM - PESPAR NOT IN (0,1), 28, 9, 2)  
C     IF (HFRACT .LE. 0.) CALL SETERR(20HDESSOM - HFRACT.LE.0, 20, 10, 2
C    1   )                                                              
C/7S                                                                    
      IF (TEMP) CALL SETERR('DESSOM - PESPAR NOT IN (0,1)', 28, 9, 2)   
      IF (HFRACT .LE. 0.) CALL SETERR('DESSOM - HFRACT.LE.0', 20, 10, 2 
     1   )                                                              
C/                                                                      
      TEMP = KINIT .LT. 1                                               
      IF (.NOT. TEMP) TEMP = KINIT .GT. KMAX                            
C/6S                                                                    
C     IF (TEMP) CALL SETERR(34HDESSOM - KINIT NOT IN (1,...,KMAX), 34,  
C    1   11, 2)                                                         
C     IF (BETA-DELTA+GAMMA .LE. 0D0) CALL SETERR(                       
C    1   30HDESSOM - BETA-DELTA+GAMMA.LE.0, 30, 12, 2)                  
C/7S                                                                    
      IF (TEMP) CALL SETERR('DESSOM - KINIT NOT IN (1,...,KMAX)', 34,   
     1   11, 2)                                                         
      IF (BETA-DELTA+GAMMA .LE. 0D0) CALL SETERR(                       
     1   'DESSOM - BETA-DELTA+GAMMA.LE.0', 30, 12, 2)                   
C/                                                                      
C ALLOCATE AND LOAD THE ARRAY SLOGN WITH LOG(N(1))+ ... +LOG(N(I-1)).   
      ISLOGN = ISTKGT(MMAX+1, 4)                                        
      WS(ISLOGN) = 0                                                    
      J = 2                                                             
         GOTO  5                                                        
   4     J = J+1                                                        
   5     IF (J .GT. MMAX) GOTO  6                                       
C/6S                                                                    
C        IF (N(J-1) .GE. N(J)) CALL SETERR(                             
C    1      37HDESSOM - N IS NOT MONOTONE INCREASING, 37, 13, 2)        
C/7S                                                                    
         IF (N(J-1) .GE. N(J)) CALL SETERR(                             
     1      'DESSOM - N IS NOT MONOTONE INCREASING', 37, 13, 2)         
C/                                                                      
         I = ISLOGN+J-1                                                 
         WS(I) = WS(I-1)+DLOG(DFLOAT(N(J-1)))                           
         GOTO  4                                                        
   6  I = ISLOGN+MMAX                                                   
      WS(I) = WS(I-1)+DLOG(DFLOAT(N(MMAX)))                             
C/6S                                                                    
C     IF (DT/DABS(DT)*(TSTOP-TSTART) .LE. 0D0) CALL SETERR(             
C    1   30HDESSOM - DT HAS THE WRONG SIGN, 30, 14, 2)                  
C/7S                                                                    
      IF (DT/DABS(DT)*(TSTOP-TSTART) .LE. 0D0) CALL SETERR(             
     1   'DESSOM - DT HAS THE WRONG SIGN', 30, 14, 2)                   
C/                                                                      
C ALLOCATE CURRENT AND OLD OPTIMAL STEP-SIZE ARRAYS.                    
      IHOPT = ISTKGT(KMAX+1, 3)                                         
      IHOPTO = ISTKGT(KMAX+1, 3)                                        
C ALLOCATE AND LOAD THE ARRAY NG WITH N(J)**GAMMA.                      
      ING = ISTKGT(MMAX, 4)                                             
      DO  7 J = 1, MMAX                                                 
         I = ING+J-1                                                    
         WS(I) = DFLOAT(N(J))**GAMMA                                    
   7     CONTINUE                                                       
C ALLOCATE SPACE FOR X1 (THE SOLUTION AT TIME T1) AND A SCRATCH ARRAY F.
      IX1 = ISTKGT(NX, 4)                                               
      IFA = ISTKGT(KMAX, 3)                                             
C ALLOCATE AND LOAD POW(J) WITH 1/(BETA-DELTA+J*GAMMA).                 
      IPOW = ISTKGT(KMAX, 3)                                            
      DO  8 J = 1, KMAX                                                 
         I = IPOW+J-1                                                   
         RS(I) = 1D0/(BETA-DELTA+DBLE(FLOAT(J))*GAMMA)                  
   8     CONTINUE                                                       
C ALLOCATE AND LOAD ARRAYS WORK AND LWORK WITH                          
C SUM(I=1,...,J)(N(I)) AND LOG(WORK(J)), RESPECTIVELY.                  
      IWORK = ISTKGT(MMAX, 3)                                           
      ILWORK = ISTKGT(MMAX, 3)                                          
      RS(IWORK) = N(1)                                                  
      RS(ILWORK) = ALOG(RS(IWORK))                                      
      J = 2                                                             
         GOTO  10                                                       
   9     J = J+1                                                        
  10     IF (J .GT. MMAX) GOTO  11                                      
         IW = IWORK+J-1                                                 
         RS(IW) = RS(IW-1)+FLOAT(N(J))                                  
         ILW = ILWORK+J-1                                               
         RS(ILW) = ALOG(RS(IW))                                         
         GOTO  9                                                        
C ALLOCATE THE COST/UNIT TIME-STEP ARRAY.                               
  11  ICOST = ISTKGT(KMAX+1, 3)                                         
C ALLOCATE THE EXTRAPOLATION LOZENGE LAST SO THAT ISTKMD CAN            
C BE USED TO LET IT GROW ONLY AS NEEDED.                                
      ILOZNG = ISTKGT(1, 4)                                             
C GET THE LOGARITHMS OF THE LARGEST AND SMALLEST NUMBERS,               
C AS WELL AS THE ROUNDING ERROR LEVEL, OF THE MACHINE.                  
      LOGLO = DLOG(D1MACH(1))                                           
      LOGHI = DLOG(D1MACH(2))                                           
      RNDING = D1MACH(4)                                                
      LOGRND = DLOG(RNDING)                                             
      HUPMAX = (-(LOGRND+4.6))/(BETA+GAMMA)                             
      MUSED = 0                                                         
      IRCNT = 0                                                         
      KOPTO = KINIT                                                     
      HUP = HUPMAX                                                      
      PRSTRT = .FALSE.                                                  
      T0 = TSTART                                                       
      T1 = T0+DT                                                        
      TEMP = (DT/DABS(DT))*(T1-TSTOP) .GE. 0D0                          
      IF (.NOT. TEMP) TEMP = DABS(T1-TSTOP) .LE. CLOSE*DABS(DT)         
      IF (TEMP) T1 = TSTOP                                              
      DT = T1-T0                                                        
C LOAD THE VARIABLE ARRAYS WITH THE APPROPRIATE VALUES.                 
      WV(1) = TSTART                                                    
      WV(2) = TSTOP                                                     
      WV(3) = BETA                                                      
      WV(4) = GAMMA                                                     
      WV(5) = DELTA                                                     
      WV(6) = DT                                                        
      WV(7) = CLOSE                                                     
      WV(9) = DT                                                        
      WV(10) = T0                                                       
      WV(11) = T1                                                       
      RV(1) = ERRPAR(1)                                                 
      RV(2) = ERRPAR(2)                                                 
      RV(3) = PESPAR                                                    
      RV(4) = HFRACT                                                    
      RV(5) = HUP0                                                      
      RV(6) = HUPMAX                                                    
      RV(7) = HUP                                                       
      RV(8) = LOGLO                                                     
      RV(9) = LOGHI                                                     
      RV(10) = LOGRND                                                   
      RV(11) = RNDING                                                   
      RV(12) = 1E-2                                                     
      IV(1) = NX                                                        
      IV(2) = KMAX                                                      
      IV(3) = MMAX                                                      
      IV(4) = KINIT                                                     
      IV(5) = ICOST                                                     
      IV(6) = IHOPT                                                     
      IV(7) = IHOPTO                                                    
      IV(8) = IWORK                                                     
      IV(9) = ILWORK                                                    
      IV(10) = IPOW                                                     
      IV(11) = ISLOGN                                                   
      IV(12) = IX1                                                      
      IV(13) = ILOZNG                                                   
      IV(14) = MRCNT                                                    
      IV(15) = 0                                                        
      IV(16) = KOPTO                                                    
      IV(17) = KOPTO                                                    
      IV(20) = MUSED                                                    
      IV(25) = IRCNT                                                    
      IV(26) = IFA                                                      
      IV(27) = ING                                                      
      LV(1) = XPOLY                                                     
      LV(3) = PRSTRT                                                    
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D4SSOM(WV, RV, IV, LV, DTE)                      
      INTEGER IV(40)                                                    
      REAL RV(30)                                                       
      LOGICAL LV(20)                                                    
      DOUBLE PRECISION WV(30), DTE                                      
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IDX, NERROR, MIN0, K, NERR, I1                            
      INTEGER I2, I3, IS(1000)                                          
      REAL ABS, EXP, SNGL, AMIN1, AMAX1, RS(1000)                       
      REAL RTEMP                                                        
      LOGICAL LS(1000), D4SSOL                                          
      DOUBLE PRECISION DABS, TEMP, DMIN1, WS(500), DSQRT                
      INTEGER TEMP2, TEMP3, TEMP4, TEMP5, TEMP6, TEMP7                  
      INTEGER TEMP8                                                     
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE BASIC STEP-SIZE AND ORDER MONITOR.                                
      IF (NERROR(NERR) .EQ. 0) GOTO 1                                   
         D4SSOM = .TRUE.                                                
         RETURN                                                         
   1     D4SSOM = .FALSE.                                               
   2  IF (IV(3) .NE. 1) GOTO 3                                          
         IF (LV(2)) CALL ISTKRL(1)                                      
         D4SSOM = .FALSE.                                               
         RETURN                                                         
   3  IF (.NOT. LV(7)) WV(11) = WV(10)                                  
C SIGNAL A RESTART.                                                     
      TEMP1 = WV(10) .EQ. WV(11)                                        
      IF (TEMP1) TEMP1 = WV(10) .NE. WV(1)                              
      IF (.NOT. TEMP1) GOTO 4                                           
         IV(25) = IV(14)                                                
         WV(12) = WV(10)+WV(6)                                          
   4  TEMP1 = LV(2)                                                     
      IF (TEMP1) TEMP1 = .NOT. LV(4)                                    
      IF (.NOT. TEMP1) GOTO 7                                           
         IF (LV(5)) GOTO 6                                              
            TEMP6 = IV(21)                                              
C FIND THE OPTIMAL DT AND M FOR                                         
C THE NEXT TIME-STEP.                                                   
            TEMP7 = IV(22)                                              
            TEMP8 = IV(10)                                              
            IF (.NOT. D4SSOL(RS(TEMP6), RS(TEMP7), IV(1), IV(18), IV(2),
     1         RS(TEMP8), RV(10))) GOTO 5                               
               D4SSOM = .TRUE.                                          
               RETURN                                                   
   5        CONTINUE                                                    
   6     TEMP8 = IV(21)                                                 
         TEMP7 = IV(11)                                                 
         TEMP6 = IV(26)                                                 
         TEMP5 = IV(10)                                                 
         TEMP4 = IV(6)                                                  
         TEMP3 = IV(9)                                                  
         TEMP2 = IV(5)                                                  
         CALL D4SSOO(RS(TEMP8), IV(1), IV(18), IV(3), IV(2), WS(TEMP7)  
     1      , WV(4), RS(TEMP6), RS(TEMP5), IV(16), WV(6), RS(TEMP4), RS(
     2      TEMP3), RS(TEMP2), RV(8), RV(9), RV(12))                    
C RELEASE THE LOZENGE ERROR ESTIMATES E2.                               
         CALL ISTKRL(1)                                                 
   7  IF (.NOT. LV(2)) GOTO 15                                          
         IV(16) = MIN0(IV(16), IV(17)+1)                                
C GET THE DT FOR THE NEXT TIME-STEP.                                    
         IF (WV(10) .NE. WV(11)) GOTO 8                                 
            IV(28) = MIN0(IV(16), IV(17))                               
            GOTO  9                                                     
   8        IV(28) = IV(16)                                             
   9     IDX = IV(6)+IV(28)                                             
C ABS(HOPT(KOPTM+1))).                                                  
         WV(8) = (WV(6)/DABS(WV(6)))*RV(3)*AMIN1(ABS(SNGL(WV(6)))*EXP(  
     1      RV(7)), ABS(RS(IDX)))                                       
C   TWO RESTARTS IN A ROW CAUSE DT TO DECREASE BY AT LEAST A            
C   FACTOR OF 10**3.                                                    
         TEMP1 = LV(3)                                                  
         IF (TEMP1) TEMP1 = WV(10) .EQ. WV(11)                          
         IF (TEMP1) WV(8) = (WV(6)/DABS(WV(6)))*DMIN1(DABS(WV(8)), 1D-3*
     1      DABS(WV(6)))                                                
         IV(15) = MIN0(IV(18), IV(2)+1)                                 
C   COMPUTE THE COST/UNIT TIME-STEP FOR EACH LOZENGE SIZE.              
C COST(K) = WORK(K+1)/ABS(HOPT(K)).                                     
         TEMP2 = IV(15)                                                 
         DO  12 K = 1, TEMP2                                            
            I1 = IV(5)+K-1                                              
            I2 = IV(8)+K                                                
            I3 = IV(6)+K-1                                              
            IF (RS(I3) .NE. 0.) GOTO 10                                 
               RS(I1) = -1                                              
               GOTO  11                                                 
  10           RS(I1) = RS(I2)/ABS(RS(I3))                              
  11        CONTINUE                                                    
  12        CONTINUE                                                    
         TEMP1 = WV(10) .NE. WV(1)                                      
         IF (TEMP1) TEMP1 = WV(10) .NE. WV(11)                          
         IF (.NOT. TEMP1) GOTO 14                                       
            IDX = MIN0(IV(16)+1, IV(19)-1, IV(18)-1)                    
C SEE IF SHOULD BE CAUTIOUS.                                            
C     IF ABS(HOPT(NEW)) < ABS(HOPT(OLD)), BE CONSERVATIVE.              
            I1 = IV(6)+IDX-1                                            
            I2 = IV(7)+IDX-1                                            
            IF (DABS(WV(8)) .GT. 1D-2*ABS(RS(I1)) .AND. ABS(RS(I1))     
     1          .LT. ABS(RS(I2))) WV(8) = WV(8)*AMAX1(ABS(RS(I1)/RS(I2))
     2         , 1E-2)                                                  
            IF (IV(25) .LE. 0) GOTO 13                                  
               IV(25) = IV(25)-1                                        
C LOGARITHMIC BISECTION.                                                
               IF (IV(25) .EQ. 0) RV(7) = 0.5*RV(5)                     
               TEMP = (WV(12)-WV(11))/WV(6)                             
               IF (TEMP .GT. 0.99) WV(8) = (WV(6)/DABS(WV(6)))*DMIN1(   
     1            0.5*DABS(WV(12)-WV(11)), DABS(WV(8)), DSQRT(TEMP)*    
     2            DABS(WV(6)))                                          
  13        CONTINUE                                                    
  14     CONTINUE                                                       
  15  IF (WV(10) .EQ. WV(11)) LV(3) = .TRUE.                            
      WV(9) = WV(6)                                                     
      IF (LV(2)) WV(6) = WV(8)                                          
      DTE = WV(6)                                                       
      IF (WV(11)+WV(6) .NE. WV(11)) GOTO 16                             
C/6S                                                                    
C        CALL SETERR(13HDESSOM - DT=0, 13, 15, 1)                       
C/7S                                                                    
         CALL SETERR('DESSOM - DT=0', 13, 15, 1)                        
C/                                                                      
         D4SSOM = .TRUE.                                                
  16  RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D4SSOR(WV, RV, IV, LV, ERRARE)                   
      INTEGER IV(40)                                                    
      REAL RV(30), ERRARE(2)                                            
      LOGICAL LV(20)                                                    
      DOUBLE PRECISION WV(30)                                           
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IDX, IFIX, I1, I2, I3, IS(1000)                           
      REAL RS(1000), RTEMP                                              
      LOGICAL LS(1000), D4SSOD, D4SSOL                                  
      DOUBLE PRECISION TEMP, WS(500)                                    
      INTEGER TEMP1, TEMP2, TEMP3, TEMP4, TEMP5, TEMP6                  
      INTEGER TEMP7                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C SEE IF A RESTART IS IN ORDER.                                         
      RV(1) = ERRARE(1)                                                 
C UPDATE ERRPAR.                                                        
      RV(2) = ERRARE(2)                                                 
      IF (.NOT. LV(7)) GOTO 1                                           
         D4SSOR = .TRUE.                                                
         RETURN                                                         
   1     D4SSOR = .FALSE.                                               
   2  IF (IV(18) .LE. IV(17)) GOTO 8                                    
         LV(5) = .TRUE.                                                 
C SEE IF A RE-START IS NECESSARY.                                       
         TEMP6 = IV(21)                                                 
         TEMP5 = IV(22)                                                 
         TEMP4 = IV(10)                                                 
         IF (.NOT. D4SSOL(RS(TEMP6), RS(TEMP5), IV(1), IV(18), IV(2),   
     1      RS(TEMP4), RV(10))) GOTO 3                                  
            D4SSOR = .TRUE.                                             
            RETURN                                                      
C   IF WILL NOT CONVERGE IN THIS LOZENGE, RESTART.                      
   3     TEMP4 = IV(21)                                                 
         TEMP5 = IV(11)                                                 
         TEMP6 = IV(26)                                                 
         TEMP7 = IV(10)                                                 
         LV(6) = D4SSOD(RS(TEMP4), IV(1), IV(18), IV(3), IV(2), WS(     
     1      TEMP5), WV(3), WV(4), WV(5), RS(TEMP6), RS(TEMP7), IV(24),  
     2      IV(23), IV(17))                                             
         IF (.NOT. LV(6)) GOTO 4                                        
            D4SSOR = .TRUE.                                             
            RETURN                                                      
   4     IF (IV(18) .LE. IV(17)+1) GOTO 7                               
            TEMP7 = IV(21)                                              
C SEE IF A RE-START WOULD BE MORE EFFICIENT.                            
            TEMP6 = IV(11)                                              
            TEMP5 = IV(26)                                              
            TEMP4 = IV(10)                                              
            TEMP3 = IV(6)                                               
            TEMP2 = IV(9)                                               
            TEMP1 = IV(5)                                               
            CALL D4SSOO(RS(TEMP7), IV(1), IV(18), IV(3), IV(2), WS(     
     1         TEMP6), WV(4), RS(TEMP5), RS(TEMP4), IV(16), WV(6), RS(  
     2         TEMP3), RS(TEMP2), RS(TEMP1), RV(8), RV(9), RV(12))      
            IDX = IV(6)+IV(16)                                          
            IF (WV(11)+RS(IDX) .NE. WV(11)) GOTO 5                      
C/6S                                                                    
C              CALL SETERR(13HDESSOM - DT=0, 13, 15, 1)                 
C/7S                                                                    
               CALL SETERR('DESSOM - DT=0', 13, 15, 1)                  
C/                                                                      
               D4SSOR = .TRUE.                                          
               RETURN                                                   
C = SNGL(DT/HOPT(KOPT+1)).                                              
   5        RTEMP = WV(6)/RS(IDX)                                       
            IF (RTEMP .LE. 1E+3) RTEMP = IFIX(RTEMP+0.99)               
C     IF ( WORK(M) + WORK(KOPT+2)*RTEMP <= WORK(L) )                    
            I1 = IV(8)+IV(18)-1                                         
            I2 = IV(8)+IV(16)+1                                         
            I3 = IV(8)+IV(24)-1                                         
            IF (RS(I1)+RS(I2)*RTEMP .GT. RS(I3)) GOTO 6                 
               CALL ISTKRL(1)                                           
               LV(4) = .TRUE.                                           
               D4SSOR = .TRUE.                                          
               RETURN                                                   
   6        CONTINUE                                                    
   7     CONTINUE                                                       
   8  IF (IV(18) .LT. IV(3)) CALL ISTKRL(2)                             
      RETURN                                                            
      END                                                               
      SUBROUTINE D4SSOO(E, NX, M, MMAX, KMAX, SLOGN, GAMMA, F,          
     1   POW, KOPT, DT, HOPT, LNWORK, COST, LOGLO, LOGHI, COSTOL)       
      INTEGER KMAX, MMAX, NX                                            
      INTEGER M, KOPT                                                   
      REAL E(NX, KMAX), F(KMAX), POW(KMAX), HOPT(1), LNWORK(MMAX), COST(
     1   1)                                                             
      REAL LOGLO, LOGHI, COSTOL                                         
      DOUBLE PRECISION SLOGN(MMAX), GAMMA, DT                           
      INTEGER JHI, KHI, KMJ, MMJ, MIN0, I                               
      INTEGER J, K                                                      
      REAL EXP, RHOMAX, MINOST, AMIN1, AMAX1, HOPTK                     
      INTEGER TEMP                                                      
C COMPUTE THE OPTIMAL K AND H.                                          
C REAL HOPT(KMAX+1),COST(KMAX+1)                                        
      KOPT = 1                                                          
      KHI = MIN0(M-1, KMAX)                                             
C COMPUTE HOPT(K), K=1,...,MIN(M,KMAX+1).                               
      TEMP = KHI+1                                                      
      DO  6 K = 1, TEMP                                                 
         JHI = MIN0(K, KHI)                                             
C   COMPUTE THE FACTORS WHICH CONVERT ERRORS INTO STEP-SIZES.           
         DO  1 J = 1, JHI                                               
            KMJ = K-J                                                   
            MMJ = M-J                                                   
            F(J) = GAMMA*POW(J)*((SLOGN(K+1)-SLOGN(M))-(SLOGN(KMJ+1)-   
     1         SLOGN(MMJ)))                                             
   1        CONTINUE                                                    
C   HOPTK IS THE OPTIMAL STEP-SIZE FOR THE K-COLUMN LOZENGE.            
         HOPTK = LOGHI                                                  
         DO  3 I = 1, NX                                                
            RHOMAX = LOGLO                                              
            DO  2 J = 1, JHI                                            
               RHOMAX = AMAX1(RHOMAX, F(J)+E(I, J))                     
   2           CONTINUE                                                 
            HOPTK = AMIN1(HOPTK, RHOMAX)                                
   3        CONTINUE                                                    
         COST(K) = LNWORK(K+1)-HOPTK                                    
         IF (K .NE. 1) GOTO 4                                           
            MINOST = COST(1)                                            
            GOTO  5                                                     
   4        IF (K .LE. KHI) MINOST = AMIN1(MINOST, COST(K))             
   5     HOPT(K) = EXP(HOPTK)*DT                                        
   6     CONTINUE                                                       
C OPTIMIZE THE COST TO WITHIN A RELATIVE TOLERANCE OF COSTTOL.          
      DO  8 K = 1, KHI                                                  
         IF (COST(K) .GT. MINOST+COSTOL) GOTO 7                         
            KOPT = K                                                    
            GOTO  9                                                     
   7     CONTINUE                                                       
   8     CONTINUE                                                       
   9  RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D4SSOX(WV, RV, IV, LV, N, ME)                    
      INTEGER ME                                                        
      INTEGER IV(40), N(ME)                                             
      REAL RV(30)                                                       
      LOGICAL LV(20)                                                    
      DOUBLE PRECISION WV(30)                                           
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKMD, ISTKGT, MIN0, MAX0, IS(1000), ITEMP               
      REAL RS(1000), FLOAT, RTEMP                                       
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION DBLE, DABS, TEMP, DMAX1, WS(500)                 
      INTEGER TEMP1, TEMP2, TEMP3, TEMP4, TEMP5                         
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C DO THE EXTRAPOLATION.                                                 
      LV(6) = .FALSE.                                                   
      LV(7) = .FALSE.                                                   
      LV(4) = .FALSE.                                                   
      LV(5) = .FALSE.                                                   
      D4SSOX = .FALSE.                                                  
      IV(18) = ME                                                       
      TEMP5 = IV(18)                                                    
      IF (10D0*DMAX1(DABS(WV(10)), DABS(WV(11)))*RV(11) .LT. DABS(WV(11)
     1   -WV(10))*RV(4)/DBLE(FLOAT(N(TEMP5)))) GOTO 1                   
C/6S                                                                    
C        CALL SETERR(13HDESSOM - DT=0, 13, 15, 1)                       
C/7S                                                                    
         CALL SETERR('DESSOM - DT=0', 13, 15, 1)                        
C/                                                                      
         D4SSOX = .TRUE.                                                
         RETURN                                                         
   1  IF (LV(2)) GOTO 2                                                 
         IV(22) = ISTKGT(IV(1), 3)                                      
         D4SSOX = .TRUE.                                                
         RETURN                                                         
   2  IF (MIN0(IV(18), IV(2)) .LE. IV(20)) GOTO 3                       
         ITEMP = ISTKMD(IV(1)*MIN0(IV(18), IV(2)))                      
C EXPAND THE EXTRAPOLATION LOZENGE.                                     
C/6S                                                                    
C        IF (IV(13) .LT. ITEMP) CALL SETERR(                            
C    1      47HDESSOM - SOMEBODY IS LEAVING STUFF ON THE STACK, 47, 20  
C    2      , 2)                                                        
C        IF (IV(13) .GT. ITEMP) CALL SETERR(                            
C    1      50HDESSOM - SOMEBODY IS REMOVING STUFF FROM THE STACK, 50,  
C    2      21, 2)                                                      
C/7S                                                                    
         IF (IV(13) .LT. ITEMP) CALL SETERR(                            
     1      'DESSOM - SOMEBODY IS LEAVING STUFF ON THE STACK', 47, 20   
     2      , 2)                                                        
         IF (IV(13) .GT. ITEMP) CALL SETERR(                            
     1      'DESSOM - SOMEBODY IS REMOVING STUFF FROM THE STACK', 50,   
     2      21, 2)                                                      
C/                                                                      
   3  IV(20) = MAX0(IV(18), IV(20))                                     
C THE BEST ERROR IN THE LOZENGE.                                        
      IV(22) = ISTKGT(IV(1), 3)                                         
C THE LOZENGE ERROR.                                                    
      IV(21) = ISTKGT(MAX0(1, IV(1)*MIN0(IV(18)-1, IV(2))), 3)          
      TEMP5 = IV(12)                                                    
      TEMP4 = IV(27)                                                    
      TEMP3 = IV(13)                                                    
      TEMP2 = IV(21)                                                    
      TEMP1 = IV(22)                                                    
      CALL DXTRAP(WS(TEMP5), IV(18), IV(1), WS(TEMP4), IV(2), LV(1), WS(
     1   TEMP3), RS(TEMP2), RS(TEMP1))                                  
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D4SSOD(E, NX, M, MMAX, KMAX, SLOGN, BETA,        
     1   GAMMA, DELTA, F, POW, LDONE, ILDONE, KOPTO)                    
      INTEGER KMAX, MMAX, NX                                            
      INTEGER M, LDONE, ILDONE, KOPTO                                   
      REAL E(NX, KMAX), F(KMAX), POW(KMAX)                              
      DOUBLE PRECISION SLOGN(MMAX), BETA, GAMMA, DELTA                  
      INTEGER JHI, LMJ, MMJ, LDONEO, MIN0, I                            
      INTEGER J, L, LMAX, LGIVE, JSAVE                                  
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      DATA LGIVE/1/                                                     
C RETURN LDONE = THE LEVEL WHERE CONVERGENCE IS EXPECTED.               
C                                                                       
C IF M = KOPTO+1, RETURN ILDONE=LDONE.                                  
C IF M > KOPTO+1, DO NOT LET LDONE > ILDONE+1 HAPPEN IN THE FIRST       
C KOPTO COLUMNS.                                                        
C                                                                       
C D4SSOD = TRUE IF WILL NOT CONVERGE IN THIS LOZENGE.                   
C D4SSOD = FALSE IF WILL CONVERGE.                                      
      D4SSOD = .TRUE.                                                   
C INITIALLY, FLAG NOT CONVERGENT.                                       
      LDONE = 0                                                         
      IF (M .LE. KOPTO+1) GOTO 1                                        
         LMAX = MIN0(ILDONE+LGIVE, MMAX)                                
         GOTO  2                                                        
   1     LMAX = MMAX                                                    
   2  IF (M .GE. LMAX) GOTO 10                                          
         TEMP1 = M+1                                                    
         DO  9 L = TEMP1, LMAX                                          
            IF (LDONE .GT. 0 .AND. (L .LT. LMAX .OR. M .LE. KOPTO+1))   
     1         GOTO  9                                                  
C     LDONEO DETERMINES IF WILL CONVERGE IN THE FIRST KOPTO             
C     COLUMNS. SET TRUE INITIALLY.                                      
            LDONEO = L                                                  
            JHI = MIN0(M-1, KMAX)                                       
C COMPUTE THE FACTORS USED FOR CONVERGENCE CHECK.                       
            DO  3 J = 1, JHI                                            
               MMJ = M-J                                                
               LMJ = L-J                                                
               F(J) = POW(J)*GAMMA*((SLOGN(M)-SLOGN(L))-(SLOGN(MMJ)-    
     1            SLOGN(LMJ)))                                          
   3           CONTINUE                                                 
C SEE IF THE I-TH VARIABLE WILL CONVERGE AT M=L.                        
            DO  7 I = 1, NX                                             
C FLAG NOT CONVERGENT.                                                  
               JSAVE = 0                                                
C CHECK EACH COLUMN FOR CONVERGENCE.                                    
               DO  5 J = 1, JHI                                         
                  IF (E(I, J) .LT. F(J)) GOTO 4                         
                     JSAVE = J                                          
                     GOTO  6                                            
   4              CONTINUE                                              
   5              CONTINUE                                              
   6           IF (JSAVE .EQ. 0) GOTO  8                                
C NO CONVERGENCE.                                                       
               IF (JSAVE .GT. KOPTO) LDONEO = 0                         
C NO CONVERGENCE IN                                                     
C COLUMNS 1,...,KOPTO.                                                  
   7           CONTINUE                                                 
   8        IF (JSAVE .EQ. 0) GOTO  9                                   
C NO CONVERGENCE, TRY THE NEXT L.                                       
            IF (LDONE .EQ. 0) LDONE = L                                 
   9        CONTINUE                                                    
  10  TEMP = LDONE .NE. 0                                               
      IF (TEMP) TEMP = LDONEO .NE. 0                                    
      IF (.NOT. TEMP) GOTO 11                                           
         IF (M .EQ. KOPTO+1) ILDONE = LDONE                             
C WILL CONVERGE AT M = LDONE.                                           
         D4SSOD = .FALSE.                                               
  11  RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D4SSOL(E2, E, NX, M, KMAX, POW, LOGRND)          
      INTEGER KMAX, NX                                                  
      INTEGER M                                                         
      REAL E2(NX, KMAX), E(NX), POW(KMAX), LOGRND                       
      INTEGER JHI, MIN0, I, J                                           
      REAL ABS, V, ALOG, LOGE, AMIN1                                    
      LOGICAL TEMP                                                      
C TO RETURN POW(J) TIMES THE LOGARITHM OF THE RATIO OF THE DESIRED      
C TO THE ATTAINED ERROR, FOR EACH ELEMENT IN THE LOZENGE.               
C                                                                       
C D4SSOL = TRUE IF NOT SUCCESSFUL.                                      
C D4SSOL = FALSE IF SUCCESSFUL.                                         
      D4SSOL = .FALSE.                                                  
      JHI = MIN0(M-1, KMAX)                                             
      DO  5 I = 1, NX                                                   
         TEMP = E(I) .EQ. 0.                                            
         IF (TEMP) TEMP = E2(I, 1) .NE. 0.                              
         IF (.NOT. TEMP) TEMP = E(I) .LT. 0.                            
         IF (.NOT. TEMP) GOTO 1                                         
C/6S                                                                    
C           CALL SETERR(37HDESSOM - E(I).LE.0 RETURNED BY SERROR, 37,   
C    1         19, 1)                                                   
C/7S                                                                    
            CALL SETERR('DESSOM - E(I).LE.0 RETURNED BY SERROR', 37,    
     1         19, 1)                                                   
C/                                                                      
            D4SSOL = .TRUE.                                             
            GOTO  6                                                     
   1     IF (E(I) .GT. 0.) LOGE = ALOG(E(I))                            
         DO  4 J = 1, JHI                                               
            TEMP = E2(I, J) .NE. 0.                                     
            IF (TEMP) TEMP = E2(I, 1) .NE. 0.                           
            IF (.NOT. TEMP) GOTO 2                                      
               V = AMIN1(LOGE-ALOG(ABS(E2(I, J))), (-LOGRND)-4.6)       
               GOTO  3                                                  
   2           V = (-LOGRND)-4.6                                        
   3        E2(I, J) = POW(J)*V                                         
   4        CONTINUE                                                    
   5     CONTINUE                                                       
   6  RETURN                                                            
      END                                                               
      SUBROUTINE ODES(F,X,NX,TSTART,TSTOP,DT,ERRPAR,HANDLE)             
C                                                                       
C  TO SOLVE THE INITIAL VALUE PROBLEM FOR                               
C                                                                       
C          DX(T)/DT = F(T,X(T)).                                        
C                                                                       
C  METHOD - RATIONAL EXTRAPOLATION OF GRAGGS MODIFIED MID-POINT RULE.   
C                                                                       
C  INPUT                                                                
C                                                                       
C    F      - CALL F(T,X,NX,FTX) SHOULD RETURN FTX(I)=F(T,X)(I),        
C             FOR I=1,...,NX. IF IT CANNOT, IT SHOULD RETURN            
C             OKAY=.FALSE. IN COMMON /ODESF/OKAY .                      
C             F SHOULD BE DECLARED EXTERNAL IN THE SUBPROGRAM           
C             CALLING ODES.                                             
C    X      - THE INITIAL VALUES FOR THE SOLUTION.                      
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    TSTART - THE INITIAL TIME.                                         
C    TSTOP  - THE FINAL TIME.                                           
C    DT     - THE INITIAL TIME-STEP TO BE USED.                         
C             THE PERFORMANCE OF ODES IS SUBSTANTIALLY                  
C             INDEPENDENT OF THE VALUE OF DT CHOSEN BY THE USER.        
C             IT IS SUFFICIENT THAT THE USERS CHOICE FOR DT MERELY BE   
C             WITHIN SEVERAL ORDERS OF MAGNITUDE OF BEING CORRECT.      
C             THE VALUE OF DT WILL BE AUTOMATICALLY CHANGED BY ODES     
C             DURING THE INTEGRATION PROCESS IN SUCH A WAY AS TO GET    
C             THE SOLUTION, TO THE DESIRED ACCURACY, AT THE LEAST       
C             POSSIBLE COST.                                            
C    ERRPAR - EACH COMPONENT X(I) OF THE SOLUTION IS TO BE COMPUTED     
C             TO WITHIN AN ABSOLUTE ERROR OF                            
C                                                                       
C                     ERRPAR(1) * ABS(X(I)) + ERRPAR(2)                 
C                                                                       
C             FOR I=1,...,NX, AT EACH TIME-STEP. THIS ERROR REQUEST MUST
C             ALWAYS BE POSITIVE.                                       
C    HANDLE - OUTPUT ROUTINE WITH A CALLING SEQUENCE OF THE FORM        
C                                                                       
C                     HANDLE(T0,X0,T1,X1,NX,DT,TSTOP,E)                 
C                                                                       
C             HANDLE WILL BE CALLED AT THE END OF EACH TIME-STEP.       
C                                                                       
C             THE INPUT TO HANDLE IS AS FOLLOWS                         
C                                                                       
C               X0,X1  - X0=X(T0) AND X1=X(T1).                         
C               T0,T1  - T0=T1 INDICATES A RESTART AND X1 IS FULL OF    
C                        GARBAGE.                                       
C               NX     - THE LENGTH OF THE SOLUTION VECTOR X.           
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE CURRENT FINAL TIME.                        
C               E      - E(I) GIVES THE REAL ABSOLUTE ERROR IN X1(I),   
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C                                                                       
C             THE OUTPUT FROM HANDLE MAY BE ANY OF                      
C                                                                       
C               X1     - MAY BE ALTERED IF DESIRED.                     
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE FINAL TIME VALUE.                          
C                                                                       
C             HANDLE SHOULD BE DECLARED EXTERNAL IN THE                 
C             SUBPROGRAM CALLING ODES.                                  
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    X      - X=X(TSTOP), THE FINAL VALUE FOR THE SOLUTION.             
C    TSTOP  - MAY BE ALTERED BY USER SUPPLIED ROUTINE HANDLE.           
C    DT     - PROPOSED TIME-STEP FOR THE NEXT STEP, IF ANY.             
C                                                                       
C  SCRATCH SPACE OF LENGTH                                              
C                                                                       
C                   S(ODES) .LE.                                        
C                                                                       
C    32 + 12*NX                                                         
C                                                                       
C  REAL WORDS +                                                         
C                                                                       
C    101 + MAX( 2*NX REAL + S(F) ,                                      
C                                                                       
C               11*NX + 10 REAL + 10 ,                                  
C                                                                       
C               NX + S(HANDLE) )                                        
C                                                                       
C  INTEGER WORDS IS ALLOCATED.                                          
C                                                                       
C  ERROR STATES                                                         
C                                                                       
C    1 - NX.LT.1.                                                       
C    2 - DT HAS THE WRONG SIGN ON INPUT.                                
C    3 - DT=0 ON INPUT.                                                 
C    4 - DT RETURNED BY HANDLE HAS THE WRONG SIGN.                      
C    5 - DT=0 WAS RETURNED BY HANDLE. (RECOVERABLE)                     
C    6 - THE ERROR DESIRED IN X(I) IS NOT POSITIVE. (RECOVERABLE)       
C    7 - DT=0. (RECOVERABLE)                                            
C    8 - CANNOT RAISE DT IN HANDLE WHEN .NOT.OKAY.                      
C                                                                       
      COMMON /ODESF/OKAY                                                
C                                                                       
      REAL X(NX),TSTART,TSTOP,DT                                        
      REAL ERRPAR(2)                                                    
      LOGICAL OKAY                                                      
      EXTERNAL F,HANDLE                                                 
C                                                                       
      EXTERNAL ODESE                                                    
C                                                                       
      IF (TSTART.EQ.TSTOP) GO TO 10                                     
C                                                                       
      CALL ODES1(F,X,NX,TSTART,TSTOP,DT,ODESE,ERRPAR,HANDLE,            
     1            .FALSE.,.FALSE.)                                      
C                                                                       
 10   RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION ODESE(X,NX,T,DT,ERRPAR,ERPUTS,E)                 
C                                                                       
C  STANDARD ERROR ROUTINE FOR ODES WITH THE OPTION FOR ERROR CONTROL    
C  BASED ON EITHER THE LOCAL VALUE OR THE GLOBAL MAXIMUM OF EACH        
C  COMPONENT.                                                           
C                                                                       
C  THE OPTION FOR ERROR CONTROL ON AN ERROR PER UNIT-TIME-STEP OR       
C  ERROR PER TIME-STEP BASIS IS ALSO PROVIDED.                          
C                                                                       
C  INPUT                                                                
C                                                                       
C    X      - X=X(T), THE APPROXIMATE SOLUTION FOR WHICH AN ERROR       
C             CRITERION IS DESIRED.                                     
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    T      - CURRENT VALUE OF THE TIME VARIABLE.                       
C    DT     - CURRENT TIME-STEP.                                        
C    ERRPAR - TWO PARAMETERS FOR USE IN DETERMINING THE DESIRED ERROR.  
C    ERPUTS - IF ERPUTS=.TRUE., THEN THE ERROR IS TO BE                 
C             PROPORTIONAL TO ABS(DT). OTHERWISE IT WILL NOT.           
C    E      - X(I) IS ACCURATE TO A REAL ABSOLUTE ERROR OF E(I),        
C             I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.             
C                                                                       
C  COMMON INPUT                                                         
C                                                                       
C    IGMAX  - THE POINTER TO THE REAL VECTOR OF CURRENT MAXIMUM ABSOLUTE
C             VALUES ATTAINED BY EACH COMPONENT OF THE SOLUTION.        
C             IGMAX=0 MEANS THIS VECTOR IS NOT USED AND HAS NOT BEEN    
C             ALLOCATED.                                                
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    E      - THE REAL ERROR VECTOR. E(I) IS THE ABSOLUTE ERROR         
C             TOLERABLE IN X(I), FOR I=1,...,NX.                        
C                                                                       
C             LET V(I) = ABS(X(I)) IF IGMAX=0                           
C                           OTHERWISE                                   
C                      = MAXIMUM(ABS(X(I)(T))) OVER ALL PREVIOUS TIME.  
C                        THIS VALUE IS STORED IN THE REAL STACK         
C                        POSITION RS(I+IGMAX-1).                        
C                                                                       
C             AND EPS = 1 IF ERPUTS=.FALSE.                             
C                           OTHERWISE                                   
C                     = ABS(DT),                                        
C                                                                       
C             THEN                                                      
C                                                                       
C                     E(I) = EPS * (ERRPAR(1)*V(I) + ERRPAR(2)),        
C                                                                       
C             FOR I=1,...,NX.                                           
C                                                                       
C  FUNCTION VALUE                                                       
C                                                                       
C    ODESE - .TRUE. IF EACH X(I) IS ACCURATE TO WITHIN AN               
C             ABSOLUTE ERROR OF E(I), I=1,...,NX, OTHERWISE .FALSE. .   
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES - NONE.                                                 
C                                                                       
      COMMON /ODESM/IGMAX,IGMAXO                                        
C                                                                       
      REAL X(NX),T,DT                                                   
      REAL ERRPAR(2),E(NX)                                              
      LOGICAL ERPUTS                                                    
C                                                                       
      REAL DTPOW,TEMP                                                   
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),RS(1))                                         
C                                                                       
      DTPOW=1.0E0                                                       
      IF (ERPUTS) DTPOW=ABS(DT)                                         
C                                                                       
      ODESE=.TRUE.                                                      
      J=IGMAX                                                           
C                                                                       
      DO 10 I=1,NX                                                      
C                                                                       
         IF (IGMAX.GT.0) TEMP=RS(J)                                     
         IF (IGMAX.EQ.0) TEMP=ABS((X(I)))                               
         TEMP=DTPOW*(ERRPAR(1)*TEMP+ERRPAR(2))                          
C                                                                       
         IF (E(I).GT.TEMP) ODESE=.FALSE.                                
C                                                                       
         E(I)=TEMP                                                      
C                                                                       
 10      J=J+1                                                          
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE ODESH(T0,X0,T1,X1,NX,DT,TSTOP,E)                       
C                                                                       
C  THE DEFAULT OUTPUT ROUTINE FOR USE WITH ODES.                        
C  IT SIMPLY RETURNS.                                                   
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES - NONE.                                                 
C                                                                       
      REAL T0,X0(NX),T1,X1(NX),DT,TSTOP                                 
      REAL E(NX)                                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION ODESQ(X,NX,T,DT,ERRPAR,ERPUTS,E)                 
C                                                                       
C  STANDARD QUADRATURE ERROR ROUTINE FOR ODES.                          
C                                                                       
C  INPUT                                                                
C                                                                       
C    X      - X=X(T), THE APPROXIMATE SOLUTION FOR WHICH AN ERROR       
C             CRITERION IS DESIRED.                                     
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    T      - CURRENT VALUE OF THE TIME VARIABLE.                       
C    DT     - CURRENT TIME-STEP.                                        
C    ERRPAR - TWO PARAMETERS FOR USE IN DETERMINING THE DESIRED ERROR.  
C             THE FINAL INTEGRAL SHOULD BE COMPUTED ACCURATE TO A REAL  
C             ABSOLUTE ERROR OF ERRPAR(2).                              
C    ERPUTS - THIS PARAMETER IS IGNORED.                                
C    E      - X(I) IS ACCURATE TO A REAL ABSOLUTE ERROR OF E(I), FOR THE
C             SINGLE CURRENT TIME-STEP, I=1,...,NX.                     
C                                                                       
C  COMMON INPUT -                                                       
C                                                                       
C    TEND   - THE END OF THE INTEGRATION INTERVAL.                      
C    RERROR - THE REMAINDER OF THE INTEGRAL SHOULD BE DONE TO WITHIN    
C             A REAL ABSOLUTE ERROR OF RERROR.                          
C             THE FIRST CALL SHOULD HAVE RERROR=ERRPAR(2).              
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    E      - E(I)=MAX(RERROR*ABS(DT/(TEND-(T-DT))),                    
C                      1.0E-3*ERRPAR(2)) ,   FOR I=1,...,NX.            
C             THUS, THE FINAL INTEGRAL SHOULD BE ACCURATE TO WITHIN     
C             A REAL ABSOLUTE ERROR OF ERRPAR(2).                       
C                                                                       
C  FUNCTION VALUE -                                                     
C                                                                       
C    ODESQ - .TRUE. IF EACH X(I) IS ACCURATE TO WITHIN AN               
C             ABSOLUTE ERROR OF E(I), I=1,...,NX, OTHERWISE .FALSE. .   
C                                                                       
C  COMMON OUTPUT -                                                      
C                                                                       
C    RERROR - IF (ODESQ) THEN                                           
C             RERROR=RERROR - MAXIMUM(E(1),...,E(NX)), WHERE THE E      
C             USED IS THE INPUT VALUE FOR THAT VARIABLE.                
C             OTHERWISE, RERROR REMAINS UNCHANGED.                      
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES - NONE.                                                 
C                                                                       
      COMMON /A0DESQ/TEND,RERROR                                        
C                                                                       
      REAL X(NX),T,DT,TEND                                              
      REAL ERRPAR(2),E(NX),RERROR                                       
      LOGICAL ERPUTS                                                    
C                                                                       
      REAL EMAX,TEMP                                                    
C                                                                       
      ODESQ=.TRUE.                                                      
      EMAX=0.0E0                                                        
C                                                                       
      DO 10 I=1,NX                                                      
C                                                                       
         TEMP=AMAX1(RERROR*ABS((DT/(TEND-(T-DT)))),                     
     1              1.0E-3*ERRPAR(2))                                   
C                                                                       
         IF (E(I).GT.TEMP) ODESQ=.FALSE.                                
C                                                                       
         EMAX=AMAX1(EMAX,E(I))                                          
C                                                                       
 10      E(I)=TEMP                                                      
C                                                                       
      IF (ODESQ) RERROR=RERROR-EMAX                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE ODES1(F,X,NX,TSTART,TSTOP,DT,ERROR,ERRPAR,HANDLE,      
     1                  GLBMAX,ERPUTS)                                  
C                                                                       
C  TO SOLVE THE INITIAL VALUE PROBLEM FOR                               
C                                                                       
C          DX(T)/DT = F(T,X(T)).                                        
C                                                                       
C  METHOD - RATIONAL EXTRAPOLATION OF GRAGGS MODIFIED MID-POINT RULE.   
C                                                                       
C  THE 3 EXTRA ARGUMENTS IN THIS SUBROUTINE PROVIDE MORE USER           
C  CONTROL OVER THE ACCURACY OF THE INTEGRATION PROCESS.                
C                                                                       
C  INPUT                                                                
C                                                                       
C    F      - CALL F(T,X,NX,FTX) SHOULD RETURN FTX(I)=F(T,X)(I),        
C             FOR I=1,...,NX. IF IT CANNOT, IT SHOULD RETURN            
C             OKAY=.FALSE. IN COMMON /ODESF/OKAY .                      
C             F SHOULD BE DECLARED EXTERNAL IN THE SUBPROGRAM           
C             CALLING ODES.                                             
C    X      - THE INITIAL VALUES FOR THE SOLUTION.                      
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    TSTART - THE INITIAL TIME.                                         
C    TSTOP  - THE FINAL TIME.                                           
C    DT     - THE INITIAL TIME-STEP TO BE USED.                         
C             THE PERFORMANCE OF ODES IS SUBSTANTIALLY                  
C             INDEPENDENT OF THE VALUE OF DT CHOSEN BY THE USER.        
C             IT IS SUFFICIENT THAT THE USERS CHOICE FOR DT MERELY BE   
C             WITHIN SEVERAL ORDERS OF MAGNITUDE OF BEING CORRECT.      
C             THE VALUE OF DT WILL BE AUTOMATICALLY CHANGED BY ODES     
C             DURING THE INTEGRATION PROCESS IN SUCH A WAY AS TO GET    
C             THE SOLUTION, TO THE DESIRED ACCURACY, AT THE LEAST       
C             POSSIBLE COST.                                            
C    ERROR  - ERROR TOLERANCE ROUTINE.                                  
C                                                                       
C                 LOGICAL FUNCTION ERROR(X,NX,T,DT,ERRPAR,ERPUTS,E)     
C                                                                       
C             HAS AS INPUT                                              
C                                                                       
C               X,T    - X=X(T), THE APPROXIMATE SOLUTION FOR WHICH     
C                        AN ERROR CRITERION IS DESIRED.                 
C               NX     - THE LENGTH OF X.                               
C               DT     - THE TIME-STEP USED TO OBTAIN X(T).             
C               ERRPAR - TWO PARAMETERS, AS GIVEN TO ODES1.             
C               ERPUTS - THIS VARIABLE HAS THE SAME VALUE AS ERPUTS IN  
C                        THE CALL TO ODES1.                             
C               E      - THE REAL ABSOLUTE ERROR IN X(I) IS E(I),       
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C                                                                       
C             THE OUTPUT IS                                             
C                                                                       
C               ERRPAR - MAY BE ALTERED IF DESIRED.                     
C               E      - E(I) IS THE REAL TOLERABLE ABSOLUTE ERROR IN   
C                        X(I), FOR I=1,...,NX. ALL THE E(I) MUST BE     
C                        POSITIVE.                                      
C                                                                       
C             FUNCTION VALUE                                            
C                                                                       
C               ERROR  - ERROR=.TRUE. IF CONVERGED.                     
C                        ERROR=.FALSE. IF NOT.                          
C    ERRPAR - TWO PARAMETERS TO BE PASSED TO ERROR.                     
C    HANDLE - OUTPUT ROUTINE WITH A CALLING SEQUENCE OF THE FORM        
C                                                                       
C                     HANDLE(T0,X0,T1,X1,NX,DT,TSTOP,E)                 
C                                                                       
C             HANDLE WILL BE CALLED AT THE END OF EACH TIME-STEP.       
C                                                                       
C             THE INPUT TO HANDLE IS AS FOLLOWS                         
C                                                                       
C               X0,X1  - X0=X(T0) AND X1=X(T1).                         
C               T0,T1  - T0=T1 INDICATES A RESTART AND X1 IS FULL OF    
C                        GARBAGE.                                       
C               NX     - THE LENGTH OF THE SOLUTION VECTOR X.           
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE CURRENT FINAL TIME.                        
C               E      - E(I) GIVES THE REAL ABSOLUTE ERROR IN X1(I),   
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C                                                                       
C             THE OUTPUT FROM HANDLE MAY BE ANY OF                      
C                                                                       
C               X1     - MAY BE ALTERED IF DESIRED.                     
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE FINAL TIME VALUE.                          
C                                                                       
C             HANDLE SHOULD BE DECLARED EXTERNAL IN THE                 
C             SUBPROGRAM CALLING ODES.                                  
C    GLBMAX - IF (GLBMAX) THEN THE GLOBAL MAXIMUM OF THE ABSOLUTE VALUE 
C             OF THE SOLUTION IS TO BE RECORDED.                        
C             THE GLOBAL MAXIMUM INFORMATION IS STORED IN COMMON,       
C             AS DESCRIBED IN ODES2.                                    
C                                                                       
C             IF THE ERROR SUBPROGRAM SUPPLIED BY THE USER IS ODESE,    
C             THEN GLBMAX DETERMINES WHETHER OR NOT THE GLOBAL          
C             MAXIMUM ABSOLUTE VALUE OF THE SOLUTION WILL BE USED IN    
C             THAT SUBPROGRAM.                                          
C    ERPUTS - IF (ERPUTS) THEN THE ERROR PER UNIT TIME-STEP CRITERION   
C             IS TO BE USED IN THE ERROR ROUTINE.                       
C             OTHERWISE, THE ERROR PER TIME-STEP CRITERION IS TO BE     
C             USED IN THE ERROR ROUTINE.                                
C                                                                       
C             IF THE ERROR SUBPROGRAM SUPPLIED BY THE USER IS ODESE,    
C             THEN ERPUTS DETERMINES WHETHER OR NOT THE ERROR           
C             PER UNIT-TIME-STEP OR ERROR PER TIME-STEP ERROR           
C             CRITERION WILL BE USED BY THAT SUBPROGRAM.                
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    X      - X=X(TSTOP), THE FINAL VALUE FOR THE SOLUTION.             
C    TSTOP  - MAY BE ALTERED BY USER SUPPLIED ROUTINE HANDLE.           
C    DT     - PROPOSED TIME-STEP FOR THE NEXT STEP, IF ANY.             
C    ERRPAR - MAY BE ALTERED BY USER SUPPLIED ROUTINE ERROR.            
C                                                                       
C  SCRATCH SPACE OF LENGTH                                              
C                                                                       
C                    S(ODES1) .LE.                                      
C                                                                       
C    32 + 12*NX                                                         
C                                                                       
C  REAL WORDS +                                                         
C                                                                       
C    101 + ( IF (GLBMAX) THEN 2*NX , OTHERWISE 0 ) +                    
C                                                                       
C    MAX( 2*NX REAL + S(F) ,                                            
C                                                                       
C         11*NX + MAX ( 10 REAL + 10 , S(ERROR) ) ,                     
C                                                                       
C         NX + S(HANDLE) )                                              
C                                                                       
C  INTEGER WORDS IS ALLOCATED.                                          
C                                                                       
C  ERROR STATES                                                         
C                                                                       
C    1 - NX.LT.1.                                                       
C    2 - DT HAS THE WRONG SIGN ON INPUT.                                
C    3 - DT=0 ON INPUT.                                                 
C    4 - DT RETURNED BY HANDLE HAS THE WRONG SIGN.                      
C    5 - DT=0 WAS RETURNED BY HANDLE. (RECOVERABLE)                     
C    6 - THE ERROR DESIRED IN X(I) IS NOT POSITIVE. (RECOVERABLE)       
C    7 - DT=0. (RECOVERABLE)                                            
C    8 - CANNOT RAISE DT IN HANDLE WHEN .NOT.OKAY.                      
C                                                                       
      COMMON /ODESF/OKAY                                                
C                                                                       
      REAL X(NX),TSTART,TSTOP,DT                                        
      REAL ERRPAR(2)                                                    
      LOGICAL GLBMAX,ERPUTS,OKAY                                        
      EXTERNAL F,ERROR,HANDLE                                           
C                                                                       
      IF (TSTART.EQ.TSTOP) GO TO 10                                     
C                                                                       
      CALL ODES2(F,X,NX,TSTART,TSTOP,DT,ERROR,ERRPAR,HANDLE,            
     1            GLBMAX,ERPUTS,10,16)                                  
C                                                                       
 10   RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE ODES2(F,X,NX,TSTART,TSTOP,DT,ERROR,ERRPAR,HANDLE,      
     1                  GLBMAX,ERPUTS,KMAX,MMAX)                        
C                                                                       
C  TO SOLVE THE INITAL VALUE PROBLEM FOR                                
C                                                                       
C          DX(T)/DT = F(T,X(T)).                                        
C                                                                       
C  METHOD - RATIONAL EXTRAPOLATION OF GRAGGS MODIFIED MID-POINT RULE.   
C                                                                       
C  THE 2 EXTRA ARGUMENTS IN THIS SUBROUTINE PROVIDE USER CONTROL        
C  OVER THE MAXIMUM ORDER AND MAXIMUM LEVEL OF EXTRAPOLATION TO BE USED 
C  BY THE PROCEDURE.                                                    
C                                                                       
C  INPUT                                                                
C                                                                       
C    F      - CALL F(T,X,NX,FTX) SHOULD RETURN FTX(I)=F(T,X)(I),        
C             FOR I=1,...,NX. IF IT CANNOT, IT SHOULD RETURN            
C             OKAY=.FALSE. IN COMMON /ODESF/OKAY .                      
C             F SHOULD BE DECLARED EXTERNAL IN THE SUBPROGRAM           
C             CALLING ODES.                                             
C    X      - THE INITIAL VALUES FOR THE SOLUTION.                      
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    TSTART - THE INITIAL TIME.                                         
C    TSTOP  - THE FINAL TIME.                                           
C    DT     - THE INITIAL TIME-STEP TO BE USED.                         
C             THE PERFORMANCE OF ODES IS SUBSTANTIALLY                  
C             INDEPENDENT OF THE VALUE OF DT CHOSEN BY THE USER.        
C             IT IS SUFFICIENT THAT THE USERS CHOICE FOR DT MERELY BE   
C             WITHIN SEVERAL ORDERS OF MAGNITUDE OF BEING CORRECT.      
C             THE VALUE OF DT WILL BE AUTOMATICALLY CHANGED BY ODES     
C             DURING THE INTEGRATION PROCESS IN SUCH A WAY AS TO GET    
C             THE SOLUTION, TO THE DESIRED ACCURACY, AT THE LEAST       
C             POSSIBLE COST.                                            
C    ERROR  - ERROR TOLERANCE ROUTINE.                                  
C                                                                       
C                 LOGICAL FUNCTION ERROR(X,NX,T,DT,ERRPAR,ERPUTS,E)     
C                                                                       
C             HAS AS INPUT                                              
C                                                                       
C               X,T    - X=X(T), THE APPROXIMATE SOLUTION FOR WHICH     
C                        AN ERROR CRITERION IS DESIRED.                 
C               NX     - THE LENGTH OF X.                               
C               DT     - THE TIME-STEP USED TO OBTAIN X(T).             
C               ERRPAR - TWO PARAMETERS, AS GIVEN TO ODES2.             
C               ERPUTS - THIS VARIABLE HAS THE SAME VALUE AS ERPUTS     
C                        IN THE CALL TO ODES2.                          
C               E      - THE REAL ABSOLUTE ERROR IN X(I) IS E(I),       
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C                                                                       
C             THE OUTPUT IS                                             
C                                                                       
C               ERRPAR - MAY BE ALTERED IF DESIRED.                     
C               E      - E(I) IS THE REAL TOLERABLE ABSOLUTE ERROR IN   
C                        X(I), I=1,...,NX. ALL E(I) MUST BE POSITIVE.   
C                                                                       
C             FUNCTION VALUE                                            
C                                                                       
C               ERROR  - ERROR=.TRUE. IF CONVERGED.                     
C                        ERROR=.FALSE. IF NOT.                          
C    ERRPAR - TWO PARAMETERS TO BE PASSED TO ERROR.                     
C    HANDLE - OUTPUT ROUTINE WITH A CALLING SEQUENCE OF THE FORM        
C                                                                       
C                     HANDLE(T0,X0,T1,X1,NX,DT,TSTOP,E)                 
C                                                                       
C             HANDLE WILL BE CALLED AT THE END OF EACH TIME-STEP.       
C                                                                       
C             THE INPUT TO HANDLE IS AS FOLLOWS                         
C                                                                       
C               X0,X1  - X0=X(T0) AND X1=X(T1).                         
C               T0,T1  - T0=T1 INDICATES A RESTART AND X1 IS FULL OF    
C                        GARBAGE.                                       
C               NX     - THE LENGTH OF THE SOLUTION VECTOR X.           
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE CURRENT FINAL TIME.                        
C               E      - E(I) GIVES THE REAL ABSOLUTE ERROR IN X1(I),   
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C                                                                       
C             THE OUTPUT FROM HANDLE MAY BE ANY OF                      
C                                                                       
C               X1     - MAY BE ALTERED IF DESIRED.                     
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE FINAL TIME VALUE.                          
C                                                                       
C             HANDLE SHOULD BE DECLARED EXTERNAL IN THE                 
C             SUBPROGRAM CALLING ODES.                                  
C    GLBMAX - IF (GLBMAX) THEN THE GLOBAL MAXIMUM ABSOLUTE VALUE OF     
C             THE SOLUTION IS TO BE RECORDED,                           
C             SEE COMMON /ODESM/ BELOW.                                 
C                                                                       
C             IF THE ERROR SUBPROGRAM SUPPLIED BY THE USER IS ODESE,    
C             THEN GLBMAX DETERMINES WHETHER OR NOT THE GLOBAL          
C             MAXIMUM ABSOLUTE VALUE OF THE SOLUTION WILL BE USED IN    
C             THAT SUBPROGRAM.                                          
C    ERPUTS - IF (ERPUTS) THEN THE ERROR PER UNIT TIME-STEP CRITERION   
C             IS TO BE USED IN THE ERROR ROUTINE.                       
C             OTHERWISE, THE ERROR PER TIME-STEP CRITERION IS TO BE     
C             USED IN THE ERROR ROUTINE.                                
C                                                                       
C             IF THE ERROR SUBPROGRAM SUPPLIED BY THE USER IS ODESE,    
C             THEN ERPUTS DETERMINES WHETHER OR NOT THE ERROR           
C             PER UNIT-TIME-STEP OR ERROR PER TIME-STEP ERROR           
C             CRITERION WILL BE USED BY THAT SUBPROGRAM.                
C    KMAX   - THE MAXIMUM NUMBER OF COLUMNS ALLOWED IN THE              
C             EXTRAPOLATION PROCESS.                                    
C    MMAX   - THE MAXIMUM NUMBER OF LEVELS OF EXTRAPOLATION PERMITTED.  
C             MMAX.GE.KMAX+2 IS REQUIRED.                               
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    X      - X=X(TSTOP), THE FINAL VALUE FOR THE SOLUTION.             
C    TSTOP  - MAY BE ALTERED BY USER SUPPLIED ROUTINE HANDLE.           
C    DT     - PROPOSED TIME-STEP FOR THE NEXT STEP, IF ANY.             
C    ERRPAR - MAY BE ALTERED BY USER SUPPLIED ROUTINE ERROR.            
C                                                                       
C  SCRATCH SPACE OF LENGTH                                              
C                                                                       
C                      S(ODES2) .LE.                                    
C                                                                       
C    2*MMAX + NX*(KMAX+2)                                               
C                                                                       
C  REAL WORDS +                                                         
C                                                                       
C    5*KMAX + 3*MMAX + 3 +                                              
C                                                                       
C    ( IF (GLBMAX) THEN 2*NX , OTHERWISE 0 ) +                          
C                                                                       
C    MAX ( 2*NX REAL + S(F) ,                                           
C                                                                       
C          NX*(KMAX+1) +                                                
C                                                                       
C          MAX( KMAX REAL + KMAX , S(ERROR) ) ,                         
C                                                                       
C          NX + S(HANDLE) )                                             
C                                                                       
C  INTEGER WORDS IS ALLOCATED.                                          
C                                                                       
C  ERROR STATES                                                         
C                                                                       
C     1 - NX.LT.1.                                                      
C     2 - DT HAS THE WRONG SIGN ON INPUT.                               
C     3 - DT=0 ON INPUT.                                                
C     4 - DT RETURNED BY HANDLE HAS THE WRONG SIGN.                     
C     5 - DT=0 WAS RETURNED BY HANDLE. (RECOVERABLE)                    
C     6 - THE ERROR DESIRED IN X(I) IS NOT POSITIVE. (RECOVERABLE)      
C     7 - DT=0. (RECOVERABLE)                                           
C     8 - CANNOT RAISE DT IN HANDLE WHEN .NOT.OKAY.                     
C     9 - KMAX.LT.1.                                                    
C    10 - KMAX.GT.MMAX-2.                                               
C                                                                       
C  INTERNAL NAMED COMMON USAGE -                                        
C                                                                       
C     ODESM HOLDS THE POINTER, IGMAX, TO THE REAL                       
C            VECTOR OF CURRENT MAXIMUM ABSOLUTE VALUES ATTAINED BY EACH 
C            COMPONENT OF THE SOLUTION, AND THE POINTER, IGMAXO, TO THE 
C            REAL VECTOR OF MAXIMUM ABSOLUTE VALUES ATTAINED BY EACH    
C            COMPONENT OF THE SOLUTION AS OF THE LAST TIME STEP.        
C            IGMAX=0 MEANS THESE VECTORS ARE NOT USED AND HAVE NOT BEEN 
C            ALLOCATED.                                                 
C     A0DESQ HOLDS THE REAL END-POINT VALUE AND THE REAL                
C            REMAINING ERROR FOR USE IN ODESQ.                          
C     A0DESP HOLDS THE POINTER TO THE REAL VECTOR                       
C            F(T0,X(T0)).                                               
C                                                                       
      COMMON /ODESF/OKAY                                                
      COMMON /ODESM/IGMAX,IGMAXO                                        
      COMMON /A0DESQ/TEND,RERROR                                        
      COMMON /A0DESP/IFTX0                                              
C                                                                       
      REAL X(NX),TSTART,TSTOP,DT,TEND                                   
      REAL ERRPAR(2),RERROR                                             
      LOGICAL GLBMAX,ERPUTS,OKAY                                        
      EXTERNAL F,ERROR,HANDLE                                           
C                                                                       
      REAL DELTA                                                        
      EXTERNAL A0DESG,A0DES0,A0DESE                                     
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      INTEGER IS(1000)                                                  
      EQUIVALENCE (DS(1),RS(1)),(DS(1),IS(1))                           
C                                                                       
      IF (TSTART.EQ.TSTOP) GO TO 20                                     
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C ... CHECK FOR ERRORS IN THE INPUT.                                    
C                                                                       
C/6S                                                                    
C     IF (NX.LT.1) CALL SETERR(16H ODES2 - NX.LT.1,16,1,2)              
C     IF ((DT/ABS(DT))*(TSTOP-TSTART).LT.0.0E0)                         
C    1   CALL SETERR(39H ODES2 - DT HAS THE WRONG SIGN ON INPUT,39,2,2) 
C     IF (TSTART+DT.EQ.TSTART)                                          
C    1   CALL SETERR(22H ODES2 - DT=0 ON INPUT,22,3,2)                  
C     IF (KMAX.LT.1) CALL SETERR(18H ODES2 - KMAX.LT.1,18,9,2)          
C     IF (KMAX.GT.MMAX-2)                                               
C    1   CALL SETERR(23H ODES2 - KMAX.GT.MMAX-2,23,10,2)                
C/7S                                                                    
      IF (NX.LT.1) CALL SETERR(' ODES2 - NX.LT.1',16,1,2)               
      IF ((DT/ABS(DT))*(TSTOP-TSTART).LT.0.0E0)                         
     1   CALL SETERR(' ODES2 - DT HAS THE WRONG SIGN ON INPUT',39,2,2)  
      IF (TSTART+DT.EQ.TSTART)                                          
     1   CALL SETERR(' ODES2 - DT=0 ON INPUT',22,3,2)                   
      IF (KMAX.LT.1) CALL SETERR(' ODES2 - KMAX.LT.1',18,9,2)           
      IF (KMAX.GT.MMAX-2)                                               
     1   CALL SETERR(' ODES2 - KMAX.GT.MMAX-2',23,10,2)                 
C/                                                                      
C                                                                       
C ... ALLOCATE AND LOAD N WITH THE SEQUENCE 1,2,3,4,6,8,12,... .        
C                                                                       
      IN=ISTKGT(MMAX,2)                                                 
C                                                                       
      DO 10 I=1,MMAX                                                    
         J=IN+I-1                                                       
         IF (I.LT.4) IS(J)=I                                            
 10      IF (I.GT.3) IS(J)=2*IS(J-2)                                    
C                                                                       
C ... DECIDE IF HAVE ERROR PER UNIT-TIME-STEP OR PER TIME-STEP.         
C                                                                       
      DELTA=0.0E0                                                       
      IF (ERPUTS) DELTA=1.0E0                                           
C                                                                       
C ... LOAD THE GLOBAL MAXIMUM ABSOLUTE VALUES OF THE SOLUTION,          
C ... IF NECESSARY.                                                     
C                                                                       
      IGMAX=0                                                           
      IF (GLBMAX) IGMAX=ISTKGT(2*NX,3)                                  
      IGMAXO=IGMAX+NX                                                   
      IF (GLBMAX) CALL A0DESL(X,NX,RS(IGMAX),RS(IGMAXO))                
C                                                                       
      TEND=TSTOP                                                        
      RERROR=ERRPAR(2)                                                  
C                                                                       
      IFTX0=ISTKGT(NX,3)                                                
C                                                                       
      CALL SXTRP(TSTART,TSTOP,A0DESG,F,1.0E0,2.0E0,DELTA,X,NX,DT,       
     1            IS(IN),KMAX,MMAX,.FALSE.,ERROR,A0DESE,ERRPAR,HANDLE,  
     2            A0DES0,0.95E0)                                        
C                                                                       
      IF (NERROR(NERR).NE.0) CALL ERROFF                                
C/6S                                                                    
C     IF (NERR.EQ.13) CALL SETERR                                       
C    1   (13H ODES2 - DT=0,13,7,1)                                      
C     IF (NERR.EQ.14) CALL SETERR                                       
C    1   (32H ODES2 - DT=0 RETURNED BY HANDLE,32,5,1)                   
C     IF (NERR.EQ.17) CALL SETERR                                       
C    1   (50H ODES2 - THE ERROR DESIRED IN X(I) IS NOT POSITIVE,50,6,1) 
C/7S                                                                    
      IF (NERR.EQ.13) CALL SETERR                                       
     1   (' ODES2 - DT=0',13,7,1)                                       
      IF (NERR.EQ.14) CALL SETERR                                       
     1   (' ODES2 - DT=0 RETURNED BY HANDLE',32,5,1)                    
      IF (NERR.EQ.17) CALL SETERR                                       
     1   (' ODES2 - THE ERROR DESIRED IN X(I) IS NOT POSITIVE',50,6,1)  
C/                                                                      
C                                                                       
      CALL LEAVE                                                        
C                                                                       
 20   RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION A0DESE(X,NX,T,DT,ERRPAR,DELTA,E,ERROR)           
C                                                                       
C  ERROR FILTER FOR KEEPING TRACK OF THE MAXIMUM ABSOLUTE VALUE OF THE  
C  SOLUTION, IF IT IS NECESSARY.                                        
C                                                                       
      COMMON /ODESM/IGMAX,IGMAXO                                        
C                                                                       
      REAL X(NX),T,DT,DELTA                                             
      REAL ERRPAR(2),E(NX)                                              
      LOGICAL ERROR                                                     
      EXTERNAL ERROR                                                    
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),RS(1))                                         
C                                                                       
C ... RAISE THE GLOBAL MAXIMUM, IF NECESSARY.                           
C                                                                       
      IF (IGMAX.GT.0) CALL A0DESU(X,NX,RS(IGMAX))                       
C                                                                       
      A0DESE=ERROR(X,NX,T,DT,ERRPAR,DELTA.EQ.1.0E0,E)                   
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE A0DES0(T0,X0,T1,X1,NX,DT,TSTOP,OK,HANDLE,E)            
C                                                                       
C  OUTPUT FILTER FOR USE WITH GLOBAL MAXIMUM ERROR OPTION IN ODES.      
C                                                                       
      COMMON /ODESM/IGMAX,IGMAXO                                        
C                                                                       
      REAL T0,X0(NX),T1,X1(NX),DT,TSTOP                                 
      REAL E(NX)                                                        
      LOGICAL OK                                                        
      EXTERNAL HANDLE                                                   
C                                                                       
      REAL TEMP                                                         
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),RS(1))                                         
C                                                                       
C ... RAISE THE GLOBAL MAXIMUM, IF NECESSARY.                           
C                                                                       
      IF (T0.NE.T1.AND.IGMAX.GT.0)                                      
     1   CALL MOVEFR(NX,RS(IGMAX),RS(IGMAXO))                           
C                                                                       
C ... OTHERWISE, RETURN IT TO ITS PREVIOUS VALUE.                       
C                                                                       
      IF (T0.EQ.T1.AND.IGMAX.GT.0)                                      
     1   CALL MOVEFR(NX,RS(IGMAXO),RS(IGMAX))                           
C                                                                       
      TEMP=DT                                                           
C                                                                       
      CALL HANDLE(T0,X0,T1,X1,NX,DT,TSTOP,E)                            
C                                                                       
C/6S                                                                    
C     IF ((DT/ABS(DT))*(TSTOP-T1).LT.0.0E0) CALL SETERR                 
C    1   (49H ODES2 - DT RETURNED BY HANDLE HAS THE WRONG SIGN,49,4,2)  
C/7S                                                                    
      IF ((DT/ABS(DT))*(TSTOP-T1).LT.0.0E0) CALL SETERR                 
     1   (' ODES2 - DT RETURNED BY HANDLE HAS THE WRONG SIGN',49,4,2)   
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (.NOT.OK .AND. ABS(TEMP).LT.ABS(DT)) CALL SETERR               
C    1   (49H ODES2 - CANNOT RAISE DT IN HANDLE WHEN .NOT.OKAY,49,8,2)  
C/7S                                                                    
      IF (.NOT.OK .AND. ABS(TEMP).LT.ABS(DT)) CALL SETERR               
     1   (' ODES2 - CANNOT RAISE DT IN HANDLE WHEN .NOT.OKAY',49,8,2)   
C/                                                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE A0DESL(X,NX,GLBMAX,GLBMXO)                             
C                                                                       
C  TO LOAD THE GLOBAL MAXIMUM.                                          
C                                                                       
      REAL X(NX)                                                        
      REAL GLBMAX(NX),GLBMXO(NX)                                        
C                                                                       
      DO 10 I=1,NX                                                      
         GLBMAX(I)=ABS((X(I)))                                          
 10      GLBMXO(I)=GLBMAX(I)                                            
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE A0DESG(T0,X0,T1,X1,NX,NT,F,OK)                         
C                                                                       
C  GRAGGS MODIFIED MID-POINT RULE FOR DY/DT = F(T,Y).                   
C                                                                       
C  INPUT                                                                
C                                                                       
C    T0     - INITIAL TIME.                                             
C    X0     - X0=X(T0).                                                 
C    T1     - FINAL TIME.                                               
C    NX     - THE LENGTH OF THE SOLUTION VECTOR.                        
C    NT     - THE NUMBER OF TIME STEPS TO BE USED IS 2*NT.              
C    F      - THE RIGHT-HAND SIDE.                                      
C             THE CALL F(T,X,NX,FTX) SHOULD SET FTX=F(T,X).             
C             IF IT CANNOT COMPUTE F(T,X), OKAY=.FALSE. SHOULD BE       
C             RETURNED IN COMMON /ODESF/OKAY .                          
C                                                                       
C  COMMON INPUT -                                                       
C                                                                       
C    IGMAX  - THE POINTER TO THE REAL VECTOR OF CURRENT MAXIMUM ABSOLUTE
C             VALUES ATTAINED BY EACH COMPONENT OF THE SOLUTION.        
C             IGMAX=0 MEANS THIS VECTOR IS NOT USED.                    
C    IFTX0  - THE POINTER TO THE REAL VECTOR F(T0,X0).                  
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    X1     - THE APPROXIMATE SOLUTION AT TIME T1.                      
C                                                                       
C  COMMON OUTPUT -                                                      
C                                                                       
C    OK     - OK=.TRUE., IF SUCCESSFUL. OTHERWISE, OK=.FALSE. .         
C    IGMAX  - THE UPDATED MAXIMUM VALUES.                               
C    IFTX0  - THE VECTOR FTX0 IS LOADED IF NT=1.                        
C                                                                       
C  SCRATCH SPACE ALLOCATED - 2*NX REAL WORDS.                           
C                                                                       
C  ERROR STATES - NONE.                                                 
C                                                                       
      COMMON /ODESM/IGMAX,IGMAXO                                        
      COMMON /A0DESP/IFTX0                                              
      COMMON /ODESF/OKAY                                                
C                                                                       
      REAL T0,X0(NX),T1,X1(NX)                                          
      LOGICAL OK,OKAY                                                   
      EXTERNAL F                                                        
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL WS(1)                                                        
      EQUIVALENCE (DS(1),WS(1))                                         
C                                                                       
C ... ALLOCATE SPACE FOR THE FUNCTION VALUES FTX AND SCRATCH VALUES XL. 
C                                                                       
      IFTX=ISTKGT(2*NX,3)                                               
      IXL=IFTX+NX                                                       
C                                                                       
      CALL A0DESR(T0,X0,T1,X1,NX,NT,F,WS(IFTX),WS(IXL),WS(IFTX0),IGMAX) 
C                                                                       
      OK=OKAY                                                           
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE A0DESR(T0,X0,T1,X1,NX,NT,F,FTX,XL,FTX0,IGMAX)          
C                                                                       
C  ACTUAL GRAGGS MODIFIED MID-POINT RULE.                               
C                                                                       
      COMMON /ODESF/OKAY                                                
C                                                                       
      REAL T0,X0(NX),T1,X1(NX),FTX(NX),XL(NX),FTX0(NX)                  
      LOGICAL OKAY                                                      
      EXTERNAL F                                                        
C                                                                       
      REAL DT,DT2,T,TEMP                                                
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),RS(1))                                         
C                                                                       
      OKAY=.TRUE.                                                       
      DT=(T1-T0)/FLOAT(NT)                                              
      DT2=0.5E0*DT                                                      
      NT2=2*NT                                                          
C                                                                       
C ... IF THIS IS THE FIRST CALL, COMPUTE FTX0=F(T0,X(T0))               
C                                                                       
      IF (NT.EQ.1) CALL F(T0,X0,NX,FTX0)                                
      IF (.NOT.OKAY) GO TO 50                                           
C                                                                       
C ... COMPUTE FORWARD-EULER INITIAL TIME-STEP.                          
C                                                                       
      DO 10 I=1,NX                                                      
         XL(I)=X0(I)                                                    
 10      X1(I)=X0(I)+DT2*FTX0(I)                                        
C                                                                       
C ... GET THE APPROXIMATE SOLUTION AT THE REST OF THE MESH-POINTS       
C ... USING THE MID-POINT RULE.                                         
C                                                                       
      DO 30 IT=2,NT2                                                    
         T=T0+FLOAT(IT-1)*DT2                                           
         CALL F(T,X1,NX,FTX)                                            
         IF (.NOT.OKAY) GO TO 50                                        
C                                                                       
         DO 20 I=1,NX                                                   
            TEMP=X1(I)                                                  
            X1(I)=XL(I)+DT*FTX(I)                                       
 20         XL(I)=TEMP                                                  
C                                                                       
C ...... IF NEED TO, RAISE THE GLOBAL MAXIMUM.                          
C                                                                       
 30      IF (IGMAX.GT.0) CALL A0DESU(X1,NX,RS(IGMAX))                   
C                                                                       
C ... SMOOTH AT THE END POINT.                                          
C                                                                       
      CALL F(T1,X1,NX,FTX)                                              
      IF (.NOT.OKAY) GO TO 50                                           
C                                                                       
      DO 40 I=1,NX                                                      
 40      X1(I)=0.5E0*(X1(I)+(XL(I)+DT2*FTX(I)))                         
C                                                                       
C ... IF NEED TO, RAISE THE GLOBAL MAXIMUM.                             
C                                                                       
      IF (IGMAX.GT.0) CALL A0DESU(X1,NX,RS(IGMAX))                      
C                                                                       
 50   RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE A0DESU(X,NX,GLBMAX)                                    
C                                                                       
C  TO RAISE THE GLOBAL MAXIMUM.                                         
C                                                                       
      REAL X(NX)                                                        
      REAL GLBMAX(NX)                                                   
C                                                                       
      DO 10 I=1,NX                                                      
 10      GLBMAX(I)=AMAX1(GLBMAX(I),ABS((X(I))))                         
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE SXTRP(TSTART,TSTOP,XA,F,BETA,GAMMA,DELTA,X,NX,DT,N,    
     1                  KMAX,MMAX,XPOLY,ERROR,SERROR,ERRPAR,OUTPUT,SOUT,
     2                  PESPAR)                                         
C                                                                       
C  LET A VECTOR VALUED FUNCTION A(H) OF LENGTH NX PRODUCE AN            
C  APPROXIMATION TO X(T1) WHEN GIVEN T0, X(T0) AND H=(T1-T0)/N WHERE    
C  N IS AN INTEGER AND X(T) IS SOME UNKNOWN VECTOR-VALUED FUNCTION      
C  OF TIME.                                                             
C                                                                       
C  ASSUME THAT                                                          
C                                                                       
C  A(H) = X(T1) +                                                       
C                                                                       
C         ABS(T1-T0)**BETA * SUM(J=1,...,INFINITY)(C(J)*H**(J*GAMMA))   
C                                                                       
C  WHERE THE C(J) ARE UNKNOWN VECTORS INDEPENDENT OF H.                 
C                                                                       
C  THIS ROUTINE THEN TAKES THE VALUE X=X(TSTART) AND, USING AN INITIAL  
C  VALUE OF T1=TSTART+DT, SEQUENTIALLY EVALUATES X(T1) UNTIL T1=TSTOP.  
C                                                                       
C  THE EVALUATION OF X(T1) IS ACCOMPLISHED USING EXTRAPOLATION TO       
C  THE LIMIT OF THE RESULTS OF A(H) FOR H=DT/N(M), M=1,...,MMAX.        
C                                                                       
C  INPUT                                                                
C                                                                       
C    TSTART - THE INITIAL VALUE FOR TIME.                               
C    TSTOP  - THE FINAL VALUE FOR TIME.                                 
C    XA     - CALL XA(T0,X0,T1,X1,NX,N,F,OK) SHOULD RETURN THE          
C             APPROXIMATION X1=A(H) TO X(T1) GIVEN T0, X0=X(T0) AND N.  
C             OK=.TRUE. SHOULD BE RETURNED IF X1 HAS BEEN SUCCESSFULLY  
C             COMPUTED. OTHERWISE, OK=.FALSE. SHOULD BE RETURNED.       
C             THIS WILL CAUSE A RESTART OF THE PROCESS FROM TIME T=T0,  
C             WITH A DEFAULT LOWERING OF DT BY 10**3.                   
C             F IS A SUBPROGRAM NAME, AS PASSED TO SXTRP.               
C    F      - A SUBPROGRAM NAME WHICH IS PASSED TO XA.                  
C    BETA   - THE POWER SERIES FOR THE ERROR IN A(H) HAS A              
C             MULTIPLICATIVE FACTOR OF ABS(T1-T0)**BETA IN FRONT OF IT. 
C    GAMMA  - THE POWER SERIES FOR THE ERROR IN A(H) IS IN THE          
C             VARIABLE H**GAMMA.                                        
C    DELTA  - THE ERROR CRITERION IS PROPORTIONAL TO                    
C             ABS(T1-T0)**DELTA.                                        
C    X      - THE INITIAL VALUES X=X(TSTART).                           
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    DT     - THE INITIAL TIME-STEP TO BE USED.                         
C             THE PERFORMANCE OF SXTRP IS SUBSTANTIALLY                 
C             INDEPENDENT OF THE VALUE OF DT CHOSEN BY THE USER.        
C             IT IS SUFFICIENT THAT THE USERS CHOICE FOR DT MERELY BE   
C             WITHIN SEVERAL ORDERS OF MAGNITUDE OF BEING CORRECT.      
C             THE VALUE OF DT WILL BE AUTOMATICALLY CHANGED BY SXTRP    
C             DURING THE INTEGRATION PROCESS IN SUCH A WAY AS TO GET    
C             THE SOLUTION, TO THE DESIRED ACCURACY, AT THE LEAST       
C             POSSIBLE COST.                                            
C    N      - H=(T1-T0)/N(M) WILL BE USED AT THE M-TH LEVEL OF          
C             EXTRAPOLATION, M=1,...,MMAX.                              
C    KMAX   - THE MAXIMAL NUMBER OF COLUMNS KEPT IN THE EXTRAPOLATION   
C             PROCESS.                                                  
C    MMAX   - THE MAXIMUM LEVEL OF EXTRAPOLATION TO BE USED.            
C             MMAX.GE.KMAX+2 IS REQUIRED.                               
C    XPOLY  - IF (XPOLY) THEN USE POLYNOMIAL EXTRAPOLATION.             
C             IF (.NOT.XPOLY) THEN USE RATIONAL EXTRAPOLATION.          
C    ERROR  - A SUBPROGRAM NAME WHICH IS PASSED TO SERROR.              
C    SERROR - A LOGICAL FUNCTION OF THE FORM                            
C                                                                       
C              LOGICAL FUNCTION SERROR(X1,NX,T1,DT,ERRPAR,DELTA,E,ERROR)
C                                                                       
C             THE INPUT TO SERROR IS                                    
C                                                                       
C               X1     - X1=X(T1), THE APPROXIMATE SOLUTION FOR WHICH   
C                        AN ERROR CRITERION IS DESIRED.                 
C               NX     - THE LENGTH OF THE SOLUTION VECTOR.             
C               T1     - THE CURRENT VALUE OF TIME, X1=X(T1).           
C               DT     - DT=T1-T0.                                      
C               ERRPAR - TWO PARAMETERS, AS PASSED TO SXTRP.            
C               DELTA  - AS PASSED TO SXTRP.                            
C               E      - E(I) IS THE REAL ABSOLUTE ERROR IN X1(I),      
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C               ERROR  - THE NAME OF A SUBPROGRAM, AS PASSED TO SXTRP.  
C                                                                       
C             THE OUTPUT FROM SERROR IS                                 
C                                                                       
C               E      - E(I) GIVES THE DESIRED REAL ABSOLUTE ERROR     
C                        IN THE I-TH COMPONENT OF X1=X(T1), I=1,...,NX. 
C               ERRPAR - MAY BE ALTERED IF DESIRED.                     
C                                                                       
C             FUNCTION VALUE -                                          
C                                                                       
C               SERROR - SERROR.TRUE. IF CONVERGED.                     
C                        SERROR=.FALSE. IF NOT.                         
C                                                                       
C    ERRPAR - A VECTOR OF LENGTH TWO TO BE PASSED TO ERROR.             
C    OUTPUT - A SUBPROGRAM NAME TO BE PASSED TO SOUT.                   
C    SOUT   - THE OUTPUT SUBROUTINE                                     
C                                                                       
C                 SOUT(T0,X0,T1,X1,NX,DT,TSTOP,OK,OUTPUT,E)             
C                                                                       
C             WILL BE CALLED AT THE END OF EACH TIME STEP.              
C                                                                       
C             THE INPUT TO SOUT IS                                      
C                                                                       
C               T0     - THE OLD VALUE OF T1.                           
C               X0     - X0=X(T0)                                       
C               T1     - CURRENT VALUE OF TIME.                         
C               X1     - X1=X(T1).                                      
C               NX     - THE LENGTH OF THE SOLUTION VECTOR.             
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE CURRENT VALUE OF THE FINAL-TIME.           
C               OK     - AS RETURNED BY XA.                             
C               OUTPUT - A SUBPROGRAM NAME, AS PASSED TO SXTRP.         
C               E      - THE REAL ABSOLUTE ERROR IN X1(I)=X(T1)(I)      
C                        IS E(I), I=1,...,NX, FOR THE SINGLE TIME-STEP  
C                        FROM T0 TO T1.                                 
C                                                                       
C             THE OUTPUT FROM SOUT MAY BE ANY OF                        
C                                                                       
C               X1     - X1=X(T1).                                      
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE FINAL-TIME VALUE.                          
C                                                                       
C    PESPAR - THE OPTIMAL TIME-STEP DT IS MULTIPLIED BY PESPAR          
C             BEFORE BEING USED FOR THE NEXT STEP.                      
C             0.LT.PESPAR.LE.1 IS REQUIRED.                             
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    X      - X=X(TSTOP), THE FINAL VALUE FOR THE SOLUTION.             
C    DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP, IF ANY.         
C    TSTOP  - MAY BE ALTERED BY USER SUPPLIED ROUTINE SOUT.             
C    ERRPAR - MAY BE ALTERED BY USER SUPPLIED ROUTINE ERROR.            
C                                                                       
C  SCRATCH SPACE OF LENGTH                                              
C                                                                       
C                   S(SXTRP) .LE.                                       
C                                                                       
C    2*MMAX + NX*(KMAX+1)                                               
C                                                                       
C  REAL WORDS +                                                         
C                                                                       
C    5*KMAX + 2*MMAX + 3 +                                              
C                                                                       
C    MAX( S(XA), NX*(KMAX+1) +                                          
C                                                                       
C         MAX( KMAX REAL + KMAX , S(ERROR) ) ,                          
C                                                                       
C         NX + S(SOUT) )                                                
C                                                                       
C  INTEGER WORDS IS ALLOCATED.                                          
C                                                                       
C  ERROR STATES                                                         
C                                                                       
C     1 - BETA.LT.0.                                                    
C     2 - GAMMA.LE.0.                                                   
C     3 - DELTA.LT.0.                                                   
C     4 - NX.LT.1.                                                      
C     5 - DT=0 ON INPUT.                                                
C     6 - N(1).LT.1.                                                    
C     7 - KMAX.LT.1.                                                    
C     8 - MMAX.LT.KMAX+2.                                               
C     9 - PESPAR NOT IN (0,1).                                          
C    10 - BETA-DELTA+GAMMA.LE.0.                                        
C    11 - N IS NOT MONOTONE INCREASING.                                 
C    12 - DT HAS THE WRONG SIGN.                                        
C    13 - DT=0. (RECOVERABLE)                                           
C    14 - DT=0 RETURNED BY SOUT. (RECOVERABLE)                          
C    15 - DT RETURNED BY SOUT HAS THE WRONG SIGN.                       
C    16 - DT RAISED BY SOUT WHEN OK=.FALSE..                            
C    17 - E(I).LE.0 RETURNED BY ERROR. (RECOVERABLE)                    
C    18 - SOMEBODY IS LEAVING STUFF ON THE STACK.                       
C                                                                       
C  WHILE SXTRP IS EXECUTING, COMMON /A9XTRP/ CONTAINS THE FOLLOWING     
C  INFORMATION -                                                        
C                                                                       
C    MC     - THE CURRENT LEVEL OF EXTRAPOLATION.                       
C    KOPTC  - THE OPTIMAL NUMBER OF COLUMNS IN THE LOZENGE.             
C             IF KOPTC IS ZERO, THEN THE NEXT THREE ITEMS ARE           
C             MEANINGLESS.                                              
C    ICOST  - THE POINTER TO THE REAL COST/UNIT TIME-STEP ARRAY.        
C    KHIC   - THE ACTIVE LENGTH OF THE COST ARRAY.                      
C    IHOPT  - THE POINTER TO THE REAL ARRAY OF OPTIMAL STEP-SIZES       
C             FOR A GIVEN NUMBER OF COLUMNS, ITS LENGTH IS KHIC.        
C    IRCNT  - IRCNT LOGARITHMIC BISECTION STEPS ARE TO BE DONE.         
C    HUP    - DT CANNOT GROW BY MORE THAN EXP(HUP) PER STEP.            
C             HUP WILL BE MULTIPLIED BY 2 AFTER EACH SUCCESSFUL         
C             TIME STEP. THIS VALUE IS TYPE REAL.                       
C    ILOZNG - THE POINTER TO THE LOWER EDGE OF THE REAL                 
C             EXTRAPOLATION LOZENGE.                                    
C    KMAXC  - THE LENGTH OF THE BOTTOM EDGE OF THE LOZENGE IS           
C             MIN(KMAXC,MC).                                            
C                                                                       
      COMMON /A9XTRP/MC,KOPTC,ICOST,KHIC,IHOPT,IRCNT,HUP,ILOZNG,KMAXC   
C                                                                       
      REAL TSTART,TSTOP,BETA,GAMMA,DELTA,X(NX),DT                       
      REAL ERRPAR(2),PESPAR                                             
      INTEGER N(MMAX)                                                   
      LOGICAL XPOLY,ERROR,SERROR                                        
      EXTERNAL XA,F,ERROR,SERROR,OUTPUT,SOUT                            
C                                                                       
      REAL HUP                                                          
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL WS(1)                                                        
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),WS(1)),(DS(1),RS(1))                           
C                                                                       
C                                                                       
      IF (TSTART.EQ.TSTOP) GO TO 50                                     
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (BETA.LT.0.0E0) CALL SETERR(18H SXTRP - BETA.LT.0,18,1,2)      
C     IF (GAMMA.LE.0.0E0) CALL SETERR(19H SXTRP - GAMMA.LE.0,19,2,2)    
C     IF (DELTA.LT.0.0E0) CALL SETERR(19H SXTRP - DELTA.LT.0,19,3,2)    
C     IF (NX.LT.1) CALL SETERR(16H SXTRP - NX.LT.1,16,4,2)              
C     IF (DT.EQ.0.0E0) CALL SETERR(22H SXTRP - DT=0 ON INPUT,22,5,2)    
C     IF (N(1).LT.1) CALL SETERR(18H SXTRP - N(1).LT.1,18,6,2)          
C     IF (KMAX.LT.1) CALL SETERR(18H SXTRP - KMAX.LT.1,18,7,2)          
C     IF (MMAX.LT.KMAX+2) CALL SETERR(23H SXTRP - MMAX.LT.KMAX+2,23,8,2)
C     IF (PESPAR.LE.0.0E0.OR.PESPAR.GT.1.0E0) CALL SETERR               
C    1   (28H SXTRP - PESPAR NOT IN (0,1),28,9,2)                       
C     IF (BETA-DELTA+GAMMA.LE.0.0E0) CALL SETERR                        
C    1   (30H SXTRP - BETA-DELTA+GAMMA.LE.0,30,10,2)                    
C/7S                                                                    
      IF (BETA.LT.0.0E0) CALL SETERR(' SXTRP - BETA.LT.0',18,1,2)       
      IF (GAMMA.LE.0.0E0) CALL SETERR(' SXTRP - GAMMA.LE.0',19,2,2)     
      IF (DELTA.LT.0.0E0) CALL SETERR(' SXTRP - DELTA.LT.0',19,3,2)     
      IF (NX.LT.1) CALL SETERR(' SXTRP - NX.LT.1',16,4,2)               
      IF (DT.EQ.0.0E0) CALL SETERR(' SXTRP - DT=0 ON INPUT',22,5,2)     
      IF (N(1).LT.1) CALL SETERR(' SXTRP - N(1).LT.1',18,6,2)           
      IF (KMAX.LT.1) CALL SETERR(' SXTRP - KMAX.LT.1',18,7,2)           
      IF (MMAX.LT.KMAX+2) CALL SETERR(' SXTRP - MMAX.LT.KMAX+2',23,8,2) 
      IF (PESPAR.LE.0.0E0.OR.PESPAR.GT.1.0E0) CALL SETERR               
     1   (' SXTRP - PESPAR NOT IN (0,1)',28,9,2)                        
      IF (BETA-DELTA+GAMMA.LE.0.0E0) CALL SETERR                        
     1   (' SXTRP - BETA-DELTA+GAMMA.LE.0',30,10,2)                     
C/                                                                      
C                                                                       
C ... ALLOCATE AND LOAD THE ARRAY LOGN WITH LOG(N(I)).                  
C                                                                       
      ILOGN=ISTKGT(MMAX,3)                                              
      WS(ILOGN)=ALOG(FLOAT(N(1)))                                       
      I=ILOGN+1                                                         
      DO 10 J=2,MMAX                                                    
C/6S                                                                    
C        IF (N(J-1).GE.N(J)) CALL SETERR                                
C    1      (37H SXTRP - N IS NOT MONOTONE INCREASING,37,11,2)          
C/7S                                                                    
         IF (N(J-1).GE.N(J)) CALL SETERR                                
     1      (' SXTRP - N IS NOT MONOTONE INCREASING',37,11,2)           
C/                                                                      
         WS(I)=ALOG(FLOAT(N(J)))                                        
 10      I=I+1                                                          
C                                                                       
C ... ALLOCATE CURRENT AND OLD OPTIMAL STEP-SIZE ARRAYS.                
C                                                                       
      IHOPT=ISTKGT(KMAX+1,3)                                            
      IHOPTO=ISTKGT(KMAX+1,3)                                           
C                                                                       
C ... ALLOCATE AND LOAD THE ARRAY NG WITH N(J)**GAMMA.                  
C                                                                       
      ING=ISTKGT(MMAX,3)                                                
      I=ING                                                             
      DO 20 J=1,MMAX                                                    
         WS(I)=FLOAT(N(J))**GAMMA                                       
 20      I=I+1                                                          
C                                                                       
C ... ALLOCATE SPACE FOR X1 (THE SOLUTION AT TIME T1),                  
C ... AND A SCRATCH ARRAY F.                                            
C                                                                       
      IX1=ISTKGT(NX,3)                                                  
      KMAXC=KMAX                                                        
      IF=ISTKGT(KMAX,3)                                                 
C                                                                       
C ... ALLOCATE AND LOAD POW(J) WITH 1/(BETA-DELTA+J*GAMMA).             
C                                                                       
      IPOW=ISTKGT(KMAX,3)                                               
      I=IPOW                                                            
      DO 30 J=1,KMAX                                                    
         RS(I)=1.0E0/(BETA-DELTA+FLOAT(J)*GAMMA)                        
 30      I=I+1                                                          
C                                                                       
C ... ALLOCATE AND LOAD ARRAYS WORK AND LWORK WITH                      
C ... SUM(I=1,...,J)(N(I)) AND LOG(WORK(J)), RESPECTIVELY.              
C                                                                       
      IWORK=ISTKGT(MMAX,3)                                              
      ILWORK=ISTKGT(MMAX,3)                                             
      IW=IWORK                                                          
      ILW=ILWORK                                                        
      RS(IW)=FLOAT(N(1))                                                
      RS(ILW)=ALOG(RS(IW))                                              
      DO 40 J=2,MMAX                                                    
         IW=IW+1                                                        
         ILW=ILW+1                                                      
         RS(IW)=RS(IW-1)+FLOAT(N(J))                                    
 40      RS(ILW)=ALOG(RS(IW))                                           
C                                                                       
C ... ALLOCATE THE COST/UNIT TIME-STEP ARRAY.                           
C                                                                       
      ICOST=ISTKGT(KMAX+1,3)                                            
C                                                                       
C ... ALLOCATE THE EXTRAPOLATION LOZENGE SO THAT ISTKMD CAN             
C ... BE USED TO LET IT GROW ONLY AS NEEDED.                            
C                                                                       
      ILOZNG=ISTKGT(1,3)                                                
C                                                                       
      CALL A8XTRP(TSTART,TSTOP,XA,F,BETA,GAMMA,DELTA,NX,DT,N,KMAX,MMAX, 
     1            XPOLY,ERROR,SERROR,ERRPAR,OUTPUT,SOUT,PESPAR,         
     2            WS(ILOGN),RS(IHOPT),RS(IHOPTO),WS(ING),X,WS(IX1),     
     3            WS(ILOZNG),RS(IF),                                    
     4            RS(IPOW),RS(IWORK),RS(ILWORK),RS(ICOST))              
C                                                                       
      CALL LEAVE                                                        
C                                                                       
 50   RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE A8XTRP(TSTART,TSTOP,XA,FCN,BETA,GAMMA,DELTA,NX,DT,N,   
     1                  KMAX,MMAX,XPOLY,ERROR,SERROR,ERRPAR,OUTPUT,SOUT,
     2                  PESPAR,LOGN,HOPT,HOPTO,NG,X0,X1,LOZNGE,F,       
     3                  POW,WORK,LNWORK,COST)                           
C                                                                       
      COMMON /A9XTRP/MC,KOPTC,ICOST,KHIC,IHOPT,IRCNT,HUP,ILOZNG,KMAXC   
C                                                                       
      REAL TSTART,TSTOP,BETA,GAMMA,DELTA,DT,LOGN(MMAX),                 
     1                 NG(MMAX),X0(NX),X1(NX),LOZNGE(NX,KMAX)           
      REAL ERRPAR(2),PESPAR,HOPT(1),HOPTO(1),                           
     1     F(KMAX),POW(KMAX),WORK(MMAX),LNWORK(MMAX),COST(1)            
C     REAL HOPT(KMAX+1),HOPTO(KMAX+1),COST(KMAX+1)                      
      INTEGER N(MMAX)                                                   
      LOGICAL XPOLY,ERROR,SERROR                                        
      EXTERNAL XA,FCN,ERROR,SERROR,OUTPUT,SOUT                          
C                                                                       
      REAL CLOSE,DTSAVE,TEMP,TBAD,T0,T1,DTNEW,R1MACH                    
      REAL LOGLO,LOGHI,HUP0,HUPMAX,HUP,RTEMP                            
      LOGICAL A8XTRD,LOGGED,FAILED,PRSTRT,A8XTRL,EEQ0,OK                
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),RS(1))                                         
C                                                                       
C  IF T0 AND T1 ARE ON THE SAME SIDE OF TSTOP AND T1 IS WITHIN          
C  CLOSE*ABS(DT) OF TSTOP, THEN T1=TSTOP IS CHOSEN.                     
C                                                                       
C  MRCNT LOGARITHMIC BISECTION STEPS ARE TAKEN AFTER EACH RESTART.      
C                                                                       
C  ALLOW HOPT(NEW)/HOPT(OLD) TO RISE BY AT MOST EXP(HUP) WHERE          
C  HUP=HUP0 AT THE END OF A RESTART SEQUENCE AND EACH SUCCESSFUL STEP   
C  THEREAFTER RESULTS IN HUP=MIN(2*HUP,HUPMAX).                         
C                                                                       
      DATA CLOSE/1.0E-2/,MRCNT/3/,HUP0/0.1E0/,HUPMAX/7.0E0/             
C                                                                       
C ... GET THE LOGARITHMS OF THE LARGEST AND SMALLEST NUMBERS IN THE     
C ... MACHINE.                                                          
C                                                                       
      LOGLO=ALOG(R1MACH(1))                                             
      LOGHI=ALOG(R1MACH(2))                                             
C                                                                       
C/6S                                                                    
C     IF ((DT/ABS(DT))*(TSTOP-TSTART).LE.0.0E0) CALL SETERR             
C    1   (30H SXTRP - DT HAS THE WRONG SIGN,30,12,2)                    
C/7S                                                                    
      IF ((DT/ABS(DT))*(TSTOP-TSTART).LE.0.0E0) CALL SETERR             
     1   (' SXTRP - DT HAS THE WRONG SIGN',30,12,2)                     
C/                                                                      
C                                                                       
C ... INITIALIZE.                                                       
C                                                                       
      MUSED=0                                                           
      KOPTC=0                                                           
      IRCNT=0                                                           
      KOPTO=1                                                           
      T1=TSTART                                                         
      HUP=HUPMAX                                                        
      PRSTRT=.FALSE.                                                    
C                                                                       
C ... TAKE A TIME-STEP.                                                 
C                                                                       
 10      T0=T1                                                          
         T1=T0+DT                                                       
C                                                                       
         IF ((DT/ABS(DT))*(T1-TSTOP).GE.0.0E0) T1=TSTOP                 
         IF (ABS(T1-TSTOP).LE.CLOSE*ABS(DT)) T1=TSTOP                   
C                                                                       
         DT=T1-T0                                                       
C/6S                                                                    
C        IF (T0+DT.EQ.T0) CALL SETERR                                   
C    1      (13H SXTRP - DT=0,13,13,1)                                  
C/7S                                                                    
         IF (T0+DT.EQ.T0) CALL SETERR                                   
     1      (' SXTRP - DT=0',13,13,1)                                   
C/                                                                      
         IF (T0+DT.EQ.T0) GO TO 140                                     
C                                                                       
         DO 40 MT=1,MMAX                                                
C                                                                       
            FAILED=.FALSE.                                              
            LOGGED=.FALSE.                                              
            M=MT                                                        
            MC=M                                                        
C                                                                       
C ......... GET X1=T(DT/N(M)).                                          
C                                                                       
            CALL XA(T0,X0,T1,X1,NX,N(M),FCN,OK)                         
C                                                                       
            IF (.NOT.OK) GO TO 130                                      
C                                                                       
C ......... EXTRAPOLATE THE RESULTS.                                    
C                                                                       
            ITEMP=ILOZNG                                                
            IF (MIN0(M,KMAX).GT.MUSED) ITEMP=ISTKMD(NX*MIN0(M,KMAX))    
C/6S                                                                    
C           IF (ILOZNG.NE.ITEMP) CALL SETERR                            
C    1         (47H SXTRP - SOMEBODY IS LEAVING STUFF ON THE STACK,47,  
C    2          18,2)                                                   
C/7S                                                                    
            IF (ILOZNG.NE.ITEMP) CALL SETERR                            
     1         (' SXTRP - SOMEBODY IS LEAVING STUFF ON THE STACK',47,   
     2          18,2)                                                   
C/                                                                      
            MUSED=MAX0(M,MUSED)                                         
C                                                                       
            IE=ISTKGT(NX,3)                                             
            IE2=ISTKGT(MAX0(1,NX*MIN0(M-1,KMAX)),3)                     
            CALL XTRAP(X1,M,NX,NG,KMAX,XPOLY,LOZNGE,RS(IE2),RS(IE))     
C                                                                       
C ......... CHECK FOR CONVERGENCE.                                      
C                                                                       
            IF (M.EQ.1) GO TO 40                                        
C                                                                       
            IF (SERROR(X1,NX,T1,DT,ERRPAR,DELTA,RS(IE),ERROR)) GO TO 60 
C                                                                       
            IF (M.LE.KOPTO) GO TO 40                                    
C                                                                       
C ......... SEE IF A RE-START IS NECESSARY.                             
C                                                                       
            EEQ0=A8XTRL(RS(IE2),RS(IE),NX,M,KMAX,POW,LOGLO)             
            LOGGED=.TRUE.                                               
C                                                                       
            IF (EEQ0) CALL ISTKRL(2)                                    
            IF (EEQ0) GO TO 140                                         
C                                                                       
C ......... IF WILL NOT CONVERGE IN THIS LOZENGE, RESTART.              
C                                                                       
            FAILED=A8XTRD(RS(IE2),NX,M,MMAX,KMAX,LOGN,BETA,GAMMA,DELTA, 
     1                    F,POW,L,LDONE,KOPTO)                          
C                                                                       
            IF (FAILED) GO TO 50                                        
C                                                                       
C ......... SEE IF A RE-START WOULD BE MORE EFFICIENT.                  
C                                                                       
            IF (M.EQ.KOPTO+1) GO TO 40                                  
C                                                                       
            CALL A8XTRO(RS(IE2),NX,M,MMAX,KMAX,LOGN,GAMMA,F,POW,KOPT,   
     1                  DT,HOPT,LNWORK,COST,LOGLO,LOGHI)                
C                                                                       
            RTEMP=(DT/HOPT(KOPT+1))                                     
            IF (RTEMP.GT.1.0E+3     .AND.                               
     1          WORK(M)+WORK(KOPT+2)*RTEMP.GT.WORK(L)) GO TO 40         
C                                                                       
            IF (RTEMP.GT.1.0E+3) GO TO 30                               
C                                                                       
            IRTEMP=IFIX(RTEMP)                                          
            RTEMP=(RTEMP-FLOAT(IRTEMP))*HOPT(KOPT+1)                    
            KF=KOPT+1                                                   
C                                                                       
            DO 20 I=1,KOPT                                              
 20            IF (ABS(RTEMP).LE.ABS(HOPT(I)).AND.KF.GT.KOPT) KF=I      
C                                                                       
            IF (RTEMP*(DT/ABS(DT)).GT.0.0E0     .AND.                   
     1          WORK(M)+WORK(KOPT+2)*FLOAT(IRTEMP)+WORK(KF+1).GT.       
     2                       WORK(L)     ) GO TO 40                     
            IF (RTEMP*(DT/ABS(DT)).LE.0.0E0     .AND.                   
     1          WORK(M)+WORK(KOPT+2)*FLOAT(IRTEMP).GT.                  
     2                       WORK(L)     ) GO TO 40                     
C                                                                       
C ......... SIGNAL A RESTART.                                           
C                                                                       
 30         T1=T0                                                       
            GO TO 70                                                    
C                                                                       
 40         IF (M.LT.MMAX) CALL ISTKRL(2)                               
C                                                                       
C ...... DID NOT CONVERGE, TRY IT OVER AGAIN.                           
C                                                                       
 50      T1=T0                                                          
C                                                                       
C ...... FIND THE OPTIMAL DT AND M FOR THE NEXT TIME-STEP.              
C                                                                       
 60      IF (.NOT.LOGGED)                                               
     1      EEQ0=A8XTRL(RS(IE2),RS(IE),NX,M,KMAX,POW,LOGLO)             
C                                                                       
         IF (EEQ0) CALL ISTKRL(2)                                       
         IF (EEQ0) GO TO 140                                            
C                                                                       
         CALL A8XTRO(RS(IE2),NX,M,MMAX,KMAX,LOGN,GAMMA,F,POW,KOPT,      
     1               DT,HOPT,LNWORK,COST,LOGLO,LOGHI)                   
C                                                                       
 70      CALL ISTKRL(1)                                                 
C                                                                       
C ...... IF HAVE A RESTART, SAVE THE BAD VALUE OF T AND                 
C ...... SET IRCNT=MRCNT. IF STEP WAS SUCCESSFUL, SAVE DT.              
C                                                                       
         IF (T0.EQ.T1) TBAD=T0+DT                                       
         IF (T0.EQ.T1.AND.T0.NE.TSTART) IRCNT=MRCNT                     
         IF (T0.NE.T1) DTSAVE=DT                                        
C                                                                       
C ...... GET THE DT FOR THE NEXT TIME-STEP.                             
C                                                                       
         KOPT=MIN0(KOPT,KOPTO+1)                                        
         KOPTM=KOPT                                                     
         IF (T0.EQ.T1) KOPTM=MIN0(KOPT,KOPTO)                           
         DTNEW=(DT/ABS(DT))*PESPAR*                                     
     1          AMIN1(ABS((DT))*EXP(HUP),ABS(HOPT(KOPTM+1)))            
C                                                                       
C ...... TWO RESTARTS IN A ROW CAUSE DT TO DECREASE BY AT LEAST A       
C ...... FACTOR OF 10**3.                                               
C                                                                       
         IF (PRSTRT .AND. T0.EQ.T1)                                     
     1      DTNEW=(DT/ABS(DT))*AMIN1(ABS(DTNEW),ABS(DT)*1.0E-3)         
         DT=DTNEW                                                       
C                                                                       
         IF (T0.EQ.T1) PRSTRT=.TRUE.                                    
C                                                                       
C/6S                                                                    
C        IF (T1+DT.EQ.T1) CALL SETERR                                   
C    1      (13H SXTRP - DT=0,13,13,1)                                  
C/7S                                                                    
         IF (T1+DT.EQ.T1) CALL SETERR                                   
     1      (' SXTRP - DT=0',13,13,1)                                   
C/                                                                      
         IF (T1+DT.EQ.T1) CALL ISTKRL(1)                                
         IF (T1+DT.EQ.T1) GO TO 140                                     
C                                                                       
         KHI=MIN0(M,KMAX+1)                                             
C                                                                       
C ...... SET THE INFORMATION IN COMMON AND COMPUTE THE                  
C ...... COST/UNIT TIME-STEP FOR EACH LOZENGE SIZE.                     
C                                                                       
         KOPTC=KOPT                                                     
         KHIC=KHI                                                       
C                                                                       
         DO 80 K=1,KHI                                                  
 80         COST(K)=WORK(K+1)/ABS(HOPT(K))                              
C                                                                       
         IF (T0.EQ.TSTART.OR.T0.EQ.T1) GO TO 90                         
C                                                                       
C ...... IF ABS(HOPT(NEW)).LT.ABS(HOPT(OLD)), BE CONSERVATIVE.          
C                                                                       
         IDX=MIN0(KOPT+1,MOLD-1,M-1)                                    
         IF (ABS(DT).GT.1.0E-2*ABS(HOPT(IDX)))                          
     1   DT=DT*AMIN1(AMAX1(ABS(HOPT(IDX)/HOPTO(IDX)),1.0E-2),1.0E0)     
C/6S                                                                    
C        IF (T1+DT.EQ.T1) CALL SETERR                                   
C    1      (13H SXTRP - DT=0,13,13,1)                                  
C/7S                                                                    
         IF (T1+DT.EQ.T1) CALL SETERR                                   
     1      (' SXTRP - DT=0',13,13,1)                                   
C/                                                                      
         IF (T1+DT.EQ.T1) CALL ISTKRL(1)                                
         IF (T1+DT.EQ.T1) GO TO 140                                     
C                                                                       
         IF (IRCNT.LE.0) GO TO 90                                       
C                                                                       
C ...... LOGARITHMIC BISECTION FOR MRCNT STEPS AFTER A RESTART.         
C                                                                       
         IRCNT=IRCNT-1                                                  
         IF (IRCNT.EQ.0) HUP=0.5E0*HUP0                                 
         TEMP=(TBAD-T1)/DTSAVE                                          
         IF (TEMP.GT.0.99E0)                                            
     1       DT=(DT/ABS(DT))*AMIN1(ABS(TBAD-T1)*0.5E0,ABS(DT),          
     2                              SQRT(TEMP)*ABS(DTSAVE))             
C/6S                                                                    
C        IF (T1+DT.EQ.T1) CALL SETERR                                   
C    1      (13H SXTRP - DT=0,13,13,1)                                  
C/7S                                                                    
         IF (T1+DT.EQ.T1) CALL SETERR                                   
     1      (' SXTRP - DT=0',13,13,1)                                   
C/                                                                      
         IF (T1+DT.EQ.T1) CALL ISTKRL(1)                                
         IF (T1+DT.EQ.T1) GO TO 140                                     
C                                                                       
C ...... OUTPUT THE RESULTS FOR THIS TIME-STEP.                         
C                                                                       
 90      CALL SOUT(T0,X0,T1,X1,NX,DT,TSTOP,OK,OUTPUT,RS(IE))            
C                                                                       
         CALL ISTKRL(1)                                                 
C                                                                       
C/6S                                                                    
C        IF (T1+DT.EQ.T1) CALL SETERR                                   
C    1      (30H SXTRP - DT=0 RETURNED BY SOUT,30,14,1)                 
C/7S                                                                    
         IF (T1+DT.EQ.T1) CALL SETERR                                   
     1      (' SXTRP - DT=0 RETURNED BY SOUT',30,14,1)                  
C/                                                                      
         IF (T1+DT.EQ.T1) GO TO 140                                     
C                                                                       
C/6S                                                                    
C        IF ((DT/ABS(DT))*(TSTOP-T1).LT.0.0E0) CALL SETERR              
C    1      (47H SXTRP - DT RETURNED BY SOUT HAS THE WRONG SIGN,47,15,2)
C/7S                                                                    
         IF ((DT/ABS(DT))*(TSTOP-T1).LT.0.0E0) CALL SETERR              
     1      (' SXTRP - DT RETURNED BY SOUT HAS THE WRONG SIGN',47,15,2) 
C/                                                                      
C                                                                       
         IF (T0.EQ.T1) KOPTO=MIN0(KOPT,KOPTO)                           
         IF (T0.EQ.T1) GO TO 120                                        
C                                                                       
C ...... UPDATE X0, HOPT AND HUP IF CONVERGED.                          
C                                                                       
         DO 100 I=1,NX                                                  
 100        X0(I)=X1(I)                                                 
C                                                                       
         DO 110 K=1,KHI                                                 
 110        HOPTO(K)=HOPT(K)                                            
C                                                                       
         HUP=AMIN1(2.0E0*HUP,HUPMAX)                                    
C                                                                       
         MOLD=M                                                         
         KOPTO=KOPT                                                     
         PRSTRT=.FALSE.                                                 
C                                                                       
 120     IF (T1.EQ.TSTOP) GO TO 140                                     
C                                                                       
C ...... GO DO THE NEXT TIME-STEP.                                      
C                                                                       
         GO TO 10                                                       
C                                                                       
C ...... THINGS ARE NOT OK.                                             
C                                                                       
 130     T1=T0                                                          
         PRSTRT=.TRUE.                                                  
C                                                                       
C ...... DO NOT LET THE USER RAISE DT WHEN OK=.FALSE.                   
C                                                                       
         TEMP=DT                                                        
C                                                                       
C ...... GO INTO A FULL RESTART SEQUENCE.                               
C                                                                       
         IF (T0.NE.TSTART) IRCNT=MRCNT                                  
         TBAD=T0+DT                                                     
C                                                                       
         CALL SOUT(T0,X0,T1,X1,NX,DT,TSTOP,OK,OUTPUT,RS(1))             
C                                                                       
C/6S                                                                    
C        IF (T1+DT.EQ.T1) CALL SETERR                                   
C    1      (30H SXTRP - DT=0 RETURNED BY SOUT,30,14,1)                 
C/7S                                                                    
         IF (T1+DT.EQ.T1) CALL SETERR                                   
     1      (' SXTRP - DT=0 RETURNED BY SOUT',30,14,1)                  
C/                                                                      
         IF (T1+DT.EQ.T1) GO TO 140                                     
C                                                                       
C/6S                                                                    
C        IF ((DT/ABS(DT))*(TSTOP-T1).LT.0.0E0) CALL SETERR              
C    1      (47H SXTRP - DT RETURNED BY SOUT HAS THE WRONG SIGN,47,15,2)
C/7S                                                                    
         IF ((DT/ABS(DT))*(TSTOP-T1).LT.0.0E0) CALL SETERR              
     1      (' SXTRP - DT RETURNED BY SOUT HAS THE WRONG SIGN',47,15,2) 
C/                                                                      
C                                                                       
C/6S                                                                    
C        IF (ABS(TEMP).LT.ABS(DT)) CALL SETERR                          
C    1      (42H SXTRP - DT RAISED BY SOUT WHEN OK=.FALSE.,42,16,2)     
C/7S                                                                    
         IF (ABS(TEMP).LT.ABS(DT)) CALL SETERR                          
     1      (' SXTRP - DT RAISED BY SOUT WHEN OK=.FALSE.',42,16,2)      
C/                                                                      
C                                                                       
C ...... THE DEFAULT RESPONSE IS TO LOWER DT BY 10**3.                  
C                                                                       
         IF (ABS(DT).EQ.ABS(TEMP)) DT=DT/1.0E+3                         
C                                                                       
C/6S                                                                    
C        IF (T0+DT.EQ.T0) CALL SETERR                                   
C    1      (13H SXTRP - DT=0,13,13,1)                                  
C/7S                                                                    
         IF (T0+DT.EQ.T0) CALL SETERR                                   
     1      (' SXTRP - DT=0',13,13,1)                                   
C/                                                                      
         IF (T0+DT.EQ.T0) GO TO 140                                     
C                                                                       
         IF (T1.NE.TSTOP) GO TO 10                                      
C                                                                       
 140  RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION A8XTRD(E,NX,M,MMAX,KMAX,LOGN,BETA,GAMMA,         
     1                        DELTA,F,POW,LDONE,ILDONE,KOPTO)           
C                                                                       
C  RETURN LDONE = THE LEVEL WHERE CONVERGENCE IS EXPECTED.              
C                                                                       
C  IF M=KOPTO+1, RETURN ILDONE=LDONE.                                   
C  IF M.GT.KOPTO+1, DO NOT LET LDONE.GT.ILDONE+1 HAPPEN IN THE FIRST    
C  KOPTO COLUMNS.                                                       
C                                                                       
C  A8XTRD = .TRUE. IF WILL NOT CONVERGE IN THIS LOZENGE.                
C  A8XTRD = .FALSE. IF WILL CONVERGE.                                   
C                                                                       
      REAL LOGN(MMAX),BETA,GAMMA,DELTA                                  
      REAL E(NX,KMAX),F(KMAX),POW(KMAX)                                 
C                                                                       
      REAL TEMP                                                         
C                                                                       
C ... INITIALLY, FLAG NOT CONVERGENT.                                   
C                                                                       
      A8XTRD=.TRUE.                                                     
      LDONE=0                                                           
C                                                                       
      LMAX=MMAX                                                         
      IF (M.GT.KOPTO+1) LMAX=MIN0(ILDONE+1,MMAX)                        
      MP1=M+1                                                           
      IF (MP1.GT.LMAX) GO TO 60                                         
C                                                                       
      DO 50 L=MP1,LMAX                                                  
C                                                                       
C ...... IF M.LE.KOPTO+1, CHECK FOR CONVERGENCE AT LEVEL L ONLY IF NOT  
C ...... ALREADY ACHIEVED. OTHERWISE, CHECK AT L IF NOT ALREADY DONE    
C ...... OR L=LMAX=ILDONE+1.                                            
C                                                                       
         IF (LDONE.GT.0.AND.(L.LT.LMAX.OR.M.LE.KOPTO+1)) GO TO 50       
C                                                                       
C ...... LDONEO DETERMINES IF WILL CONVERGE IN THE FIRST KOPTO          
C ...... COLUMNS.                                                       
C                                                                       
         LDONEO=L                                                       
         JHI=MIN0(M-1,KMAX)                                             
C                                                                       
C ...... COMPUTE THE FACTORS NEEDED TO CHECK FOR CONVERGENCE.           
C                                                                       
         DO 20 J=1,JHI                                                  
C                                                                       
            MMA=M                                                       
            LMA=L                                                       
            TEMP=0.0E0                                                  
C                                                                       
            DO 10 I=1,J                                                 
C                                                                       
               LMA=LMA-1                                                
               MMA=MMA-1                                                
 10            TEMP=TEMP+(LOGN(MMA)-LOGN(LMA))                          
C                                                                       
 20         F(J)=POW(J)*GAMMA*TEMP                                      
C                                                                       
C ...... SEE IF THE I-TH VARIABLE WILL CONVERGE AT M=L.                 
C                                                                       
         DO 40 I=1,NX                                                   
C                                                                       
C ......... CHECK EACH COLUMN FOR CONVERGENCE.                          
C                                                                       
            DO 30 J=1,JHI                                               
C                                                                       
               JSAVE=J                                                  
C                                                                       
               IF (E(I,J).GE.F(J)) GO TO 40                             
C                                                                       
 30            CONTINUE                                                 
C                                                                       
C ......... NO CONVERGENCE HERE.                                        
C                                                                       
            GO TO 50                                                    
C                                                                       
C ......... IF WILL NOT CONVERGE IN THE FIRST KOPTO COLUMNS,            
C ......... FLAG LDONEO.                                                
C                                                                       
 40         IF (JSAVE.GT.KOPTO) LDONEO=0                                
C                                                                       
C ...... HAVE CONVERGENCE HERE.                                         
C                                                                       
         IF (LDONE.EQ.0) LDONE=L                                        
C                                                                       
 50      CONTINUE                                                       
C                                                                       
 60   IF (LDONE.EQ.0.OR.LDONEO.EQ.0) GO TO 70                           
C                                                                       
      IF (M.EQ.KOPTO+1) ILDONE=LDONE                                    
C                                                                       
      A8XTRD=.FALSE.                                                    
C                                                                       
 70   RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION A8XTRL(E2,E,NX,M,KMAX,POW,LOGLO)                 
C                                                                       
C  TO RETURN POW(J) TIMES THE LOGARITHM OF THE RATIO OF THE DESIRED     
C  TO THE ATTAINED ERROR, FOR EACH ELEMENT IN THE LOZENGE.              
C                                                                       
C  A8XTRL = .TRUE. IF NOT SUCCESSFUL.                                   
C  A8XTRL = .FALSE. IF SUCCESSFUL.                                      
C                                                                       
      REAL E2(NX,KMAX),E(NX),POW(KMAX),LOGLO                            
C                                                                       
      REAL LOGE,V,R1MACH                                                
C                                                                       
      A8XTRL=.FALSE.                                                    
      JHI=MIN0(M-1,KMAX)                                                
C                                                                       
      DO 20 I=1,NX                                                      
C                                                                       
         IF (E(I).LE.0.0E0) GO TO 30                                    
C                                                                       
         LOGE=ALOG(E(I))                                                
C                                                                       
         DO 10 J=1,JHI                                                  
C                                                                       
            V=-ALOG(R1MACH(4))-4.6E0                                    
            IF (E2(I,J).NE.0.0E0) V=LOGE-ALOG(ABS(E2(I,J)))             
 10         E2(I,J)=POW(J)*V                                            
C                                                                       
 20      CONTINUE                                                       
C                                                                       
      GO TO 40                                                          
C                                                                       
C ... HERE FOR A NON-POSITIVE ERROR REQUEST.                            
C                                                                       
C/6S                                                                    
C30   CALL SETERR(36H SXTRP - E(I).LE.0 RETURNED BY ERROR,36,17,1)      
C/7S                                                                    
 30   CALL SETERR(' SXTRP - E(I).LE.0 RETURNED BY ERROR',36,17,1)       
C/                                                                      
      A8XTRL=.TRUE.                                                     
C                                                                       
 40   RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE A8XTRO(E,NX,M,MMAX,KMAX,LOGN,GAMMA,F,POW,KOPT,         
     1                  DT,HOPT,LNWORK,COST,LOGLO,LOGHI)                
C                                                                       
C  COMPUTE THE OPTIMAL K AND H.                                         
C                                                                       
      REAL LOGN(MMAX),GAMMA,DT,TEMP                                     
      REAL E(NX,KMAX),F(KMAX),POW(KMAX),                                
     1     HOPT(1),LNWORK(MMAX),COST(1),LOGLO,LOGHI                     
C     REAL HOPT(KMAX+1),COST(KMAX+1)                                    
C                                                                       
      REAL RHOMAX,HOPTK,ROMAX,HOPTM,HTOL,LOGDT                          
C                                                                       
C  HTOL IS THE RELATIVE TOLERANCE TO WHICH LOG(HOPT) WILL BE COMPUTED.  
C                                                                       
      DATA HTOL/5.0E-2/                                                 
C                                                                       
      LOGDT=ALOG((ABS(DT)))                                             
      KHI=MIN0(M-1,KMAX)                                                
      KHIP1=KHI+1                                                       
      KOPT=1                                                            
C                                                                       
C ... COMPUTE HOPT(K), K=1,...,MIN(M,KMAX+1).                           
C                                                                       
      DO 50 K=1,KHIP1                                                   
C                                                                       
         JHI=MIN0(K,KHI)                                                
         JHIM1=JHI-1                                                    
C                                                                       
C ...... COMPUTE THE FACTORS WHICH CONVERT ERRORS INTO STEP-SIZES.      
C                                                                       
         DO 20 J=1,JHI                                                  
C                                                                       
            MMA=M                                                       
            LMA=K+1                                                     
            TEMP=0.0E0                                                  
C                                                                       
            DO 10 I=1,J                                                 
C                                                                       
               LMA=LMA-1                                                
               MMA=MMA-1                                                
 10            TEMP=TEMP+(LOGN(LMA)-LOGN(MMA))                          
C                                                                       
 20         F(J)=GAMMA*TEMP*POW(J)                                      
C                                                                       
C ...... HOPTK IS THE OPTIMAL STEP-SIZE FOR THE K-COLUMN LOZENGE.       
C ...... HOPTM IS THE OPTIMAL STEP-SIZE FOR THE FIRST (K-1)-COLUMNS     
C ...... OF THE FULL K-COLUMN LOZENGE.                                  
C                                                                       
         HOPTK=LOGHI                                                    
         HOPTM=LOGHI                                                    
C                                                                       
         DO 40 I=1,NX                                                   
C                                                                       
            RHOMAX=LOGLO                                                
            ROMAX=LOGLO                                                 
C                                                                       
            DO 30 J=1,JHI                                               
C                                                                       
               RHOMAX=AMAX1(RHOMAX,F(J)+E(I,J))                         
C                                                                       
C ............ SAVE THE OPTIMAL FACTOR FOR THE (K-1) COLUMN SUB-LOZENGE.
C                                                                       
 30            IF (J.EQ.JHIM1) ROMAX=RHOMAX                             
C                                                                       
            HOPTM=AMIN1(HOPTM,ROMAX)                                    
 40         HOPTK=AMIN1(HOPTK,RHOMAX)                                   
C                                                                       
         COST(K)=LNWORK(K+1)-HOPTK                                      
         IF (HOPTK.GT.HOPTM+HTOL.AND.K.LT.KHIP1) KOPT=K                 
C                                                                       
 50      HOPT(K)=EXP(AMIN1(HOPTK+LOGDT,0.9999E0*LOGHI))*(DT/ABS(DT))    
C                                                                       
C ... SEE IF A LOWER K IS CHEAPER THAN KOPT. IF SO, USE IT.             
C                                                                       
      DO 60 K=1,KOPT                                                    
         KS=K                                                           
         IF (COST(K).LE.COST(KOPT)) GO TO 70                            
 60      CONTINUE                                                       
C                                                                       
 70   KOPT=KS                                                           
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE XTRAP(TM,M,NVAR,NG,KMAX,XPOLY,T,ERROR,EBEST)           
C                                                                       
C  ASSUME AN EXPANSION FOR THE VECTOR VALUED FUNCTION T(H) OF THE FORM  
C                                                                       
C            T(H) = T(0) + SUM(J=1,2,3,...)(A(J)*H**(J*GAMMA))          
C                                                                       
C  WHERE THE A(J) ARE CONSTANT VECTORS AND GAMMA IS A POSITIVE CONSTANT.
C                                                                       
C  GIVEN T(H(M)), WHERE H(M)=H0/N(M), M=1,2,3,..., THIS ROUTINE USES    
C  POLYNOMIAL (XPOLY), OR RATIONAL (.NOT.XPOLY), EXTRAPOLATION TO       
C  SEQUENTIALLY APPROXIMATE T(0).                                       
C                                                                       
C  INPUT                                                                
C                                                                       
C    TM     - TM = T(H(M)) FOR THIS CALL.                               
C    M      - H(M) WAS USED TO OBTAIN TM.                               
C    NVAR   - THE LENGTH OF THE VECTOR TM.                              
C    NG     - THE REAL VALUES                                           
C                                                                       
C                 NG(I) = N(I)**GAMMA                                   
C                                                                       
C             FOR I=1,...,M. NG MUST BE A MONOTONE INCREASING ARRAY.    
C    KMAX   - THE MAXIMUM NUMBER OF COLUMNS TO BE USED IN THE           
C             EXTRAPOLATION PROCESS.                                    
C    XPOLY  - IF XPOLY=.TRUE., THEN USE POLYNOMIAL EXTRAPOLATION.       
C             IF XPOLY=.FALSE., THEN USE RATIONAL EXTRAPOLATION.        
C    T      - THE BOTTOM EDGE OF THE EXTRAPOLATION LOZENGE.             
C             T(I,J) SHOULD CONTAIN THE J-TH EXTRAPOLATE OF THE I-TH    
C             COMPONENT OF T(H) BASED ON THE SEQUENCE H(1),...,H(M-1),  
C             FOR I=1,...,NVAR AND J=1,...,MIN(M-1,KMAX).               
C                                                                       
C             WHEN M=1, T MAY CONTAIN ANYTHING.                         
C                                                                       
C             FOR M.GT.1, NOTE THAT THE OUTPUT VALUE OF T AT THE        
C             (M-1)-ST CALL IS THE INPUT FOR THE M-TH CALL.             
C             THUS, THE USER NEED NEVER PUT ANYTHING INTO T,            
C             BUT HE CAN NOT ALTER ANY ELEMENT OF T BETWEEN             
C             CALLS TO XTRAP.                                           
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    TM     - TM(I)=THE MOST ACCURATE APPROXIMATION IN THE LOZENGE      
C             FOR THE I-TH VARIABLE, I=1,...,NVAR.                      
C    T      - T(I,J) CONTAINS THE J-TH EXTRAPOLATE OF THE I-TH          
C             COMPONENT OF T(H) BASED ON THE SEQUENCE H(1),...,H(M),    
C             FOR I=1,...,NVAR AND J=1,...,MIN(M,KMAX).                 
C    ERROR  - ERROR(I,J) GIVES THE SIGNED BULIRSCH-STOER ESTIMATE OF THE
C             ERROR IN THE J-TH EXTRAPOLATE OF THE I-TH COMPONENT OF    
C             T(H) BASED ON THE SEQUENCE H(1),...,H(M-1),               
C             FOR I=1,...,NVAR AND J=1,...,MIN(M-1,KMAX).               
C             IF ERROR=EBEST AS ARRAYS, THEN THE ABOVE ELEMENTS         
C             ARE NOT STORED. RATHER, EBEST=ERROR IS LOADED AS DESCRIBED
C             BELOW.                                                    
C    EBEST  - EBEST(I)=THE ABSOLUTE VALUE OF THE ERROR IN TM(I),        
C             I=1,...,NVAR. THIS ARRAY IS FULL OF GARBAGE WHEN M=1.     
C                                                                       
C  SCRATCH SPACE ALLOCATED - MIN(M-1,KMAX) REAL WORDS +                 
C                                                                       
C                            MIN(M-1,KMAX) INTEGER WORDS.               
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - M.LT.1.                                                        
C    2 - NVAR.LT.1.                                                     
C    3 - NG(1).LT.1.                                                    
C    4 - KMAX.LT.1.                                                     
C    5 - NG IS NOT MONOTONE INCREASING.                                 
C                                                                       
      REAL TM(NVAR),NG(M),T(NVAR,1)                                     
C     REAL T(NVAR,MIN(M,KMAX))                                          
      REAL ERROR(NVAR,1),EBEST(NVAR)                                    
C     REAL ERROR(NVAR,MIN(M-1,KMAX))                                    
      LOGICAL XPOLY                                                     
C                                                                       
      LOGICAL ESAVE                                                     
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL WS(1)                                                        
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),WS(1)),(DS(1),RS(1))                           
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (M.LT.1) CALL SETERR(15H XTRAP - M.LT.1,15,1,2)                
C     IF (NVAR.LT.1) CALL SETERR(18H XTRAP - NVAR.LT.1,18,2,2)          
C     IF (NG(1).LT.1.0E0) CALL SETERR(19H XTRAP - NG(1).LT.1,19,3,2)    
C     IF (KMAX.LT.1) CALL SETERR(18H XTRAP - KMAX.LT.1,18,4,2)          
C/7S                                                                    
      IF (M.LT.1) CALL SETERR(' XTRAP - M.LT.1',15,1,2)                 
      IF (NVAR.LT.1) CALL SETERR(' XTRAP - NVAR.LT.1',18,2,2)           
      IF (NG(1).LT.1.0E0) CALL SETERR(' XTRAP - NG(1).LT.1',19,3,2)     
      IF (KMAX.LT.1) CALL SETERR(' XTRAP - KMAX.LT.1',18,4,2)           
C/                                                                      
C                                                                       
      IF (M.EQ.1) GO TO 20                                              
C                                                                       
      DO 10 I=2,M                                                       
C/6S                                                                    
C        IF (NG(I-1).GE.NG(I)) CALL SETERR                              
C    1      (38H XTRAP - NG IS NOT MONOTONE INCREASING,38,5,2)          
C/7S                                                                    
         IF (NG(I-1).GE.NG(I)) CALL SETERR                              
     1      (' XTRAP - NG IS NOT MONOTONE INCREASING',38,5,2)           
C/                                                                      
 10      CONTINUE                                                       
C                                                                       
C ... SEE IF ERROR=EBEST AS ARRAYS. IF (ESAVE), THEN LOAD ERROR.        
C                                                                       
 20   ERROR(1,1)=1.0E0                                                  
      EBEST(1)=2.0E0                                                    
      ESAVE=ERROR(1,1).NE.EBEST(1)                                      
C                                                                       
C ... ALLOCATE SCRATCH SPACE.                                           
C                                                                       
      IRHG=1                                                            
      IEMAG=1                                                           
      IF (M.GT.1) IRHG=ISTKGT(MIN0(M-1,KMAX),3)                         
      IF (M.GT.1) IEMAG=ISTKGT(MIN0(M-1,KMAX),3)                        
C                                                                       
      CALL A0XTRP(TM,M,NVAR,NG,KMAX,XPOLY,T,ERROR,EBEST,WS(IRHG),       
     1            RS(IEMAG),ESAVE)                                      
C                                                                       
      IF (M.GT.1) CALL ISTKRL(2)                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE A0XTRP(TM,M,NVAR,NG,KMAX,XPOLY,T,ERROR,EBEST,RHG,EMAG, 
     1                  ESAVE)                                          
C                                                                       
      REAL TM(NVAR),NG(M),T(NVAR,KMAX),RHG(1)                           
C     REAL RHG(MIN(M-1,KMAX))                                           
      REAL ERROR(NVAR,1),EBEST(NVAR),EMAG(1)                            
C     REAL ERROR(NVAR,MIN(M-1,KMAX)),EMAG(MIN(M-1,KMAX))                
      LOGICAL XPOLY,ESAVE                                               
C                                                                       
      REAL U,V,TI,TV,TEMP                                               
      REAL ERR                                                          
C                                                                       
      IF (M.GT.1) GO TO 20                                              
C                                                                       
C ... INITIALIZE T.                                                     
C                                                                       
      DO 10 I=1,NVAR                                                    
 10      T(I,1)=TM(I)                                                   
C                                                                       
      GO TO 80                                                          
C                                                                       
 20   MR=MIN0(M-1,KMAX)                                                 
C                                                                       
      DO 30 J=1,MR                                                      
         MMJ=M-J                                                        
         RHG(J)=NG(M)/NG(MMJ)                                           
         EMAG(J)=1.0E0+1.0E0/(RHG(J)-1.0E0)                             
         IF (XPOLY) RHG(J)=RHG(J)-1.0E0                                 
 30      CONTINUE                                                       
C                                                                       
      DO 70 I=1,NVAR                                                    
C                                                                       
         V=0.0E0                                                        
         U=T(I,1)                                                       
         TI=TM(I)                                                       
         T(I,1)=TI                                                      
C                                                                       
         DO 60 J=1,MR                                                   
C                                                                       
C ......... OBTAIN SIGNED ERROR ESTIMATE.                               
C                                                                       
            ERR=(T(I,J)-U)*EMAG(J)                                      
            IF (ESAVE) ERROR(I,J)=ERR                                   
            ERR=ABS(ERR)                                                
            IF (J.EQ.1) EBEST(I)=ERR                                    
            EBEST(I)=AMIN1(EBEST(I),ERR)                                
            IF (EBEST(I).EQ.ERR) JBEST=J                                
C                                                                       
            IF (J.EQ.KMAX) GO TO 60                                     
C                                                                       
            IF (XPOLY) GO TO 40                                         
C                                                                       
C ......... RATIONAL EXTRAPOLATION.                                     
C                                                                       
            TV=TI-V                                                     
            TEMP=RHG(J)*(U-V)-TV                                        
            IF (TEMP.NE.0.0E0) TI=TI+(TI-U)*(TV/TEMP)                   
            V=U                                                         
            GO TO 50                                                    
C                                                                       
C ......... POLYNOMIAL EXTRAPOLATION.                                   
C                                                                       
 40         TI=TI+(TI-U)/RHG(J)                                         
C                                                                       
 50         U=T(I,J+1)                                                  
            T(I,J+1)=TI                                                 
 60         CONTINUE                                                    
C                                                                       
 70      TM(I)=T(I,JBEST)                                               
C                                                                       
 80   RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DODES(F,X,NX,TSTART,TSTOP,DT,ERRPAR,HANDLE)            
C                                                                       
C  TO SOLVE THE INITIAL VALUE PROBLEM FOR                               
C                                                                       
C          DX(T)/DT = F(T,X(T)).                                        
C                                                                       
C  METHOD - RATIONAL EXTRAPOLATION OF GRAGGS MODIFIED MID-POINT RULE.   
C                                                                       
C  INPUT                                                                
C                                                                       
C    F      - CALL F(T,X,NX,FTX) SHOULD RETURN FTX(I)=F(T,X)(I),        
C             FOR I=1,...,NX. IF IT CANNOT, IT SHOULD RETURN            
C             OKAY=.FALSE. IN COMMON /DODESF/OKAY .                     
C             F SHOULD BE DECLARED EXTERNAL IN THE SUBPROGRAM           
C             CALLING DODES.                                            
C    X      - THE INITIAL VALUES FOR THE SOLUTION.                      
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    TSTART - THE INITIAL TIME.                                         
C    TSTOP  - THE FINAL TIME.                                           
C    DT     - THE INITIAL TIME-STEP TO BE USED.                         
C             THE PERFORMANCE OF DODES IS SUBSTANTIALLY                 
C             INDEPENDENT OF THE VALUE OF DT CHOSEN BY THE USER.        
C             IT IS SUFFICIENT THAT THE USERS CHOICE FOR DT MERELY BE   
C             WITHIN SEVERAL ORDERS OF MAGNITUDE OF BEING CORRECT.      
C             THE VALUE OF DT WILL BE AUTOMATICALLY CHANGED BY DODES    
C             DURING THE INTEGRATION PROCESS IN SUCH A WAY AS TO GET    
C             THE SOLUTION, TO THE DESIRED ACCURACY, AT THE LEAST       
C             POSSIBLE COST.                                            
C    ERRPAR - EACH COMPONENT X(I) OF THE SOLUTION IS TO BE COMPUTED     
C             TO WITHIN AN ABSOLUTE ERROR OF                            
C                                                                       
C                     ERRPAR(1) * ABS(X(I)) + ERRPAR(2)                 
C                                                                       
C             FOR I=1,...,NX, AT EACH TIME-STEP. THIS ERROR REQUEST MUST
C             ALWAYS BE POSITIVE.                                       
C    HANDLE - OUTPUT ROUTINE WITH A CALLING SEQUENCE OF THE FORM        
C                                                                       
C                     HANDLE(T0,X0,T1,X1,NX,DT,TSTOP,E)                 
C                                                                       
C             HANDLE WILL BE CALLED AT THE END OF EACH TIME-STEP.       
C                                                                       
C             THE INPUT TO HANDLE IS AS FOLLOWS                         
C                                                                       
C               X0,X1  - X0=X(T0) AND X1=X(T1).                         
C               T0,T1  - T0=T1 INDICATES A RESTART AND X1 IS FULL OF    
C                        GARBAGE.                                       
C               NX     - THE LENGTH OF THE SOLUTION VECTOR X.           
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE CURRENT FINAL TIME.                        
C               E      - E(I) GIVES THE REAL ABSOLUTE ERROR IN X1(I),   
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C                                                                       
C             THE OUTPUT FROM HANDLE MAY BE ANY OF                      
C                                                                       
C               X1     - MAY BE ALTERED IF DESIRED.                     
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE FINAL TIME VALUE.                          
C                                                                       
C             HANDLE SHOULD BE DECLARED EXTERNAL IN THE                 
C             SUBPROGRAM CALLING DODES.                                 
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    X      - X=X(TSTOP), THE FINAL VALUE FOR THE SOLUTION.             
C    TSTOP  - MAY BE ALTERED BY USER SUPPLIED ROUTINE HANDLE.           
C    DT     - PROPOSED TIME-STEP FOR THE NEXT STEP, IF ANY.             
C                                                                       
C  SCRATCH SPACE OF LENGTH                                              
C                                                                       
C                   S(DODES) .LE.                                       
C                                                                       
C    32 + 12*NX                                                         
C                                                                       
C  DOUBLE PRECISION WORDS +                                             
C                                                                       
C    101 + MAX( 2*NX DOUBLE PRECISION + S(F) ,                          
C                                                                       
C               11*NX + 10 DOUBLE PRECISION + 10 ,                      
C                                                                       
C               NX + S(HANDLE) )                                        
C                                                                       
C  INTEGER WORDS IS ALLOCATED.                                          
C                                                                       
C  ERROR STATES                                                         
C                                                                       
C    1 - NX.LT.1.                                                       
C    2 - DT HAS THE WRONG SIGN ON INPUT.                                
C    3 - DT=0 ON INPUT.                                                 
C    4 - DT RETURNED BY HANDLE HAS THE WRONG SIGN.                      
C    5 - DT=0 WAS RETURNED BY HANDLE. (RECOVERABLE)                     
C    6 - THE ERROR DESIRED IN X(I) IS NOT POSITIVE. (RECOVERABLE)       
C    7 - DT=0. (RECOVERABLE)                                            
C    8 - CANNOT RAISE DT IN HANDLE WHEN .NOT.OKAY.                      
C                                                                       
      COMMON /DODESF/OKAY                                               
C                                                                       
      DOUBLE PRECISION X(NX),TSTART,TSTOP,DT                            
      REAL ERRPAR(2)                                                    
      LOGICAL OKAY                                                      
      EXTERNAL F,HANDLE                                                 
C                                                                       
      EXTERNAL DODESE                                                   
C                                                                       
      IF (TSTART.EQ.TSTOP) GO TO 10                                     
C                                                                       
      CALL DODES1(F,X,NX,TSTART,TSTOP,DT,DODESE,ERRPAR,HANDLE,          
     1            .FALSE.,.FALSE.)                                      
C                                                                       
 10   RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION DODESE(X,NX,T,DT,ERRPAR,ERPUTS,E)                
C                                                                       
C  STANDARD ERROR ROUTINE FOR DODES WITH THE OPTION FOR ERROR CONTROL   
C  BASED ON EITHER THE LOCAL VALUE OR THE GLOBAL MAXIMUM OF EACH        
C  COMPONENT.                                                           
C                                                                       
C  THE OPTION FOR ERROR CONTROL ON AN ERROR PER UNIT-TIME-STEP OR       
C  ERROR PER TIME-STEP BASIS IS ALSO PROVIDED.                          
C                                                                       
C  INPUT                                                                
C                                                                       
C    X      - X=X(T), THE APPROXIMATE SOLUTION FOR WHICH AN ERROR       
C             CRITERION IS DESIRED.                                     
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    T      - CURRENT VALUE OF THE TIME VARIABLE.                       
C    DT     - CURRENT TIME-STEP.                                        
C    ERRPAR - TWO PARAMETERS FOR USE IN DETERMINING THE DESIRED ERROR.  
C    ERPUTS - IF ERPUTS=.TRUE., THEN THE ERROR IS TO BE                 
C             PROPORTIONAL TO DABS(DT). OTHERWISE IT WILL NOT.          
C    E      - X(I) IS ACCURATE TO A REAL ABSOLUTE ERROR OF E(I),        
C             I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.             
C                                                                       
C  COMMON INPUT                                                         
C                                                                       
C    IGMAX  - THE POINTER TO THE REAL VECTOR OF CURRENT MAXIMUM ABSOLUTE
C             VALUES ATTAINED BY EACH COMPONENT OF THE SOLUTION.        
C             IGMAX=0 MEANS THIS VECTOR IS NOT USED AND HAS NOT BEEN    
C             ALLOCATED.                                                
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    E      - THE REAL ERROR VECTOR. E(I) IS THE ABSOLUTE ERROR         
C             TOLERABLE IN X(I), FOR I=1,...,NX.                        
C                                                                       
C             LET V(I) = ABS(X(I)) IF IGMAX=0                           
C                           OTHERWISE                                   
C                      = MAXIMUM(ABS(X(I)(T))) OVER ALL PREVIOUS TIME.  
C                        THIS VALUE IS STORED IN THE REAL STACK         
C                        POSITION RS(I+IGMAX-1).                        
C                                                                       
C             AND EPS = 1 IF ERPUTS=.FALSE.                             
C                           OTHERWISE                                   
C                     = DABS(DT),                                       
C                                                                       
C             THEN                                                      
C                                                                       
C                     E(I) = EPS * (ERRPAR(1)*V(I) + ERRPAR(2)),        
C                                                                       
C             FOR I=1,...,NX.                                           
C                                                                       
C  FUNCTION VALUE                                                       
C                                                                       
C    DODESE - .TRUE. IF EACH X(I) IS ACCURATE TO WITHIN AN              
C             ABSOLUTE ERROR OF E(I), I=1,...,NX, OTHERWISE .FALSE. .   
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES - NONE.                                                 
C                                                                       
      COMMON /DODESM/IGMAX,IGMAXO                                       
C                                                                       
      DOUBLE PRECISION X(NX),T,DT                                       
      REAL ERRPAR(2),E(NX)                                              
      LOGICAL ERPUTS                                                    
C                                                                       
      REAL DTPOW,TEMP                                                   
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),RS(1))                                         
C                                                                       
      DTPOW=1.0E0                                                       
      IF (ERPUTS) DTPOW=DABS(DT)                                        
C                                                                       
      DODESE=.TRUE.                                                     
      J=IGMAX                                                           
C                                                                       
      DO 10 I=1,NX                                                      
C                                                                       
         IF (IGMAX.GT.0) TEMP=RS(J)                                     
         IF (IGMAX.EQ.0) TEMP=ABS(SNGL(X(I)))                           
         TEMP=DTPOW*(ERRPAR(1)*TEMP+ERRPAR(2))                          
C                                                                       
         IF (E(I).GT.TEMP) DODESE=.FALSE.                               
C                                                                       
         E(I)=TEMP                                                      
C                                                                       
 10      J=J+1                                                          
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DODESH(T0,X0,T1,X1,NX,DT,TSTOP,E)                      
C                                                                       
C  THE DEFAULT OUTPUT ROUTINE FOR USE WITH DODES.                       
C  IT SIMPLY RETURNS.                                                   
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES - NONE.                                                 
C                                                                       
      DOUBLE PRECISION T0,X0(NX),T1,X1(NX),DT,TSTOP                     
      REAL E(NX)                                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION DODESQ(X,NX,T,DT,ERRPAR,ERPUTS,E)                
C                                                                       
C  STANDARD QUADRATURE ERROR ROUTINE FOR DODES.                         
C                                                                       
C  INPUT                                                                
C                                                                       
C    X      - X=X(T), THE APPROXIMATE SOLUTION FOR WHICH AN ERROR       
C             CRITERION IS DESIRED.                                     
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    T      - CURRENT VALUE OF THE TIME VARIABLE.                       
C    DT     - CURRENT TIME-STEP.                                        
C    ERRPAR - TWO PARAMETERS FOR USE IN DETERMINING THE DESIRED ERROR.  
C             THE FINAL INTEGRAL SHOULD BE COMPUTED ACCURATE TO A REAL  
C             ABSOLUTE ERROR OF ERRPAR(2).                              
C    ERPUTS - THIS PARAMETER IS IGNORED.                                
C    E      - X(I) IS ACCURATE TO A REAL ABSOLUTE ERROR OF E(I), FOR THE
C             SINGLE CURRENT TIME-STEP, I=1,...,NX.                     
C                                                                       
C  COMMON INPUT -                                                       
C                                                                       
C    TEND   - THE END OF THE INTEGRATION INTERVAL.                      
C    RERROR - THE REMAINDER OF THE INTEGRAL SHOULD BE DONE TO WITHIN    
C             A REAL ABSOLUTE ERROR OF RERROR.                          
C             THE FIRST CALL SHOULD HAVE RERROR=ERRPAR(2).              
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    E      - E(I)=MAX(RERROR*ABS(DT/(TEND-(T-DT))),                    
C                      1.0E-3*ERRPAR(2)) ,   FOR I=1,...,NX.            
C             THUS, THE FINAL INTEGRAL SHOULD BE ACCURATE TO WITHIN     
C             A REAL ABSOLUTE ERROR OF ERRPAR(2).                       
C                                                                       
C  FUNCTION VALUE -                                                     
C                                                                       
C    DODESQ - .TRUE. IF EACH X(I) IS ACCURATE TO WITHIN AN              
C             ABSOLUTE ERROR OF E(I), I=1,...,NX, OTHERWISE .FALSE. .   
C                                                                       
C  COMMON OUTPUT -                                                      
C                                                                       
C    RERROR - IF (DODESQ) THEN                                          
C             RERROR=RERROR - MAXIMUM(E(1),...,E(NX)), WHERE THE E      
C             USED IS THE INPUT VALUE FOR THAT VARIABLE.                
C             OTHERWISE, RERROR REMAINS UNCHANGED.                      
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES - NONE.                                                 
C                                                                       
      COMMON /D0DESQ/TEND,RERROR                                        
C                                                                       
      DOUBLE PRECISION X(NX),T,DT,TEND                                  
      REAL ERRPAR(2),E(NX),RERROR                                       
      LOGICAL ERPUTS                                                    
C                                                                       
      REAL EMAX,TEMP                                                    
C                                                                       
      DODESQ=.TRUE.                                                     
      EMAX=0.0E0                                                        
C                                                                       
      DO 10 I=1,NX                                                      
C                                                                       
         TEMP=AMAX1(RERROR*ABS(SNGL(DT/(TEND-(T-DT)))),                 
     1              1.0E-3*ERRPAR(2))                                   
C                                                                       
         IF (E(I).GT.TEMP) DODESQ=.FALSE.                               
C                                                                       
         EMAX=AMAX1(EMAX,E(I))                                          
C                                                                       
 10      E(I)=TEMP                                                      
C                                                                       
      IF (DODESQ) RERROR=RERROR-EMAX                                    
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DODES1(F,X,NX,TSTART,TSTOP,DT,ERROR,ERRPAR,HANDLE,     
     1                  GLBMAX,ERPUTS)                                  
C                                                                       
C  TO SOLVE THE INITIAL VALUE PROBLEM FOR                               
C                                                                       
C          DX(T)/DT = F(T,X(T)).                                        
C                                                                       
C  METHOD - RATIONAL EXTRAPOLATION OF GRAGGS MODIFIED MID-POINT RULE.   
C                                                                       
C  THE 3 EXTRA ARGUMENTS IN THIS SUBROUTINE PROVIDE MORE USER           
C  CONTROL OVER THE ACCURACY OF THE INTEGRATION PROCESS.                
C                                                                       
C  INPUT                                                                
C                                                                       
C    F      - CALL F(T,X,NX,FTX) SHOULD RETURN FTX(I)=F(T,X)(I),        
C             FOR I=1,...,NX. IF IT CANNOT, IT SHOULD RETURN            
C             OKAY=.FALSE. IN COMMON /DODESF/OKAY .                     
C             F SHOULD BE DECLARED EXTERNAL IN THE SUBPROGRAM           
C             CALLING DODES.                                            
C    X      - THE INITIAL VALUES FOR THE SOLUTION.                      
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    TSTART - THE INITIAL TIME.                                         
C    TSTOP  - THE FINAL TIME.                                           
C    DT     - THE INITIAL TIME-STEP TO BE USED.                         
C             THE PERFORMANCE OF DODES IS SUBSTANTIALLY                 
C             INDEPENDENT OF THE VALUE OF DT CHOSEN BY THE USER.        
C             IT IS SUFFICIENT THAT THE USERS CHOICE FOR DT MERELY BE   
C             WITHIN SEVERAL ORDERS OF MAGNITUDE OF BEING CORRECT.      
C             THE VALUE OF DT WILL BE AUTOMATICALLY CHANGED BY DODES    
C             DURING THE INTEGRATION PROCESS IN SUCH A WAY AS TO GET    
C             THE SOLUTION, TO THE DESIRED ACCURACY, AT THE LEAST       
C             POSSIBLE COST.                                            
C    ERROR  - ERROR TOLERANCE ROUTINE.                                  
C                                                                       
C                 LOGICAL FUNCTION ERROR(X,NX,T,DT,ERRPAR,ERPUTS,E)     
C                                                                       
C             HAS AS INPUT                                              
C                                                                       
C               X,T    - X=X(T), THE APPROXIMATE SOLUTION FOR WHICH     
C                        AN ERROR CRITERION IS DESIRED.                 
C               NX     - THE LENGTH OF X.                               
C               DT     - THE TIME-STEP USED TO OBTAIN X(T).             
C               ERRPAR - TWO PARAMETERS, AS GIVEN TO DODES1.            
C               ERPUTS - THIS VARIABLE HAS THE SAME VALUE AS ERPUTS IN  
C                        THE CALL TO DODES1.                            
C               E      - THE REAL ABSOLUTE ERROR IN X(I) IS E(I),       
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C                                                                       
C             THE OUTPUT IS                                             
C                                                                       
C               ERRPAR - MAY BE ALTERED IF DESIRED.                     
C               E      - E(I) IS THE REAL TOLERABLE ABSOLUTE ERROR IN   
C                        X(I), FOR I=1,...,NX. ALL THE E(I) MUST BE     
C                        POSITIVE.                                      
C                                                                       
C             FUNCTION VALUE                                            
C                                                                       
C               ERROR  - ERROR=.TRUE. IF CONVERGED.                     
C                        ERROR=.FALSE. IF NOT.                          
C    ERRPAR - TWO PARAMETERS TO BE PASSED TO ERROR.                     
C    HANDLE - OUTPUT ROUTINE WITH A CALLING SEQUENCE OF THE FORM        
C                                                                       
C                     HANDLE(T0,X0,T1,X1,NX,DT,TSTOP,E)                 
C                                                                       
C             HANDLE WILL BE CALLED AT THE END OF EACH TIME-STEP.       
C                                                                       
C             THE INPUT TO HANDLE IS AS FOLLOWS                         
C                                                                       
C               X0,X1  - X0=X(T0) AND X1=X(T1).                         
C               T0,T1  - T0=T1 INDICATES A RESTART AND X1 IS FULL OF    
C                        GARBAGE.                                       
C               NX     - THE LENGTH OF THE SOLUTION VECTOR X.           
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE CURRENT FINAL TIME.                        
C               E      - E(I) GIVES THE REAL ABSOLUTE ERROR IN X1(I),   
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C                                                                       
C             THE OUTPUT FROM HANDLE MAY BE ANY OF                      
C                                                                       
C               X1     - MAY BE ALTERED IF DESIRED.                     
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE FINAL TIME VALUE.                          
C                                                                       
C             HANDLE SHOULD BE DECLARED EXTERNAL IN THE                 
C             SUBPROGRAM CALLING DODES.                                 
C    GLBMAX - IF (GLBMAX) THEN THE GLOBAL MAXIMUM OF THE ABSOLUTE VALUE 
C             OF THE SOLUTION IS TO BE RECORDED.                        
C             THE GLOBAL MAXIMUM INFORMATION IS STORED IN COMMON,       
C             AS DESCRIBED IN DODES2.                                   
C                                                                       
C             IF THE ERROR SUBPROGRAM SUPPLIED BY THE USER IS DODESE,   
C             THEN GLBMAX DETERMINES WHETHER OR NOT THE GLOBAL          
C             MAXIMUM ABSOLUTE VALUE OF THE SOLUTION WILL BE USED IN    
C             THAT SUBPROGRAM.                                          
C    ERPUTS - IF (ERPUTS) THEN THE ERROR PER UNIT TIME-STEP CRITERION   
C             IS TO BE USED IN THE ERROR ROUTINE.                       
C             OTHERWISE, THE ERROR PER TIME-STEP CRITERION IS TO BE     
C             USED IN THE ERROR ROUTINE.                                
C                                                                       
C             IF THE ERROR SUBPROGRAM SUPPLIED BY THE USER IS DODESE,   
C             THEN ERPUTS DETERMINES WHETHER OR NOT THE ERROR           
C             PER UNIT-TIME-STEP OR ERROR PER TIME-STEP ERROR           
C             CRITERION WILL BE USED BY THAT SUBPROGRAM.                
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    X      - X=X(TSTOP), THE FINAL VALUE FOR THE SOLUTION.             
C    TSTOP  - MAY BE ALTERED BY USER SUPPLIED ROUTINE HANDLE.           
C    DT     - PROPOSED TIME-STEP FOR THE NEXT STEP, IF ANY.             
C    ERRPAR - MAY BE ALTERED BY USER SUPPLIED ROUTINE ERROR.            
C                                                                       
C  SCRATCH SPACE OF LENGTH                                              
C                                                                       
C                    S(DODES1) .LE.                                     
C                                                                       
C    32 + 12*NX                                                         
C                                                                       
C  DOUBLE PRECISION WORDS +                                             
C                                                                       
C    101 + ( IF (GLBMAX) THEN 2*NX , OTHERWISE 0 ) +                    
C                                                                       
C    MAX( 2*NX DOUBLE PRECISION + S(F) ,                                
C                                                                       
C         11*NX + MAX ( 10 DOUBLE PRECISION + 10 , S(ERROR) ) ,         
C                                                                       
C         NX + S(HANDLE) )                                              
C                                                                       
C  INTEGER WORDS IS ALLOCATED.                                          
C                                                                       
C  ERROR STATES                                                         
C                                                                       
C    1 - NX.LT.1.                                                       
C    2 - DT HAS THE WRONG SIGN ON INPUT.                                
C    3 - DT=0 ON INPUT.                                                 
C    4 - DT RETURNED BY HANDLE HAS THE WRONG SIGN.                      
C    5 - DT=0 WAS RETURNED BY HANDLE. (RECOVERABLE)                     
C    6 - THE ERROR DESIRED IN X(I) IS NOT POSITIVE. (RECOVERABLE)       
C    7 - DT=0. (RECOVERABLE)                                            
C    8 - CANNOT RAISE DT IN HANDLE WHEN .NOT.OKAY.                      
C                                                                       
      COMMON /DODESF/OKAY                                               
C                                                                       
      DOUBLE PRECISION X(NX),TSTART,TSTOP,DT                            
      REAL ERRPAR(2)                                                    
      LOGICAL GLBMAX,ERPUTS,OKAY                                        
      EXTERNAL F,ERROR,HANDLE                                           
C                                                                       
      IF (TSTART.EQ.TSTOP) GO TO 10                                     
C                                                                       
      CALL DODES2(F,X,NX,TSTART,TSTOP,DT,ERROR,ERRPAR,HANDLE,           
     1            GLBMAX,ERPUTS,10,16)                                  
C                                                                       
 10   RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DODES2(F,X,NX,TSTART,TSTOP,DT,ERROR,ERRPAR,HANDLE,     
     1                  GLBMAX,ERPUTS,KMAX,MMAX)                        
C                                                                       
C  TO SOLVE THE INITAL VALUE PROBLEM FOR                                
C                                                                       
C          DX(T)/DT = F(T,X(T)).                                        
C                                                                       
C  METHOD - RATIONAL EXTRAPOLATION OF GRAGGS MODIFIED MID-POINT RULE.   
C                                                                       
C  THE 2 EXTRA ARGUMENTS IN THIS SUBROUTINE PROVIDE USER CONTROL        
C  OVER THE MAXIMUM ORDER AND MAXIMUM LEVEL OF EXTRAPOLATION TO BE USED 
C  BY THE PROCEDURE.                                                    
C                                                                       
C  INPUT                                                                
C                                                                       
C    F      - CALL F(T,X,NX,FTX) SHOULD RETURN FTX(I)=F(T,X)(I),        
C             FOR I=1,...,NX. IF IT CANNOT, IT SHOULD RETURN            
C             OKAY=.FALSE. IN COMMON /DODESF/OKAY .                     
C             F SHOULD BE DECLARED EXTERNAL IN THE SUBPROGRAM           
C             CALLING DODES.                                            
C    X      - THE INITIAL VALUES FOR THE SOLUTION.                      
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    TSTART - THE INITIAL TIME.                                         
C    TSTOP  - THE FINAL TIME.                                           
C    DT     - THE INITIAL TIME-STEP TO BE USED.                         
C             THE PERFORMANCE OF DODES IS SUBSTANTIALLY                 
C             INDEPENDENT OF THE VALUE OF DT CHOSEN BY THE USER.        
C             IT IS SUFFICIENT THAT THE USERS CHOICE FOR DT MERELY BE   
C             WITHIN SEVERAL ORDERS OF MAGNITUDE OF BEING CORRECT.      
C             THE VALUE OF DT WILL BE AUTOMATICALLY CHANGED BY DODES    
C             DURING THE INTEGRATION PROCESS IN SUCH A WAY AS TO GET    
C             THE SOLUTION, TO THE DESIRED ACCURACY, AT THE LEAST       
C             POSSIBLE COST.                                            
C    ERROR  - ERROR TOLERANCE ROUTINE.                                  
C                                                                       
C                 LOGICAL FUNCTION ERROR(X,NX,T,DT,ERRPAR,ERPUTS,E)     
C                                                                       
C             HAS AS INPUT                                              
C                                                                       
C               X,T    - X=X(T), THE APPROXIMATE SOLUTION FOR WHICH     
C                        AN ERROR CRITERION IS DESIRED.                 
C               NX     - THE LENGTH OF X.                               
C               DT     - THE TIME-STEP USED TO OBTAIN X(T).             
C               ERRPAR - TWO PARAMETERS, AS GIVEN TO DODES2.            
C               ERPUTS - THIS VARIABLE HAS THE SAME VALUE AS ERPUTS     
C                        IN THE CALL TO DODES2.                         
C               E      - THE REAL ABSOLUTE ERROR IN X(I) IS E(I),       
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C                                                                       
C             THE OUTPUT IS                                             
C                                                                       
C               ERRPAR - MAY BE ALTERED IF DESIRED.                     
C               E      - E(I) IS THE REAL TOLERABLE ABSOLUTE ERROR IN   
C                        X(I), I=1,...,NX. ALL E(I) MUST BE POSITIVE.   
C                                                                       
C             FUNCTION VALUE                                            
C                                                                       
C               ERROR  - ERROR=.TRUE. IF CONVERGED.                     
C                        ERROR=.FALSE. IF NOT.                          
C    ERRPAR - TWO PARAMETERS TO BE PASSED TO ERROR.                     
C    HANDLE - OUTPUT ROUTINE WITH A CALLING SEQUENCE OF THE FORM        
C                                                                       
C                     HANDLE(T0,X0,T1,X1,NX,DT,TSTOP,E)                 
C                                                                       
C             HANDLE WILL BE CALLED AT THE END OF EACH TIME-STEP.       
C                                                                       
C             THE INPUT TO HANDLE IS AS FOLLOWS                         
C                                                                       
C               X0,X1  - X0=X(T0) AND X1=X(T1).                         
C               T0,T1  - T0=T1 INDICATES A RESTART AND X1 IS FULL OF    
C                        GARBAGE.                                       
C               NX     - THE LENGTH OF THE SOLUTION VECTOR X.           
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE CURRENT FINAL TIME.                        
C               E      - E(I) GIVES THE REAL ABSOLUTE ERROR IN X1(I),   
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C                                                                       
C             THE OUTPUT FROM HANDLE MAY BE ANY OF                      
C                                                                       
C               X1     - MAY BE ALTERED IF DESIRED.                     
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE FINAL TIME VALUE.                          
C                                                                       
C             HANDLE SHOULD BE DECLARED EXTERNAL IN THE                 
C             SUBPROGRAM CALLING DODES.                                 
C    GLBMAX - IF (GLBMAX) THEN THE GLOBAL MAXIMUM ABSOLUTE VALUE OF     
C             THE SOLUTION IS TO BE RECORDED,                           
C             SEE COMMON /DODESM/ BELOW.                                
C                                                                       
C             IF THE ERROR SUBPROGRAM SUPPLIED BY THE USER IS DODESE,   
C             THEN GLBMAX DETERMINES WHETHER OR NOT THE GLOBAL          
C             MAXIMUM ABSOLUTE VALUE OF THE SOLUTION WILL BE USED IN    
C             THAT SUBPROGRAM.                                          
C    ERPUTS - IF (ERPUTS) THEN THE ERROR PER UNIT TIME-STEP CRITERION   
C             IS TO BE USED IN THE ERROR ROUTINE.                       
C             OTHERWISE, THE ERROR PER TIME-STEP CRITERION IS TO BE     
C             USED IN THE ERROR ROUTINE.                                
C                                                                       
C             IF THE ERROR SUBPROGRAM SUPPLIED BY THE USER IS DODESE,   
C             THEN ERPUTS DETERMINES WHETHER OR NOT THE ERROR           
C             PER UNIT-TIME-STEP OR ERROR PER TIME-STEP ERROR           
C             CRITERION WILL BE USED BY THAT SUBPROGRAM.                
C    KMAX   - THE MAXIMUM NUMBER OF COLUMNS ALLOWED IN THE              
C             EXTRAPOLATION PROCESS.                                    
C    MMAX   - THE MAXIMUM NUMBER OF LEVELS OF EXTRAPOLATION PERMITTED.  
C             MMAX.GE.KMAX+2 IS REQUIRED.                               
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    X      - X=X(TSTOP), THE FINAL VALUE FOR THE SOLUTION.             
C    TSTOP  - MAY BE ALTERED BY USER SUPPLIED ROUTINE HANDLE.           
C    DT     - PROPOSED TIME-STEP FOR THE NEXT STEP, IF ANY.             
C    ERRPAR - MAY BE ALTERED BY USER SUPPLIED ROUTINE ERROR.            
C                                                                       
C  SCRATCH SPACE OF LENGTH                                              
C                                                                       
C                      S(DODES2) .LE.                                   
C                                                                       
C    2*MMAX + NX*(KMAX+2)                                               
C                                                                       
C  DOUBLE PRECISION WORDS +                                             
C                                                                       
C    5*KMAX + 3*MMAX + 3 +                                              
C                                                                       
C    ( IF (GLBMAX) THEN 2*NX , OTHERWISE 0 ) +                          
C                                                                       
C    MAX ( 2*NX DOUBLE PRECISION + S(F) ,                               
C                                                                       
C          NX*(KMAX+1) +                                                
C                                                                       
C          MAX( KMAX DOUBLE PRECISION + KMAX , S(ERROR) ) ,             
C                                                                       
C          NX + S(HANDLE) )                                             
C                                                                       
C  INTEGER WORDS IS ALLOCATED.                                          
C                                                                       
C  ERROR STATES                                                         
C                                                                       
C     1 - NX.LT.1.                                                      
C     2 - DT HAS THE WRONG SIGN ON INPUT.                               
C     3 - DT=0 ON INPUT.                                                
C     4 - DT RETURNED BY HANDLE HAS THE WRONG SIGN.                     
C     5 - DT=0 WAS RETURNED BY HANDLE. (RECOVERABLE)                    
C     6 - THE ERROR DESIRED IN X(I) IS NOT POSITIVE. (RECOVERABLE)      
C     7 - DT=0. (RECOVERABLE)                                           
C     8 - CANNOT RAISE DT IN HANDLE WHEN .NOT.OKAY.                     
C     9 - KMAX.LT.1.                                                    
C    10 - KMAX.GT.MMAX-2.                                               
C                                                                       
C  INTERNAL NAMED COMMON USAGE -                                        
C                                                                       
C     DODESM HOLDS THE POINTER, IGMAX, TO THE REAL                      
C            VECTOR OF CURRENT MAXIMUM ABSOLUTE VALUES ATTAINED BY EACH 
C            COMPONENT OF THE SOLUTION, AND THE POINTER, IGMAXO, TO THE 
C            REAL VECTOR OF MAXIMUM ABSOLUTE VALUES ATTAINED BY EACH    
C            COMPONENT OF THE SOLUTION AS OF THE LAST TIME STEP.        
C            IGMAX=0 MEANS THESE VECTORS ARE NOT USED AND HAVE NOT BEEN 
C            ALLOCATED.                                                 
C     D0DESQ HOLDS THE DOUBLE PRECISION END-POINT VALUE AND THE REAL    
C            REMAINING ERROR FOR USE IN DODESQ.                         
C     D0DESP HOLDS THE POINTER TO THE DOUBLE PRECISION VECTOR           
C            F(T0,X(T0)).                                               
C                                                                       
      COMMON /DODESF/OKAY                                               
      COMMON /DODESM/IGMAX,IGMAXO                                       
      COMMON /D0DESQ/TEND,RERROR                                        
      COMMON /D0DESP/IFTX0                                              
C                                                                       
      DOUBLE PRECISION X(NX),TSTART,TSTOP,DT,TEND                       
      REAL ERRPAR(2),RERROR                                             
      LOGICAL GLBMAX,ERPUTS,OKAY                                        
      EXTERNAL F,ERROR,HANDLE                                           
C                                                                       
      DOUBLE PRECISION DELTA                                            
      EXTERNAL D0DESG,D0DES0,D0DESE                                     
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      INTEGER IS(1000)                                                  
      EQUIVALENCE (DS(1),RS(1)),(DS(1),IS(1))                           
C                                                                       
      IF (TSTART.EQ.TSTOP) GO TO 20                                     
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C ... CHECK FOR ERRORS IN THE INPUT.                                    
C                                                                       
C/6S                                                                    
C     IF (NX.LT.1) CALL SETERR(16HDODES2 - NX.LT.1,16,1,2)              
C     IF ((DT/DABS(DT))*(TSTOP-TSTART).LT.0.0D0)                        
C    1   CALL SETERR(39HDODES2 - DT HAS THE WRONG SIGN ON INPUT,39,2,2) 
C     IF (TSTART+DT.EQ.TSTART)                                          
C    1   CALL SETERR(22HDODES2 - DT=0 ON INPUT,22,3,2)                  
C     IF (KMAX.LT.1) CALL SETERR(18HDODES2 - KMAX.LT.1,18,9,2)          
C     IF (KMAX.GT.MMAX-2)                                               
C    1   CALL SETERR(23HDODES2 - KMAX.GT.MMAX-2,23,10,2)                
C/7S                                                                    
      IF (NX.LT.1) CALL SETERR('DODES2 - NX.LT.1',16,1,2)               
      IF ((DT/DABS(DT))*(TSTOP-TSTART).LT.0.0D0)                        
     1   CALL SETERR('DODES2 - DT HAS THE WRONG SIGN ON INPUT',39,2,2)  
      IF (TSTART+DT.EQ.TSTART)                                          
     1   CALL SETERR('DODES2 - DT=0 ON INPUT',22,3,2)                   
      IF (KMAX.LT.1) CALL SETERR('DODES2 - KMAX.LT.1',18,9,2)           
      IF (KMAX.GT.MMAX-2)                                               
     1   CALL SETERR('DODES2 - KMAX.GT.MMAX-2',23,10,2)                 
C/                                                                      
C                                                                       
C ... ALLOCATE AND LOAD N WITH THE SEQUENCE 1,2,3,4,6,8,12,... .        
C                                                                       
      IN=ISTKGT(MMAX,2)                                                 
C                                                                       
      DO 10 I=1,MMAX                                                    
         J=IN+I-1                                                       
         IF (I.LT.4) IS(J)=I                                            
 10      IF (I.GT.3) IS(J)=2*IS(J-2)                                    
C                                                                       
C ... DECIDE IF HAVE ERROR PER UNIT-TIME-STEP OR PER TIME-STEP.         
C                                                                       
      DELTA=0.0D0                                                       
      IF (ERPUTS) DELTA=1.0D0                                           
C                                                                       
C ... LOAD THE GLOBAL MAXIMUM ABSOLUTE VALUES OF THE SOLUTION,          
C ... IF NECESSARY.                                                     
C                                                                       
      IGMAX=0                                                           
      IF (GLBMAX) IGMAX=ISTKGT(2*NX,3)                                  
      IGMAXO=IGMAX+NX                                                   
      IF (GLBMAX) CALL D0DESL(X,NX,RS(IGMAX),RS(IGMAXO))                
C                                                                       
      TEND=TSTOP                                                        
      RERROR=ERRPAR(2)                                                  
C                                                                       
      IFTX0=ISTKGT(NX,4)                                                
C                                                                       
      CALL DSXTRP(TSTART,TSTOP,D0DESG,F,1.0D0,2.0D0,DELTA,X,NX,DT,      
     1            IS(IN),KMAX,MMAX,.FALSE.,ERROR,D0DESE,ERRPAR,HANDLE,  
     2            D0DES0,0.95E0)                                        
C                                                                       
      IF (NERROR(NERR).NE.0) CALL ERROFF                                
C/6S                                                                    
C     IF (NERR.EQ.13) CALL SETERR                                       
C    1   (13HDODES2 - DT=0,13,7,1)                                      
C     IF (NERR.EQ.14) CALL SETERR                                       
C    1   (32HDODES2 - DT=0 RETURNED BY HANDLE,32,5,1)                   
C     IF (NERR.EQ.17) CALL SETERR                                       
C    1   (50HDODES2 - THE ERROR DESIRED IN X(I) IS NOT POSITIVE,50,6,1) 
C/7S                                                                    
      IF (NERR.EQ.13) CALL SETERR                                       
     1   ('DODES2 - DT=0',13,7,1)                                       
      IF (NERR.EQ.14) CALL SETERR                                       
     1   ('DODES2 - DT=0 RETURNED BY HANDLE',32,5,1)                    
      IF (NERR.EQ.17) CALL SETERR                                       
     1   ('DODES2 - THE ERROR DESIRED IN X(I) IS NOT POSITIVE',50,6,1)  
C/                                                                      
C                                                                       
      CALL LEAVE                                                        
C                                                                       
 20   RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION D0DESE(X,NX,T,DT,ERRPAR,DELTA,E,ERROR)           
C                                                                       
C  ERROR FILTER FOR KEEPING TRACK OF THE MAXIMUM ABSOLUTE VALUE OF THE  
C  SOLUTION, IF IT IS NECESSARY.                                        
C                                                                       
      COMMON /DODESM/IGMAX,IGMAXO                                       
C                                                                       
      DOUBLE PRECISION X(NX),T,DT,DELTA                                 
      REAL ERRPAR(2),E(NX)                                              
      LOGICAL ERROR                                                     
      EXTERNAL ERROR                                                    
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),RS(1))                                         
C                                                                       
C ... RAISE THE GLOBAL MAXIMUM, IF NECESSARY.                           
C                                                                       
      IF (IGMAX.GT.0) CALL D0DESU(X,NX,RS(IGMAX))                       
C                                                                       
      D0DESE=ERROR(X,NX,T,DT,ERRPAR,DELTA.EQ.1.0D0,E)                   
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE D0DES0(T0,X0,T1,X1,NX,DT,TSTOP,OK,HANDLE,E)            
C                                                                       
C  OUTPUT FILTER FOR USE WITH GLOBAL MAXIMUM ERROR OPTION IN DODES.     
C                                                                       
      COMMON /DODESM/IGMAX,IGMAXO                                       
C                                                                       
      DOUBLE PRECISION T0,X0(NX),T1,X1(NX),DT,TSTOP                     
      REAL E(NX)                                                        
      LOGICAL OK                                                        
      EXTERNAL HANDLE                                                   
C                                                                       
      DOUBLE PRECISION TEMP                                             
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),RS(1))                                         
C                                                                       
C ... RAISE THE GLOBAL MAXIMUM, IF NECESSARY.                           
C                                                                       
      IF (T0.NE.T1.AND.IGMAX.GT.0)                                      
     1   CALL MOVEFR(NX,RS(IGMAX),RS(IGMAXO))                           
C                                                                       
C ... OTHERWISE, RETURN IT TO ITS PREVIOUS VALUE.                       
C                                                                       
      IF (T0.EQ.T1.AND.IGMAX.GT.0)                                      
     1   CALL MOVEFR(NX,RS(IGMAXO),RS(IGMAX))                           
C                                                                       
      TEMP=DT                                                           
C                                                                       
      CALL HANDLE(T0,X0,T1,X1,NX,DT,TSTOP,E)                            
C                                                                       
C/6S                                                                    
C     IF ((DT/DABS(DT))*(TSTOP-T1).LT.0.0D0) CALL SETERR                
C    1   (49HDODES2 - DT RETURNED BY HANDLE HAS THE WRONG SIGN,49,4,2)  
C/7S                                                                    
      IF ((DT/DABS(DT))*(TSTOP-T1).LT.0.0D0) CALL SETERR                
     1   ('DODES2 - DT RETURNED BY HANDLE HAS THE WRONG SIGN',49,4,2)   
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (.NOT.OK .AND. DABS(TEMP).LT.DABS(DT)) CALL SETERR             
C    1   (49HDODES2 - CANNOT RAISE DT IN HANDLE WHEN .NOT.OKAY,49,8,2)  
C/7S                                                                    
      IF (.NOT.OK .AND. DABS(TEMP).LT.DABS(DT)) CALL SETERR             
     1   ('DODES2 - CANNOT RAISE DT IN HANDLE WHEN .NOT.OKAY',49,8,2)   
C/                                                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE D0DESL(X,NX,GLBMAX,GLBMXO)                             
C                                                                       
C  TO LOAD THE GLOBAL MAXIMUM.                                          
C                                                                       
      DOUBLE PRECISION X(NX)                                            
      REAL GLBMAX(NX),GLBMXO(NX)                                        
C                                                                       
      DO 10 I=1,NX                                                      
         GLBMAX(I)=ABS(SNGL(X(I)))                                      
 10      GLBMXO(I)=GLBMAX(I)                                            
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE D0DESG(T0,X0,T1,X1,NX,NT,F,OK)                         
C                                                                       
C  GRAGG'S MODIFIED MID-POINT RULE FOR Y' = F(T,Y).                     
C                                                                       
C  INPUT                                                                
C                                                                       
C    T0     - INITIAL TIME.                                             
C    X0     - X0=X(T0).                                                 
C    T1     - FINAL TIME.                                               
C    NX     - THE LENGTH OF THE SOLUTION VECTOR.                        
C    NT     - THE NUMBER OF TIME STEPS TO BE USED IS 2*NT.              
C    F      - THE RIGHT-HAND SIDE.                                      
C             THE CALL F(T,X,NX,FTX) SHOULD SET FTX=F(T,X).             
C             IF IT CANNOT COMPUTE F(T,X), OKAY=.FALSE. SHOULD BE       
C             RETURNED IN COMMON /DODESF/OKAY .                         
C                                                                       
C  COMMON INPUT -                                                       
C                                                                       
C    IGMAX  - THE POINTER TO THE REAL VECTOR OF CURRENT MAXIMUM ABSOLUTE
C             VALUES ATTAINED BY EACH COMPONENT OF THE SOLUTION.        
C             IGMAX=0 MEANS THIS VECTOR IS NOT USED.                    
C    IFTX0  - THE POINTER TO THE DOUBLE PRECISION VECTOR F(T0,X0).      
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    X1     - THE APPROXIMATE SOLUTION AT TIME T1.                      
C                                                                       
C  COMMON OUTPUT -                                                      
C                                                                       
C    OK     - OK=.TRUE., IF SUCCESSFUL. OTHERWISE, OK=.FALSE. .         
C    IGMAX  - THE UPDATED MAXIMUM VALUES.                               
C    IFTX0  - THE VECTOR FTX0 IS LOADED IF NT=1.                        
C                                                                       
C  SCRATCH SPACE ALLOCATED - 2*NX DOUBLE PRECISION WORDS.               
C                                                                       
C  ERROR STATES - NONE.                                                 
C                                                                       
      COMMON /DODESM/IGMAX,IGMAXO                                       
      COMMON /D0DESP/IFTX0                                              
      COMMON /DODESF/OKAY                                               
C                                                                       
      DOUBLE PRECISION T0,X0(NX),T1,X1(NX)                              
      LOGICAL OK,OKAY                                                   
      EXTERNAL F                                                        
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      DOUBLE PRECISION WS(1)                                            
      EQUIVALENCE (DS(1),WS(1))                                         
C                                                                       
C ... ALLOCATE SPACE FOR THE FUNCTION VALUES FTX AND SCRATCH VALUES XL. 
C                                                                       
      IFTX=ISTKGT(2*NX,4)                                               
      IXL=IFTX+NX                                                       
C                                                                       
      CALL D0DESR(T0,X0,T1,X1,NX,NT,F,WS(IFTX),WS(IXL),WS(IFTX0),IGMAX) 
C                                                                       
      OK=OKAY                                                           
C                                                                       
      CALL ISTKRL(1)                                                    
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE D0DESR(T0,X0,T1,X1,NX,NT,F,FTX,XL,FTX0,IGMAX)          
C                                                                       
C  ACTUAL GRAGGS MODIFIED MID-POINT RULE.                               
C                                                                       
      COMMON /DODESF/OKAY                                               
C                                                                       
      DOUBLE PRECISION T0,X0(NX),T1,X1(NX),FTX(NX),XL(NX),FTX0(NX)      
      LOGICAL OKAY                                                      
      EXTERNAL F                                                        
C                                                                       
      DOUBLE PRECISION DT,DT2,T,TEMP                                    
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),RS(1))                                         
C                                                                       
      OKAY=.TRUE.                                                       
      DT=(T1-T0)/FLOAT(NT)                                              
      DT2=0.5D0*DT                                                      
      NT2=2*NT                                                          
C                                                                       
C ... IF THIS IS THE FIRST CALL, COMPUTE FTX0=F(T0,X(T0))               
C                                                                       
      IF (NT.EQ.1) CALL F(T0,X0,NX,FTX0)                                
      IF (.NOT.OKAY) GO TO 50                                           
C                                                                       
C ... COMPUTE FORWARD-EULER INITIAL TIME-STEP.                          
C                                                                       
      DO 10 I=1,NX                                                      
         XL(I)=X0(I)                                                    
 10      X1(I)=X0(I)+DT2*FTX0(I)                                        
C                                                                       
C ... GET THE APPROXIMATE SOLUTION AT THE REST OF THE MESH-POINTS       
C ... USING THE MID-POINT RULE.                                         
C                                                                       
      DO 30 IT=2,NT2                                                    
         T=T0+FLOAT(IT-1)*DT2                                           
         CALL F(T,X1,NX,FTX)                                            
         IF (.NOT.OKAY) GO TO 50                                        
C                                                                       
         DO 20 I=1,NX                                                   
            TEMP=X1(I)                                                  
            X1(I)=XL(I)+DT*FTX(I)                                       
 20         XL(I)=TEMP                                                  
C                                                                       
C ...... IF NEED TO, RAISE THE GLOBAL MAXIMUM.                          
C                                                                       
 30      IF (IGMAX.GT.0) CALL D0DESU(X1,NX,RS(IGMAX))                   
C                                                                       
C ... SMOOTH AT THE END POINT.                                          
C                                                                       
      CALL F(T1,X1,NX,FTX)                                              
      IF (.NOT.OKAY) GO TO 50                                           
C                                                                       
      DO 40 I=1,NX                                                      
 40      X1(I)=0.5D0*(X1(I)+(XL(I)+DT2*FTX(I)))                         
C                                                                       
C ... IF NEED TO, RAISE THE GLOBAL MAXIMUM.                             
C                                                                       
      IF (IGMAX.GT.0) CALL D0DESU(X1,NX,RS(IGMAX))                      
C                                                                       
 50   RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE D0DESU(X,NX,GLBMAX)                                    
C                                                                       
C  TO RAISE THE GLOBAL MAXIMUM.                                         
C                                                                       
      DOUBLE PRECISION X(NX)                                            
      REAL GLBMAX(NX)                                                   
C                                                                       
      DO 10 I=1,NX                                                      
 10      GLBMAX(I)=AMAX1(GLBMAX(I),ABS(SNGL(X(I))))                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DSXTRP(TSTART,TSTOP,XA,F,BETA,GAMMA,DELTA,X,NX,DT,N,   
     1                  KMAX,MMAX,XPOLY,ERROR,SERROR,ERRPAR,OUTPUT,SOUT,
     2                  PESPAR)                                         
C                                                                       
C  LET A VECTOR VALUED FUNCTION A(H) OF LENGTH NX PRODUCE AN            
C  APPROXIMATION TO X(T1) WHEN GIVEN T0, X(T0) AND H=(T1-T0)/N WHERE    
C  N IS AN INTEGER AND X(T) IS SOME UNKNOWN VECTOR-VALUED FUNCTION      
C  OF TIME.                                                             
C                                                                       
C  ASSUME THAT                                                          
C                                                                       
C  A(H) = X(T1) +                                                       
C                                                                       
C         ABS(T1-T0)**BETA * SUM(J=1,...,INFINITY)(C(J)*H**(J*GAMMA))   
C                                                                       
C  WHERE THE C(J) ARE UNKNOWN VECTORS INDEPENDENT OF H.                 
C                                                                       
C  THIS ROUTINE THEN TAKES THE VALUE X=X(TSTART) AND, USING AN INITIAL  
C  VALUE OF T1=TSTART+DT, SEQUENTIALLY EVALUATES X(T1) UNTIL T1=TSTOP.  
C                                                                       
C  THE EVALUATION OF X(T1) IS ACCOMPLISHED USING EXTRAPOLATION TO       
C  THE LIMIT OF THE RESULTS OF A(H) FOR H=DT/N(M), M=1,...,MMAX.        
C                                                                       
C  INPUT                                                                
C                                                                       
C    TSTART - THE INITIAL VALUE FOR TIME.                               
C    TSTOP  - THE FINAL VALUE FOR TIME.                                 
C    XA     - CALL XA(T0,X0,T1,X1,NX,N,F,OK) SHOULD RETURN THE          
C             APPROXIMATION X1=A(H) TO X(T1) GIVEN T0, X0=X(T0) AND N.  
C             OK=.TRUE. SHOULD BE RETURNED IF X1 HAS BEEN SUCCESSFULLY  
C             COMPUTED. OTHERWISE, OK=.FALSE. SHOULD BE RETURNED.       
C             THIS WILL CAUSE A RESTART OF THE PROCESS FROM TIME T=T0,  
C             WITH A DEFAULT LOWERING OF DT BY 10**3.                   
C             F IS A SUBPROGRAM NAME, AS PASSED TO DSXTRP.              
C    F      - A SUBPROGRAM NAME WHICH IS PASSED TO XA.                  
C    BETA   - THE POWER SERIES FOR THE ERROR IN A(H) HAS A              
C             MULTIPLICATIVE FACTOR OF ABS(T1-T0)**BETA IN FRONT OF IT. 
C    GAMMA  - THE POWER SERIES FOR THE ERROR IN A(H) IS IN THE          
C             VARIABLE H**GAMMA.                                        
C    DELTA  - THE ERROR CRITERION IS PROPORTIONAL TO                    
C             ABS(T1-T0)**DELTA.                                        
C    X      - THE INITIAL VALUES X=X(TSTART).                           
C    NX     - THE LENGTH OF THE SOLUTION VECTOR X.                      
C    DT     - THE INITIAL TIME-STEP TO BE USED.                         
C             THE PERFORMANCE OF DSXTRP IS SUBSTANTIALLY                
C             INDEPENDENT OF THE VALUE OF DT CHOSEN BY THE USER.        
C             IT IS SUFFICIENT THAT THE USERS CHOICE FOR DT MERELY BE   
C             WITHIN SEVERAL ORDERS OF MAGNITUDE OF BEING CORRECT.      
C             THE VALUE OF DT WILL BE AUTOMATICALLY CHANGED BY DSXTRP   
C             DURING THE INTEGRATION PROCESS IN SUCH A WAY AS TO GET    
C             THE SOLUTION, TO THE DESIRED ACCURACY, AT THE LEAST       
C             POSSIBLE COST.                                            
C    N      - H=(T1-T0)/N(M) WILL BE USED AT THE M-TH LEVEL OF          
C             EXTRAPOLATION, M=1,...,MMAX.                              
C    KMAX   - THE MAXIMAL NUMBER OF COLUMNS KEPT IN THE EXTRAPOLATION   
C             PROCESS.                                                  
C    MMAX   - THE MAXIMUM LEVEL OF EXTRAPOLATION TO BE USED.            
C             MMAX.GE.KMAX+2 IS REQUIRED.                               
C    XPOLY  - IF (XPOLY) THEN USE POLYNOMIAL EXTRAPOLATION.             
C             IF (.NOT.XPOLY) THEN USE RATIONAL EXTRAPOLATION.          
C    ERROR  - A SUBPROGRAM NAME WHICH IS PASSED TO SERROR.              
C    SERROR - A LOGICAL FUNCTION OF THE FORM                            
C                                                                       
C              LOGICAL FUNCTION SERROR(X1,NX,T1,DT,ERRPAR,DELTA,E,ERROR)
C                                                                       
C             THE INPUT TO SERROR IS                                    
C                                                                       
C               X1     - X1=X(T1), THE APPROXIMATE SOLUTION FOR WHICH   
C                        AN ERROR CRITERION IS DESIRED.                 
C               NX     - THE LENGTH OF THE SOLUTION VECTOR.             
C               T1     - THE CURRENT VALUE OF TIME, X1=X(T1).           
C               DT     - DT=T1-T0.                                      
C               ERRPAR - TWO PARAMETERS, AS PASSED TO DSXTRP.           
C               DELTA  - AS PASSED TO DSXTRP.                           
C               E      - E(I) IS THE REAL ABSOLUTE ERROR IN X1(I),      
C                        I=1,...,NX, FOR THE SINGLE CURRENT TIME-STEP.  
C               ERROR  - THE NAME OF A SUBPROGRAM, AS PASSED TO DSXTRP. 
C                                                                       
C             THE OUTPUT FROM SERROR IS                                 
C                                                                       
C               E      - E(I) GIVES THE DESIRED REAL ABSOLUTE ERROR     
C                        IN THE I-TH COMPONENT OF X1=X(T1), I=1,...,NX. 
C               ERRPAR - MAY BE ALTERED IF DESIRED.                     
C                                                                       
C             FUNCTION VALUE -                                          
C                                                                       
C               SERROR - SERROR.TRUE. IF CONVERGED.                     
C                        SERROR=.FALSE. IF NOT.                         
C                                                                       
C    ERRPAR - A VECTOR OF LENGTH TWO TO BE PASSED TO ERROR.             
C    OUTPUT - A SUBPROGRAM NAME TO BE PASSED TO SOUT.                   
C    SOUT   - THE OUTPUT SUBROUTINE                                     
C                                                                       
C                 SOUT(T0,X0,T1,X1,NX,DT,TSTOP,OK,OUTPUT,E)             
C                                                                       
C             WILL BE CALLED AT THE END OF EACH TIME STEP.              
C                                                                       
C             THE INPUT TO SOUT IS                                      
C                                                                       
C               T0     - THE OLD VALUE OF T1.                           
C               X0     - X0=X(T0)                                       
C               T1     - CURRENT VALUE OF TIME.                         
C               X1     - X1=X(T1).                                      
C               NX     - THE LENGTH OF THE SOLUTION VECTOR.             
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE CURRENT VALUE OF THE FINAL-TIME.           
C               OK     - AS RETURNED BY XA.                             
C               OUTPUT - A SUBPROGRAM NAME, AS PASSED TO DSXTRP.        
C               E      - THE REAL ABSOLUTE ERROR IN X1(I)=X(T1)(I)      
C                        IS E(I), I=1,...,NX, FOR THE SINGLE TIME-STEP  
C                        FROM T0 TO T1.                                 
C                                                                       
C             THE OUTPUT FROM SOUT MAY BE ANY OF                        
C                                                                       
C               X1     - X1=X(T1).                                      
C               DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP.      
C               TSTOP  - THE FINAL-TIME VALUE.                          
C                                                                       
C    PESPAR - THE OPTIMAL TIME-STEP DT IS MULTIPLIED BY PESPAR          
C             BEFORE BEING USED FOR THE NEXT STEP.                      
C             0.LT.PESPAR.LE.1 IS REQUIRED.                             
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    X      - X=X(TSTOP), THE FINAL VALUE FOR THE SOLUTION.             
C    DT     - THE PROPOSED TIME-STEP FOR THE NEXT STEP, IF ANY.         
C    TSTOP  - MAY BE ALTERED BY USER SUPPLIED ROUTINE SOUT.             
C    ERRPAR - MAY BE ALTERED BY USER SUPPLIED ROUTINE ERROR.            
C                                                                       
C  SCRATCH SPACE OF LENGTH                                              
C                                                                       
C                   S(DSXTRP) .LE.                                      
C                                                                       
C    2*MMAX + NX*(KMAX+1)                                               
C                                                                       
C  DOUBLE PRECISION WORDS +                                             
C                                                                       
C    5*KMAX + 2*MMAX + 3 +                                              
C                                                                       
C    MAX( S(XA), NX*(KMAX+1) +                                          
C                                                                       
C         MAX( KMAX DOUBLE PRECISION + KMAX , S(ERROR) ) ,              
C                                                                       
C         NX + S(SOUT) )                                                
C                                                                       
C  INTEGER WORDS IS ALLOCATED.                                          
C                                                                       
C  ERROR STATES                                                         
C                                                                       
C     1 - BETA.LT.0.                                                    
C     2 - GAMMA.LE.0.                                                   
C     3 - DELTA.LT.0.                                                   
C     4 - NX.LT.1.                                                      
C     5 - DT=0 ON INPUT.                                                
C     6 - N(1).LT.1.                                                    
C     7 - KMAX.LT.1.                                                    
C     8 - MMAX.LT.KMAX+2.                                               
C     9 - PESPAR NOT IN (0,1).                                          
C    10 - BETA-DELTA+GAMMA.LE.0.                                        
C    11 - N IS NOT MONOTONE INCREASING.                                 
C    12 - DT HAS THE WRONG SIGN.                                        
C    13 - DT=0. (RECOVERABLE)                                           
C    14 - DT=0 RETURNED BY SOUT. (RECOVERABLE)                          
C    15 - DT RETURNED BY SOUT HAS THE WRONG SIGN.                       
C    16 - DT RAISED BY SOUT WHEN OK=.FALSE..                            
C    17 - E(I).LE.0 RETURNED BY ERROR. (RECOVERABLE)                    
C    18 - SOMEBODY IS LEAVING STUFF ON THE STACK.                       
C                                                                       
C  WHILE DSXTRP IS EXECUTING, COMMON /D9XTRP/ CONTAINS THE FOLLOWING    
C  INFORMATION -                                                        
C                                                                       
C    MC     - THE CURRENT LEVEL OF EXTRAPOLATION.                       
C    KOPTC  - THE OPTIMAL NUMBER OF COLUMNS IN THE LOZENGE.             
C             IF KOPTC IS ZERO, THEN THE NEXT THREE ITEMS ARE           
C             MEANINGLESS.                                              
C    ICOST  - THE POINTER TO THE REAL COST/UNIT TIME-STEP ARRAY.        
C    KHIC   - THE ACTIVE LENGTH OF THE COST ARRAY.                      
C    IHOPT  - THE POINTER TO THE REAL ARRAY OF OPTIMAL STEP-SIZES       
C             FOR A GIVEN NUMBER OF COLUMNS, ITS LENGTH IS KHIC.        
C    IRCNT  - IRCNT LOGARITHMIC BISECTION STEPS ARE TO BE DONE.         
C    HUP    - DT CANNOT GROW BY MORE THAN EXP(HUP) PER STEP.            
C             HUP WILL BE MULTIPLIED BY 2 AFTER EACH SUCCESSFUL         
C             TIME STEP. THIS VALUE IS TYPE REAL.                       
C    ILOZNG - THE POINTER TO THE LOWER EDGE OF THE DOUBLE PRECISION     
C             EXTRAPOLATION LOZENGE.                                    
C    KMAXC  - THE LENGTH OF THE BOTTOM EDGE OF THE LOZENGE IS           
C             MIN(KMAXC,MC).                                            
C                                                                       
      COMMON /D9XTRP/MC,KOPTC,ICOST,KHIC,IHOPT,IRCNT,HUP,ILOZNG,KMAXC   
C                                                                       
      DOUBLE PRECISION TSTART,TSTOP,BETA,GAMMA,DELTA,X(NX),DT           
      REAL ERRPAR(2),PESPAR                                             
      INTEGER N(MMAX)                                                   
      LOGICAL XPOLY,ERROR,SERROR                                        
      EXTERNAL XA,F,ERROR,SERROR,OUTPUT,SOUT                            
C                                                                       
      DOUBLE PRECISION DFLOAT, DLOG                                     
      REAL HUP                                                          
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      DOUBLE PRECISION WS(1)                                            
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),WS(1)),(DS(1),RS(1))                           
C                                                                       
      DFLOAT(IDUMMY)=DBLE(FLOAT(IDUMMY))                                
C                                                                       
      IF (TSTART.EQ.TSTOP) GO TO 50                                     
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (BETA.LT.0.0D0) CALL SETERR(18HDSXTRP - BETA.LT.0,18,1,2)      
C     IF (GAMMA.LE.0.0D0) CALL SETERR(19HDSXTRP - GAMMA.LE.0,19,2,2)    
C     IF (DELTA.LT.0.0D0) CALL SETERR(19HDSXTRP - DELTA.LT.0,19,3,2)    
C     IF (NX.LT.1) CALL SETERR(16HDSXTRP - NX.LT.1,16,4,2)              
C     IF (DT.EQ.0.0D0) CALL SETERR(22HDSXTRP - DT=0 ON INPUT,22,5,2)    
C     IF (N(1).LT.1) CALL SETERR(18HDSXTRP - N(1).LT.1,18,6,2)          
C     IF (KMAX.LT.1) CALL SETERR(18HDSXTRP - KMAX.LT.1,18,7,2)          
C     IF (MMAX.LT.KMAX+2) CALL SETERR(23HDSXTRP - MMAX.LT.KMAX+2,23,8,2)
C     IF (PESPAR.LE.0.0E0.OR.PESPAR.GT.1.0E0) CALL SETERR               
C    1   (28HDSXTRP - PESPAR NOT IN (0,1),28,9,2)                       
C     IF (BETA-DELTA+GAMMA.LE.0.0D0) CALL SETERR                        
C    1   (30HDSXTRP - BETA-DELTA+GAMMA.LE.0,30,10,2)                    
C/7S                                                                    
      IF (BETA.LT.0.0D0) CALL SETERR('DSXTRP - BETA.LT.0',18,1,2)       
      IF (GAMMA.LE.0.0D0) CALL SETERR('DSXTRP - GAMMA.LE.0',19,2,2)     
      IF (DELTA.LT.0.0D0) CALL SETERR('DSXTRP - DELTA.LT.0',19,3,2)     
      IF (NX.LT.1) CALL SETERR('DSXTRP - NX.LT.1',16,4,2)               
      IF (DT.EQ.0.0D0) CALL SETERR('DSXTRP - DT=0 ON INPUT',22,5,2)     
      IF (N(1).LT.1) CALL SETERR('DSXTRP - N(1).LT.1',18,6,2)           
      IF (KMAX.LT.1) CALL SETERR('DSXTRP - KMAX.LT.1',18,7,2)           
      IF (MMAX.LT.KMAX+2) CALL SETERR('DSXTRP - MMAX.LT.KMAX+2',23,8,2) 
      IF (PESPAR.LE.0.0E0.OR.PESPAR.GT.1.0E0) CALL SETERR               
     1   ('DSXTRP - PESPAR NOT IN (0,1)',28,9,2)                        
      IF (BETA-DELTA+GAMMA.LE.0.0D0) CALL SETERR                        
     1   ('DSXTRP - BETA-DELTA+GAMMA.LE.0',30,10,2)                     
C/                                                                      
C                                                                       
C ... ALLOCATE AND LOAD THE ARRAY LOGN WITH LOG(N(I)).                  
C                                                                       
      ILOGN=ISTKGT(MMAX,4)                                              
      WS(ILOGN)=DLOG(DFLOAT(N(1)))                                      
      I=ILOGN+1                                                         
      DO 10 J=2,MMAX                                                    
C/6S                                                                    
C        IF (N(J-1).GE.N(J)) CALL SETERR                                
C    1      (37HDSXTRP - N IS NOT MONOTONE INCREASING,37,11,2)          
C/7S                                                                    
         IF (N(J-1).GE.N(J)) CALL SETERR                                
     1      ('DSXTRP - N IS NOT MONOTONE INCREASING',37,11,2)           
C/                                                                      
         WS(I)=DLOG(DFLOAT(N(J)))                                       
 10      I=I+1                                                          
C                                                                       
C ... ALLOCATE CURRENT AND OLD OPTIMAL STEP-SIZE ARRAYS.                
C                                                                       
      IHOPT=ISTKGT(KMAX+1,3)                                            
      IHOPTO=ISTKGT(KMAX+1,3)                                           
C                                                                       
C ... ALLOCATE AND LOAD THE ARRAY NG WITH N(J)**GAMMA.                  
C                                                                       
      ING=ISTKGT(MMAX,4)                                                
      I=ING                                                             
      DO 20 J=1,MMAX                                                    
         WS(I)=DFLOAT(N(J))**GAMMA                                      
 20      I=I+1                                                          
C                                                                       
C ... ALLOCATE SPACE FOR X1 (THE SOLUTION AT TIME T1),                  
C ... AND A SCRATCH ARRAY F.                                            
C                                                                       
      IX1=ISTKGT(NX,4)                                                  
      KMAXC=KMAX                                                        
      IF=ISTKGT(KMAX,3)                                                 
C                                                                       
C ... ALLOCATE AND LOAD POW(J) WITH 1/(BETA-DELTA+J*GAMMA).             
C                                                                       
      IPOW=ISTKGT(KMAX,3)                                               
      I=IPOW                                                            
      DO 30 J=1,KMAX                                                    
         RS(I)=1.0E0/(BETA-DELTA+FLOAT(J)*GAMMA)                        
 30      I=I+1                                                          
C                                                                       
C ... ALLOCATE AND LOAD ARRAYS WORK AND LWORK WITH                      
C ... SUM(I=1,...,J)(N(I)) AND LOG(WORK(J)), RESPECTIVELY.              
C                                                                       
      IWORK=ISTKGT(MMAX,3)                                              
      ILWORK=ISTKGT(MMAX,3)                                             
      IW=IWORK                                                          
      ILW=ILWORK                                                        
      RS(IW)=FLOAT(N(1))                                                
      RS(ILW)=ALOG(RS(IW))                                              
      DO 40 J=2,MMAX                                                    
         IW=IW+1                                                        
         ILW=ILW+1                                                      
         RS(IW)=RS(IW-1)+FLOAT(N(J))                                    
 40      RS(ILW)=ALOG(RS(IW))                                           
C                                                                       
C ... ALLOCATE THE COST/UNIT TIME-STEP ARRAY.                           
C                                                                       
      ICOST=ISTKGT(KMAX+1,3)                                            
C                                                                       
C ... ALLOCATE THE EXTRAPOLATION LOZENGE SO THAT ISTKMD CAN             
C ... BE USED TO LET IT GROW ONLY AS NEEDED.                            
C                                                                       
      ILOZNG=ISTKGT(1,4)                                                
C                                                                       
      CALL D8XTRP(TSTART,TSTOP,XA,F,BETA,GAMMA,DELTA,NX,DT,N,KMAX,MMAX, 
     1            XPOLY,ERROR,SERROR,ERRPAR,OUTPUT,SOUT,PESPAR,         
     2            WS(ILOGN),RS(IHOPT),RS(IHOPTO),WS(ING),X,WS(IX1),     
     3            WS(ILOZNG),RS(IF),                                    
     4            RS(IPOW),RS(IWORK),RS(ILWORK),RS(ICOST))              
C                                                                       
      CALL LEAVE                                                        
C                                                                       
 50   RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE D8XTRP(TSTART,TSTOP,XA,FCN,BETA,GAMMA,DELTA,NX,DT,N,   
     1                  KMAX,MMAX,XPOLY,ERROR,SERROR,ERRPAR,OUTPUT,SOUT,
     2                  PESPAR,LOGN,HOPT,HOPTO,NG,X0,X1,LOZNGE,F,       
     3                  POW,WORK,LNWORK,COST)                           
C                                                                       
      COMMON /D9XTRP/MC,KOPTC,ICOST,KHIC,IHOPT,IRCNT,HUP,ILOZNG,KMAXC   
C                                                                       
      DOUBLE PRECISION TSTART,TSTOP,BETA,GAMMA,DELTA,DT,LOGN(MMAX),     
     1                 NG(MMAX),X0(NX),X1(NX),LOZNGE(NX,KMAX)           
      REAL ERRPAR(2),PESPAR,HOPT(1),HOPTO(1),                           
     1     F(KMAX),POW(KMAX),WORK(MMAX),LNWORK(MMAX),COST(1)            
C     REAL HOPT(KMAX+1),HOPTO(KMAX+1),COST(KMAX+1)                      
      INTEGER N(MMAX)                                                   
      LOGICAL XPOLY,ERROR,SERROR                                        
      EXTERNAL XA,FCN,ERROR,SERROR,OUTPUT,SOUT                          
C                                                                       
      DOUBLE PRECISION CLOSE,DTSAVE,TEMP,TBAD,T0,T1,DTNEW,D1MACH,DLOG   
      REAL LOGLO,LOGHI,HUP0,HUPMAX,HUP,RTEMP                            
      LOGICAL D8XTRD,LOGGED,FAILED,PRSTRT,D8XTRL,EEQ0,OK                
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),RS(1))                                         
C                                                                       
C  IF T0 AND T1 ARE ON THE SAME SIDE OF TSTOP AND T1 IS WITHIN          
C  CLOSE*ABS(DT) OF TSTOP, THEN T1=TSTOP IS CHOSEN.                     
C                                                                       
C  MRCNT LOGARITHMIC BISECTION STEPS ARE TAKEN AFTER EACH RESTART.      
C                                                                       
C  ALLOW HOPT(NEW)/HOPT(OLD) TO RISE BY AT MOST EXP(HUP) WHERE          
C  HUP=HUP0 AT THE END OF A RESTART SEQUENCE AND EACH SUCCESSFUL STEP   
C  THEREAFTER RESULTS IN HUP=MIN(2*HUP,HUPMAX).                         
C                                                                       
      DATA CLOSE/1.0D-2/,MRCNT/3/,HUP0/0.1E0/,HUPMAX/7.0E0/             
C                                                                       
C ... GET THE LOGARITHMS OF THE LARGEST AND SMALLEST NUMBERS IN THE     
C ... MACHINE.                                                          
C                                                                       
      LOGLO=DLOG(D1MACH(1))                                             
      LOGHI=DLOG(D1MACH(2))                                             
C                                                                       
C/6S                                                                    
C     IF ((DT/DABS(DT))*(TSTOP-TSTART).LE.0.0D0) CALL SETERR            
C    1   (30HDSXTRP - DT HAS THE WRONG SIGN,30,12,2)                    
C/7S                                                                    
      IF ((DT/DABS(DT))*(TSTOP-TSTART).LE.0.0D0) CALL SETERR            
     1   ('DSXTRP - DT HAS THE WRONG SIGN',30,12,2)                     
C/                                                                      
C                                                                       
C ... INITIALIZE.                                                       
C                                                                       
      MUSED=0                                                           
      KOPTC=0                                                           
      IRCNT=0                                                           
      KOPTO=1                                                           
      T1=TSTART                                                         
      HUP=HUPMAX                                                        
      PRSTRT=.FALSE.                                                    
C                                                                       
C ... TAKE A TIME-STEP.                                                 
C                                                                       
 10      T0=T1                                                          
         T1=T0+DT                                                       
C                                                                       
         IF ((DT/DABS(DT))*(T1-TSTOP).GE.0.0D0) T1=TSTOP                
         IF (DABS(T1-TSTOP).LE.CLOSE*DABS(DT)) T1=TSTOP                 
C                                                                       
         DT=T1-T0                                                       
C/6S                                                                    
C        IF (T0+DT.EQ.T0) CALL SETERR                                   
C    1      (13HDSXTRP - DT=0,13,13,1)                                  
C/7S                                                                    
         IF (T0+DT.EQ.T0) CALL SETERR                                   
     1      ('DSXTRP - DT=0',13,13,1)                                   
C/                                                                      
         IF (T0+DT.EQ.T0) GO TO 140                                     
C                                                                       
         DO 40 MT=1,MMAX                                                
C                                                                       
            FAILED=.FALSE.                                              
            LOGGED=.FALSE.                                              
            M=MT                                                        
            MC=M                                                        
C                                                                       
C ......... GET X1=T(DT/N(M)).                                          
C                                                                       
            CALL XA(T0,X0,T1,X1,NX,N(M),FCN,OK)                         
C                                                                       
            IF (.NOT.OK) GO TO 130                                      
C                                                                       
C ......... EXTRAPOLATE THE RESULTS.                                    
C                                                                       
            ITEMP=ILOZNG                                                
            IF (MIN0(M,KMAX).GT.MUSED) ITEMP=ISTKMD(NX*MIN0(M,KMAX))    
C/6S                                                                    
C           IF (ILOZNG.NE.ITEMP) CALL SETERR                            
C    1         (47HDSXTRP - SOMEBODY IS LEAVING STUFF ON THE STACK,47,  
C    2          18,2)                                                   
C/7S                                                                    
            IF (ILOZNG.NE.ITEMP) CALL SETERR                            
     1         ('DSXTRP - SOMEBODY IS LEAVING STUFF ON THE STACK',47,   
     2          18,2)                                                   
C/                                                                      
            MUSED=MAX0(M,MUSED)                                         
C                                                                       
            IE=ISTKGT(NX,3)                                             
            IE2=ISTKGT(MAX0(1,NX*MIN0(M-1,KMAX)),3)                     
            CALL DXTRAP(X1,M,NX,NG,KMAX,XPOLY,LOZNGE,RS(IE2),RS(IE))    
C                                                                       
C ......... CHECK FOR CONVERGENCE.                                      
C                                                                       
            IF (M.EQ.1) GO TO 40                                        
C                                                                       
            IF (SERROR(X1,NX,T1,DT,ERRPAR,DELTA,RS(IE),ERROR)) GO TO 60 
C                                                                       
            IF (M.LE.KOPTO) GO TO 40                                    
C                                                                       
C ......... SEE IF A RE-START IS NECESSARY.                             
C                                                                       
            EEQ0=D8XTRL(RS(IE2),RS(IE),NX,M,KMAX,POW,LOGLO)             
            LOGGED=.TRUE.                                               
C                                                                       
            IF (EEQ0) CALL ISTKRL(2)                                    
            IF (EEQ0) GO TO 140                                         
C                                                                       
C ......... IF WILL NOT CONVERGE IN THIS LOZENGE, RESTART.              
C                                                                       
            FAILED=D8XTRD(RS(IE2),NX,M,MMAX,KMAX,LOGN,BETA,GAMMA,DELTA, 
     1                    F,POW,L,LDONE,KOPTO)                          
C                                                                       
            IF (FAILED) GO TO 50                                        
C                                                                       
C ......... SEE IF A RE-START WOULD BE MORE EFFICIENT.                  
C                                                                       
            IF (M.EQ.KOPTO+1) GO TO 40                                  
C                                                                       
            CALL D8XTRO(RS(IE2),NX,M,MMAX,KMAX,LOGN,GAMMA,F,POW,KOPT,   
     1                  DT,HOPT,LNWORK,COST,LOGLO,LOGHI)                
C                                                                       
            RTEMP=SNGL(DT/HOPT(KOPT+1))                                 
            IF (RTEMP.GT.1.0E+3     .AND.                               
     1          WORK(M)+WORK(KOPT+2)*RTEMP.GT.WORK(L)) GO TO 40         
C                                                                       
            IF (RTEMP.GT.1.0E+3) GO TO 30                               
C                                                                       
            IRTEMP=IFIX(RTEMP)                                          
            RTEMP=(RTEMP-FLOAT(IRTEMP))*HOPT(KOPT+1)                    
            KF=KOPT+1                                                   
C                                                                       
            DO 20 I=1,KOPT                                              
 20            IF (ABS(RTEMP).LE.ABS(HOPT(I)).AND.KF.GT.KOPT) KF=I      
C                                                                       
            IF (RTEMP*SNGL(DT/DABS(DT)).GT.0.0E0     .AND.              
     1          WORK(M)+WORK(KOPT+2)*FLOAT(IRTEMP)+WORK(KF+1).GT.       
     2                       WORK(L)     ) GO TO 40                     
            IF (RTEMP*SNGL(DT/DABS(DT)).LE.0.0E0     .AND.              
     1          WORK(M)+WORK(KOPT+2)*FLOAT(IRTEMP).GT.                  
     2                       WORK(L)     ) GO TO 40                     
C                                                                       
C ......... SIGNAL A RESTART.                                           
C                                                                       
 30         T1=T0                                                       
            GO TO 70                                                    
C                                                                       
 40         IF (M.LT.MMAX) CALL ISTKRL(2)                               
C                                                                       
C ...... DID NOT CONVERGE, TRY IT OVER AGAIN.                           
C                                                                       
 50      T1=T0                                                          
C                                                                       
C ...... FIND THE OPTIMAL DT AND M FOR THE NEXT TIME-STEP.              
C                                                                       
 60      IF (.NOT.LOGGED)                                               
     1      EEQ0=D8XTRL(RS(IE2),RS(IE),NX,M,KMAX,POW,LOGLO)             
C                                                                       
         IF (EEQ0) CALL ISTKRL(2)                                       
         IF (EEQ0) GO TO 140                                            
C                                                                       
         CALL D8XTRO(RS(IE2),NX,M,MMAX,KMAX,LOGN,GAMMA,F,POW,KOPT,      
     1               DT,HOPT,LNWORK,COST,LOGLO,LOGHI)                   
C                                                                       
 70      CALL ISTKRL(1)                                                 
C                                                                       
C ...... IF HAVE A RESTART, SAVE THE BAD VALUE OF T AND                 
C ...... SET IRCNT=MRCNT. IF STEP WAS SUCCESSFUL, SAVE DT.              
C                                                                       
         IF (T0.EQ.T1) TBAD=T0+DT                                       
         IF (T0.EQ.T1.AND.T0.NE.TSTART) IRCNT=MRCNT                     
         IF (T0.NE.T1) DTSAVE=DT                                        
C                                                                       
C ...... GET THE DT FOR THE NEXT TIME-STEP.                             
C                                                                       
         KOPT=MIN0(KOPT,KOPTO+1)                                        
         KOPTM=KOPT                                                     
         IF (T0.EQ.T1) KOPTM=MIN0(KOPT,KOPTO)                           
         DTNEW=(DT/DABS(DT))*PESPAR*                                    
     1          AMIN1(ABS(SNGL(DT))*EXP(HUP),ABS(HOPT(KOPTM+1)))        
C                                                                       
C ...... TWO RESTARTS IN A ROW CAUSE DT TO DECREASE BY AT LEAST A       
C ...... FACTOR OF 10**3.                                               
C                                                                       
         IF (PRSTRT .AND. T0.EQ.T1)                                     
     1      DTNEW=(DT/DABS(DT))*DMIN1(DABS(DTNEW),DABS(DT)*1.0D-3)      
         DT=DTNEW                                                       
C                                                                       
         IF (T0.EQ.T1) PRSTRT=.TRUE.                                    
C                                                                       
C/6S                                                                    
C        IF (T1+DT.EQ.T1) CALL SETERR                                   
C    1      (13HDSXTRP - DT=0,13,13,1)                                  
C/7S                                                                    
         IF (T1+DT.EQ.T1) CALL SETERR                                   
     1      ('DSXTRP - DT=0',13,13,1)                                   
C/                                                                      
         IF (T1+DT.EQ.T1) CALL ISTKRL(1)                                
         IF (T1+DT.EQ.T1) GO TO 140                                     
C                                                                       
         KHI=MIN0(M,KMAX+1)                                             
C                                                                       
C ...... SET THE INFORMATION IN COMMON AND COMPUTE THE                  
C ...... COST/UNIT TIME-STEP FOR EACH LOZENGE SIZE.                     
C                                                                       
         KOPTC=KOPT                                                     
         KHIC=KHI                                                       
C                                                                       
         DO 80 K=1,KHI                                                  
 80         COST(K)=WORK(K+1)/ABS(HOPT(K))                              
C                                                                       
         IF (T0.EQ.TSTART.OR.T0.EQ.T1) GO TO 90                         
C                                                                       
C ...... IF ABS(HOPT(NEW)).LT.ABS(HOPT(OLD)), BE CONSERVATIVE.          
C                                                                       
         IDX=MIN0(KOPT+1,MOLD-1,M-1)                                    
         IF (DABS(DT).GT.1.0D-2*ABS(HOPT(IDX)))                         
     1   DT=DT*AMIN1(AMAX1(ABS(HOPT(IDX)/HOPTO(IDX)),1.0E-2),1.0E0)     
C/6S                                                                    
C        IF (T1+DT.EQ.T1) CALL SETERR                                   
C    1      (13HDSXTRP - DT=0,13,13,1)                                  
C/7S                                                                    
         IF (T1+DT.EQ.T1) CALL SETERR                                   
     1      ('DSXTRP - DT=0',13,13,1)                                   
C/                                                                      
         IF (T1+DT.EQ.T1) CALL ISTKRL(1)                                
         IF (T1+DT.EQ.T1) GO TO 140                                     
C                                                                       
         IF (IRCNT.LE.0) GO TO 90                                       
C                                                                       
C ...... LOGARITHMIC BISECTION FOR MRCNT STEPS AFTER A RESTART.         
C                                                                       
         IRCNT=IRCNT-1                                                  
         IF (IRCNT.EQ.0) HUP=0.5E0*HUP0                                 
         TEMP=(TBAD-T1)/DTSAVE                                          
         IF (TEMP.GT.0.99D0)                                            
     1       DT=(DT/DABS(DT))*DMIN1(DABS(TBAD-T1)*0.5D0,DABS(DT),       
     2                              DSQRT(TEMP)*DABS(DTSAVE))           
C/6S                                                                    
C        IF (T1+DT.EQ.T1) CALL SETERR                                   
C    1      (13HDSXTRP - DT=0,13,13,1)                                  
C/7S                                                                    
         IF (T1+DT.EQ.T1) CALL SETERR                                   
     1      ('DSXTRP - DT=0',13,13,1)                                   
C/                                                                      
         IF (T1+DT.EQ.T1) CALL ISTKRL(1)                                
         IF (T1+DT.EQ.T1) GO TO 140                                     
C                                                                       
C ...... OUTPUT THE RESULTS FOR THIS TIME-STEP.                         
C                                                                       
 90      CALL SOUT(T0,X0,T1,X1,NX,DT,TSTOP,OK,OUTPUT,RS(IE))            
C                                                                       
         CALL ISTKRL(1)                                                 
C                                                                       
C/6S                                                                    
C        IF (T1+DT.EQ.T1) CALL SETERR                                   
C    1      (30HDSXTRP - DT=0 RETURNED BY SOUT,30,14,1)                 
C/7S                                                                    
         IF (T1+DT.EQ.T1) CALL SETERR                                   
     1      ('DSXTRP - DT=0 RETURNED BY SOUT',30,14,1)                  
C/                                                                      
         IF (T1+DT.EQ.T1) GO TO 140                                     
C                                                                       
C/6S                                                                    
C        IF ((DT/DABS(DT))*(TSTOP-T1).LT.0.0D0) CALL SETERR             
C    1      (47HDSXTRP - DT RETURNED BY SOUT HAS THE WRONG SIGN,47,15,2)
C/7S                                                                    
         IF ((DT/DABS(DT))*(TSTOP-T1).LT.0.0D0) CALL SETERR             
     1      ('DSXTRP - DT RETURNED BY SOUT HAS THE WRONG SIGN',47,15,2) 
C/                                                                      
C                                                                       
         IF (T0.EQ.T1) KOPTO=MIN0(KOPT,KOPTO)                           
         IF (T0.EQ.T1) GO TO 120                                        
C                                                                       
C ...... UPDATE X0, HOPT AND HUP IF CONVERGED.                          
C                                                                       
         DO 100 I=1,NX                                                  
 100        X0(I)=X1(I)                                                 
C                                                                       
         DO 110 K=1,KHI                                                 
 110        HOPTO(K)=HOPT(K)                                            
C                                                                       
         HUP=AMIN1(2.0E0*HUP,HUPMAX)                                    
C                                                                       
         MOLD=M                                                         
         KOPTO=KOPT                                                     
         PRSTRT=.FALSE.                                                 
C                                                                       
 120     IF (T1.EQ.TSTOP) GO TO 140                                     
C                                                                       
C ...... GO DO THE NEXT TIME-STEP.                                      
C                                                                       
         GO TO 10                                                       
C                                                                       
C ...... THINGS ARE NOT OK.                                             
C                                                                       
 130     T1=T0                                                          
         PRSTRT=.TRUE.                                                  
C                                                                       
C ...... DO NOT LET THE USER RAISE DT WHEN OK=.FALSE.                   
C                                                                       
         TEMP=DT                                                        
C                                                                       
C ...... GO INTO A FULL RESTART SEQUENCE.                               
C                                                                       
         IF (T0.NE.TSTART) IRCNT=MRCNT                                  
         TBAD=T0+DT                                                     
C                                                                       
         CALL SOUT(T0,X0,T1,X1,NX,DT,TSTOP,OK,OUTPUT,RS(1))             
C                                                                       
C/6S                                                                    
C        IF (T1+DT.EQ.T1) CALL SETERR                                   
C    1      (30HDSXTRP - DT=0 RETURNED BY SOUT,30,14,1)                 
C/7S                                                                    
         IF (T1+DT.EQ.T1) CALL SETERR                                   
     1      ('DSXTRP - DT=0 RETURNED BY SOUT',30,14,1)                  
C/                                                                      
         IF (T1+DT.EQ.T1) GO TO 140                                     
C                                                                       
C/6S                                                                    
C        IF ((DT/DABS(DT))*(TSTOP-T1).LT.0.0D0) CALL SETERR             
C    1      (47HDSXTRP - DT RETURNED BY SOUT HAS THE WRONG SIGN,47,15,2)
C/7S                                                                    
         IF ((DT/DABS(DT))*(TSTOP-T1).LT.0.0D0) CALL SETERR             
     1      ('DSXTRP - DT RETURNED BY SOUT HAS THE WRONG SIGN',47,15,2) 
C/                                                                      
C                                                                       
C/6S                                                                    
C        IF (DABS(TEMP).LT.DABS(DT)) CALL SETERR                        
C    1      (42HDSXTRP - DT RAISED BY SOUT WHEN OK=.FALSE.,42,16,2)     
C/7S                                                                    
         IF (DABS(TEMP).LT.DABS(DT)) CALL SETERR                        
     1      ('DSXTRP - DT RAISED BY SOUT WHEN OK=.FALSE.',42,16,2)      
C/                                                                      
C                                                                       
C ...... THE DEFAULT RESPONSE IS TO LOWER DT BY 10**3.                  
C                                                                       
         IF (DABS(DT).EQ.DABS(TEMP)) DT=DT/1.0D+3                       
C                                                                       
C/6S                                                                    
C        IF (T0+DT.EQ.T0) CALL SETERR                                   
C    1      (13HDSXTRP - DT=0,13,13,1)                                  
C/7S                                                                    
         IF (T0+DT.EQ.T0) CALL SETERR                                   
     1      ('DSXTRP - DT=0',13,13,1)                                   
C/                                                                      
         IF (T0+DT.EQ.T0) GO TO 140                                     
C                                                                       
         IF (T1.NE.TSTOP) GO TO 10                                      
C                                                                       
 140  RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION D8XTRD(E,NX,M,MMAX,KMAX,LOGN,BETA,GAMMA,         
     1                        DELTA,F,POW,LDONE,ILDONE,KOPTO)           
C                                                                       
C  RETURN LDONE = THE LEVEL WHERE CONVERGENCE IS EXPECTED.              
C                                                                       
C  IF M=KOPTO+1, RETURN ILDONE=LDONE.                                   
C  IF M.GT.KOPTO+1, DO NOT LET LDONE.GT.ILDONE+1 HAPPEN IN THE FIRST    
C  KOPTO COLUMNS.                                                       
C                                                                       
C  D8XTRD = .TRUE. IF WILL NOT CONVERGE IN THIS LOZENGE.                
C  D8XTRD = .FALSE. IF WILL CONVERGE.                                   
C                                                                       
      DOUBLE PRECISION LOGN(MMAX),BETA,GAMMA,DELTA                      
      REAL E(NX,KMAX),F(KMAX),POW(KMAX)                                 
C                                                                       
      DOUBLE PRECISION TEMP                                             
C                                                                       
C ... INITIALLY, FLAG NOT CONVERGENT.                                   
C                                                                       
      D8XTRD=.TRUE.                                                     
      LDONE=0                                                           
C                                                                       
      LMAX=MMAX                                                         
      IF (M.GT.KOPTO+1) LMAX=MIN0(ILDONE+1,MMAX)                        
      MP1=M+1                                                           
      IF (MP1.GT.LMAX) GO TO 60                                         
C                                                                       
      DO 50 L=MP1,LMAX                                                  
C                                                                       
C ...... IF M.LE.KOPTO+1, CHECK FOR CONVERGENCE AT LEVEL L ONLY IF NOT  
C ...... ALREADY ACHIEVED. OTHERWISE, CHECK AT L IF NOT ALREADY DONE    
C ...... OR L=LMAX=ILDONE+1.                                            
C                                                                       
         IF (LDONE.GT.0.AND.(L.LT.LMAX.OR.M.LE.KOPTO+1)) GO TO 50       
C                                                                       
C ...... LDONEO DETERMINES IF WILL CONVERGE IN THE FIRST KOPTO          
C ...... COLUMNS.                                                       
C                                                                       
         LDONEO=L                                                       
         JHI=MIN0(M-1,KMAX)                                             
C                                                                       
C ...... COMPUTE THE FACTORS NEEDED TO CHECK FOR CONVERGENCE.           
C                                                                       
         DO 20 J=1,JHI                                                  
C                                                                       
            MMA=M                                                       
            LMA=L                                                       
            TEMP=0.0E0                                                  
C                                                                       
            DO 10 I=1,J                                                 
C                                                                       
               LMA=LMA-1                                                
               MMA=MMA-1                                                
 10            TEMP=TEMP+(LOGN(MMA)-LOGN(LMA))                          
C                                                                       
 20         F(J)=POW(J)*GAMMA*TEMP                                      
C                                                                       
C ...... SEE IF THE I-TH VARIABLE WILL CONVERGE AT M=L.                 
C                                                                       
         DO 40 I=1,NX                                                   
C                                                                       
C ......... CHECK EACH COLUMN FOR CONVERGENCE.                          
C                                                                       
            DO 30 J=1,JHI                                               
C                                                                       
               JSAVE=J                                                  
C                                                                       
               IF (E(I,J).GE.F(J)) GO TO 40                             
C                                                                       
 30            CONTINUE                                                 
C                                                                       
C ......... NO CONVERGENCE HERE.                                        
C                                                                       
            GO TO 50                                                    
C                                                                       
C ......... IF WILL NOT CONVERGE IN THE FIRST KOPTO COLUMNS,            
C ......... FLAG LDONEO.                                                
C                                                                       
 40         IF (JSAVE.GT.KOPTO) LDONEO=0                                
C                                                                       
C ...... HAVE CONVERGENCE HERE.                                         
C                                                                       
         IF (LDONE.EQ.0) LDONE=L                                        
C                                                                       
 50      CONTINUE                                                       
C                                                                       
 60   IF (LDONE.EQ.0.OR.LDONEO.EQ.0) GO TO 70                           
C                                                                       
      IF (M.EQ.KOPTO+1) ILDONE=LDONE                                    
C                                                                       
      D8XTRD=.FALSE.                                                    
C                                                                       
 70   RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION D8XTRL(E2,E,NX,M,KMAX,POW,LOGLO)                 
C                                                                       
C  TO RETURN POW(J) TIMES THE LOGARITHM OF THE RATIO OF THE DESIRED     
C  TO THE ATTAINED ERROR, FOR EACH ELEMENT IN THE LOZENGE.              
C                                                                       
C  D8XTRL = .TRUE. IF NOT SUCCESSFUL.                                   
C  D8XTRL = .FALSE. IF SUCCESSFUL.                                      
C                                                                       
      REAL E2(NX,KMAX),E(NX),POW(KMAX),LOGLO                            
C                                                                       
      REAL LOGE,V                                                       
C                                                                       
      D8XTRL=.FALSE.                                                    
      JHI=MIN0(M-1,KMAX)                                                
C                                                                       
      DO 20 I=1,NX                                                      
C                                                                       
         IF (E(I).LE.0.0E0) GO TO 30                                    
C                                                                       
         LOGE=ALOG(E(I))                                                
C                                                                       
         DO 10 J=1,JHI                                                  
C                                                                       
            V=LOGE-LOGLO                                                
            IF (E2(I,J).NE.0.0E0) V=LOGE-ALOG(ABS(E2(I,J)))             
 10         E2(I,J)=POW(J)*V                                            
C                                                                       
 20      CONTINUE                                                       
C                                                                       
      GO TO 40                                                          
C                                                                       
C ... HERE FOR A NON-POSITIVE ERROR REQUEST.                            
C                                                                       
C/6S                                                                    
C30   CALL SETERR(36HDSXTRP - E(I).LE.0 RETURNED BY ERROR,36,17,1)      
C/7S                                                                    
 30   CALL SETERR('DSXTRP - E(I).LE.0 RETURNED BY ERROR',36,17,1)       
C/                                                                      
      D8XTRL=.TRUE.                                                     
C                                                                       
 40   RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE D8XTRO(E,NX,M,MMAX,KMAX,LOGN,GAMMA,F,POW,KOPT,         
     1                  DT,HOPT,LNWORK,COST,LOGLO,LOGHI)                
C                                                                       
C  COMPUTE THE OPTIMAL K AND H.                                         
C                                                                       
      DOUBLE PRECISION LOGN(MMAX),GAMMA,DT,TEMP                         
      REAL E(NX,KMAX),F(KMAX),POW(KMAX),                                
     1     HOPT(1),LNWORK(MMAX),COST(1),LOGLO,LOGHI                     
C     REAL HOPT(KMAX+1),COST(KMAX+1)                                    
C                                                                       
      REAL RHOMAX,HOPTK,ROMAX,HOPTM,HTOL,LOGDT                          
C                                                                       
C  HTOL IS THE RELATIVE TOLERANCE TO WHICH LOG(HOPT) WILL BE COMPUTED.  
C                                                                       
      DATA HTOL/5.0E-2/                                                 
C                                                                       
      LOGDT=ALOG(SNGL(DABS(DT)))                                        
      KHI=MIN0(M-1,KMAX)                                                
      KHIP1=KHI+1                                                       
      KOPT=1                                                            
C                                                                       
C ... COMPUTE HOPT(K), K=1,...,MIN(M,KMAX+1).                           
C                                                                       
      DO 50 K=1,KHIP1                                                   
C                                                                       
         JHI=MIN0(K,KHI)                                                
         JHIM1=JHI-1                                                    
C                                                                       
C ...... COMPUTE THE FACTORS WHICH CONVERT ERRORS INTO STEP-SIZES.      
C                                                                       
         DO 20 J=1,JHI                                                  
C                                                                       
            MMA=M                                                       
            LMA=K+1                                                     
            TEMP=0.0E0                                                  
C                                                                       
            DO 10 I=1,J                                                 
C                                                                       
               LMA=LMA-1                                                
               MMA=MMA-1                                                
 10            TEMP=TEMP+(LOGN(LMA)-LOGN(MMA))                          
C                                                                       
 20         F(J)=GAMMA*TEMP*POW(J)                                      
C                                                                       
C ...... HOPTK IS THE OPTIMAL STEP-SIZE FOR THE K-COLUMN LOZENGE.       
C ...... HOPTM IS THE OPTIMAL STEP-SIZE FOR THE FIRST (K-1)-COLUMNS     
C ...... OF THE FULL K-COLUMN LOZENGE.                                  
C                                                                       
         HOPTK=LOGHI                                                    
         HOPTM=LOGHI                                                    
C                                                                       
         DO 40 I=1,NX                                                   
C                                                                       
            RHOMAX=LOGLO                                                
            ROMAX=LOGLO                                                 
C                                                                       
            DO 30 J=1,JHI                                               
C                                                                       
               RHOMAX=AMAX1(RHOMAX,F(J)+E(I,J))                         
C                                                                       
C ............ SAVE THE OPTIMAL FACTOR FOR THE (K-1) COLUMN SUB-LOZENGE.
C                                                                       
 30            IF (J.EQ.JHIM1) ROMAX=RHOMAX                             
C                                                                       
            HOPTM=AMIN1(HOPTM,ROMAX)                                    
 40         HOPTK=AMIN1(HOPTK,RHOMAX)                                   
C                                                                       
         COST(K)=LNWORK(K+1)-HOPTK                                      
         IF (HOPTK.GT.HOPTM+HTOL.AND.K.LT.KHIP1) KOPT=K                 
C                                                                       
 50      HOPT(K)=EXP(AMIN1(HOPTK+LOGDT,0.9999E0*LOGHI))*(DT/DABS(DT))   
C                                                                       
C ... SEE IF A LOWER K IS CHEAPER THAN KOPT. IF SO, USE IT.             
C                                                                       
      DO 60 K=1,KOPT                                                    
         KS=K                                                           
         IF (COST(K).LE.COST(KOPT)) GO TO 70                            
 60      CONTINUE                                                       
C                                                                       
 70   KOPT=KS                                                           
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DXTRAP(TM,M,NVAR,NG,KMAX,XPOLY,T,ERROR,EBEST)          
C                                                                       
C  ASSUME AN EXPANSION FOR THE VECTOR VALUED FUNCTION T(H) OF THE FORM  
C                                                                       
C            T(H) = T(0) + SUM(J=1,2,3,...)(A(J)*H**(J*GAMMA))          
C                                                                       
C  WHERE THE A(J) ARE CONSTANT VECTORS AND GAMMA IS A POSITIVE CONSTANT.
C                                                                       
C  GIVEN T(H(M)), WHERE H(M)=H0/N(M), M=1,2,3,..., THIS ROUTINE USES    
C  POLYNOMIAL (XPOLY), OR RATIONAL (.NOT.XPOLY), EXTRAPOLATION TO       
C  SEQUENTIALLY APPROXIMATE T(0).                                       
C                                                                       
C  INPUT                                                                
C                                                                       
C    TM     - TM = T(H(M)) FOR THIS CALL.                               
C    M      - H(M) WAS USED TO OBTAIN TM.                               
C    NVAR   - THE LENGTH OF THE VECTOR TM.                              
C    NG     - THE DOUBLE PRECISION VALUES                               
C                                                                       
C                 NG(I) = N(I)**GAMMA                                   
C                                                                       
C             FOR I=1,...,M. NG MUST BE A MONOTONE INCREASING ARRAY.    
C    KMAX   - THE MAXIMUM NUMBER OF COLUMNS TO BE USED IN THE           
C             EXTRAPOLATION PROCESS.                                    
C    XPOLY  - IF XPOLY=.TRUE., THEN USE POLYNOMIAL EXTRAPOLATION.       
C             IF XPOLY=.FALSE., THEN USE RATIONAL EXTRAPOLATION.        
C    T      - THE BOTTOM EDGE OF THE EXTRAPOLATION LOZENGE.             
C             T(I,J) SHOULD CONTAIN THE J-TH EXTRAPOLATE OF THE I-TH    
C             COMPONENT OF T(H) BASED ON THE SEQUENCE H(1),...,H(M-1),  
C             FOR I=1,...,NVAR AND J=1,...,MIN(M-1,KMAX).               
C                                                                       
C             WHEN M=1, T MAY CONTAIN ANYTHING.                         
C                                                                       
C             FOR M.GT.1, NOTE THAT THE OUTPUT VALUE OF T AT THE        
C             (M-1)-ST CALL IS THE INPUT FOR THE M-TH CALL.             
C             THUS, THE USER NEED NEVER PUT ANYTHING INTO T,            
C             BUT HE CAN NOT ALTER ANY ELEMENT OF T BETWEEN             
C             CALLS TO DXTRAP.                                          
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    TM     - TM(I)=THE MOST ACCURATE APPROXIMATION IN THE LOZENGE      
C             FOR THE I-TH VARIABLE, I=1,...,NVAR.                      
C    T      - T(I,J) CONTAINS THE J-TH EXTRAPOLATE OF THE I-TH          
C             COMPONENT OF T(H) BASED ON THE SEQUENCE H(1),...,H(M),    
C             FOR I=1,...,NVAR AND J=1,...,MIN(M,KMAX).                 
C    ERROR  - ERROR(I,J) GIVES THE SIGNED BULIRSCH-STOER ESTIMATE OF THE
C             ERROR IN THE J-TH EXTRAPOLATE OF THE I-TH COMPONENT OF    
C             T(H) BASED ON THE SEQUENCE H(1),...,H(M-1),               
C             FOR I=1,...,NVAR AND J=1,...,MIN(M-1,KMAX).               
C             IF ERROR=EBEST AS ARRAYS, THEN THE ABOVE ELEMENTS         
C             ARE NOT STORED. RATHER, EBEST=ERROR IS LOADED AS DESCRIBED
C             BELOW.                                                    
C    EBEST  - EBEST(I)=THE ABSOLUTE VALUE OF THE ERROR IN TM(I),        
C             I=1,...,NVAR. THIS ARRAY IS FULL OF GARBAGE WHEN M=1.     
C                                                                       
C  SCRATCH SPACE ALLOCATED - MIN(M-1,KMAX) DOUBLE PRECISION WORDS +     
C                                                                       
C                            MIN(M-1,KMAX) INTEGER WORDS.               
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - M.LT.1.                                                        
C    2 - NVAR.LT.1.                                                     
C    3 - NG(1).LT.1.                                                    
C    4 - KMAX.LT.1.                                                     
C    5 - NG IS NOT MONOTONE INCREASING.                                 
C                                                                       
      DOUBLE PRECISION TM(NVAR),NG(M),T(NVAR,1)                         
C     DOUBLE PRECISION T(NVAR,MIN(M,KMAX))                              
      REAL ERROR(NVAR,1),EBEST(NVAR)                                    
C     REAL ERROR(NVAR,MIN(M-1,KMAX))                                    
      LOGICAL XPOLY                                                     
C                                                                       
      LOGICAL ESAVE                                                     
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      DOUBLE PRECISION WS(1)                                            
      REAL RS(1000)                                                     
      EQUIVALENCE (DS(1),WS(1)),(DS(1),RS(1))                           
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (M.LT.1) CALL SETERR(15HDXTRAP - M.LT.1,15,1,2)                
C     IF (NVAR.LT.1) CALL SETERR(18HDXTRAP - NVAR.LT.1,18,2,2)          
C     IF (NG(1).LT.1.0D0) CALL SETERR(19HDXTRAP - NG(1).LT.1,19,3,2)    
C     IF (KMAX.LT.1) CALL SETERR(18HDXTRAP - KMAX.LT.1,18,4,2)          
C/7S                                                                    
      IF (M.LT.1) CALL SETERR('DXTRAP - M.LT.1',15,1,2)                 
      IF (NVAR.LT.1) CALL SETERR('DXTRAP - NVAR.LT.1',18,2,2)           
      IF (NG(1).LT.1.0D0) CALL SETERR('DXTRAP - NG(1).LT.1',19,3,2)     
      IF (KMAX.LT.1) CALL SETERR('DXTRAP - KMAX.LT.1',18,4,2)           
C/                                                                      
C                                                                       
      IF (M.EQ.1) GO TO 20                                              
C                                                                       
      DO 10 I=2,M                                                       
C/6S                                                                    
C        IF (NG(I-1).GE.NG(I)) CALL SETERR                              
C    1      (38HDXTRAP - NG IS NOT MONOTONE INCREASING,38,5,2)          
C/7S                                                                    
         IF (NG(I-1).GE.NG(I)) CALL SETERR                              
     1      ('DXTRAP - NG IS NOT MONOTONE INCREASING',38,5,2)           
C/                                                                      
 10      CONTINUE                                                       
C                                                                       
C ... SEE IF ERROR=EBEST AS ARRAYS. IF (ESAVE), THEN LOAD ERROR.        
C                                                                       
 20   ERROR(1,1)=1.0E0                                                  
      EBEST(1)=2.0E0                                                    
      ESAVE=ERROR(1,1).NE.EBEST(1)                                      
C                                                                       
C ... ALLOCATE SCRATCH SPACE.                                           
C                                                                       
      IRHG=1                                                            
      IEMAG=1                                                           
      IF (M.GT.1) IRHG=ISTKGT(MIN0(M-1,KMAX),4)                         
      IF (M.GT.1) IEMAG=ISTKGT(MIN0(M-1,KMAX),3)                        
C                                                                       
      CALL D0XTRP(TM,M,NVAR,NG,KMAX,XPOLY,T,ERROR,EBEST,WS(IRHG),       
     1            RS(IEMAG),ESAVE)                                      
C                                                                       
      IF (M.GT.1) CALL ISTKRL(2)                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE D0XTRP(TM,M,NVAR,NG,KMAX,XPOLY,T,ERROR,EBEST,RHG,EMAG, 
     1                  ESAVE)                                          
C                                                                       
      DOUBLE PRECISION TM(NVAR),NG(M),T(NVAR,KMAX),RHG(1)               
C     DOUBLE PRECISION RHG(MIN(M-1,KMAX))                               
      REAL ERROR(NVAR,1),EBEST(NVAR),EMAG(1)                            
C     REAL ERROR(NVAR,MIN(M-1,KMAX)),EMAG(MIN(M-1,KMAX))                
      LOGICAL XPOLY,ESAVE                                               
C                                                                       
      DOUBLE PRECISION U,V,TI,TV,TEMP                                   
      REAL ERR                                                          
C                                                                       
      IF (M.GT.1) GO TO 20                                              
C                                                                       
C ... INITIALIZE T.                                                     
C                                                                       
      DO 10 I=1,NVAR                                                    
 10      T(I,1)=TM(I)                                                   
C                                                                       
      GO TO 80                                                          
C                                                                       
 20   MR=MIN0(M-1,KMAX)                                                 
C                                                                       
      DO 30 J=1,MR                                                      
         MMJ=M-J                                                        
         RHG(J)=NG(M)/NG(MMJ)                                           
         EMAG(J)=1.0D0+1.0D0/(RHG(J)-1.0D0)                             
         IF (XPOLY) RHG(J)=RHG(J)-1.0D0                                 
 30      CONTINUE                                                       
C                                                                       
      DO 70 I=1,NVAR                                                    
C                                                                       
         V=0.0D0                                                        
         U=T(I,1)                                                       
         TI=TM(I)                                                       
         T(I,1)=TI                                                      
C                                                                       
         DO 60 J=1,MR                                                   
C                                                                       
C ......... OBTAIN SIGNED ERROR ESTIMATE.                               
C                                                                       
            ERR=(T(I,J)-U)*EMAG(J)                                      
            IF (ESAVE) ERROR(I,J)=ERR                                   
            ERR=ABS(ERR)                                                
            IF (J.EQ.1) EBEST(I)=ERR                                    
            EBEST(I)=AMIN1(EBEST(I),ERR)                                
            IF (EBEST(I).EQ.ERR) JBEST=J                                
C                                                                       
            IF (J.EQ.KMAX) GO TO 60                                     
C                                                                       
            IF (XPOLY) GO TO 40                                         
C                                                                       
C ......... RATIONAL EXTRAPOLATION.                                     
C                                                                       
            TV=TI-V                                                     
            TEMP=RHG(J)*(U-V)-TV                                        
            IF (TEMP.NE.0.0D0) TI=TI+(TI-U)*(TV/TEMP)                   
            V=U                                                         
            GO TO 50                                                    
C                                                                       
C ......... POLYNOMIAL EXTRAPOLATION.                                   
C                                                                       
 40         TI=TI+(TI-U)/RHG(J)                                         
C                                                                       
 50         U=T(I,J+1)                                                  
            T(I,J+1)=TI                                                 
 60         CONTINUE                                                    
C                                                                       
 70      TM(I)=T(I,JBEST)                                               
C                                                                       
 80   RETURN                                                            
C                                                                       
      END                                                               
C****END OF ROUTINES NEEDED FOR PORT 3 DIFFERENTIAL EQUATION CHAPTER****