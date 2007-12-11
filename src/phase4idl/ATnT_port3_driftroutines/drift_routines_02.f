      SUBROUTINE POST(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,        
     1   AF, BC, D, ERRPAR, HANDLE)                                     
      EXTERNAL AF, BC, D, HANDLE                                        
      INTEGER NU, K, NX, NV                                             
      REAL U(1), X(1), V(1), TSTART, TSTOP, DT                          
      REAL ERRPAR(2)                                                    
      EXTERNAL POSTE, POSTN, POSTP                                      
C THE FIRST LEVEL OF  POST.                                             
C U(NX-K,NU),X(NX),V(NV).                                               
      CALL POSTR(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT, AF, BC, D,  
     1   POSTE, ERRPAR, POSTN, POSTP, HANDLE)                           
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTS(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT        
     1   , AF, BC, D, ERRPAR, HANDLE)                                   
      EXTERNAL AF, BC, D, HANDLE                                        
      INTEGER NU, K, NX, NV                                             
      REAL U(1), X(1), V(1), TSTART, TSTOP, DT                          
      REAL ERRPAR(2)                                                    
      EXTERNAL POSTE                                                    
      LOGICAL ERPUTS                                                    
C THE FIRST LEVEL OF  POSTS.                                            
C U(NX-K,NU),X(NX),V(NV).                                               
      ERPUTS = .FALSE.                                                  
      CALL POST1(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT, AF, BC, D,  
     1   POSTE, ERRPAR, ERPUTS, HANDLE)                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE POST1(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT        
     1   , AF, BC, D, ERROR, ERRPAR, ERPUTS, HANDLE)                    
      EXTERNAL AF, BC, D, ERROR, HANDLE                                 
      INTEGER NU, K, NX, NV                                             
      REAL U(1), X(1), V(1), TSTART, TSTOP, DT                          
      REAL ERRPAR(2)                                                    
      LOGICAL ERPUTS                                                    
      INTEGER KMAX, KINIT                                               
      LOGICAL EQUIL, XPOLY                                              
C THE SECOND LEVEL OF  POSTS.                                           
C U(NX-K,NU),X(NX),V(NV).                                               
      KMAX = 10                                                         
      XPOLY = .FALSE.                                                   
      KINIT = 2                                                         
      EQUIL = .TRUE.                                                    
      CALL POST2(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT, AF, BC, D,  
     1   EQUIL, KMAX, XPOLY, KINIT, ERROR, ERRPAR, ERPUTS, HANDLE)      
      RETURN                                                            
      END                                                               
      SUBROUTINE POST2(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT        
     1   , AF, BC, D, EQUIL, KMAX, XPOLY, KINIT, ERROR, ERRPAR, ERPUTS  
     2   , HANDLE)                                                      
      EXTERNAL AF, BC, D, ERROR, HANDLE                                 
      INTEGER NU, K, NX, NV, KMAX, KINIT                                
      REAL U(1), X(1), V(1), TSTART, TSTOP, DT                          
      REAL ERRPAR(2)                                                    
      LOGICAL EQUIL, XPOLY, ERPUTS                                      
      EXTERNAL POSTN, POSTP                                             
      INTEGER MGQ, NERROR, KEEJAC, NERR, MINIT, MAXIT                   
      REAL T0, T1, THETA                                                
C THE THIRD LEVEL OF  POSTS.                                            
C U(NX-K,NU),X(NX),V(NV).                                               
C CHECK THE INPUT FOR ERRORS.                                           
      CALL ENTER(1)                                                     
      IF (.NOT. EQUIL) GOTO 1                                           
         THETA = 1                                                      
         GOTO  2                                                        
   1     THETA = 0.5E0                                                  
   2  KEEJAC = 0                                                        
      IF (.NOT. EQUIL) GOTO 3                                           
         MINIT = 1                                                      
         MAXIT = 1                                                      
         GOTO  4                                                        
   3     MINIT = 10                                                     
         MAXIT = 50                                                     
   4  T0 = TSTART                                                       
      T1 = TSTOP                                                        
      MGQ = K-1                                                         
         GOTO  6                                                        
   5     MGQ = MGQ+1                                                    
   6     IF (MGQ .GT. K) GOTO  7                                        
C LOOP UNTIL GQ WORKS.                                                  
         CALL POST3(U, NU, K, X, NX, V, NV, T0, T1, DT, AF, BC, D,      
     1      THETA, KEEJAC, MINIT, MAXIT, MGQ, KMAX, XPOLY, KINIT, ERROR,
     2      ERRPAR, ERPUTS, POSTN, POSTP, HANDLE)                       
         IF (NERROR(NERR) .NE. 1011) GOTO  7                            
         CALL ERROFF                                                    
         T0 = T1                                                        
         T1 = TSTOP                                                     
         GOTO  5                                                        
   7  TSTOP = T1                                                        
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE POST3(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT        
     1   , AF, BC, D, THETA, KEEJAC, MINIT, MAXIT, MGQ, KMAX, XPOLY,    
     2   KINIT, ERROR, ERRPAR, ERPUTS, INMI, SCALE, HANDLE)             
      EXTERNAL AF, BC, D, ERROR, INMI, SCALE                            
      EXTERNAL HANDLE                                                   
      INTEGER NU, K, NX, NV, KEEJAC, MINIT                              
      INTEGER MAXIT, MGQ, KMAX, KINIT                                   
      REAL U(1), X(1), V(1), TSTART, TSTOP, DT                          
      REAL THETA, ERRPAR(2)                                             
      LOGICAL XPOLY, ERPUTS                                             
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, I, MMAX, IN, IS(1000), SAVEB                      
      REAL HFRACT, BETA, GAMMA, DELTA, EGIVE, RS(1000)                  
      REAL WS(1000)                                                     
      LOGICAL LS(1000)                                                  
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE FOURTH LEVEL OF POSTS.                                            
C SCRATCH SPACE ALLOCATED -                                             
C     S( POST3) = 10*NU**2 + 8*NU + 2*NU*(NV+1) + S(A9OSTS) +           
C                 IF ( KEEPJAC > 0 ) { 2*K*MGQ*(NX-2*K+1) }             
C REAL WORDS +                                                          
C                 MMAX                                                  
C INTEGER WORDS.                                                        
C U(NX-K,NU),X(NX),V(NV).                                               
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      CALL ENTER(1)                                                     
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
C DEFAULT.                                                              
   7  SAVEB = 0                                                         
      IF (THETA .NE. 0.5) GOTO 9                                        
         HFRACT = 0.5                                                   
         DO  8 I = 1, MMAX                                              
            TEMP = IN+I                                                 
            IS(TEMP-1) = 2*IS(TEMP-1)                                   
   8        CONTINUE                                                    
         GOTO  10                                                       
   9     HFRACT = 1                                                     
  10  EGIVE = 1E+2                                                      
      CALL POST4(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT, AF, BC, D,  
     1   THETA, KEEJAC, MINIT, MAXIT, MGQ, BETA, GAMMA, DELTA, IS(IN),  
     2   MMAX, HFRACT, EGIVE, SAVEB, KMAX, XPOLY, KINIT, ERROR, ERRPAR  
     3   , ERPUTS, INMI, SCALE, HANDLE)                                 
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTR(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT        
     1   , AF, BC, D, ERROR, ERRPAR, INMI, SCALE, HANDLE)               
      EXTERNAL AF, BC, D, ERROR, INMI, SCALE                            
      EXTERNAL HANDLE                                                   
      INTEGER NU, K, NX, NV                                             
      REAL U(1), X(1), V(1), TSTART, TSTOP, DT                          
      REAL ERRPAR(2)                                                    
      INTEGER MGQ, NERROR, MAX0, I, N(100), KEEJAC                      
      INTEGER IMGQ, KMAX, MMAX, IZAP, NERR, SAVEB                       
      INTEGER KINIT, MINIT, MAXIT                                       
      REAL HFRACT, BETA, FZAP, RZAP, T0, T1                             
      REAL GAMMA, DELTA, EGIVE, THETA                                   
      LOGICAL USENFD, USENGJ, USENNS, ERPUTS, LZAP, XPOLY               
C THE ROUTINE LEVEL OF POST.                                            
C SCRATCH SPACE ALLOCATED -                                             
C     S( POST3) = 10*NU**2 + 8*NU + 2*NU*(NV+1) + S(A9OSTS) +           
C                 IF ( KEEPJAC > 0 ) { 2*K*MGQ*(NX-2*K+1) }             
C REAL WORDS +                                                          
C                 MMAX                                                  
C INTEGER WORDS.                                                        
C U(NX-K,NU),X(NX),V(NV).                                               
      CALL ENTER(1)                                                     
C RETRIEVE THE VALUES TO BE USED.                                       
      CALL POSTW(-1, THETA, RZAP, IZAP, LZAP)                           
      CALL POSTW(-2, BETA, RZAP, IZAP, LZAP)                            
      CALL POSTW(-3, GAMMA, RZAP, IZAP, LZAP)                           
      CALL POSTW(-4, DELTA, RZAP, IZAP, LZAP)                           
      CALL POSTW(-1001, FZAP, HFRACT, IZAP, LZAP)                       
      CALL POSTW(-1002, FZAP, EGIVE, IZAP, LZAP)                        
      CALL POSTW(-2001, FZAP, RZAP, KEEJAC, LZAP)                       
      CALL POSTW(-2002, FZAP, RZAP, MINIT, LZAP)                        
      CALL POSTW(-2003, FZAP, RZAP, MAXIT, LZAP)                        
      CALL POSTW(-2004, FZAP, RZAP, KMAX, LZAP)                         
      CALL POSTW(-2005, FZAP, RZAP, KINIT, LZAP)                        
      CALL POSTW(-2006, FZAP, RZAP, MMAX, LZAP)                         
      CALL POSTW(-2007, FZAP, RZAP, MGQ, LZAP)                          
      CALL POSTW(-2008, FZAP, RZAP, SAVEB, LZAP)                        
      CALL POSTW(-3001, FZAP, RZAP, IZAP, XPOLY)                        
      CALL POSTW(-3002, FZAP, RZAP, IZAP, ERPUTS)                       
      CALL POSTW(-3003, FZAP, RZAP, IZAP, USENGJ)                       
      CALL POSTW(-3004, FZAP, RZAP, IZAP, USENNS)                       
      CALL POSTW(-3005, FZAP, RZAP, IZAP, USENFD)                       
C TEST FOR ERRORS.                                                      
C/6S                                                                    
C     IF (KMAX .LT. 1) CALL SETERR(18H POST4 - KMAX.LT.1, 18, 13, 2)    
C     IF (MMAX .LT. KMAX+2) CALL SETERR(23H POST4 - MMAX.LT.KMAX+2, 23  
C    1   , 23, 2)                                                       
C/7S                                                                    
      IF (KMAX .LT. 1) CALL SETERR(' POST4 - KMAX.LT.1', 18, 13, 2)     
      IF (MMAX .LT. KMAX+2) CALL SETERR(' POST4 - MMAX.LT.KMAX+2', 23   
     1   , 23, 2)                                                       
C/                                                                      
      DO  1 I = 1, MMAX                                                 
         CALL POSTW(-(I+4000), FZAP, RZAP, N(I), LZAP)                  
   1     CONTINUE                                                       
C TEST N FOR MONOTONICITY.                                              
      DO  2 I = 2, MMAX                                                 
C/6S                                                                    
C        IF (N(I) .LE. N(I-1)) CALL SETERR(                             
C    1      37H POST4 - N IS NOT MONOTONE INCREASING, 37, 25, 2)        
C/7S                                                                    
         IF (N(I) .LE. N(I-1)) CALL SETERR(                             
     1      ' POST4 - N IS NOT MONOTONE INCREASING', 37, 25, 2)         
C/                                                                      
   2     CONTINUE                                                       
      T0 = TSTART                                                       
      T1 = TSTOP                                                        
      IMGQ = MAX0(K-1, MGQ)                                             
         GOTO  4                                                        
   3     IMGQ = IMGQ+1                                                  
   4     IF (IMGQ .GT. MAX0(K, MGQ)) GOTO  5                            
C LOOP TILL GQ WORKS.                                                   
         CALL POST4(U, NU, K, X, NX, V, NV, T0, T1, DT, AF, BC, D,      
     1      THETA, KEEJAC, MINIT, MAXIT, IMGQ, BETA, GAMMA, DELTA, N,   
     2      MMAX, HFRACT, EGIVE, SAVEB, KMAX, XPOLY, KINIT, ERROR,      
     3      ERRPAR, ERPUTS, INMI, SCALE, HANDLE)                        
         IF (NERROR(NERR) .NE. 1011) GOTO  5                            
         CALL ERROFF                                                    
         T0 = T1                                                        
         T1 = TSTOP                                                     
         GOTO  3                                                        
   5  TSTOP = T1                                                        
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE POST4(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT        
     1   , AF, BC, D, THETA, KEEJAC, MINIT, MAXIT, MGQ, BETA, GAMMA,    
     2   DELTA, N, MMAX, HFRACT, EGIVE, SAVEB, KMAX, XPOLY, KINIT,      
     3   ERROR, ERRPAR, ERPUTS, INMI, SCALE, HANDLE)                    
      INTEGER MMAX                                                      
      EXTERNAL AF, BC, D, ERROR, INMI, SCALE                            
      EXTERNAL HANDLE                                                   
      INTEGER NU, K, NX, NV, KEEJAC, MINIT                              
      INTEGER MAXIT, MGQ, N(MMAX), SAVEB, KMAX, KINIT                   
      REAL U(1), X(1), V(1), TSTART, TSTOP, DT                          
      REAL THETA, BETA, GAMMA, DELTA, HFRACT, EGIVE                     
      REAL ERRPAR(2)                                                    
      LOGICAL XPOLY, ERPUTS                                             
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /POSTF/ FAILED                                             
      LOGICAL FAILED                                                    
      COMMON /A90STY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL WV(30), RV(30)                                               
      LOGICAL LV(20)                                                    
      COMMON /A90STV/ IEU                                               
      INTEGER IEU                                                       
      COMMON /A90STT/ TGOOD                                             
      REAL TGOOD                                                        
      COMMON /A90STR/ STATS                                             
      INTEGER STATS(8)                                                  
      COMMON /A90STQ/ IXGQ, IWGQ, MGQQ                                  
      INTEGER IXGQ, IWGQ, MGQQ                                          
      COMMON /A90STK/ NUU, NVV, KK, NXX                                 
      INTEGER NUU, NVV, KK, NXX                                         
      COMMON /A90STB/ IGSSIS, SET                                       
      INTEGER IGSSIS                                                    
      LOGICAL SET                                                       
      COMMON /A9OSTT/ TC, DTC                                           
      REAL TC, DTC                                                      
      COMMON /A9OSTS/ IMEM                                              
      INTEGER IMEM(4)                                                   
      COMMON /A9OSTM/ THETAC, EGIVEC, MINITC, MAXITC, KEEACC            
      INTEGER MINITC, MAXITC, KEEACC                                    
      REAL THETAC, EGIVEC                                               
      COMMON /A9OSTL/ ERPTSC                                            
      LOGICAL ERPTSC                                                    
      COMMON /A9OSTK/ IUTETA, IVTETA, IUT, IVT                          
      INTEGER IUTETA, IVTETA, IUT, IVT                                  
      COMMON /A9OSTJ/ IJP, IB, IAFB, IALFA, IBETA, IGAMMA, ID4, ID5,    
     1   IORDER, IBC, IEQS, IAA, IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT,  
     2   IDMAT, IDIAG, IDPVOT                                           
      INTEGER IJP(3), IB(3), IAFB(3), IALFA(3), IBETA(3), IGAMMA(3)     
      INTEGER ID4(3), ID5(3), IORDER, IBC, IEQS, IAA                    
      INTEGER IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT                      
      INTEGER IDMAT, IDIAG, IDPVOT                                      
      COMMON /A9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE                                            
      COMMON /A9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      EXTERNAL A9OSTN, A9OSTP, A9OSTA, A9OSTB, A9OSTD, A9OSTE           
      EXTERNAL A9OSTH                                                   
      INTEGER ISTKGT, NERROR, I, NERR, IS(1000)                         
      REAL ABS, WS(1000), RS(1000)                                      
      LOGICAL LS(1000)                                                  
      INTEGER TEMP, TEMP2                                               
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE BOTTOM LEVEL OF POSTS.                                            
C SCRATCH SPACE ALLOCATED -                                             
C     S( POST4) = 10*NU**2 + 8*NU + 2*NU*(NV+1) + S(A9OSTS) +           
C                 IF ( KEEPJAC > 0 ) { 2*K*MGQ*(NX-2*K+1) }             
C REAL WORDS +                                                          
C                 MMAX                                                  
C INTEGER WORDS.                                                        
C U(NX-K,NU),X(NX),V(NV).                                               
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      IF (TSTART .EQ. TSTOP) RETURN                                     
      CALL ENTER(1)                                                     
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (NU .LT. 0) CALL SETERR(16H POST4 - NU.LT.0, 16, 1, 2)         
C     IF (NV .LT. 0) CALL SETERR(16H POST4 - NV.LT.0, 16, 2, 2)         
C/7S                                                                    
      IF (NU .LT. 0) CALL SETERR(' POST4 - NU.LT.0', 16, 1, 2)          
      IF (NV .LT. 0) CALL SETERR(' POST4 - NV.LT.0', 16, 2, 2)          
C/                                                                      
      TEMP1 = NU .EQ. NV                                                
      IF (TEMP1) TEMP1 = NV .EQ. 0                                      
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(16H POST4 - NU=NV=0, 16, 3, 2)             
C/7S                                                                    
      IF (TEMP1) CALL SETERR(' POST4 - NU=NV=0', 16, 3, 2)              
C/                                                                      
      TEMP1 = NU .GT. 0                                                 
      IF (TEMP1) TEMP1 = K .LT. 2                                       
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(32H POST4 - K.LT.2 WHEN NU POSITIVE, 32, 4,
C    1   2)                                                             
C/7S                                                                    
      IF (TEMP1) CALL SETERR(' POST4 - K.LT.2 WHEN NU POSITIVE', 32, 4, 
     1   2)                                                             
C/                                                                      
      TEMP1 = NU .GT. 0                                                 
      IF (TEMP1) TEMP1 = NX .LT. 2*K                                    
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(35H POST4 - NX.LT.2*K WHEN NU POSITIVE, 35,
C    1   5, 2)                                                          
C     IF (TSTART+DT .EQ. TSTART) CALL SETERR(                           
C    1   31H POST4 - INPUT VALUE OF DT IS 0, 31, 6, 2)                  
C     IF ((DT/ABS(DT))*(TSTOP-TSTART) .LE. 0.) CALL SETERR(             
C    1   45H POST4 - INPUT VALUE OF DT HAS THE WRONG SIGN, 45, 7, 2)    
C/7S                                                                    
      IF (TEMP1) CALL SETERR(' POST4 - NX.LT.2*K WHEN NU POSITIVE', 35, 
     1   5, 2)                                                          
      IF (TSTART+DT .EQ. TSTART) CALL SETERR(                           
     1   ' POST4 - INPUT VALUE OF DT IS 0', 31, 6, 2)                   
      IF ((DT/ABS(DT))*(TSTOP-TSTART) .LE. 0.) CALL SETERR(             
     1   ' POST4 - INPUT VALUE OF DT HAS THE WRONG SIGN', 45, 7, 2)     
C/                                                                      
C ???                                                                   
      TEMP1 = THETA .LT. 0.                                             
      IF (.NOT. TEMP1) TEMP1 = THETA .GT. 1.                            
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(27H POST4 - THETA NOT IN (0,1), 27, 8, 2)  
C/7S                                                                    
      IF (TEMP1) CALL SETERR(' POST4 - THETA NOT IN (0,1)', 27, 8, 2)   
C/                                                                      
C ???                                                                   
      TEMP1 = KEEJAC .LT. 0                                             
      IF (.NOT. TEMP1) TEMP1 = KEEJAC .GT. 5                            
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(37H POST4 - KEEPJAC NOT ONE OF (0,...,5),  
C    1   37, 9, 2)                                                      
C     IF (MINIT .LT. 1) CALL SETERR(19H POST4 - MINIT.LT.1, 19, 10, 2)  
C     IF (MAXIT .LT. 1) CALL SETERR(19H POST4 - MAXIT.LT.1, 19, 11, 2)  
C/7S                                                                    
      IF (TEMP1) CALL SETERR(' POST4 - KEEPJAC NOT ONE OF (0,...,5)',   
     1   37, 9, 2)                                                      
      IF (MINIT .LT. 1) CALL SETERR(' POST4 - MINIT.LT.1', 19, 10, 2)   
      IF (MAXIT .LT. 1) CALL SETERR(' POST4 - MAXIT.LT.1', 19, 11, 2)   
C/                                                                      
      TEMP1 = NU .GT. 0                                                 
      IF (TEMP1) TEMP1 = MGQ .LT. 1                                     
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(37H POST4 - MGQ.LT.1 WHEN NU IS POSITIVE,  
C    1   37, 12, 2)                                                     
C     IF (KMAX .LT. 1) CALL SETERR(18H POST4 - KMAX.LT.1, 18, 13, 2)    
C     IF (KINIT .LT. 1) CALL SETERR(19H POST4 - KINIT.LT.1, 19, 14, 2)  
C/7S                                                                    
      IF (TEMP1) CALL SETERR(' POST4 - MGQ.LT.1 WHEN NU IS POSITIVE',   
     1   37, 12, 2)                                                     
      IF (KMAX .LT. 1) CALL SETERR(' POST4 - KMAX.LT.1', 18, 13, 2)     
      IF (KINIT .LT. 1) CALL SETERR(' POST4 - KINIT.LT.1', 19, 14, 2)   
C/                                                                      
      IF (NU .LE. 0) GOTO 3                                             
         DO  1 I = 1, K                                                 
C/6S                                                                    
C           IF (X(I) .NE. X(1)) CALL SETERR(                            
C    1         35H POST4 - X(1) NOT OF MULTIPLICITY K, 35, 15, 2)       
C/7S                                                                    
            IF (X(I) .NE. X(1)) CALL SETERR(                            
     1         ' POST4 - X(1) NOT OF MULTIPLICITY K', 35, 15, 2)        
C/                                                                      
            TEMP = NX-K+I                                               
C/6S                                                                    
C           IF (X(TEMP) .NE. X(NX)) CALL SETERR(                        
C    1         36H POST4 - X(NX) NOT OF MULTIPLICITY K, 36, 16, 2)      
C/7S                                                                    
            IF (X(TEMP) .NE. X(NX)) CALL SETERR(                        
     1         ' POST4 - X(NX) NOT OF MULTIPLICITY K', 36, 16, 2)       
C/                                                                      
   1        CONTINUE                                                    
         TEMP = NX-K                                                    
         DO  2 I = K, TEMP                                              
C/6S                                                                    
C           IF (X(I) .GT. X(I+1)) CALL SETERR(                          
C    1         34H POST4 - X NOT MONOTONE INCREASING, 34, 17, 2)        
C/7S                                                                    
            IF (X(I) .GT. X(I+1)) CALL SETERR(                          
     1         ' POST4 - X NOT MONOTONE INCREASING', 34, 17, 2)         
C/                                                                      
            IF (I+K .GT. NX) GOTO  2                                    
            TEMP2 = I+K                                                 
C/6S                                                                    
C           IF (X(TEMP2) .LE. X(I)) CALL SETERR(                        
C    1         34H POST4 - X NOT MONOTONE INCREASING, 34, 17, 2)        
C/7S                                                                    
            IF (X(TEMP2) .LE. X(I)) CALL SETERR(                        
     1         ' POST4 - X NOT MONOTONE INCREASING', 34, 17, 2)         
C/                                                                      
   2        CONTINUE                                                    
C/6S                                                                    
C  3  IF (BETA .LE. 0.) CALL SETERR(19H POST4 - BETA .LE.0, 19, 19, 2)  
C     IF (GAMMA .LE. 0.) CALL SETERR(20H POST4 - GAMMA .LE.0, 20, 20, 2)
C     IF (DELTA .LT. 0.) CALL SETERR(20H POST4 - DELTA .LT.0, 20, 21, 2)
C     IF (BETA+GAMMA-DELTA .LE. 0.) CALL SETERR(                        
C    1   30H POST4 - BETA+GAMMA-DELTA.LE.0, 30, 22, 2)                  
C     IF (MMAX .LT. KMAX+2) CALL SETERR(23H POST4 - MMAX.LT.KMAX+2, 23  
C    1   , 23, 2)                                                       
C     IF (N(1) .LT. 1) CALL SETERR(18H POST4 - N(1).LT.1, 18, 24, 2)    
C/7S                                                                    
   3  IF (BETA .LE. 0.) CALL SETERR(' POST4 - BETA .LE.0', 19, 19, 2)   
      IF (GAMMA .LE. 0.) CALL SETERR(' POST4 - GAMMA .LE.0', 20, 20, 2) 
      IF (DELTA .LT. 0.) CALL SETERR(' POST4 - DELTA .LT.0', 20, 21, 2) 
      IF (BETA+GAMMA-DELTA .LE. 0.) CALL SETERR(                        
     1   ' POST4 - BETA+GAMMA-DELTA.LE.0', 30, 22, 2)                   
      IF (MMAX .LT. KMAX+2) CALL SETERR(' POST4 - MMAX.LT.KMAX+2', 23   
     1   , 23, 2)                                                       
      IF (N(1) .LT. 1) CALL SETERR(' POST4 - N(1).LT.1', 18, 24, 2)     
C/                                                                      
      DO  4 I = 2, MMAX                                                 
C/6S                                                                    
C        IF (N(I) .LE. N(I-1)) CALL SETERR(                             
C    1      37H POST4 - N IS NOT MONOTONE INCREASING, 37, 25, 2)        
C/7S                                                                    
         IF (N(I) .LE. N(I-1)) CALL SETERR(                             
     1      ' POST4 - N IS NOT MONOTONE INCREASING', 37, 25, 2)         
C/                                                                      
   4     CONTINUE                                                       
C/6S                                                                    
C     IF (HFRACT .LE. 0.) CALL SETERR(20H POST4 - HFRACT.LE.0, 20, 26, 2
C    1   )                                                              
C     IF (EGIVE .LT. 0.) CALL SETERR(21H POST4 - EGIVE .LT. 0, 21, 27, 2
C    1   )                                                              
C/7S                                                                    
      IF (HFRACT .LE. 0.) CALL SETERR(' POST4 - HFRACT.LE.0', 20, 26, 2 
     1   )                                                              
      IF (EGIVE .LT. 0.) CALL SETERR(' POST4 - EGIVE .LT. 0', 21, 27, 2 
     1   )                                                              
C/                                                                      
      ERPTSC = ERPUTS                                                   
      THETAC = THETA                                                    
      MINITC = MINIT                                                    
      MAXITC = MAXIT                                                    
      KEEACC = KEEJAC                                                   
      TEMP1 = KEEJAC .EQ. 1                                             
      IF (TEMP1) TEMP1 = MAXIT .EQ. 1                                   
      IF (TEMP1) KEEACC = 0                                             
      IF (SAVEB .LE. 0) GOTO 5                                          
         IGSSIS = ISTKGT(2*K*MGQ*(NX-2*K+1), 3)                         
         GOTO  10                                                       
   5     IF (SAVEB .GE. 0) GOTO 6                                       
            IGSSIS = 1                                                  
            GOTO  9                                                     
   6        TEMP1 = NU .GT. 0                                           
            IF (TEMP1) TEMP1 = KEEACC .GT. 1                            
            IF (.NOT. TEMP1) GOTO 7                                     
               IGSSIS = ISTKGT(2*K*MGQ*(NX-2*K+1), 3)                   
               GOTO  8                                                  
   7           IGSSIS = 1                                               
   8        CONTINUE                                                    
   9  CONTINUE                                                          
  10  SET = .FALSE.                                                     
      IF (KEEACC .LE. 1) GOTO 11                                        
         SEPATE = .TRUE.                                                
         GOTO  12                                                       
  11     SEPATE = .FALSE.                                               
  12  IF (KEEACC .LT. 3) GOTO 13                                        
         GETJAC = .TRUE.                                                
         TJ = TSTART                                                    
         GOTO  18                                                       
  13     IF (KEEACC .NE. 2) GOTO 14                                     
            TJ = TSTOP                                                  
C CANNOT BE TSTART.                                                     
            GOTO  17                                                    
  14        IF (THETA .LE. 0.5) GOTO 15                                 
               TJ = TSTART                                              
C CANNOT BE TSTART+THETA*DT/N.                                          
               GOTO  16                                                 
  15           TJ = TSTOP                                               
  16     CONTINUE                                                       
C CANNOT BE TSTART.                                                     
  17     CONTINUE                                                       
  18  DTJ = 0                                                           
C START WITH NO ERROR STATES.                                           
      FNUM = 0                                                          
      IORDER = ISTKGT(2*NU**2, 2)                                       
      IBC = ISTKGT(8*NU, 2)                                             
      IEQS = IBC+4*NU                                                   
      IF (NU .LE. 0) GOTO 19                                            
         CALL SETI(4*NU, 1, IS(IEQS))                                   
         CALL SETI(4*NU, -2, IS(IBC))                                   
  19  IAFB(1) = ISTKGT(6*NU*(2*NU+NV+1), 3)                             
      IAFB(2) = IAFB(1)+2*NU*(2*NU+NV+1)                                
      IAFB(3) = IAFB(2)+2*NU*(2*NU+NV+1)                                
      IDMAT = ISTKGT(NV**2, 3)                                          
      IALFA(1) = ISTKGT(6*NU*(2*NU+NV+1), 3)                            
      IBETA(1) = IALFA(1)+2*NU**2                                       
      IGAMMA(1) = IBETA(1)+2*NU**2                                      
      IALFA(2) = IGAMMA(1)+2*NU*(NV+1)                                  
      IBETA(2) = IALFA(2)+2*NU**2                                       
      IGAMMA(2) = IBETA(2)+2*NU**2                                      
      IALFA(3) = IGAMMA(2)+2*NU*(NV+1)                                  
      IBETA(3) = IALFA(3)+2*NU**2                                       
      IGAMMA(3) = IBETA(3)+2*NU**2                                      
      IL = 1                                                            
      IPPVOT = 1                                                        
      IDIAG = ISTKGT(NV, 3)                                             
      IDPVOT = ISTKGT(NV, 2)                                            
      IAA = ISTKGT(2*NU*(3*NU+2*(NV+1)), 3)                             
      IBB = IAA+2*NU**2                                                 
      ICC = IBB+2*NU**2                                                 
      ISGMAD = ICC+2*NU**2                                              
      ISGMAM = ISGMAD+2*NU*(NV+1)                                       
      ID4(1) = ISTKGT(3*NV**2, 3)                                       
      ID4(2) = ID4(1)+NV**2                                             
      ID4(3) = ID4(2)+NV**2                                             
      IF (.NOT. SEPATE) GOTO 20                                         
         IJP(1) = ISTKGT(3*(2*K*NU-1)*(NX-K)*NU, 3)                     
         IJP(2) = IJP(1)+(2*K*NU-1)*(NX-K)*NU                           
         IJP(3) = IJP(2)+(2*K*NU-1)*(NX-K)*NU                           
         IB(1) = ISTKGT(3*NU*(NX-K)*(NV+1), 3)                          
         IB(2) = IB(1)+NU*(NX-K)*(NV+1)                                 
         IB(3) = IB(2)+NU*(NX-K)*(NV+1)                                 
         ID5(1) = ISTKGT(3*NU*(NX-K)*NV, 3)                             
         ID5(2) = ID5(1)+NU*(NX-K)*NV                                   
         ID5(3) = ID5(2)+NU*(NX-K)*NV                                   
         IL = ISTKGT((K*NU-1)*(NX-K)*NU, 3)                             
         IPPVOT = ISTKGT((NX-K)*NU, 2)                                  
         GOTO  22                                                       
  20     CALL SETI(3, 1, IJP)                                           
         CALL SETI(3, 1, IB)                                            
         IF (KEEACC .NE. 1) GOTO 21                                     
            IJP(1) = ISTKGT(NU*(NX-K)*(2*K*NU-1), 3)                    
            IJP(3) = IJP(1)                                             
            IL = ISTKGT(NU*(NX-K)*(K*NU-1), 3)                          
            IB(1) = ISTKGT(NU*(NX-K)*(NV+1), 3)                         
            IB(3) = IB(1)                                               
            IPPVOT = ISTKGT(NU*(NX-K), 2)                               
  21     IAFB(3) = IAFB(1)                                              
         IALFA(3) = IALFA(1)                                            
         IBETA(3) = IBETA(1)                                            
         IGAMMA(3) = IGAMMA(1)                                          
         ID4(3) = ID4(1)                                                
         ID5(1) = ISTKGT(2*NU*(NX-K)*NV, 3)                             
         ID5(2) = ID5(1)+NU*(NX-K)*NV                                   
         ID5(3) = ID5(1)                                                
C FLAG SCALING WORK-SPACE AS UN-ALLOCATED.                              
  22  CALL SETI(4, 0, IMEM)                                             
C GET SPACE FOR PDE SCALING.                                            
      TEMP = IJP(3)                                                     
      CALL SCALE(1, 1, WS(TEMP), NU*(NX-K), 2*K*NU-1)                   
C GET SPACE FOR DIRICHLET BC SCALING.                                   
      CALL SCALE(2, 1, WS(IAA), NU, NU)                                 
C GET SPACE FOR NEUMAN BC SCALING.                                      
      CALL SCALE(3, 1, WS(IAA), NU, NU)                                 
C GET SPACE FOR ODE SCALING.                                            
      TEMP = ID4(3)                                                     
      CALL SCALE(4, 1, WS(TEMP), NV, NV)                                
      DO  23 I = 1, 4                                                   
C/6S                                                                    
C        IF (IMEM(I) .LE. 0) CALL SETERR(                               
C    1      51H POST4 - SCALE FAILED TO INITIALIZE COMMON /A9OSTS/, 51  
C    2      , 18, 2)                                                    
C/7S                                                                    
         IF (IMEM(I) .LE. 0) CALL SETERR(                               
     1      ' POST4 - SCALE FAILED TO INITIALIZE COMMON /A9OSTS/', 51   
     2      , 18, 2)                                                    
C/                                                                      
  23     CONTINUE                                                       
      EGIVEC = EGIVE                                                    
      TGOOD = TSTART                                                    
      IF (NU .LE. 0) GOTO 24                                            
         IXGQ = ISTKGT(2*MGQ, 3)                                        
         IWGQ = IXGQ+MGQ                                                
         MGQQ = MGQ                                                     
         CALL GQ1(MGQ, WS(IXGQ), WS(IWGQ))                              
         GOTO  25                                                       
  24     IXGQ = 1                                                       
         IWGQ = 1                                                       
         MGQQ = 0                                                       
  25  NUU = NU                                                          
      NVV = NV                                                          
      IF (NU .NE. 0) GOTO 26                                            
         KK = 2                                                         
         NXX = 2*KK                                                     
         GOTO  27                                                       
  26     KK = K                                                         
         NXX = NX                                                       
C TELL STATS ROUTINE IN POST.                                           
  27  CALL A9OSTX(STATS, 1)                                             
      CALL A90STS(U, NU, KK, X, NXX, V, NV, TSTART, TSTOP, DT, A9OSTA,  
     1   AF, A9OSTB, BC, A9OSTD, D, A9OSTE, ERROR, ERRPAR, INMI, SCALE  
     2   , A9OSTH, HANDLE, BETA, GAMMA, DELTA, N, KMAX, MMAX, XPOLY,    
     3   KINIT, HFRACT, A9OSTP, A9OSTN)                                 
C TELL STATS ROUTINE OUT OF POST.                                       
      CALL A9OSTX(STATS, -1)                                            
      TSTOP = TGOOD                                                     
C CAPTURE THE ERROR NUMBER, IF ANY.                                     
      NERR = NERROR(NERR)                                               
      IF (NERR .NE. 15) GOTO 28                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(13H POST4 - DT=0, 13, 1000, 1)                     
C/7S                                                                    
         CALL SETERR(' POST4 - DT=0', 13, 1000, 1)                      
C/                                                                      
  28  IF (NERR .NE. 16) GOTO 29                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(32H POST4 - DT=0 RETURNED BY HANDLE, 32, 1001, 1)  
C/7S                                                                    
         CALL SETERR(' POST4 - DT=0 RETURNED BY HANDLE', 32, 1001, 1)   
C/                                                                      
  29  IF (NERR .NE. 17) GOTO 30                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(45H POST4 - DT RETURNED BY HANDLE HAS WRONG SIGN,  
C    1      45, 1002, 1)                                                
C/7S                                                                    
         CALL SETERR(' POST4 - DT RETURNED BY HANDLE HAS WRONG SIGN',   
     1      45, 1002, 1)                                                
C/                                                                      
  30  IF (NERR .NE. 18) GOTO 31                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(46H POST4 - CANNOT RAISE DT IN HANDLE WHEN FAILED  
C    1      , 46, 1003, 1)                                              
C/7S                                                                    
         CALL SETERR(' POST4 - CANNOT RAISE DT IN HANDLE WHEN FAILED'   
     1      , 46, 1003, 1)                                              
C/                                                                      
  31  IF (NERR .NE. 19) GOTO 32                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(36H POST4 - E(I).LE.0 RETURNED BY ERROR, 36, 1004  
C    1      , 1)                                                        
C/7S                                                                    
         CALL SETERR(' POST4 - E(I).LE.0 RETURNED BY ERROR', 36, 1004   
     1      , 1)                                                        
C/                                                                      
  32  IF (NERR .NE. 15) GOTO 42                                         
         IF (FNUM .NE. 1) GOTO 33                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(19H POST4 - AF FAILURE, 19, 1013, 1)            
C/7S                                                                    
            CALL SETERR(' POST4 - AF FAILURE', 19, 1013, 1)             
C/                                                                      
  33     IF (FNUM .NE. 2) GOTO 34                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(19H POST4 - BC FAILURE, 19, 1014, 1)            
C/7S                                                                    
            CALL SETERR(' POST4 - BC FAILURE', 19, 1014, 1)             
C/                                                                      
  34     IF (FNUM .NE. 3) GOTO 35                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(18H POST4 - D FAILURE, 18, 1015, 1)             
C/7S                                                                    
            CALL SETERR(' POST4 - D FAILURE', 18, 1015, 1)              
C/                                                                      
  35     IF (FNUM .NE. 4) GOTO 36                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(                                                
C    1         47H POST4 - SINGULAR DIRICHLET BOUNDARY CONDITIONS, 47,  
C    2         1016, 1)                                                 
C/7S                                                                    
            CALL SETERR(                                                
     1         ' POST4 - SINGULAR DIRICHLET BOUNDARY CONDITIONS', 47,   
     2         1016, 1)                                                 
C/                                                                      
  36     IF (FNUM .NE. 5) GOTO 37                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(43H POST4 - SINGULAR MIXED BOUNDARY CONDITIONS  
C    1         , 43, 1017, 1)                                           
C/7S                                                                    
            CALL SETERR(' POST4 - SINGULAR MIXED BOUNDARY CONDITIONS'   
     1         , 43, 1017, 1)                                           
C/                                                                      
  37     IF (FNUM .NE. 6) GOTO 38                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(30H POST4 - SINGULAR PDE JACOBIAN, 30, 1018, 1) 
C/7S                                                                    
            CALL SETERR(' POST4 - SINGULAR PDE JACOBIAN', 30, 1018, 1)  
C/                                                                      
  38     IF (FNUM .NE. 7) GOTO 39                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(30H POST4 - SINGULAR ODE JACOBIAN, 30, 1019, 1) 
C/7S                                                                    
            CALL SETERR(' POST4 - SINGULAR ODE JACOBIAN', 30, 1019, 1)  
C/                                                                      
  39     IF (FNUM .NE. 8) GOTO 40                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(                                                
C    1         45H POST4 - TOO MANY NEWTON ITERATIONS PREDICTED, 45,    
C    2         1020, 1)                                                 
C/7S                                                                    
            CALL SETERR(                                                
     1         ' POST4 - TOO MANY NEWTON ITERATIONS PREDICTED', 45,     
     2         1020, 1)                                                 
C/                                                                      
  40     IF (FNUM .NE. 9) GOTO 41                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(42H POST4 - TOO MANY NEWTON ITERATIONS NEEDED,  
C    1         42, 1021, 1)                                             
C/7S                                                                    
            CALL SETERR(' POST4 - TOO MANY NEWTON ITERATIONS NEEDED',   
     1         42, 1021, 1)                                             
C/                                                                      
  41     CONTINUE                                                       
  42  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTH(T0, U0, V0, T, U, V, NU, NXMK, NV, K, X,         
     1   NX, DT, TSTOP)                                                 
      INTEGER NXMK, NX                                                  
      INTEGER NU, NV, K                                                 
      REAL T0, U0(NXMK, 1), V0(1), T, U(NXMK, 1), V(1)                  
      REAL X(NX), DT, TSTOP                                             
C DEFAULT HANDLE PROCEDURE FOR POSTS.                                   
C SCRATCH SPACE ALLOCATED - NONE.                                       
C REAL (U0,U)(NXMK,NU),(V0,V)(NV).                                      
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION POSTE(U, NU, NXMK, K, X, NX, V, NV, T, DT        
     1   , ERRPAR, ERPUTS, EU, EV)                                      
      INTEGER NXMK, NX                                                  
      INTEGER NU, K, NV                                                 
      REAL U(NXMK, 1), X(NX), V(1), T, DT, ERRPAR(2)                    
      REAL EU(NXMK, 1), EV(1)                                           
      LOGICAL ERPUTS                                                    
      INTEGER I, J                                                      
      REAL ABS, EMAX, TEMP, AMAX1, DTPOW, UNORM                         
      LOGICAL CONGED                                                    
C THE STANDARD ERROR PROCEDURE FOR  POSTS.                              
C SCRATCH SPACE ALLOCATED - NONE.                                       
C U(NXMK,NU),V(NV).                                                     
C EU(NXMK,NU),EV(NV).                                                   
      IF (.NOT. ERPUTS) GOTO 1                                          
         DTPOW = ABS(DT)                                                
         GOTO  2                                                        
   1     DTPOW = 1                                                      
   2  CONGED = .TRUE.                                                   
      I = 1                                                             
         GOTO  4                                                        
   3     I = I+1                                                        
   4     IF (I .GT. NV) GOTO  5                                         
C ERROR FOR V.                                                          
         TEMP = DTPOW*(ERRPAR(1)*ABS(V(I))+ERRPAR(2))                   
         IF (TEMP .LT. EV(I)) CONGED = .FALSE.                          
         EV(I) = TEMP                                                   
         GOTO  3                                                        
   5  J = 1                                                             
         GOTO  7                                                        
   6     J = J+1                                                        
   7     IF (J .GT. NU) GOTO  9                                         
C FIND || U(J) || AND || EU(J) || .                                     
         EMAX = 0                                                       
         UNORM = 0                                                      
         DO  8 I = 1, NXMK                                              
            EMAX = AMAX1(EMAX, ABS(EU(I, J)))                           
            UNORM = AMAX1(UNORM, ABS(U(I, J)))                          
   8        CONTINUE                                                    
         TEMP = DTPOW*(ERRPAR(1)*UNORM+ERRPAR(2))                       
         IF (TEMP .LT. EMAX) CONGED = .FALSE.                           
         CALL SETR(NXMK, TEMP, EU(1, J))                                
         GOTO  6                                                        
   9  POSTE = CONGED                                                    
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION POSTC(U, NU, NXMK, K, X, NX, V, NV, T, DT        
     1   , ERRPAR, ERPUTS, EU, EV)                                      
      INTEGER NXMK, NX                                                  
      INTEGER NU, K, NV                                                 
      REAL U(NXMK, 1), X(NX), V(1), T, DT, ERRPAR(2)                    
      REAL EU(NXMK, 1), EV(1)                                           
      LOGICAL ERPUTS                                                    
      INTEGER I, J                                                      
      REAL ABS, TEMP, DTPOW                                             
      LOGICAL CONGED                                                    
C THE COMPONENT ERROR PROCEDURE FOR  POSTS.                             
C SCRATCH SPACE ALLOCATED - NONE.                                       
C U(NXMK,NU),V(NV).                                                     
C EU(NXMK,NU),EV(NV).                                                   
      IF (.NOT. ERPUTS) GOTO 1                                          
         DTPOW = ABS(DT)                                                
         GOTO  2                                                        
   1     DTPOW = 1                                                      
   2  CONGED = .TRUE.                                                   
      I = 1                                                             
         GOTO  4                                                        
   3     I = I+1                                                        
   4     IF (I .GT. NV) GOTO  5                                         
C ERROR FOR V.                                                          
         TEMP = DTPOW*(ERRPAR(1)*ABS(V(I))+ERRPAR(2))                   
         IF (TEMP .LT. EV(I)) CONGED = .FALSE.                          
         EV(I) = TEMP                                                   
         GOTO  3                                                        
   5  J = 1                                                             
         GOTO  7                                                        
   6     J = J+1                                                        
   7     IF (J .GT. NU) GOTO  9                                         
C ERROR FOR U.                                                          
         DO  8 I = 1, NXMK                                              
            TEMP = DTPOW*(ERRPAR(1)*ABS(U(I, J))+ERRPAR(2))             
            IF (TEMP .LT. EU(I, J)) CONGED = .FALSE.                    
            EU(I, J) = TEMP                                             
   8        CONTINUE                                                    
         GOTO  6                                                        
   9  POSTC = CONGED                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTN(NU, NV, NXMK, K, X, NX, T, DT, UOLD, VOLD        
     1   , U, UT, V, VT)                                                
      INTEGER NXMK, NX                                                  
      INTEGER NU, NV, K                                                 
      REAL X(NX), T, DT, UOLD(NXMK, 1), VOLD(1), U(NXMK, 1)             
      REAL UT(NXMK, 1), V(1), VT(1)                                     
C THE DEFAULT NEWTON ITERATION INITIALIZER FOR POSTS.                   
C UOLD(NXMK,NU),VOLD(NV).                                               
C (U,UT)(NXMK,NU).                                                      
C (V,VT)(NV).                                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTP(I, J, A, M, N)                                   
      INTEGER I, J, M, N                                                
      REAL A(1)                                                         
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A9OSTS/ IMEM                                              
      INTEGER IMEM(4)                                                   
      INTEGER ISTKGT, II, IS(1000)                                      
      REAL RS(1000), WS(1000)                                           
      LOGICAL LS(1000)                                                  
      INTEGER TEMP1, TEMP2, TEMP3, TEMP4, TEMP5, TEMP6                  
      INTEGER TEMP7, TEMP8                                              
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE DEFAULT LINEAR SYSTEM PRE-CONDITIONING ROUTINE FOR POSTS.         
C A(M,N)                                                                
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      II = I                                                            
      IF (IMEM(I) .NE. 0) GOTO 3                                        
         IMEM(I) = ISTKGT(5, 2)                                         
C INITIALIZE.                                                           
C R, CA, RCA, CB AND RCB.                                               
CR.                                                                     
         TEMP3 = IMEM(II)                                               
         IS(TEMP3) = ISTKGT(M, 3)                                       
C CA.                                                                   
         TEMP3 = IMEM(II)                                               
         IS(TEMP3+1) = ISTKGT(M, 3)                                     
C RCA.                                                                  
         TEMP3 = IMEM(II)                                               
         IS(TEMP3+2) = ISTKGT(M, 2)                                     
         IF (I .NE. 4) GOTO 2                                           
            TEMP3 = IMEM(II)                                            
            IS(TEMP3+3) = ISTKGT(1, 3)                                  
            TEMP3 = IMEM(II)                                            
            IS(TEMP3+4) = ISTKGT(1, 2)                                  
            DO  1 II = 1, 3                                             
               TEMP3 = IMEM(II)                                         
               IS(TEMP3+3) = ISTKGT(M+1, 3)                             
               TEMP3 = IMEM(II)                                         
               IS(TEMP3+4) = ISTKGT(M+1, 2)                             
   1           CONTINUE                                                 
   2     CONTINUE                                                       
         GOTO  10                                                       
   3     TEMP = I .EQ. 1                                                
C SCALE.                                                                
         IF (TEMP) TEMP = J .EQ. 1                                      
         IF (.NOT. TEMP) GOTO 4                                         
            TEMP3 = IMEM(II)                                            
C SCALE THE BANDED PDE JACOBIAN.                                        
            TEMP4 = IS(TEMP3)                                           
            TEMP5 = IMEM(II)                                            
            TEMP6 = IS(TEMP5+1)                                         
            TEMP7 = IMEM(II)                                            
            TEMP8 = IS(TEMP7+2)                                         
            CALL RCSBA(A, M, N, WS(TEMP4), WS(TEMP6), IS(TEMP8))        
            GOTO  9                                                     
   4        IF (J .NE. 1) GOTO 5                                        
               TEMP8 = IMEM(II)                                         
C SCALE THE DENSE JACOBIAN.                                             
               TEMP7 = IS(TEMP8)                                        
               TEMP6 = IMEM(II)                                         
               TEMP5 = IS(TEMP6+1)                                      
               TEMP4 = IMEM(II)                                         
               TEMP3 = IS(TEMP4+2)                                      
               CALL RCSA(A, M, N, WS(TEMP7), WS(TEMP5), IS(TEMP3))      
               GOTO  8                                                  
   5           IF (J .NE. 2) GOTO 6                                     
                  TEMP3 = IMEM(II)                                      
C SCALE THE RHS OF THE EQUATIONS.                                       
                  TEMP4 = IS(TEMP3)                                     
                  TEMP5 = IMEM(II)                                      
                  TEMP6 = IS(TEMP5+3)                                   
                  TEMP7 = IMEM(II)                                      
                  TEMP8 = IS(TEMP7+4)                                   
                  CALL RCSB(A, M, N, WS(TEMP4), WS(TEMP6), IS(TEMP8))   
                  GOTO  7                                               
   6              TEMP8 = IMEM(II)                                      
C SCALE THE SOLUTION X.                                                 
                  TEMP7 = IS(TEMP8+1)                                   
                  TEMP6 = IMEM(II)                                      
                  TEMP5 = IS(TEMP6+2)                                   
                  TEMP4 = IMEM(II)                                      
                  TEMP3 = IS(TEMP4+3)                                   
                  TEMP2 = IMEM(II)                                      
                  TEMP1 = IS(TEMP2+4)                                   
                  CALL RCSX(A, M, N, WS(TEMP7), IS(TEMP5), WS(TEMP3),   
     1               IS(TEMP1))                                         
   7        CONTINUE                                                    
   8     CONTINUE                                                       
   9     CONTINUE                                                       
  10  RETURN                                                            
      END                                                               
      SUBROUTINE POSTW(J, F, R, I, L)                                   
      INTEGER J, I                                                      
      REAL F, R                                                         
      LOGICAL L                                                         
      INTEGER MGQ, MAX0, K, M, N(100), IABS                             
      INTEGER KEEJAC, KMAX, MMAX, SAVEB, KINIT, MINIT                   
      INTEGER MAXIT                                                     
      REAL HFRACT, BETA, SQRT, GAMMA, DELTA, EGIVE                      
      REAL THETA, FLOAT                                                 
      LOGICAL USENFD, USENGJ, USENNS, ERPUTS, XPOLY                     
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
C/7                                                                     
      SAVE K                                                            
C/                                                                      
      DATA K/0/                                                         
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
      DATA MGQ/0/                                                       
      DATA SAVEB/0/                                                     
      DATA XPOLY/.FALSE./                                               
      DATA ERPUTS/.FALSE./                                              
      DATA USENGJ/.FALSE./                                              
      DATA USENNS/.FALSE./                                              
      DATA USENFD/.FALSE./                                              
      DATA N(1)/1/, N(2)/0/, N(3)/0/                                    
C THE PARAMETER SETTING ROUTINE FOR POST.                               
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
C J = 2007. 0 IMPLIES MGQ = K-1 BY DEFAULT.                             
C J = 2008. -1 DO NOT SAVE, 0 DEFAULT, +1 SAVE.                         
C J = 3001.                                                             
C J = 3002.                                                             
C J = 3003.                                                             
C J = 3004.                                                             
C J = 3005.                                                             
C J = 4001, ... , 4100.                                                 
      GOTO  62                                                          
C   EXPORT THE VARIABLES.                                               
   1     F = THETA                                                      
         GOTO  63                                                       
   2     F = BETA                                                       
         GOTO  63                                                       
   3     F = GAMMA                                                      
         GOTO  63                                                       
   4     F = DELTA                                                      
         GOTO  63                                                       
   5     R = HFRACT                                                     
         GOTO  63                                                       
   6     R = EGIVE                                                      
         GOTO  63                                                       
   7     I = KEEJAC                                                     
         GOTO  63                                                       
   8     I = MINIT                                                      
         GOTO  63                                                       
   9     I = MAXIT                                                      
         GOTO  63                                                       
  10     I = KMAX                                                       
         GOTO  63                                                       
  11     I = KINIT                                                      
         GOTO  63                                                       
  12     I = MMAX                                                       
         GOTO  63                                                       
  13     IF (MGQ .NE. 0) GOTO 14                                        
            I = K-1                                                     
            GOTO  15                                                    
  14        I = MGQ                                                     
  15     GOTO  63                                                       
  16     I = SAVEB                                                      
         GOTO  63                                                       
  17     L = XPOLY                                                      
         GOTO  63                                                       
  18     L = ERPUTS                                                     
         GOTO  63                                                       
  19     L = USENGJ                                                     
         GOTO  63                                                       
  20     L = USENNS                                                     
         GOTO  63                                                       
  21     L = USENFD                                                     
         GOTO  63                                                       
C POST VERSION NUMBER.                                                  
  22     F = 3E0                                                        
         GOTO  63                                                       
C SET THE VARIABLES TO THE DEFAULTS.                                    
  23     THETA = 1E0                                                    
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
C 0 IMPLIES MGQ = K-1, BY DEFAULT.                                      
         MGQ = 0                                                        
         SAVEB = 0                                                      
         XPOLY = .FALSE.                                                
         ERPUTS = .FALSE.                                               
         USENGJ = .FALSE.                                               
         USENNS = .FALSE.                                               
         USENFD = .FALSE.                                               
         CALL SETI(100, 0, N)                                           
         N(1) = 1                                                       
C   IMPORT THE VARIABLES.                                               
         GOTO  63                                                       
  24     THETA = F                                                      
         IF (THETA .EQ. 0.5) GOTO 25                                    
            GAMMA = 1                                                   
            HFRACT = 1                                                  
            GOTO  29                                                    
  25        GAMMA = 2                                                   
            HFRACT = 0.5                                                
            N(1) = 2                                                    
            N(2) = 4                                                    
            N(3) = 6                                                    
            M = 4                                                       
               GOTO  27                                                 
  26           M = M+1                                                  
  27           IF (M .GT. MMAX) GOTO  28                                
               N(M) = 2*N(M-2)                                          
               GOTO  26                                                 
  28        CONTINUE                                                    
  29     GOTO  63                                                       
  30     BETA = F                                                       
         GOTO  63                                                       
  31     GAMMA = F                                                      
         GOTO  63                                                       
  32     DELTA = F                                                      
         GOTO  63                                                       
  33     HFRACT = R                                                     
         GOTO  63                                                       
  34     EGIVE = R                                                      
         GOTO  63                                                       
  35     KEEJAC = I                                                     
         GOTO  63                                                       
  36     MINIT = I                                                      
         GOTO  63                                                       
  37     MAXIT = I                                                      
         GOTO  63                                                       
  38     KMAX = I                                                       
         MMAX = KMAX+5                                                  
         GOTO  63                                                       
  39     KINIT = I                                                      
         GOTO  63                                                       
  40     MMAX = I                                                       
         GOTO  63                                                       
  41     MGQ = I                                                        
         GOTO  63                                                       
  42     SAVEB = I                                                      
         GOTO  63                                                       
  43     XPOLY = L                                                      
         GOTO  63                                                       
  44     ERPUTS = L                                                     
         IF (.NOT. ERPUTS) GOTO 45                                      
            DELTA = 1                                                   
            GOTO  46                                                    
  45        DELTA = 0                                                   
  46     GOTO  63                                                       
  47     USENGJ = L                                                     
         GOTO  63                                                       
  48     USENNS = L                                                     
         GOTO  63                                                       
  49     USENFD = L                                                     
         GOTO  63                                                       
  50     TEMP1 = IABS(J) .GT. 4100                                      
         IF (.NOT. TEMP1) TEMP1 = IABS(J) .LT. 4001                     
C/6S                                                                    
C        IF (TEMP1) CALL SETERR(24H POSTW - J OUT OF BOUNDS, 24, 1, 2)  
C/7S                                                                    
         IF (TEMP1) CALL SETERR(' POSTW - J OUT OF BOUNDS', 24, 1, 2)   
C/                                                                      
         IF (J .GE. 0) GOTO 60                                          
            IF (N(2) .NE. 0) GOTO 51                                    
               N(2) = SQRT(2E0)*FLOAT(N(1))                             
C EXPORT N(ABS(J)-4000)                                                 
C ONLY N(1) IS GIVEN, USE SQRT(2) INCREASE.                             
               IF (N(2) .EQ. N(1)) N(2) = N(2)+1                        
               N(3) = SQRT(2E0)*FLOAT(N(2))                             
               IF (N(3) .EQ. N(2)) N(3) = N(3)+1                        
               N(4) = 0                                                 
  51        TEMP = IABS(J)                                              
            IF (N(TEMP-4000) .NE. 0) GOTO 59                            
               DO  57 K = 1, MMAX                                       
C FILL IN THE MISSING N(M).                                             
                  IF (N(K) .NE. 0) GOTO 56                              
                     IF (K .NE. 3) GOTO 53                              
                        DO  52 M = K, MMAX                              
                           N(M) = (N(2)*N(M-1))/MAX0(1, N(1))           
  52                       CONTINUE                                     
                        GOTO  55                                        
  53                    DO  54 M = K, MMAX                              
                           N(M) = 2*N(M-2)                              
  54                       CONTINUE                                     
  55                 GOTO  58                                           
  56              CONTINUE                                              
  57              CONTINUE                                              
  58           CONTINUE                                                 
  59        TEMP = IABS(J)                                              
            I = N(TEMP-4000)                                            
            GOTO  61                                                    
  60        N(J-4000) = I                                               
C IMPORT N(J-4000)                                                      
            IF (J-4000 .LT. 100) N(J-3999) = 0                          
  61     CONTINUE                                                       
         GOTO  63                                                       
  62     IF (J .EQ. 3005) GOTO  49                                      
         IF (J .EQ. 3004) GOTO  48                                      
         IF (J .EQ. 3003) GOTO  47                                      
         IF (J .EQ. 3002) GOTO  44                                      
         IF (J .EQ. 3001) GOTO  43                                      
         IF (J .EQ. 2008) GOTO  42                                      
         IF (J .EQ. 2007) GOTO  41                                      
         IF (J .EQ. 2006) GOTO  40                                      
         IF (J .EQ. 2005) GOTO  39                                      
         IF (J .EQ. 2004) GOTO  38                                      
         IF (J .EQ. 2003) GOTO  37                                      
         IF (J .EQ. 2002) GOTO  36                                      
         IF (J .EQ. 2001) GOTO  35                                      
         IF (J .EQ. 1002) GOTO  34                                      
         IF (J .EQ. 1001) GOTO  33                                      
         IF (J .EQ. 4) GOTO  32                                         
         IF (J .EQ. 3) GOTO  31                                         
         IF (J .EQ. 2) GOTO  30                                         
         IF (J .EQ. 1) GOTO  24                                         
         IF (J .EQ. 0) GOTO  23                                         
         IF (J .EQ. (-6000)) GOTO  22                                   
         IF (J .EQ. (-3005)) GOTO  21                                   
         IF (J .EQ. (-3004)) GOTO  20                                   
         IF (J .EQ. (-3003)) GOTO  19                                   
         IF (J .EQ. (-3002)) GOTO  18                                   
         IF (J .EQ. (-3001)) GOTO  17                                   
         IF (J .EQ. (-2008)) GOTO  16                                   
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
         GOTO  50                                                       
  63  RETURN                                                            
      END                                                               
      SUBROUTINE POSTA(T, X, NX, U, UX, UT, UTX, NU, V, VT, NV, A,      
     1   AU, AUX, AUT, AUTX, AV, AVT, F, FU, FUX, FUT, FUTX, FV, FVT)   
      INTEGER NU, NV, NX                                                
      REAL T, X(NX), U(NX, NU), UX(NX, NU), UT(NX, NU), UTX(NX, NU)     
      REAL V(NV), VT(NV), A(NX, NU), AU(NX, NU, NU), AUX(NX, NU, NU),   
     1   AUT(NX, NU, NU)                                                
      REAL AUTX(NX, NU, NU), AV(NX, NU, NV), AVT(NX, NU, NV), F(NX, NU),
     1   FU(NX, NU, NU), FUX(NX, NU, NU)                                
      REAL FUT(NX, NU, NU), FUTX(NX, NU, NU), FV(NX, NU, NV), FVT(NX,   
     1   NU, NV)                                                        
C THE DEFAULT, NULL AF ROUTINE FOR POST.                                
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTB(T, L, R, U, UX, UT, UTX, NU, V, VT, NV, B        
     1   , BU, BUX, BUT, BUTX, BV, BVT)                                 
      INTEGER NU, NV                                                    
      REAL T, L, R, U(NU, 2), UX(NU, 2), UT(NU, 2)                      
      REAL UTX(NU, 2), V(NV), VT(NV), B(NU, 2), BU(NU, NU, 2), BUX(NU,  
     1   NU, 2)                                                         
      REAL BUT(NU, NU, 2), BUTX(NU, NU, 2), BV(NU, NV, 2), BVT(NU, NV, 2
     1   )                                                              
C THE DEFAULT, NULL BC ROUTINE FOR POST.                                
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTD(T, K, X, NX, U, UT, NU, NXMK, V, VT, NV, D,      
     1   DU, DUT, DV, DVT)                                              
      INTEGER NXMK, NV, NX                                              
      INTEGER K, NU                                                     
      REAL T, X(NX), U(NXMK, 1), UT(NXMK, 1), V(NV), VT(NV)             
      REAL D(NV), DU(NV, NXMK, 1), DUT(NV, NXMK, 1), DV(NV, NV), DVT(NV,
     1   NV)                                                            
C SCRATCH SPACE ALLOCATED - NONE.                                       
C (U,UT)(NXMK,NU).                                                      
C (DU,DUT)(NV,NXMK,NU).                                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTI(XI, X, XT, XXI, XV, XTV, XXIV, XVV, NX, UX,      
     1   UT, NU, V, VT, NV, IV, NVM, A, AX, AU, AUX, AUT, AUTX, AV, AVT,
     2   F, FX, FU, FUX, FUT, FUTX, FV, FVT)                            
      INTEGER NVM, NU, NV, NX                                           
      INTEGER IV                                                        
      REAL XI(NX), X(NX), XT(NX), XXI(NX), XV(NX, NVM), XTV(NX, NVM)    
      REAL XXIV(NX, NVM), XVV(NX, NVM, NVM), UX(NX, NU), UT(NX, NU), V( 
     1   NV), VT(NV)                                                    
      REAL A(NX, NU), AX(NX, NU), AU(NX, NU, NU), AUX(NX, NU, NU), AUT( 
     1   NX, NU, NU), AUTX(NX, NU, NU)                                  
      REAL AV(NX, NU, NV), AVT(NX, NU, NV), F(NX, NU), FX(NX, NU), FU(  
     1   NX, NU, NU), FUX(NX, NU, NU)                                   
      REAL FUT(NX, NU, NU), FUTX(NX, NU, NU), FV(NX, NU, NV), FVT(NX,   
     1   NU, NV)                                                        
      INTEGER I, J, L, I1, IX                                           
      REAL TERMAT, TERMAX, TERMFT, TERMFX, TERMVV, TERM                 
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
C TO PERFORM INTERVAL MAPPING FROM USER COORDINATES BACK INTO INTERNAL  
C POST COORDINATES.                                                     
C/6S                                                                    
C     IF (NX .LT. 1) CALL SETERR(18H POSTI - NX .LT. 1, 18, 1, 2)       
C     IF (NU .LT. 1) CALL SETERR(18H POSTI - NU .LT. 1, 18, 2, 2)       
C     IF (NV .LT. 0) CALL SETERR(18H POSTI - NV .LT. 0, 18, 3, 2)       
C/7S                                                                    
      IF (NX .LT. 1) CALL SETERR(' POSTI - NX .LT. 1', 18, 1, 2)        
      IF (NU .LT. 1) CALL SETERR(' POSTI - NU .LT. 1', 18, 2, 2)        
      IF (NV .LT. 0) CALL SETERR(' POSTI - NV .LT. 0', 18, 3, 2)        
C/                                                                      
      TEMP1 = IV .LT. 0                                                 
      IF (.NOT. TEMP1) TEMP1 = IV .GT. NV                               
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(29H POSTI - IV MUST BE IN (0,NV), 29, 4, 2)
C/7S                                                                    
      IF (TEMP1) CALL SETERR(' POSTI - IV MUST BE IN (0,NV)', 29, 4, 2) 
C/                                                                      
      TEMP1 = NVM .LT. 0                                                
      IF (.NOT. TEMP1) TEMP1 = IV+NVM-1 .GT. NV                         
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(30H POSTI - NVM MUST BE IN (0,NV), 30, 5, 2
C    1   )                                                              
C/7S                                                                    
      IF (TEMP1) CALL SETERR(' POSTI - NVM MUST BE IN (0,NV)', 30, 5, 2 
     1   )                                                              
C/                                                                      
      DO  13 IX = 1, NX                                                 
         TERM = XT(IX)                                                  
         J = 1                                                          
            GOTO  2                                                     
   1        J = J+1                                                     
   2        IF (J .GT. NVM) GOTO  3                                     
            TEMP = J+IV                                                 
            TERM = TERM+XV(IX, J)*VT(TEMP-1)                            
            GOTO  1                                                     
   3     DO  12 I = 1, NU                                               
            DO  4 J = 1, NV                                             
               FV(IX, I, J) = FV(IX, I, J)*XXI(IX)                      
               FVT(IX, I, J) = FVT(IX, I, J)*XXI(IX)                    
   4           CONTINUE                                                 
            TERMAX = 0                                                  
            TERMFX = 0                                                  
            TERMAT = 0                                                  
            TERMFT = 0                                                  
            DO  5 J = 1, NU                                             
               TERMAX = TERMAX+AUX(IX, I, J)*UX(IX, J)                  
               TERMFX = TERMFX+FUX(IX, I, J)*UX(IX, J)                  
               TERMAT = TERMAT+AUT(IX, I, J)*UX(IX, J)                  
               TERMFT = TERMFT+FUT(IX, I, J)*UX(IX, J)                  
               AUX(IX, I, J) = (AUX(IX, I, J)-TERM*AUT(IX, I, J))/XXI(  
     1            IX)                                                   
               FUX(IX, I, J) = FUX(IX, I, J)-TERM*FUT(IX, I, J)         
               FUT(IX, I, J) = FUT(IX, I, J)*XXI(IX)                    
               FU(IX, I, J) = FU(IX, I, J)*XXI(IX)                      
               TEMP1 = AUTX(IX, I, J) .NE. 0.                           
               IF (.NOT. TEMP1) TEMP1 = FUTX(IX, I, J) .NE. 0.          
C/6S                                                                    
C              IF (TEMP1) CALL SETERR(                                  
C    1            34H POSTI - MUST HAVE AUTX = 0 = FUTX, 34, 6, 2)      
C/7S                                                                    
               IF (TEMP1) CALL SETERR(                                  
     1            ' POSTI - MUST HAVE AUTX = 0 = FUTX', 34, 6, 2)       
C/                                                                      
   5           CONTINUE                                                 
            J = 1                                                       
               GOTO  7                                                  
   6           J = J+1                                                  
   7           IF (J .GT. NVM) GOTO  11                                 
               I1 = J+IV-1                                              
               TERMVV = XTV(IX, J)                                      
               L = 1                                                    
                  GOTO  9                                               
   8              L = L+1                                               
   9              IF (L .GT. NVM) GOTO  10                              
                  TEMP = L+IV                                           
                  TERMVV = TERMVV+XVV(IX, L, J)*VT(TEMP-1)              
                  GOTO  8                                               
  10           AV(IX, I, I1) = AV(IX, I, I1)+AX(IX, I)*XV(IX, J)-(      
     1            TERMAX*XXIV(IX, J)+TERMAT*(TERMVV*XXI(IX)-TERM*XXIV(  
     2            IX, J)))/XXI(IX)                                      
               FV(IX, I, I1) = FV(IX, I, I1)+FX(IX, I)*XXI(IX)*XV(IX, J)
     1            -(TERMFX*XXIV(IX, J)+TERMFT*(TERMVV*XXI(IX)-TERM*XXIV(
     2            IX, J)))+XXIV(IX, J)*F(IX, I)                         
               AVT(IX, I, I1) = AVT(IX, I, I1)-TERMAT*XV(IX, J)         
               FVT(IX, I, I1) = FVT(IX, I, I1)-TERMFT*XV(IX, J)*XXI(IX) 
               GOTO  6                                                  
  11        F(IX, I) = F(IX, I)*XXI(IX)                                 
  12        CONTINUE                                                    
  13     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTJ(XI, X, XT, XXI, XV, XTV, XXIV, XVV, UX, UT,      
     1   NU, V, VT, NV, IV, NVM, B, BX, BU, BUX, BUT, BUTX, BV, BVT)    
      INTEGER NVM, NU, NV                                               
      INTEGER IV                                                        
      REAL XI(2), X(2), XT(2), XXI(2), XV(2, NVM), XTV(2, NVM)          
      REAL XXIV(2, NVM), XVV(2, NVM, NVM), UX(NU, 2), UT(NU, 2), V(NV)  
     1   , VT(NV)                                                       
      REAL B(NU, 2), BX(NU, 2), BU(NU, NU, 2), BUX(NU, NU, 2), BUT(NU,  
     1   NU, 2), BUTX(NU, NU, 2)                                        
      REAL BV(NU, NV, 2), BVT(NU, NV, 2)                                
      INTEGER I, J, L, I1, IX                                           
      REAL TERMBT, TERMBX, TERMVV, TERM                                 
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
C TO PERFORM INTERVAL MAPPING FROM USER COORDINATES BACK INTO INTERNAL  
C POST COORDINATES.                                                     
C/6S                                                                    
C     IF (NU .LT. 1) CALL SETERR(18H POSTJ - NU .LT. 1, 18, 1, 2)       
C     IF (NV .LT. 0) CALL SETERR(18H POSTJ - NV .LT. 0, 18, 2, 2)       
C/7S                                                                    
      IF (NU .LT. 1) CALL SETERR(' POSTJ - NU .LT. 1', 18, 1, 2)        
      IF (NV .LT. 0) CALL SETERR(' POSTJ - NV .LT. 0', 18, 2, 2)        
C/                                                                      
      TEMP1 = IV .LT. 0                                                 
      IF (.NOT. TEMP1) TEMP1 = IV .GT. NV                               
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(29H POSTJ - IV MUST BE IN (0,NV), 29, 3, 2)
C/7S                                                                    
      IF (TEMP1) CALL SETERR(' POSTJ - IV MUST BE IN (0,NV)', 29, 3, 2) 
C/                                                                      
      TEMP1 = NVM .LT. 0                                                
      IF (.NOT. TEMP1) TEMP1 = IV+NVM-1 .GT. NV                         
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(30H POSTJ - NVM MUST BE IN (0,NV), 30, 4, 2
C    1   )                                                              
C/7S                                                                    
      IF (TEMP1) CALL SETERR(' POSTJ - NVM MUST BE IN (0,NV)', 30, 4, 2 
     1   )                                                              
C/                                                                      
      DO  12 IX = 1, 2                                                  
         TERM = XT(IX)                                                  
         J = 1                                                          
            GOTO  2                                                     
   1        J = J+1                                                     
   2        IF (J .GT. NVM) GOTO  3                                     
            TEMP = J+IV                                                 
            TERM = TERM+XV(IX, J)*VT(TEMP-1)                            
            GOTO  1                                                     
   3     DO  11 I = 1, NU                                               
            TERMBX = 0                                                  
            TERMBT = 0                                                  
            DO  4 J = 1, NU                                             
               TERMBX = TERMBX+BUX(I, J, IX)*UX(J, IX)                  
               TERMBT = TERMBT+BUT(I, J, IX)*UX(J, IX)                  
               BUX(I, J, IX) = BUX(I, J, IX)-TERM*BUT(I, J, IX)         
               BUX(I, J, IX) = BUX(I, J, IX)/XXI(IX)                    
C/6S                                                                    
C              IF (BUTX(I, J, IX) .NE. 0.) CALL SETERR(                 
C    1            27H POSTJ - MUST HAVE BUTX = 0, 27, 5, 2)             
C/7S                                                                    
               IF (BUTX(I, J, IX) .NE. 0.) CALL SETERR(                 
     1            ' POSTJ - MUST HAVE BUTX = 0', 27, 5, 2)              
C/                                                                      
   4           CONTINUE                                                 
            J = 1                                                       
               GOTO  6                                                  
   5           J = J+1                                                  
   6           IF (J .GT. NVM) GOTO  10                                 
               I1 = J+IV-1                                              
               TERMVV = XTV(IX, J)                                      
               L = 1                                                    
                  GOTO  8                                               
   7              L = L+1                                               
   8              IF (L .GT. NVM) GOTO  9                               
                  TEMP = L+IV                                           
                  TERMVV = TERMVV+XVV(IX, L, J)*VT(TEMP-1)              
                  GOTO  7                                               
   9           BV(I, I1, IX) = BV(I, I1, IX)+BX(I, IX)*XV(IX, J)-(      
     1            TERMBX*XXIV(IX, J)+TERMBT*(TERMVV*XXI(IX)-TERM*XXIV(  
     2            IX, J)))/XXI(IX)                                      
               BVT(I, I1, IX) = BVT(I, I1, IX)-TERMBT*XV(IX, J)         
               GOTO  5                                                  
  10        CONTINUE                                                    
  11        CONTINUE                                                    
  12     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTU(XI, X, XT, XXI, XV, VT, NX, NV, UX, UT, NU,      
     1   AX, FX)                                                        
      INTEGER NU, NV, NX                                                
      REAL XI(NX), X(NX), XT(NX), XXI(NX), XV(NX, NV), VT(NV)           
      REAL UX(NX, NU), UT(NX, NU), AX(NX, NU), FX(NX, NU)               
      INTEGER I, J, IX                                                  
      REAL XVSUM                                                        
C TO PERFORM INTERVAL MAPPING FROM INTERNAL POST COORDINATES            
C INTO USER COORDINATES.                                                
C/6S                                                                    
C     IF (NX .LT. 1) CALL SETERR(18H POSTU - NX .LT. 1, 18, 1, 2)       
C     IF (NV .LT. 0) CALL SETERR(18H POSTU - NV .LT. 0, 18, 2, 2)       
C     IF (NU .LT. 1) CALL SETERR(18H POSTU - NU .LT. 1, 18, 3, 2)       
C/7S                                                                    
      IF (NX .LT. 1) CALL SETERR(' POSTU - NX .LT. 1', 18, 1, 2)        
      IF (NV .LT. 0) CALL SETERR(' POSTU - NV .LT. 0', 18, 2, 2)        
      IF (NU .LT. 1) CALL SETERR(' POSTU - NU .LT. 1', 18, 3, 2)        
C/                                                                      
C MAP INTO USER SYSTEM.                                                 
      DO  5 IX = 1, NX                                                  
         XVSUM = 0                                                      
         J = 1                                                          
            GOTO  2                                                     
   1        J = J+1                                                     
   2        IF (J .GT. NV) GOTO  3                                      
            XVSUM = XVSUM+VT(J)*XV(IX, J)                               
            GOTO  1                                                     
   3     DO  4 I = 1, NU                                                
            UX(IX, I) = UX(IX, I)/XXI(IX)                               
            UT(IX, I) = UT(IX, I)-UX(IX, I)*(XT(IX)+XVSUM)              
   4        CONTINUE                                                    
   5     CONTINUE                                                       
      CALL SETR(NX*NU, 0E0, AX)                                         
C AX = 0 = FX BY DEFAULT.                                               
      CALL SETR(NX*NU, 0E0, FX)                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTV(XI, X, XT, XXI, XV, VT, NV, UX, UT, NU, BX)      
      INTEGER NU, NV                                                    
      REAL XI(2), X(2), XT(2), XXI(2), XV(2, NV), VT(NV)                
      REAL UX(NU, 2), UT(NU, 2), BX(NU, 2)                              
      INTEGER I, J, IX                                                  
      REAL XVSUM                                                        
C TO PERFORM INTERVAL MAPPING FROM INTERNAL POST COORDINATES            
C INTO USER COORDINATES.                                                
C/6S                                                                    
C     IF (NV .LT. 0) CALL SETERR(18H POSTV - NV .LT. 0, 18, 1, 2)       
C     IF (NU .LT. 1) CALL SETERR(18H POSTV - NU .LT. 1, 18, 2, 2)       
C/7S                                                                    
      IF (NV .LT. 0) CALL SETERR(' POSTV - NV .LT. 0', 18, 1, 2)        
      IF (NU .LT. 1) CALL SETERR(' POSTV - NU .LT. 1', 18, 2, 2)        
C/                                                                      
C MAP INTO USER SYSTEM.                                                 
      DO  5 IX = 1, 2                                                   
         XVSUM = 0                                                      
         J = 1                                                          
            GOTO  2                                                     
   1        J = J+1                                                     
   2        IF (J .GT. NV) GOTO  3                                      
            XVSUM = XVSUM+VT(J)*XV(IX, J)                               
            GOTO  1                                                     
   3     DO  4 I = 1, NU                                                
            UX(I, IX) = UX(I, IX)/XXI(IX)                               
            UT(I, IX) = UT(I, IX)-UX(I, IX)*(XT(IX)+XVSUM)              
   4        CONTINUE                                                    
   5     CONTINUE                                                       
C BX = 0 BY DEFAULT.                                                    
      CALL SETR(2*NU, 0E0, BX)                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE LPLMX(XI, NX, V, NV, X, XV)                            
      INTEGER NV, NX                                                    
      REAL XI(NX), V(NV), X(NX), XV(NX, NV)                             
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IXT, ISTKGT, IXXI, IXTV, IXVV, IS(1000)                   
      INTEGER IXXIV                                                     
      REAL RS(1000), WS(500)                                            
      LOGICAL LS(1000)                                                  
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C X(XI,V) IS A PIECEWISE LINEAR MAP IN XI, V == BREAKPOINTS.            
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (NX .LT. 1) CALL SETERR(18H LPLMX - NX .LT. 1, 18, 1, 2)       
C     IF (NV .LT. 2) CALL SETERR(18H LPLMX - NV .LT. 2, 18, 2, 2)       
C/7S                                                                    
      IF (NX .LT. 1) CALL SETERR(' LPLMX - NX .LT. 1', 18, 1, 2)        
      IF (NV .LT. 2) CALL SETERR(' LPLMX - NV .LT. 2', 18, 2, 2)        
C/                                                                      
      IXXI = ISTKGT(NX, 3)                                              
      IXXIV = ISTKGT(NX*NV, 3)                                          
      IXVV = ISTKGT(NX*NV**2, 3)                                        
      IXT = ISTKGT(NX, 3)                                               
      IXTV = ISTKGT(NX*NV, 3)                                           
      CALL LPLM(XI, NX, V, NV, X, WS(IXXI), WS(IXXIV), XV, WS(IXVV), WS(
     1   IXT), WS(IXTV))                                                
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE LPLM(XI, NX, V, NV, X, XXI, XXIV, XV, XVV, XT,         
     1   XTV)                                                           
      INTEGER NV, NX                                                    
      REAL XI(NX), V(NV), X(NX), XXI(NX), XXIV(NX, NV), XV(NX, NV)      
      REAL XVV(NX, NV, NV), XT(NX), XTV(NX, NV)                         
      INTEGER MIN0, I, IX                                               
      REAL FLOAT                                                        
      LOGICAL TEMP                                                      
C X(XI,V) IS A PIECEWISE LINEAR MAP IN XI, V == BREAKPOINTS.            
C/6S                                                                    
C     IF (NX .LT. 1) CALL SETERR(27H LPLM - MUST HAVE NX .GE. 1, 27, 1  
C    1   , 2)                                                           
C     IF (NV .LT. 2) CALL SETERR(27H LPLM - MUST HAVE NV .GE. 2, 27, 2  
C    1   , 2)                                                           
C/7S                                                                    
      IF (NX .LT. 1) CALL SETERR(' LPLM - MUST HAVE NX .GE. 1', 27, 1   
     1   , 2)                                                           
      IF (NV .LT. 2) CALL SETERR(' LPLM - MUST HAVE NV .GE. 2', 27, 2   
     1   , 2)                                                           
C/                                                                      
      CALL SETR(NX*NV, 0E0, XXIV)                                       
      CALL SETR(NX*NV, 0E0, XV)                                         
      CALL SETR(NX*NV**2, 0E0, XVV)                                     
      CALL SETR(NX, 0E0, XT)                                            
      CALL SETR(NX*NV, 0E0, XTV)                                        
      DO  1 IX = 1, NX                                                  
         TEMP = XI(IX) .LT. 0.                                          
         IF (.NOT. TEMP) TEMP = XI(IX) .GT. FLOAT(NV-1)                 
C/6S                                                                    
C        IF (TEMP) CALL SETERR(30H LPLM - XI(IX) NOT IN (0,NV-1), 30, 3,
C    1      2)                                                          
C/7S                                                                    
         IF (TEMP) CALL SETERR(' LPLM - XI(IX) NOT IN (0,NV-1)', 30, 3, 
     1      2)                                                          
C/                                                                      
         I = XI(IX)                                                     
C THE INDEX OF XI(IX) IN V.                                             
         I = I+1                                                        
C DO NOT RUN OFF THE END.                                               
         I = MIN0(NV-1, I)                                              
         X(IX) = V(I)+(V(I+1)-V(I))*(XI(IX)-FLOAT(I-1))                 
         XV(IX, I) = FLOAT(I)-XI(IX)                                    
         XV(IX, I+1) = XI(IX)-FLOAT(I-1)                                
         XXI(IX) = V(I+1)-V(I)                                          
         XXIV(IX, I) = -1                                               
         XXIV(IX, I+1) = 1                                              
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE LPLMG(NV, X, V)                                        
      INTEGER NV                                                        
      REAL X(NV), V(NV)                                                 
      INTEGER I                                                         
C FORCE X(I-1,V) = X(I), I = 1,..., NV, FOR THE LPLM MAPS.              
C/6S                                                                    
C     IF (NV .LT. 2) CALL SETERR(18H LPLMG - NV .LT. 2, 18, 1, 2)       
C/7S                                                                    
      IF (NV .LT. 2) CALL SETERR(' LPLMG - NV .LT. 2', 18, 1, 2)        
C/                                                                      
      DO  1 I = 2, NV                                                   
C/6S                                                                    
C        IF (X(I) .LE. X(I-1)) CALL SETERR(                             
C    1      46H LPLMG - X IS NOT STRICTLY MONOTONE INCREASING, 46, 2, 2)
C/7S                                                                    
         IF (X(I) .LE. X(I-1)) CALL SETERR(                             
     1      ' LPLMG - X IS NOT STRICTLY MONOTONE INCREASING', 46, 2, 2) 
C/                                                                      
   1     CONTINUE                                                       
C GET V.                                                                
      DO  2 I = 1, NV                                                   
         V(I) = X(I)                                                    
   2     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE LPLMI(NV, V, X, NX, XI)                                
      INTEGER NV, NX                                                    
      REAL V(NV), X(NX), XI(NX)                                         
      INTEGER I, IX                                                     
      REAL FLOAT                                                        
      LOGICAL TEMP                                                      
C GET XI(X) FROM X(XI) = X.                                             
C/6S                                                                    
C     IF (NV .LT. 2) CALL SETERR(18H LPLMI - NV .LT. 2, 18, 1, 2)       
C     IF (NX .LT. 1) CALL SETERR(18H LPLMI - NX .LT. 1, 18, 2, 2)       
C/7S                                                                    
      IF (NV .LT. 2) CALL SETERR(' LPLMI - NV .LT. 2', 18, 1, 2)        
      IF (NX .LT. 1) CALL SETERR(' LPLMI - NX .LT. 1', 18, 2, 2)        
C/                                                                      
      DO  4 IX = 1, NX                                                  
         I = 1                                                          
            GOTO  2                                                     
   1        I = I+1                                                     
   2        IF (I .GE. NV) GOTO  3                                      
C FIND X(IX) IN RANGE OF X.                                             
            TEMP = X(IX) .GE. V(I)                                      
            IF (TEMP) TEMP = X(IX) .LE. V(I+1)                          
            IF (TEMP) GOTO  3                                           
            GOTO  1                                                     
   3     IF (I .GE. NV) I = NV-1                                        
C X NOT IN RANGE, USE LAST INTERVAL.                                    
         XI(IX) = (X(IX)-V(I))/(V(I+1)-V(I))+FLOAT(I)-1.                
   4     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      REAL FUNCTION LPLMT(T1, V1, NV, T2, V2, F, DT)                    
      INTEGER NV                                                        
      REAL T1, V1(NV), T2, V2(NV), F, DT                                
      INTEGER I, J                                                      
      REAL ABS, DTT, SGNDT, SGNT21, SGNV21                              
      LOGICAL TEMP                                                      
C TO RETURN THE VALUE OF DT THAT WILL CAUSE THE BREAK-POINTS TO MOVE    
C NO MORE THAN A FACTOR OF F CLOSER TOGETHER OVER THE NEXT TIME-STEP.   
C/6S                                                                    
C     IF (T1 .EQ. T2) CALL SETERR(17H LPLMT - T1 == T2, 17, 1, 2)       
C     IF (NV .LT. 2) CALL SETERR(18H LPLMT - NV .LT. 2, 18, 2, 2)       
C/7S                                                                    
      IF (T1 .EQ. T2) CALL SETERR(' LPLMT - T1 == T2', 17, 1, 2)        
      IF (NV .LT. 2) CALL SETERR(' LPLMT - NV .LT. 2', 18, 2, 2)        
C/                                                                      
      TEMP = F .LE. 0.                                                  
      IF (.NOT. TEMP) TEMP = F .GE. 1.                                  
C/6S                                                                    
C     IF (TEMP) CALL SETERR(23H LPLMT - F NOT IN (0,1), 23, 3, 2)       
C     IF (DT .EQ. 0.) CALL SETERR(16H LPLMT - DT == 0, 16, 4, 2)        
C/7S                                                                    
      IF (TEMP) CALL SETERR(' LPLMT - F NOT IN (0,1)', 23, 3, 2)        
      IF (DT .EQ. 0.) CALL SETERR(' LPLMT - DT == 0', 16, 4, 2)         
C/                                                                      
      DTT = DT                                                          
      SGNDT = DT/ABS(DT)                                                
      SGNT21 = (T2-T1)/ABS(T2-T1)                                       
      DO  3 I = 1, NV                                                   
         IF (V2(I) .EQ. V1(I)) GOTO  3                                  
         SGNV21 = (V2(I)-V1(I))/ABS(V2(I)-V1(I))                        
         DO  2 J = 1, NV                                                
            IF (I .EQ. J) GOTO  2                                       
            IF (V2(I) .NE. V2(J)) GOTO 1                                
               DTT = 0                                                  
               LPLMT = DTT                                              
               RETURN                                                   
   1        TEMP = SGNDT*SGNT21*((V2(I)-V1(I))/ABS(V2(I)-V1(I)))*((V2(I)
     1         -V2(J))/ABS(V2(I)-V2(J))) .LT. 0.                        
            IF (TEMP) TEMP = SGNDT*SGNV21*(DTT*(V2(I)-V1(I))-(T2-T1)*(  
     1         V2(I)-V2(J))*(F-1.)) .GT. 0.                             
            IF (TEMP) DTT = (T2-T1)*(V2(I)-V2(J))*(F-1.)/(V2(I)-V1(I))  
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      LPLMT = DTT                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSA(A, M, N, R, C, RC)                                
      INTEGER M, N                                                      
      INTEGER RC(N)                                                     
      REAL A(M, N), R(M), C(N)                                          
      INTEGER NERROR, NERR, IROLD                                       
C TO GET THE ROW AND COLUMN SCALE FACTORS FOR A                         
C AND PERFORM THE SCALING.                                              
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16H RCSA - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16H RCSA - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' RCSA - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR(' RCSA - N .LT. 1', 16, 2, 2)           
C/                                                                      
C TURN ERROR RECOVERY ON AND SAVE OLD VALUE.                            
      CALL ENTSRC(IROLD, 1)                                             
C GET ROW AND COLUMN SCALE FACTORS.                                     
      CALL RCSM(A, M, N, R, C, RC)                                      
C APPLY THE SCALE FACTORS TO A.                                         
      CALL RCSS(A, M, N, R, C, RC)                                      
      IF (NERROR(NERR) .EQ. 0) GOTO 1                                   
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(42H RCSA - MUST HAVE 1/(S*L) IN MACHINE RANGE, 42  
C    1      , 3, 1)                                                     
C/7S                                                                    
         CALL SETERR(' RCSA - MUST HAVE 1/(S*L) IN MACHINE RANGE', 42   
     1      , 3, 1)                                                     
C/                                                                      
         RETURN                                                         
C RESTORE OLD RECOVERY VALUE.                                           
   1  CALL RETSRC(IROLD)                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSBA(A, M, N, R, C, RC)                               
      INTEGER M, N                                                      
      INTEGER RC(M)                                                     
      REAL A(M, N), R(M), C(M)                                          
      INTEGER NERROR, NERR                                              
C TO GET THE ROW AND COLUMN SCALE FACTORS FOR A                         
C AND PERFORM THE SCALING.                                              
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(17H RCSBA - M .LT. 1, 17, 1, 2)         
C     IF (N .LT. 1) CALL SETERR(17H RCSBA - N .LT. 1, 17, 2, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' RCSBA - M .LT. 1', 17, 1, 2)          
      IF (N .LT. 1) CALL SETERR(' RCSBA - N .LT. 1', 17, 2, 2)          
C/                                                                      
      CALL ENTER(1)                                                     
C GET ROW AND COLUMN SCALE FACTORS.                                     
      CALL RCSBM(A, M, N, R, C, RC)                                     
C APPLY THE SCALE FACTORS TO A.                                         
      CALL RCSBS(A, M, N, R, C, RC)                                     
      IF (NERROR(NERR) .EQ. 0) GOTO 1                                   
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(43H RCSBA - MUST HAVE 1/(S*L) IN MACHINE RANGE, 43,
C    1      3, 1)                                                       
C/7S                                                                    
         CALL SETERR(' RCSBA - MUST HAVE 1/(S*L) IN MACHINE RANGE', 43, 
     1      3, 1)                                                       
C/                                                                      
         RETURN                                                         
   1  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSBM(A, M, N, R, C, RC)                               
      INTEGER M, N                                                      
      INTEGER RC(M)                                                     
      REAL A(M, N), R(M), C(M)                                          
C TO GET THE ROW AND COLUMN SCALE FACTORS FOR A.                        
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(17H RCSBM - M .LT. 1, 17, 1, 2)         
C     IF (N .LT. 1) CALL SETERR(17H RCSBM - N .LT. 1, 17, 2, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' RCSBM - M .LT. 1', 17, 1, 2)          
      IF (N .LT. 1) CALL SETERR(' RCSBM - N .LT. 1', 17, 2, 2)          
C/                                                                      
C GET THE ROW FACTOR.                                                   
      CALL RCSBR(A, M, N, R)                                            
C GET THE COLUMN SCALE FACTOR.                                          
      CALL RCSBC(A, M, N, R, C, RC)                                     
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSBR(A, M, N, R)                                      
      INTEGER M, N                                                      
      REAL A(M, N), R(M)                                                
      INTEGER I, J, N2, MIN0, MAX0                                      
      REAL ABS, AMAX1                                                   
      INTEGER TEMP, TEMP1                                               
C TO GET THE ROW SCALE FACTOR FOR A.                                    
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(17H RCSBR - M .LT. 1, 17, 1, 2)         
C     IF (N .LT. 1) CALL SETERR(17H RCSBR - N .LT. 1, 17, 2, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' RCSBR - M .LT. 1', 17, 1, 2)          
      IF (N .LT. 1) CALL SETERR(' RCSBR - N .LT. 1', 17, 2, 2)          
C/                                                                      
      N2 = (N+1)/2                                                      
      DO  2 I = 1, M                                                    
         R(I) = 0                                                       
         TEMP1 = MAX0(1, N2+1-I)                                        
         TEMP = MIN0(N, M+N2-I)                                         
         DO  1 J = TEMP1, TEMP                                          
            R(I) = AMAX1(ABS(A(I, J)), R(I))                            
   1        CONTINUE                                                    
   2     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSBS(A, M, N, R, C, RC)                               
      INTEGER M, N                                                      
      INTEGER RC(M)                                                     
      REAL A(M, N), R(M), C(M)                                          
      INTEGER I, J, N2, RD2, MIN0, MAX0                                 
      REAL R1MACH, L, S, D1, D2                                         
      LOGICAL BADNGE                                                    
      INTEGER TEMP, TEMP1, TEMP2                                        
C TO SCALE ((1/R)*A)*(1/C).                                             
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(17H RCSBS - M .LT. 1, 17, 1, 2)         
C     IF (N .LT. 1) CALL SETERR(17H RCSBS - N .LT. 1, 17, 2, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' RCSBS - M .LT. 1', 17, 1, 2)          
      IF (N .LT. 1) CALL SETERR(' RCSBS - N .LT. 1', 17, 2, 2)          
C/                                                                      
      CALL ENTER(1)                                                     
      S = R1MACH(1)                                                     
      L = R1MACH(2)                                                     
      DO  1 I = 1, M                                                    
         IF (R(I) .EQ. 0.) GOTO  1                                      
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      37H RCSBS - MUST HAVE S .LE. R(I) .LE. L, 37, 3, 2)         
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      ' RCSBS - MUST HAVE S .LE. R(I) .LE. L', 37, 3, 2)          
C/                                                                      
   1     CONTINUE                                                       
      DO  2 I = 1, M                                                    
         IF (C(I) .EQ. 0.) GOTO  2                                      
C/6S                                                                    
C        IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
C    1      37H RCSBS - MUST HAVE S .LE. C(I) .LE. L, 37, 4, 2)         
C        IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
C    1      36H RCSS - MUST HAVE RC(I) IN (-1,0,+1), 36, 5, 2)          
C/7S                                                                    
         IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
     1      ' RCSBS - MUST HAVE S .LE. C(I) .LE. L', 37, 4, 2)          
         IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
     1      ' RCSS - MUST HAVE RC(I) IN (-1,0,+1)', 36, 5, 2)           
C/                                                                      
   2     CONTINUE                                                       
C CHECK 1/(S*L) RANGE.                                                  
      BADNGE = .FALSE.                                                  
      IF (S*L .GT. 1.) GOTO 3                                           
         IF (1./L .GT. S*L) BADNGE = .TRUE.                             
         GOTO  4                                                        
   3     IF (S*L .GT. 1./S) BADNGE = .TRUE.                             
C S*L > 1.                                                              
C/6S                                                                    
C  4  IF (BADNGE) CALL SETERR(                                          
C    1   43H RCSBX - MUST HAVE 1/(S*L) IN MACHINE RANGE, 43, 6, 1)      
C/7S                                                                    
   4  IF (BADNGE) CALL SETERR(                                          
     1   ' RCSBX - MUST HAVE 1/(S*L) IN MACHINE RANGE', 43, 6, 1)       
C/                                                                      
      N2 = (N+1)/2                                                      
      DO  32 I = 1, M                                                   
         D1 = R(I)                                                      
         IF (D1 .EQ. 0.) GOTO  32                                       
         TEMP1 = MAX0(1, N2+1-I)                                        
         TEMP = MIN0(N, M+N2-I)                                         
         DO  31 J = TEMP1, TEMP                                         
            TEMP2 = I+J-N2                                              
            D2 = C(TEMP2)                                               
            TEMP2 = I+J-N2                                              
            RD2 = RC(TEMP2)                                             
            IF (A(I, J) .NE. 0. .AND. D2 .NE. 0.) GOTO 5                
               GOTO  31                                                 
   5           IF (D1 .LT. 1.) GOTO 18                                  
                  IF (RD2 .LE. 0) GOTO 10                               
                     IF (D2 .LT. 1.) GOTO 6                             
                        A(I, J) = S*((A(I, J)/D1)/D2)                   
C D2 OVERFLOWED.                                                        
                        GOTO  9                                         
   6                    IF (D1*D2 .LT. 1.) GOTO 7                       
                           A(I, J) = S*(A(I, J)/(D1*D2))                
C D2 < 1.                                                               
                           GOTO  8                                      
   7                       A(I, J) = A(I, J)*(S/(D1*D2))                
   8                    CONTINUE                                        
   9                 CONTINUE                                           
                     GOTO  17                                           
  10                 IF (D2 .LT. 1.) GOTO 11                            
                        A(I, J) = (A(I, J)/D1)/D2                       
                        GOTO  16                                        
  11                    IF (RD2 .GE. 0) GOTO 14                         
                           IF (D2 .LT. 1./D1) GOTO 12                   
                              A(I, J) = A(I, J)*((L/D1)/D2)             
C D2 UNDERFLOWED.                                                       
                              GOTO  13                                  
  12                          A(I, J) = L*(A(I, J)/(D1*D2))             
  13                       CONTINUE                                     
                           GOTO  15                                     
  14                       A(I, J) = A(I, J)/(D1*D2)                    
C D2 < 1.                                                               
  15                 CONTINUE                                           
  16              CONTINUE                                              
  17              CONTINUE                                              
                  GOTO  29                                              
  18              IF (RD2 .LE. 0) GOTO 21                               
                     IF (D1*D2 .LT. 1.) GOTO 19                         
                        A(I, J) = S*(A(I, J)/(D1*D2))                   
C D1 < 1.                                                               
C D2 OVERFLOWED.                                                        
                        GOTO  20                                        
  19                    A(I, J) = A(I, J)*((S/D1)/D2)                   
  20                 CONTINUE                                           
                     GOTO  28                                           
  21                 IF (D2 .LT. 1.) GOTO 22                            
                        A(I, J) = A(I, J)/(D1*D2)                       
                        GOTO  27                                        
  22                    IF (RD2 .GE. 0) GOTO 25                         
                           IF (D1*D2 .GT. 1.) GOTO 23                   
                              A(I, J) = L*(A(I, J)/(D1*D2))             
C D2 UNDERFLOWED.                                                       
                              GOTO  24                                  
  23                          A(I, J) = A(I, J)*(L/(D1*D2))             
  24                       CONTINUE                                     
                           GOTO  26                                     
  25                       A(I, J) = (A(I, J)/D1)/D2                    
C D2 < 1.                                                               
  26                 CONTINUE                                           
  27              CONTINUE                                              
  28              CONTINUE                                              
  29        CONTINUE                                                    
  30        CONTINUE                                                    
  31        CONTINUE                                                    
  32     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSM(A, M, N, R, C, RC)                                
      INTEGER M, N                                                      
      INTEGER RC(N)                                                     
      REAL A(M, N), R(M), C(N)                                          
C TO GET THE ROW AND COLUMN SCALE FACTORS FOR A.                        
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16H RCSM - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16H RCSM - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' RCSM - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR(' RCSM - N .LT. 1', 16, 2, 2)           
C/                                                                      
C GET THE ROW FACTOR.                                                   
      CALL RCSR(A, M, N, R)                                             
C GET THE COLUMN SCALE FACTOR.                                          
      CALL RCSC(A, M, N, R, C, RC)                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSR(A, M, N, R)                                       
      INTEGER M, N                                                      
      REAL A(M, N), R(M)                                                
      INTEGER I, J                                                      
      REAL ABS, AMAX1                                                   
C TO GET THE ROW SCALE FACTOR FOR A.                                    
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16H RCSR - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16H RCSR - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' RCSR - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR(' RCSR - N .LT. 1', 16, 2, 2)           
C/                                                                      
      DO  2 I = 1, M                                                    
         R(I) = 0                                                       
         DO  1 J = 1, N                                                 
            R(I) = AMAX1(ABS(A(I, J)), R(I))                            
   1        CONTINUE                                                    
   2     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE RCSBC(A, M, N, R, C, RC)                               
      INTEGER M, N                                                      
      INTEGER RC(M)                                                     
      REAL A(M, N), R(M), C(M)                                          
      INTEGER I, J, N2, RD2, MIN0, MAX0                                 
      REAL R1MACH, L, S, D1, D2, AIJ                                    
      REAL ABS, AAIJ, AMAX1                                             
      INTEGER TEMP, TEMP1, TEMP2                                        
C TO GET THE COLUMN SCALE FACTOR FOR (1/R)*A.                           
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(17H RCSBC - M .LT. 1, 17, 1, 2)         
C     IF (N .LT. 1) CALL SETERR(17H RCSBC - N .LT. 1, 17, 2, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' RCSBC - M .LT. 1', 17, 1, 2)          
      IF (N .LT. 1) CALL SETERR(' RCSBC - N .LT. 1', 17, 2, 2)          
C/                                                                      
      S = R1MACH(1)                                                     
      L = R1MACH(2)                                                     
      DO  1 I = 1, M                                                    
         IF (R(I) .EQ. 0.) GOTO  1                                      
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      37H RCSBC - MUST HAVE S .LE. R(I) .LE. L, 37, 3, 2)         
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      ' RCSBC - MUST HAVE S .LE. R(I) .LE. L', 37, 3, 2)          
C/                                                                      
   1     CONTINUE                                                       
      N2 = (N+1)/2                                                      
      DO  17 J = 1, M                                                   
         D2 = 0                                                         
C -1 = UNDERFLOW, 0 = IN-RANGE, +1 = OVERFLOW.                          
         RD2 = -1                                                       
         TEMP1 = MAX0(1, J+N2-M)                                        
         TEMP = MIN0(N, J+N2-1)                                         
         DO  16 I = TEMP1, TEMP                                         
            TEMP2 = J+N2-I                                              
            AIJ = A(TEMP2, I)                                           
            AAIJ = ABS(AIJ)                                             
            TEMP2 = J+N2-I                                              
            D1 = R(TEMP2)                                               
            IF (AIJ .EQ. 0. .OR. D1 .EQ. 0.) GOTO  16                   
            IF (D1 .GE. 1.) GOTO 8                                      
               IF (AAIJ .LE. D1*L) GOTO 2                               
                  IF (RD2 .LT. 1) D2 = 0                                
C CHECK FOR OVERFLOW.                                                   
C OVERFLOW.                                                             
                  RD2 = 1                                               
                  D2 = AMAX1(D2, AAIJ*(S/D1))                           
                  GOTO  7                                               
   2              IF (RD2 .LE. 0) GOTO 3                                
                     GOTO  16                                           
C THIS ELEMENT IS IN-RANGE.                                             
C ALREADY OVERFLOWED, NO EFFECT.                                        
   3                 IF (RD2 .NE. 0) GOTO 4                             
                        D2 = AMAX1(D2, AAIJ/D1)                         
                        GOTO  5                                         
   4                    RD2 = 0                                         
C RD2 = -1.                                                             
                        D2 = AAIJ/D1                                    
   5              CONTINUE                                              
   6              CONTINUE                                              
   7           CONTINUE                                                 
               GOTO  15                                                 
   8           IF (AAIJ .GE. D1*S) GOTO 9                               
                  IF (RD2 .GE. 0) GOTO  16                              
C ELEMENT UNDERFLOW, D1 >= 1.                                           
C NO-EFFECT.                                                            
                  D2 = AMAX1(D2, AAIJ*(L/D1))                           
                  GOTO  14                                              
   9              IF (RD2 .LE. 0) GOTO 10                               
                     GOTO  16                                           
C IN-RANGE.                                                             
C NO-EFFECT.                                                            
  10                 IF (RD2 .NE. 0) GOTO 11                            
                        D2 = AMAX1(D2, AAIJ/D1)                         
                        GOTO  12                                        
  11                    RD2 = 0                                         
C UNDERFLOWED SO FAR.                                                   
                        D2 = AAIJ/D1                                    
  12              CONTINUE                                              
  13              CONTINUE                                              
  14        CONTINUE                                                    
  15        CONTINUE                                                    
  16        CONTINUE                                                    
         C(J) = D2                                                      
         RC(J) = RD2                                                    
  17     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE A90STS(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,      
     1   AF, AF1, BC, BC1, D, D1, ERROR, ERROR1, ERRPAR, INMI, SCALE,   
     2   HANDLE, HANLE1, BETA, GAMMA, DELTA, N, KMAX, MMAX, XPOLY,      
     3   KINIT, HFRACT, STEPS, NLEQS)                                   
      INTEGER MMAX                                                      
      EXTERNAL AF, AF1, BC, BC1, D, D1                                  
      EXTERNAL ERROR, ERROR1, INMI, SCALE, HANDLE, HANLE1               
      EXTERNAL STEPS, NLEQS                                             
      INTEGER NU, K, NX, NV, N(MMAX), KMAX                              
      INTEGER KINIT                                                     
      REAL U(1), X(1), V(1), TSTART, TSTOP, DT                          
      REAL ERRPAR(2), BETA, GAMMA, DELTA, HFRACT                        
      LOGICAL XPOLY                                                     
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      EXTERNAL A90STE, A90STH                                           
      INTEGER ISTKGT, IS(1000), IUOLD, IVOLD                            
      REAL RS(1000), WS(500)                                            
      LOGICAL LS(1000)                                                  
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE BASIC POSTS SOLVER.                                               
C SCRATCH SPACE ALLOCATED -                                             
C      S(A90STS) <= 2*MGQ +                                             
C                   MAX ( 17*MGQ, NU*(NX-K) + NV + S(A90STX) )          
C REAL WORDS.                                                           
C U(NX-K,NU),X(NX),V(NV),                                               
      CALL ENTER(1)                                                     
C THE SOLUTION IS (V,U).                                                
      IVOLD = ISTKGT(NU*(NX-K)+NV, 3)                                   
      IUOLD = IVOLD+NV                                                  
      IF (NU .GT. 0) CALL MOVEFR(NU*(NX-K), U, WS(IUOLD))               
      IF (NV .GT. 0) CALL MOVEFR(NV, V, WS(IVOLD))                      
      CALL A90STX(TSTART, TSTOP, STEPS, NLEQS, X, AF, AF1, BC, BC1, D,  
     1   D1, BETA, GAMMA, DELTA, WS(IVOLD), NU*(NX-K)+NV, DT, N, KMAX,  
     2   MMAX, XPOLY, A90STE, ERROR, ERROR1, ERRPAR, INMI, SCALE,       
     3   A90STH, HANDLE, HANLE1, 0.9E0, HFRACT, KINIT)                  
      IF (NU .GT. 0) CALL MOVEFR(NU*(NX-K), WS(IUOLD), U)               
      IF (NV .GT. 0) CALL MOVEFR(NV, WS(IVOLD), V)                      
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE A90STX(TSTART, TSTOP, XA, F, MESH, AF, AF1, BC,        
     1   BC1, D, D1, BETA, GAMMA, DELTA, X, NX, DT, N, KMAX, MMAX,      
     2   XPOLY, SERROR, ERROR1, ERROR2, ERRPAR, INMI, SCALE, SOUT,      
     3   OUTUT1, OUTUT2, PESPAR, HFRACT, KINIT)                         
      INTEGER MMAX, NX                                                  
      EXTERNAL XA, F, AF, AF1, BC, BC1                                  
      EXTERNAL D, D1, SERROR, ERROR1, ERROR2, INMI                      
      EXTERNAL SCALE, SOUT, OUTUT1, OUTUT2                              
      INTEGER N(MMAX), KMAX, KINIT                                      
      REAL TSTART, TSTOP, MESH(1), BETA, GAMMA, DELTA                   
      REAL X(NX), DT, ERRPAR(2), PESPAR, HFRACT                         
      LOGICAL XPOLY, SERROR                                             
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A90STY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL WV(30), RV(30)                                               
      LOGICAL LV(20)                                                    
      COMMON /A9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL THETA, EGIVE                                                 
      COMMON /A9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE                                            
      INTEGER NERROR, M, NERR, IE, IS(1000), IX1                        
      REAL T0, T1, RS(1000), WS(500)                                    
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
C     S(A90STX) <= 2*MMAX + 1 + NX*(KMAX+1) +                           
C     ( 5*KMAX + 2*MMAX + 3 ) INTEGER +                                 
C     MAX ( S(XA), NX*(KMAX+1) REAL +                                   
C           MAX ( KMAX + KMAX INTEGER, S(SERROR) ),                     
C           NX REAL + S(SOUT) )                                         
C REAL.                                                                 
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
            CALL XA(T0, X, T1, WS(IX1), NX, N(M), MESH, F, AF, AF1, BC  
     1         , BC1, D, D1, OK, ERROR2, ERRPAR, INMI, SCALE)           
            IF (OK) GOTO 4                                              
               IF (NERROR(NERR) .EQ. 0) GOTO 3                          
                  CALL LEAVE                                            
                  RETURN                                                
   3           CONTINUE                                                 
C     EXTRAPOLATE THE RESULTS.                                          
   4        IF (A4SSOX(WV, RV, IV, LV, N, M)) GOTO  6                   
            IF (M .GT. 1) DONE = SERROR(WS(IX1), NX, MESH, T1, DT,      
     1         ERRPAR, DELTA, RS(IE), ERROR1, ERROR2)                   
C CHECK FOR CONVERGENCE.                                                
C     CHECK FOR A RESTART.                                              
            IF (A4SSOR(WV, RV, IV, LV, ERRPAR)) GOTO  6                 
   5        CONTINUE                                                    
C   GET OPTIMAL DT AND ORDER ( LOZENGE SIZE ).                          
   6     IF (A4SSOM(WV, RV, IV, LV, DT)) GOTO  7                        
C   OUTPUT THE RESULTS FOR THIS TIME-STEP.                              
         CALL SOUT(T0, X, MESH, T1, WS(IX1), NX, DT, TSTOP, OK, RS(IE)  
     1      , OUTUT1, OUTUT2)                                           
C   WIND-UP THIS TIME-STEP.                                             
         IF (A4SSOE(WV, RV, IV, LV, X, TSTOP, DT)) GOTO  7              
         GOTO  1                                                        
   7  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A9OSTA(X, NX, NU, NRHS, NRHSG, GETJAC,           
     1   SEPATE, A1, A1T, A2, A2T, A3, A3T, A4, A4T, F1, F1T, F2, F2T,  
     2   INTVAL, SBASIS, K, MESH, NMESH, AF)                            
      INTEGER K, NRHS, NU, NX, NMESH                                    
      EXTERNAL AF                                                       
      INTEGER NRHSG, INTVAL                                             
      REAL X(NX), A1(NX, NU, NU), A1T(NX, NU, NU), A2(NX, NU, NU), A2T( 
     1   NX, NU, NU), A3(NX, NU, NU)                                    
      REAL A3T(NX, NU, NU), A4(NX, NU, NU), A4T(NX, NU, NU), F1(NX, NU  
     1   , NRHS), F1T(NX, NU, NRHS), F2(NX, NU, NRHS)                   
      REAL F2T(NX, NU, NRHS), SBASIS(NX, K, 2), MESH(NMESH)             
      LOGICAL GETJAC, SEPATE                                            
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /POSTF/ FAILED                                             
      LOGICAL FAILED                                                    
      COMMON /A9OSTT/ T, DT                                             
      REAL T, DT                                                        
      COMMON /A9OSTM/ THETA, EGIVE, IZAP                                
      INTEGER IZAP(3)                                                   
      REAL THETA, EGIVE                                                 
      COMMON /A9OSTK/ IUTETA, IVTETA, IUT, IVT                          
      INTEGER IUTETA, IVTETA, IUT, IVT                                  
      COMMON /A9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /A90STV/ IEU                                               
      INTEGER IEU                                                       
      COMMON /A90STK/ IZAP1, NV, IZAP2                                  
      INTEGER IZAP1, NV, IZAP2(2)                                       
      INTEGER I, J, IS(1000), IX                                        
      REAL RS(1000), WS(500)                                            
      LOGICAL BEULER, LS(1000)                                          
      INTEGER TEMP, TEMP1, TEMP2                                        
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO LINEARIZE THE AF RESULTS.                                          
C SCRATCH SPACE ALLOCATED - S(A9OSTA) =                                 
C                           S(AF) + 2*NX*NU*(NV+2)                      
C REAL WORDS.                                                           
      BEULER = THETA .EQ. 1.                                            
      CALL QSPLN(WS(IUTETA), NU, NMESH-K, K, X, NX, INTVAL, SBASIS, 2,  
     1   WS(IEU))                                                       
      TEMP2 = IEU+2*NX*NU                                               
      CALL QSPLN(WS(IUT), NU, NMESH-K, K, X, NX, INTVAL, SBASIS, 2, WS( 
     1   TEMP2))                                                        
      FAILED = .FALSE.                                                  
      TEMP2 = IEU+NX*NU                                                 
      TEMP1 = IEU+2*NX*NU                                               
      TEMP = IEU+3*NX*NU                                                
      CALL AF(T, X, NX, WS(IEU), WS(TEMP2), WS(TEMP1), WS(TEMP), NU, WS(
     1   IVTETA), WS(IVT), NV, F1, A2, A1, A2T, A1T, F1(1, 1, 2), F1T(1,
     2   1, 2), F2, A4, A3, A4T, A3T, F2(1, 1, 2), F2T(1, 1, 2))        
      IF (.NOT. FAILED) GOTO 1                                          
         FNUM = 1                                                       
         A9OSTA = .TRUE.                                                
         RETURN                                                         
   1  DO  10 IX = 1, NX                                                 
         DO  9 I = 1, NU                                                
            F1(IX, I, 1) = -F1(IX, I, 1)                                
            IF (.NOT. GETJAC) GOTO  9                                   
            DO  4 J = 1, NU                                             
               IF (BEULER) GOTO 2                                       
                  A1(IX, I, J) = A1(IX, I, J)*THETA                     
                  A2(IX, I, J) = A2(IX, I, J)*THETA                     
                  A3(IX, I, J) = A3(IX, I, J)*THETA                     
                  A4(IX, I, J) = A4(IX, I, J)*THETA                     
   2           IF (SEPATE) GOTO 3                                       
                  A1(IX, I, J) = A1(IX, I, J)+A1T(IX, I, J)/DT          
                  A2(IX, I, J) = A2(IX, I, J)+A2T(IX, I, J)/DT          
                  A3(IX, I, J) = A3(IX, I, J)+A3T(IX, I, J)/DT          
                  A4(IX, I, J) = A4(IX, I, J)+A4T(IX, I, J)/DT          
   3           CONTINUE                                                 
   4           CONTINUE                                                 
            IF (NV .EQ. 0) GOTO  9                                      
            TEMP = NV+1                                                 
            DO  8 J = 2, TEMP                                           
               IF (BEULER) GOTO 5                                       
                  F1(IX, I, J) = F1(IX, I, J)*THETA                     
                  F2(IX, I, J) = F2(IX, I, J)*THETA                     
   5           IF (SEPATE) GOTO 6                                       
                  F1(IX, I, J) = -(F1(IX, I, J)+F1T(IX, I, J)/DT)       
                  F2(IX, I, J) = F2(IX, I, J)+F2T(IX, I, J)/DT          
                  GOTO  7                                               
   6              F1(IX, I, J) = -F1(IX, I, J)                          
                  F1T(IX, I, J) = -F1T(IX, I, J)                        
   7           CONTINUE                                                 
   8           CONTINUE                                                 
   9        CONTINUE                                                    
  10     CONTINUE                                                       
      A9OSTA = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A9OSTB(U, UT, NU, V, VT, NV, T, DT, K, X,        
     1   NX, GETJAC, SEPATE, BC, ALFA, BETA, GAMMA, ALFAT, BETAT,       
     2   GAMMAT, NXMK, NRHS, NRHSG, EU, EUX, EUT, EUTX)                 
      INTEGER NRHS, NXMK, NU, NX                                        
      EXTERNAL BC                                                       
      INTEGER NV, K, NRHSG                                              
      REAL U(NXMK, NU), UT(NXMK, NU), V(1), VT(1), T, DT                
      REAL X(NX), ALFA(NU, NU, 2), BETA(NU, NU, 2), GAMMA(NU, NRHS, 2)  
     1   , ALFAT(NU, NU, 2), BETAT(NU, NU, 2)                           
      REAL GAMMAT(NU, NRHS, 2), EU(NU, 2), EUX(NU, 2), EUT(NU, 2), EUTX(
     1   NU, 2)                                                         
      LOGICAL GETJAC, SEPATE                                            
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A9OSTM/ THETA, EGIVE, IZAP                                
      INTEGER IZAP(3)                                                   
      REAL THETA, EGIVE                                                 
      COMMON /A9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /POSTF/ FAILED                                             
      LOGICAL FAILED                                                    
      INTEGER IGAMMA, ISTKGT, IGAMA0, IGAMA1, I, J                      
      INTEGER L, ID(1), IALFA, IBETA, IS(1000), IALFA1                  
      INTEGER IBETA1                                                    
      REAL LR(2), RS(1000), WS(500)                                     
      LOGICAL BEULER, LS(1000)                                          
      INTEGER TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      DATA ID(1)/1/                                                     
C SCRATCH SPACE ALLOCATED - S(A9OSTB) = MAX ( 6*K, S(BC) )              
C                                       REAL WORDS.                     
C (V,VT)(NV).                                                           
      CALL ENTER(1)                                                     
      BEULER = THETA .EQ. 1.                                            
      LR(1) = X(1)                                                      
C SO THAT BC CAN CLOBBER L AND R.                                       
      LR(2) = X(NX)                                                     
      DO  1 I = 1, NU                                                   
         EU(I, 1) = U(1, I)                                             
         EU(I, 2) = U(NXMK, I)                                          
         CALL SPLN1(K, X, NX, U(1, I), X(1), 1, ID, 1, EUX(I, 1))       
         CALL SPLN1(K, X, NX, U(1, I), X(NX), 1, ID, 1, EUX(I, 2))      
         EUT(I, 1) = UT(1, I)                                           
         EUT(I, 2) = UT(NXMK, I)                                        
         CALL SPLN1(K, X, NX, UT(1, I), X(1), 1, ID, 1, EUTX(I, 1))     
         CALL SPLN1(K, X, NX, UT(1, I), X(NX), 1, ID, 1, EUTX(I, 2))    
   1     CONTINUE                                                       
      FAILED = .FALSE.                                                  
      IF (.NOT. GETJAC) GOTO 3                                          
         IGAMMA = ISTKGT(2*NU, 3)                                       
         CALL SETR(2*NU**2, 0E0, ALFA)                                  
         CALL SETR(2*NU**2, 0E0, BETA)                                  
         CALL SETR(2*NU, 0E0, WS(IGAMMA))                               
         CALL SETR(2*NU**2, 0E0, ALFAT)                                 
         CALL SETR(2*NU**2, 0E0, BETAT)                                 
         CALL SETR(2*NU*NRHS, 0E0, GAMMA)                               
         CALL SETR(2*NU*NRHS, 0E0, GAMMAT)                              
         CALL BC(T, LR(1), LR(2), EU, EUX, EUT, EUTX, NU, V, VT, NV, WS(
     1      IGAMMA), ALFA, BETA, ALFAT, BETAT, GAMMA(1, 2, 1), GAMMAT(1,
     2      2, 1))                                                      
         IF (NV .LE. 0) GOTO 2                                          
            CALL MOVEBR(NU*NV, GAMMA(1, 1, 2), GAMMA(1, 2, 2))          
            CALL MOVEBR(NU*NV, GAMMAT(1, 1, 2), GAMMAT(1, 2, 2))        
   2     CALL SETR(NU, 0E0, GAMMAT(1, 1, 1))                            
         CALL SETR(NU, 0E0, GAMMAT(1, 1, 2))                            
         GOTO  4                                                        
   3     IGAMMA = ISTKGT(2*NU*(4*NU+1+2*NV), 3)                         
         IALFA = IGAMMA+2*NU                                            
         IBETA = IALFA+2*NU**2                                          
         IALFA1 = IBETA+2*NU**2                                         
         IBETA1 = IALFA1+2*NU**2                                        
         IGAMA0 = IBETA1+2*NU**2                                        
         IGAMA1 = IGAMA0+2*NU*NV                                        
         CALL SETR(2*NU*(4*NU+1+2*NV), 0E0, WS(IGAMMA))                 
         CALL BC(T, LR(1), LR(2), EU, EUX, EUT, EUTX, NU, V, VT, NV, WS(
     1      IGAMMA), WS(IALFA), WS(IBETA), WS(IALFA1), WS(IBETA1), WS(  
     2      IGAMA0), WS(IGAMA1))                                        
   4  IF (.NOT. FAILED) GOTO 5                                          
         FNUM = 2                                                       
         CALL LEAVE                                                     
         A9OSTB = .TRUE.                                                
         RETURN                                                         
   5  IF (NRHSG .LE. 0) GOTO 6                                          
         CALL MOVEFR(NU, WS(IGAMMA), GAMMA)                             
         TEMP = IGAMMA+NU                                               
         CALL MOVEFR(NU, WS(TEMP), GAMMA(1, 1, 2))                      
   6  DO  14 L = 1, 2                                                   
         DO  13 I = 1, NU                                               
            GAMMA(I, 1, L) = -GAMMA(I, 1, L)                            
            IF (.NOT. GETJAC) GOTO  13                                  
            DO  9 J = 1, NU                                             
               IF (BEULER) GOTO 7                                       
                  ALFA(I, J, L) = ALFA(I, J, L)*THETA                   
                  BETA(I, J, L) = BETA(I, J, L)*THETA                   
   7           IF (SEPATE) GOTO 8                                       
                  ALFA(I, J, L) = ALFA(I, J, L)+ALFAT(I, J, L)/DT       
                  BETA(I, J, L) = BETA(I, J, L)+BETAT(I, J, L)/DT       
   8           CONTINUE                                                 
   9           CONTINUE                                                 
            IF (NV .EQ. 0) GOTO  13                                     
            TEMP = NV+1                                                 
            DO  12 J = 2, TEMP                                          
               IF (.NOT. BEULER) GAMMA(I, J, L) = GAMMA(I, J, L)*THETA  
               IF (SEPATE) GOTO 10                                      
                  GAMMA(I, J, L) = -(GAMMA(I, J, L)+GAMMAT(I, J, L)/DT) 
                  GOTO  11                                              
  10              GAMMA(I, J, L) = -GAMMA(I, J, L)                      
                  GAMMAT(I, J, L) = -GAMMAT(I, J, L)                    
  11           CONTINUE                                                 
  12           CONTINUE                                                 
  13        CONTINUE                                                    
  14     CONTINUE                                                       
      CALL LEAVE                                                        
      A9OSTB = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A9OSTD(K, X, NX, NXMK, U, UT, NU, V, VT,         
     1   NV, T, GETJAC, SEPATE, D, D1, D1T, D2, D3, D3T)                
      INTEGER NXMK, NV, NX                                              
      EXTERNAL D                                                        
      INTEGER K, NU                                                     
      REAL X(NX), U(NXMK, 1), UT(NXMK, 1), V(NV), VT(NV), T             
      REAL D1(NV, NXMK, 1), D1T(NV, NXMK, 1), D2(NV), D3(NV, NV), D3T(  
     1   NV, NV)                                                        
      LOGICAL GETJAC, SEPATE                                            
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A9OSTT/ TT, DT                                            
      REAL TT, DT                                                       
      COMMON /A9OSTM/ THETA, EGIVE, IZAP                                
      INTEGER IZAP(3)                                                   
      REAL THETA, EGIVE                                                 
      COMMON /A9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /POSTF/ FAILED                                             
      LOGICAL FAILED                                                    
      INTEGER ISTKGT, ID1T, ID3T, I, J, L                               
      INTEGER IS(1000), ID1, ID3                                        
      REAL RS(1000), WS(500)                                            
      LOGICAL BEULER, LS(1000)                                          
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO LINEARIZE THE D RESULTS.                                           
C SCRATCH SPACE ALLOCATED - S(A9OSTD) = S(D) REAL WORDS.                
C (U,UT)(NXMK,NU).                                                      
C           (D1,D1T)(NV,NXMK,NU).                                       
      BEULER = THETA .EQ. 1.                                            
      FAILED = .FALSE.                                                  
      CALL SETR(NV, 0E0, D2)                                            
      IF (.NOT. GETJAC) GOTO 1                                          
         IF (NU .GT. 0) CALL SETR(2*NV*NU*NXMK, 0E0, D1)                
         CALL SETR(2*NV**2, 0E0, D3)                                    
         CALL D(T, K, X, NX, U, UT, NU, NX-K, V, VT, NV, D2, D1, D1T,   
     1      D3, D3T)                                                    
         GOTO  2                                                        
   1     ID1 = ISTKGT(2*NV*(NU*NXMK+NV), 3)                             
         ID1T = ID1+NV*NU*NXMK                                          
         ID3 = ID1T+NV*NU*NXMK                                          
         ID3T = ID3+NV**2                                               
C DEFAULT VALUES.                                                       
         CALL SETR(2*NV*(NU*NXMK+NV), 0E0, WS(ID1))                     
         CALL D(T, K, X, NX, U, UT, NU, NX-K, V, VT, NV, D2, WS(ID1),   
     1      WS(ID1T), WS(ID3), WS(ID3T))                                
         CALL ISTKRL(1)                                                 
   2  IF (.NOT. FAILED) GOTO 3                                          
         FNUM = 3                                                       
         A9OSTD = .TRUE.                                                
         RETURN                                                         
   3  DO  9 I = 1, NV                                                   
         D2(I) = -D2(I)                                                 
         IF (.NOT. GETJAC) GOTO  9                                      
         DO  6 J = 1, NV                                                
            IF (.NOT. BEULER) D3(I, J) = D3(I, J)*THETA                 
            IF (SEPATE) GOTO 4                                          
               D3(I, J) = -(D3(I, J)+D3T(I, J)/DT)                      
               GOTO  5                                                  
   4           D3(I, J) = -D3(I, J)                                     
               D3T(I, J) = -D3T(I, J)                                   
   5        CONTINUE                                                    
   6        CONTINUE                                                    
         IF (NU .EQ. 0) GOTO  9                                         
         DO  8 J = 1, NU                                                
            DO  7 L = 1, NXMK                                           
               IF (.NOT. BEULER) D1(I, L, J) = D1(I, L, J)*THETA        
               IF (.NOT. SEPATE) D1(I, L, J) = D1(I, L, J)+D1T(I, L, J)/
     1            DT                                                    
   7           CONTINUE                                                 
   8        CONTINUE                                                    
   9     CONTINUE                                                       
      A9OSTD = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A9OSTE(U, NU, K, X, NX, V, NV, T, DT,            
     1   ERRPAR, ERPUTS, EU, EV, ERROR)                                 
      INTEGER NX                                                        
      EXTERNAL ERROR                                                    
      INTEGER NU, K, NV                                                 
      REAL U(1), X(NX), V(1), T, DT, ERRPAR(2)                          
      REAL EU(1), EV(1)                                                 
      LOGICAL ERPUTS, ERROR                                             
C THE ERROR FILTER FOR NLEQS.                                           
C SCRATCH SPACE ALLOCATED - S(A9OSTE) = S(ERROR).                       
C U(NX-K,NU),V(NV).                                                     
C EU(NX-K,NU),EV(NV).                                                   
      A9OSTE = ERROR(U, NU, NX-K, K, X, NX, V, NV, T, DT, ERRPAR,       
     1   ERPUTS, EU, EV)                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE A9OSTH(T0, U0, V0, T, U, V, NU, NV, K, X, NX, DT,      
     1   TSTOP, EU, EV, OK, HANDLE)                                     
      INTEGER NX                                                        
      EXTERNAL HANDLE                                                   
      INTEGER NU, NV, K                                                 
      REAL T0, U0(1), V0(1), T, U(1), V(1)                              
      REAL X(NX), DT, TSTOP, EU(1), EV(1)                               
      LOGICAL OK                                                        
      COMMON /A9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL THETA, EGIVE                                                 
      COMMON /A9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE                                            
      COMMON /A9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /A90STT/ TGOOD                                             
      REAL TGOOD                                                        
      COMMON /A90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS      
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS                         
      INTEGER NNFS, NRS                                                 
      LOGICAL TEMP                                                      
C OUTPUT FILTER FOR POSTS.                                              
C SCRATCH SPACE ALLOCATED - S(A9OSTH) = S(HANDLE).                      
C (U0,U)(NX-K,NU),(V0,V)(NV).                                           
C EU(NX-K,NU),EV(NV).                                                   
      IF (T0 .EQ. T) GOTO 1                                             
         FNUM = 0                                                       
         TGOOD = T                                                      
         GOTO  2                                                        
   1     NRS = NRS+1                                                    
   2  TEMP = T0 .EQ. T                                                  
      IF (TEMP) TEMP = KEEJAC .EQ. 3                                    
      IF (.NOT. TEMP) GOTO 3                                            
         GETJAC = T0 .NE. TJ                                            
         TJ = T0                                                        
   3  NTSS = NTSS+1                                                     
      CALL HANDLE(T0, U0, V0, T, U, V, NU, NX-K, NV, K, X, NX, DT,      
     1   TSTOP)                                                         
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A9OSTN(U, NU, V, NV, T, DT, K, X, NX, AF,        
     1   AF1, BC, BC1, D, D1234, ERROR, INMI, SCALE, ERRPAR)            
      INTEGER NX                                                        
      EXTERNAL AF, AF1, BC, BC1, D, D1234                               
      EXTERNAL ERROR, INMI, SCALE                                       
      INTEGER NU, NV, K                                                 
      REAL U(1), V(1), T, DT, X(NX), ERRPAR(2)                          
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL THETA, EGIVE                                                 
      COMMON /A9OSTK/ IUTETA, IVTETA, IUT, IVT                          
      INTEGER IUTETA, IVTETA, IUT, IVT                                  
      INTEGER IDU, IEU, IEV, IDV, ISTKGT, IEU1                          
      INTEGER IEV1, IEU2, IEV2, IS(1000), IUOLD, IVOLD                  
      REAL RS(1000), WS(1000)                                           
      LOGICAL A9OSTO, DONE, LS(1000)                                    
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C NONLINEAR EQUATION SOLVER FOR POSTS.                                  
C SCRATCH SPACE ALLOCATED -                                             
C     S(A9OSTN) = IF ( THETA ~= 1 ) { NU*(NX-K)+NV } +                  
C                 3*(NU*(NX-K)+NV) +                                    
C                 ( NU*(NX-K)+NV ) INTEGER +                            
C                 MAX ( S(A90STC), S(ERROR) )                           
C REAL WORDS.                                                           
C U(NX-K,NU),V(NV).                                                     
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      CALL ENTER(1)                                                     
      IVOLD = ISTKGT(NU*(NX-K)+NV, 3)                                   
      IUOLD = IVOLD+NV                                                  
      IVTETA = ISTKGT(NU*(NX-K)+NV, 3)                                  
      IUTETA = IVTETA+NV                                                
      IVT = ISTKGT(NU*(NX-K)+NV, 3)                                     
      IUT = IVT+NV                                                      
      IEV = ISTKGT(NU*(NX-K)+NV, 3)                                     
      IEU = IEV+NV                                                      
      IDV = ISTKGT(NU*(NX-K)+NV, 3)                                     
      IDU = IDV+NV                                                      
      IEV1 = ISTKGT(NU*(NX-K)+NV, 3)                                    
      IEU1 = IEV1+NV                                                    
      IEV2 = ISTKGT(NU*(NX-K)+NV, 3)                                    
      IEU2 = IEV2+NV                                                    
      DONE = A9OSTO(U, NU, V, NV, T, DT, K, X, NX, AF, AF1, BC, BC1, D  
     1   , D1234, ERROR, INMI, SCALE, ERRPAR, WS(IUTETA), WS(IUT), WS(  
     2   IVTETA), WS(IVT), WS(IUOLD), WS(IVOLD), RS(IEU), RS(IEV), RS(  
     3   IEU1), RS(IEV1), RS(IEU2), RS(IEV2), WS(IDU), WS(IDV), NX-K,   
     4   THETA, MINIT, MAXIT, KEEJAC, EGIVE)                            
      CALL LEAVE                                                        
      A9OSTN = DONE                                                     
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A9OSTO(U, NU, V, NV, T, DT, K, X, NX, AF,        
     1   AF1, BC, BC1, D, D1234, ERROR, INMI, SCALE, ERRPAR, UTHETA, UT,
     2   VTHETA, VT, UOLD, VOLD, EU, EV, EU1, EV1, EU2, EV2, DU, DV,    
     3   NXMK, THETA, MINIT, MAXIT, KEEJAC, EGIVE)                      
      INTEGER NXMK, NX                                                  
      EXTERNAL AF, AF1, BC, BC1, D, D1234                               
      EXTERNAL ERROR, INMI, SCALE                                       
      INTEGER NU, NV, K, MINIT, MAXIT, KEEJAC                           
      REAL U(NXMK, 1), V(1), T, DT, X(NX), ERRPAR(2)                    
      REAL UTHETA(NXMK, 1), UT(NXMK, 1), VTHETA(1), VT(1), UOLD(NXMK, 1)
     1   , VOLD(1)                                                      
      REAL EU(NXMK, 1), EV(1), EU1(NXMK, 1), EV1(1), EU2(NXMK, 1), EV2(1
     1   )                                                              
      REAL DU(NXMK, 1), DV(1), THETA, EGIVE                             
      LOGICAL ERROR                                                     
      COMMON /A90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS      
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS                         
      INTEGER NNFS, NRS                                                 
      COMMON /A9OSTL/ ERPUTS                                            
      LOGICAL ERPUTS                                                    
      COMMON /A9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE                                            
      COMMON /A9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      INTEGER I, J, ITER                                                
      REAL ABS, RHO, PROD, TEMP, POWER, R1MACH                          
      LOGICAL DONE, NCGCE, A90STC                                       
      INTEGER TEMP2, TEMP3                                              
      LOGICAL TEMP1                                                     
C U(NXMK,NU),V(NV).                                                     
C           (UTHETA,UT)(NXMK,NU),(VTHETA,VT)(NV).                       
C UOLD(NXMK,NU),VOLD(NV).                                               
C DU(NXMK,NU),DV(NV).                                                   
C REAL EU(NXMK,NU),EV(NV),EU1(NXMK,NU),EV1(NV),EU2(NXMK,NU),EV2(NV).    
      IF (NU .LE. 0) GOTO 1                                             
         CALL MOVEFR(NU*(NX-K), U, UOLD)                                
         CALL SETR(NU*(NX-K), 0E0, UT)                                  
   1  IF (NV .LE. 0) GOTO 2                                             
         CALL MOVEFR(NV, V, VOLD)                                       
         CALL SETR(NV, 0E0, VT)                                         
C GET INITIAL NEWTON METHOD GUESS.                                      
   2  CALL INMI(NU, NV, NX-K, K, X, NX, T, DT, UOLD, VOLD, U, UT, V, VT)
      DO  49 ITER = 1, MAXIT                                            
         IF (KEEJAC .NE. 0) GOTO 3                                      
            GETJAC = .TRUE.                                             
            TJ = T                                                      
   3     IF (GETJAC) NJS = NJS+1                                        
         NNITS = NNITS+1                                                
         J = 1                                                          
            GOTO  5                                                     
   4        J = J+1                                                     
   5        IF (J .GT. NU) GOTO  7                                      
            DO  6 I = 1, NXMK                                           
               UTHETA(I, J) = THETA*(U(I, J)-UOLD(I, J))+UOLD(I, J)     
   6           CONTINUE                                                 
            GOTO  4                                                     
   7     I = 1                                                          
            GOTO  9                                                     
   8        I = I+1                                                     
   9        IF (I .GT. NV) GOTO  10                                     
            VTHETA(I) = THETA*(V(I)-VOLD(I))+VOLD(I)                    
            GOTO  8                                                     
  10     DONE = A90STC(UTHETA, UT, NU, VTHETA, VT, NV, T, DT, K, X, NX  
     1      , AF, AF1, BC, BC1, D, D1234, SCALE, DU, DV)                
         IF (.NOT. DONE) GOTO 11                                        
            DONE = .FALSE.                                              
            GOTO  50                                                    
  11     J = 1                                                          
            GOTO  13                                                    
  12        J = J+1                                                     
  13        IF (J .GT. NU) GOTO  15                                     
            DO  14 I = 1, NXMK                                          
               U(I, J) = U(I, J)+DU(I, J)                               
               EU(I, J) = EGIVE*ABS(DU(I, J))                           
  14           CONTINUE                                                 
            GOTO  12                                                    
  15     I = 1                                                          
            GOTO  17                                                    
  16        I = I+1                                                     
  17        IF (I .GT. NV) GOTO  18                                     
            V(I) = V(I)+DV(I)                                           
            EV(I) = EGIVE*ABS(DV(I))                                    
            GOTO  16                                                    
  18     IF (MAXIT .NE. 1) GOTO 19                                      
            DONE = .TRUE.                                               
            GOTO  50                                                    
  19     CALL MOVEFR(NU*NXMK+NV, EV, EV2)                               
         DONE = ERROR(U, NU, NX-K, K, X, NX, V, NV, T, DT, ERRPAR,      
     1      ERPUTS, EU, EV)                                             
C CHECK FOR NEGATIVE ERROR REQUESTS.                                    
         TEMP2 = NU*NXMK+NV                                             
         DO  21 I = 1, TEMP2                                            
            TEMP1 = EV(I) .EQ. 0.                                       
            IF (TEMP1) TEMP1 = EV2(I) .NE. 0.                           
            IF (.NOT. TEMP1) TEMP1 = EV(I) .LT. 0.                      
            IF (.NOT. TEMP1) GOTO 20                                    
C/6S                                                                    
C              CALL SETERR(37H ESSOM - E(I).LE.0 RETURNED BY SERROR, 37,
C    1            19, 1)                                                
C/7S                                                                    
               CALL SETERR(' ESSOM - E(I).LE.0 RETURNED BY SERROR', 37, 
     1            19, 1)                                                
C/                                                                      
               A9OSTO = .FALSE.                                         
               RETURN                                                   
  20        CONTINUE                                                    
  21        CONTINUE                                                    
         IF (.NOT. DONE) GOTO 22                                        
            GOTO  50                                                    
  22        IF (ITER .NE. MAXIT) GOTO 23                                
               FNUM = 9                                                 
               NNFS = NNFS+1                                            
               GOTO  50                                                 
  23     CONTINUE                                                       
C NO CONVERGENCE.                                                       
  24     NCGCE = .FALSE.                                                
         TEMP2 = NU*NXMK+NV                                             
         DO  34 I = 1, TEMP2                                            
            TEMP1 = ITER .GT. MINIT                                     
            IF (TEMP1) TEMP1 = EV(I) .LT. EV2(I)                        
            IF (.NOT. TEMP1) GOTO 33                                    
               IF (EV1(I) .LE. EV2(I)) GOTO 25                          
                  RHO = EV2(I)/EV1(I)                                   
C CAN CHECK CONVERGENCE RATE.                                           
                  GOTO  26                                              
  25              RHO = 1                                               
  26           IF (RHO .LT. 1.) GOTO 27                                 
                  NCGCE = .TRUE.                                        
C DIVERGING.                                                            
                  GOTO  32                                              
  27              IF (KEEJAC .NE. 0) GOTO 30                            
                     PROD = 1                                           
C CONVERGING.                                                           
C CHECK QUADRATIC CONVERGENCE RATE.                                     
                     POWER = RHO**2                                     
C < 1.                                                                  
                     TEMP = EV(I)/EV2(I)                                
                     TEMP3 = MAXIT-ITER                                 
                     DO  28 J = 1, TEMP3                                
                        PROD = PROD*POWER                               
                        POWER = POWER**2                                
                        IF (PROD .LE. TEMP) GOTO  29                    
  28                    CONTINUE                                        
  29                 IF (PROD .GT. TEMP) NCGCE = .TRUE.                 
C SLOW CONVERGENCE.                                                     
                     GOTO  31                                           
  30                 IF (RHO**(MAXIT-ITER)*EV2(I) .GT. EV(I)) NCGCE =   
     1                  .TRUE.                                          
C KEEPJAC > 0 AND SHOULD CHECK LINEAR CONVERGENCE RATE.                 
C SLOW CONVERGENCE.                                                     
  31              CONTINUE                                              
  32           IF (NCGCE) GOTO  35                                      
  33        EV1(I) = EV2(I)                                             
  34        CONTINUE                                                    
  35     TEMP1 = NCGCE                                                  
         IF (TEMP1) TEMP1 = KEEJAC .EQ. 4                               
         IF (.NOT. TEMP1) GOTO 39                                       
            IF (T .NE. TJ) GOTO 36                                      
               NNDS = NNDS+1                                            
               FNUM = 8                                                 
               A9OSTO = DONE                                            
               RETURN                                                   
C HAVE NEW JACOBIAN, DIE.                                               
  36        IF (NU .LE. 0) GOTO 37                                      
               CALL MOVEFR(NU*(NX-K), UOLD, U)                          
               CALL SETR(NU*(NX-K), 0E0, UT)                            
  37        IF (NV .LE. 0) GOTO 38                                      
               CALL MOVEFR(NV, VOLD, V)                                 
               CALL SETR(NV, 0E0, VT)                                   
C     GET INITIAL NEWTON METHOD GUESS.                                  
  38        CALL INMI(NU, NV, NX-K, K, X, NX, T, DT, UOLD, VOLD, U, UT  
     1         , V, VT)                                                 
            TJ = T                                                      
            GETJAC = .TRUE.                                             
            FNUM = 0                                                    
C EV1 == BIG FOR NEXT ITERATION.                                        
            CALL SETR(NU*NXMK+NV, R1MACH(2), EV1)                       
            GOTO  49                                                    
  39        IF (.NOT. NCGCE) GOTO 40                                    
               NNDS = NNDS+1                                            
               FNUM = 8                                                 
               A9OSTO = DONE                                            
               RETURN                                                   
  40     CONTINUE                                                       
  41     J = 1                                                          
            GOTO  43                                                    
  42        J = J+1                                                     
  43        IF (J .GT. NU) GOTO  45                                     
            DO  44 I = 1, NXMK                                          
               UT(I, J) = UT(I, J)+DU(I, J)/DT                          
  44           CONTINUE                                                 
            GOTO  42                                                    
  45     I = 1                                                          
            GOTO  47                                                    
  46        I = I+1                                                     
  47        IF (I .GT. NV) GOTO  48                                     
            VT(I) = VT(I)+DV(I)/DT                                      
            GOTO  46                                                    
  48     CONTINUE                                                       
  49     CONTINUE                                                       
  50  A9OSTO = DONE                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE A9OSTP(T0, S0, T1, S1, NS, N, X, NLEQS, AF, AF1        
     1   , BC, BC1, D, D1234, OK, ERROR, ERRPAR, INMI, SCALE)           
      INTEGER NS                                                        
      EXTERNAL NLEQS, AF, AF1, BC, BC1, D                               
      EXTERNAL D1234, ERROR, INMI, SCALE                                
      INTEGER N                                                         
      REAL T0, S0(NS), T1, S1(NS), X(1), ERRPAR(2)                      
      LOGICAL NLEQS, OK                                                 
      COMMON /A90STY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL WV(30), RV(30)                                               
      LOGICAL LV(20)                                                    
      COMMON /A90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS      
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS                         
      INTEGER NNFS, NRS                                                 
      COMMON /A90STK/ NU, NV, K, NX                                     
      INTEGER NU, NV, K, NX                                             
      COMMON /A9OSTT/ TC, DTC                                           
      REAL TC, DTC                                                      
      COMMON /A9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL THETA, EGIVE                                                 
      COMMON /A9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE                                            
      COMMON /A9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      INTEGER ISTEP                                                     
      REAL TSTART, T, DT, FLOAT                                         
      LOGICAL TEMP, TEMP1                                               
      EQUIVALENCE (TSTART, WV(1))                                       
C TIME-STEPPING SCHEME FOR POSTS.                                       
C SCRATCH SPACE ALLOCATED -                                             
C     S(A9OSTP) = NU*(NX-K)*(2*K*NU-1) + NV**2 + S(NLEQS).              
C REAL WORDS.                                                           
      CALL ENTER(1)                                                     
      DT = (T1-T0)/FLOAT(N)                                             
      DTC = DT                                                          
C INITIAL APPROXIMATION FOR S1.                                         
      CALL MOVEFR(NS, S0, S1)                                           
      DO  4 ISTEP = 1, N                                                
         T = T0+(FLOAT(ISTEP-1)+THETA)*DT                               
         TC = T                                                         
         NSSS = NSSS+1                                                  
         IF (KEEJAC .NE. 1) GOTO 1                                      
            GETJAC = .TRUE.                                             
            TJ = T                                                      
   1     TEMP = DT .GT. 0.                                              
         IF (TEMP) TEMP = T .GT. T1                                     
         IF (TEMP) GOTO 2                                               
            TEMP1 = DT .LT. 0.                                          
            IF (TEMP1) TEMP1 = T .LT. T1                                
            TEMP = TEMP1                                                
   2     IF (.NOT. TEMP) GOTO 3                                         
            T = T1                                                      
            TC = T                                                      
   3     OK = NLEQS(S1(NV+1), NU, S1, NV, T, DT, K, X, NX, AF, AF1, BC  
     1      , BC1, D, D1234, ERROR, INMI, SCALE, ERRPAR)                
         IF (.NOT. OK) GOTO  5                                          
   4     CONTINUE                                                       
   5  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE POSTX                                                  
      COMMON /A90STR/ STATS                                             
      INTEGER STATS(8)                                                  
      INTEGER I1MACH                                                    
      INTEGER TEMP                                                      
C TO PRINT THE RUN-TIME STATISTICS FOR POST.                            
      CALL A9OSTX(STATS, 0)                                             
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  1) STATS                                            
   1  FORMAT (31H  POST(J,F,TS,SS,NIT,ND,NF,R) =, 8(I5))                
      RETURN                                                            
      END                                                               
      SUBROUTINE A9OSTX(XSTATS, IFLAG)                                  
      INTEGER XSTATS(8), IFLAG                                          
      INTEGER STATS(8)                                                  
      LOGICAL INPOST                                                    
      DATA STATS(1)/0/                                                  
      DATA STATS(2)/0/                                                  
      DATA STATS(3)/0/                                                  
      DATA STATS(4)/0/                                                  
      DATA STATS(5)/0/                                                  
      DATA STATS(6)/0/                                                  
      DATA STATS(7)/0/                                                  
      DATA STATS(8)/0/                                                  
      DATA INPOST/.FALSE./                                              
C INTERNAL SAVING OF STATISTICS FOR POST.                               
C FOR IFLAG = 0, THE STATS ARE SIMPLY REPORTED.                         
C FOR IFLAG > 0, IT ENTERS POST.                                        
C FOR IFLAG < 0, IT EXITS POST.                                         
      IF (IFLAG .NE. 0) GOTO 1                                          
         IF (.NOT. INPOST) CALL MOVEFI(8, STATS, XSTATS)                
         GOTO  4                                                        
   1     IF (IFLAG .LE. 0) GOTO 2                                       
            INPOST = .TRUE.                                             
            CALL SETI(8, 0, STATS)                                      
            CALL MOVEFI(8, STATS, XSTATS)                               
            GOTO  3                                                     
   2        INPOST = .FALSE.                                            
C IFLAG < 0.                                                            
            CALL MOVEFI(8, XSTATS, STATS)                               
   3  CONTINUE                                                          
   4  RETURN                                                            
      END                                                               
      SUBROUTINE QSPLN(A, NA, NXMK, K, Y, NY, INTVAL, SBASIS, ND        
     1   , SPLINE)                                                      
      INTEGER K, NXMK, NA, ND, NY                                       
      INTEGER INTVAL                                                    
      REAL A(NXMK, NA), Y(NY), SBASIS(NY, K, ND), SPLINE(NY, NA, ND)    
      INTEGER MIN0, MAX0, J, IA, ID, IY                                 
      REAL T                                                            
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
C TO EVALUATE A B-SPLINE WHEN THE BASIS SPLINES ARE KNOWN.              
C SCRATCH SPACE ALLOCATED - NONE.                                       
C/6S                                                                    
C     IF (NA .LT. 1) CALL SETERR(16H QSPLN - NA.LT.1, 16, 1, 2)         
C     IF (NXMK .LE. 0) CALL SETERR(18H QSPLN - NXMK.LE.0, 18, 2, 2)     
C     IF (K .LT. 2) CALL SETERR(15H QSPLN - K.LT.2, 15, 3, 2)           
C     IF (NY .LT. 1) CALL SETERR(16H QSPLN - NY.LT.1, 16, 4, 2)         
C/7S                                                                    
      IF (NA .LT. 1) CALL SETERR(' QSPLN - NA.LT.1', 16, 1, 2)          
      IF (NXMK .LE. 0) CALL SETERR(' QSPLN - NXMK.LE.0', 18, 2, 2)      
      IF (K .LT. 2) CALL SETERR(' QSPLN - K.LT.2', 15, 3, 2)            
      IF (NY .LT. 1) CALL SETERR(' QSPLN - NY.LT.1', 16, 4, 2)          
C/                                                                      
      TEMP1 = INTVAL .LT. 1                                             
      IF (.NOT. TEMP1) TEMP1 = INTVAL .GT. NXMK+K-1                     
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(                                           
C    1   45H QSPLN - INTERVAL POINTS OUTSIDE (X(1),X(NX)), 45, 5, 2)    
C     IF (ND .LT. 1) CALL SETERR(16H QSPLN - ND.LT.1, 16, 6, 2)         
C/7S                                                                    
      IF (TEMP1) CALL SETERR(                                           
     1   ' QSPLN - INTERVAL POINTS OUTSIDE (X(1),X(NX))', 45, 5, 2)     
      IF (ND .LT. 1) CALL SETERR(' QSPLN - ND.LT.1', 16, 6, 2)          
C/                                                                      
      DO  6 IY = 1, NY                                                  
         DO  5 ID = 1, ND                                               
            DO  4 IA = 1, NA                                            
               T = 0                                                    
               J = MAX0(INTVAL-K+1, 1)                                  
                  GOTO  2                                               
   1              J = J+1                                               
   2              IF (J .GT. MIN0(INTVAL, NXMK)) GOTO  3                
                  TEMP = J+K-INTVAL                                     
                  T = T+A(J, IA)*SBASIS(IY, TEMP, ID)                   
                  GOTO  1                                               
   3           SPLINE(IY, IA, ID) = T                                   
   4           CONTINUE                                                 
   5        CONTINUE                                                    
   6     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A90STC(U, UT, NU, V, VT, NV, T, DT, K, X,        
     1   NX, AF, AF1, BC, BC1, D, D1234, SCALE, DU, DV)                 
      INTEGER NX                                                        
      EXTERNAL AF, AF1, BC, BC1, D, D1234                               
      EXTERNAL SCALE                                                    
      INTEGER NU, NV, K                                                 
      REAL U(1), UT(1), V(1), VT(1), T, DT                              
      REAL X(NX), DU(1), DV(1)                                          
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL THETA, EGIVE                                                 
      COMMON /A9OSTJ/ IJP, IB, IAFB, IALFA, IBETA, IGAMMA, ID4, ID5,    
     1   IORDER, IBC, IEQS, IAA, IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT,  
     2   IDMAT, IDIAG, IDPVOT                                           
      INTEGER IJP(3), IB(3), IAFB(3), IALFA(3), IBETA(3), IGAMMA(3)     
      INTEGER ID4(3), ID5(3), IORDER, IBC, IEQS, IAA                    
      INTEGER IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT                      
      INTEGER IDMAT, IDIAG, IDPVOT                                      
      COMMON /A9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE                                            
      COMMON /A90STV/ IEU                                               
      INTEGER IEU                                                       
      COMMON /A90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS      
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS                         
      INTEGER NNFS, NRS                                                 
      COMMON /A90STQ/ IXGQ, IWGQ, MGQ                                   
      INTEGER IXGQ, IWGQ, MGQ                                           
      INTEGER IEXCHG, ISTKGT, MAX0, IS(1000)                            
      REAL RS(1000), WS(500)                                            
      LOGICAL FAILED, LS(1000), A90STA, A90STD                          
      INTEGER TEMP, TEMP1, TEMP2, TEMP3, TEMP4, TEMP5                   
      INTEGER TEMP6                                                     
      LOGICAL TEMP7                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO COMPUTE DU AND DV FOR EACH TIME-STEP.                              
C SCRATCH SPACE ALLOCATED - S(A90STC) =                                 
C     2*NU*(3*NU+2*(NV+1)) + NU*(NX-K)*(NV+2) +                         
C     IF ( NU > 0 )                                                     
C       MAX ( 4*NU*(NU+NV+2) + S(BC), S( GLSSB), S( GLSIN),             
C             2*NU*(NV+1) + S( GLSBT), S( GLSBC),                       
C             S( GLSBP), NU*(NX-K)*(K*NU-1) + NU*(NX-K) INTEGER ) +     
C     IF ( NV > 0 ) S(A90STD)                                           
C REAL WORDS.                                                           
C (U,UT)(NX-K,NU).                                                      
C (V,VT)(NV).                                                           
C DU(NX-K,NU),DV(NV).                                                   
      CALL ENTER(1)                                                     
      IF (GETJAC) DTJ = 0                                               
      IF (KEEJAC .NE. 0) GOTO 5                                         
         IF (NU .LE. 0) GOTO 1                                          
            IJP(1) = ISTKGT(NU*(NX-K)*(2*K*NU-1), 3)                    
            GOTO  2                                                     
   1        IJP(1) = 1                                                  
   2     IJP(3) = IJP(1)                                                
         IF (NU .LE. 0) GOTO 3                                          
            IB(1) = ISTKGT(NU*(NX-K)*(NV+1), 3)                         
            GOTO  4                                                     
   3        IB(1) = 1                                                   
   4     IB(3) = IB(1)                                                  
   5  IEXCHG = ISTKGT((NX-K)*NU, 3)                                     
      TEMP7 = DT .NE. DTJ                                               
      IF (TEMP7) TEMP7 = SEPATE                                         
      IF (.NOT. TEMP7) TEMP7 = GETJAC                                   
      IF (TEMP7) NFS = NFS+1                                            
      IF (NU .LE. 0) GOTO 7                                             
         IEU = ISTKGT(4*MAX0(MGQ, 2)*NU, 3)                             
         TEMP6 = IALFA(3)                                               
         TEMP5 = IBETA(3)                                               
         TEMP4 = IGAMMA(1)                                              
         TEMP3 = IGAMMA(2)                                              
         TEMP2 = IGAMMA(3)                                              
         TEMP = IB(3)                                                   
         TEMP1 = IB(3)                                                  
         FAILED = A90STA(U, UT, NU, V, VT, NV, T, DT, K, X, NX, AF, AF1,
     1      BC, BC1, D, D1234, SCALE, DU, WS(TEMP6), WS(TEMP5), WS(     
     2      TEMP4), WS(TEMP3), WS(TEMP2), WS(IAA), WS(IBB), WS(ICC), WS(
     3      ISGMAD), WS(ISGMAM), IS(IBC), IS(IEQS), IS(IORDER), WS(TEMP)
     4      , WS(IEXCHG), WS(TEMP1), NX-K, NV+1)                        
         IF (.NOT. FAILED) GOTO 6                                       
            CALL LEAVE                                                  
            A90STC = FAILED                                             
            RETURN                                                      
   6     CONTINUE                                                       
   7  IF (NV .LE. 0) GOTO 9                                             
         TEMP1 = IB(3)                                                  
         TEMP = IB(3)                                                   
         FAILED = A90STD(K, X, NX, U, UT, NU, V, VT, NV, T, DT, WS(     
     1      TEMP1), WS(TEMP), MAX0(1, NU*(NX-K)), D, D1234, SCALE, DU,  
     2      DV)                                                         
         IF (.NOT. FAILED) GOTO 8                                       
            CALL LEAVE                                                  
            A90STC = .TRUE.                                             
            RETURN                                                      
   8     CONTINUE                                                       
   9  CALL LEAVE                                                        
      GETJAC = .FALSE.                                                  
      DTJ = DT                                                          
      A90STC = FAILED                                                   
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A90STD(K, X, NX, U, UT, NU, V, VT, NV, T,        
     1   DT, UOFV, UOFVT, JACDIM, D1234, D, SCALE, DU, DV)              
      INTEGER JACDIM, NV                                                
      EXTERNAL D1234, D, SCALE                                          
      INTEGER K, NX, NU                                                 
      REAL X(1), U(1), UT(1), V(NV), VT(NV), T                          
      REAL DT, UOFV(JACDIM, 1), UOFVT(JACDIM, 1), DU(1), DV(NV)         
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE                                            
      COMMON /A9OSTJ/ IZAP, ID4, ID5, IZAP1, ID, IDIAG, IPIVOT          
      INTEGER IZAP(18), ID4(3), ID5(3), IZAP1(10), ID, IDIAG            
      INTEGER IPIVOT                                                    
      INTEGER I, L, IS(1000)                                            
      REAL RS(1000), WS(500)                                            
      LOGICAL FAILED, A90ST0, LS(1000)                                  
      INTEGER TEMP, TEMP1, TEMP2, TEMP3, TEMP4, TEMP5                   
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C SCRATCH SPACE ALLOCATED - S(A90STD) =                                 
C     2*K*(K+NU*(NV+1)) + NV**2 +  NV*(2*NV+1) +                        
C     MAX ( S(D1234), 3*K, 2*NV + NV INTEGER )                          
C REAL WORDS.                                                           
C X(NX),(U,UT)(NX-K,NU),                                                
C           (UOFV,UOFVT)(JACDIM,NV+1), DU(NX-K,NU).                     
      CALL ENTER(1)                                                     
      CALL SETR(NV, 0E0, DV)                                            
      TEMP5 = ID4(1)                                                    
      TEMP4 = ID4(2)                                                    
      TEMP3 = ID4(3)                                                    
      TEMP2 = ID5(1)                                                    
      TEMP1 = ID5(2)                                                    
      TEMP = ID5(3)                                                     
      FAILED = A90ST0(K, X, NX, U, UT, NU, V, VT, NV, T, DT, UOFV,      
     1   D1234, D, SCALE, DV, WS(TEMP5), WS(TEMP4), WS(TEMP3), WS(ID),  
     2   WS(TEMP2), WS(TEMP1), WS(TEMP), JACDIM, NX-K, WS(IDIAG), IS(   
     3   IPIVOT))                                                       
      IF (NU .LE. 0) GOTO 3                                             
         DO  2 L = 1, NV                                                
            DO  1 I = 1, JACDIM                                         
               DU(I) = DU(I)+DV(L)*UOFVT(I, L+1)                        
   1           CONTINUE                                                 
   2        CONTINUE                                                    
   3  CALL LEAVE                                                        
      A90STD = FAILED                                                   
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A90STE(Y, NY, X, T, DT, ERRPAR, DELTA, E,        
     1   ERROR, ERROR1)                                                 
      INTEGER NY                                                        
      EXTERNAL ERROR, ERROR1                                            
      REAL Y(NY), X(1), T, DT, ERRPAR(2), DELTA                         
      REAL E(NY)                                                        
      LOGICAL ERROR                                                     
      COMMON /A90STK/ NU, NV, K, NX                                     
      INTEGER NU, NV, K, NX                                             
      LOGICAL ERPUTS                                                    
C THE ERROR FILTER FOR  ESSOM.                                          
C SCRATCH SPACE ALLOCATED - S(A90STE) = S(ERROR).                       
C X(NX).                                                                
      ERPUTS = DELTA .EQ. 1.                                            
      A90STE = ERROR(Y(NV+1), NU, K, X, NX, Y, NV, T, DT, ERRPAR,       
     1   ERPUTS, E(NV+1), E, ERROR1)                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE A90STH(T0, S0, X, T1, S1, NS, DT, TSTOP, OK, E,        
     1   HANDLE, HANLE1)                                                
      INTEGER NS                                                        
      EXTERNAL HANDLE, HANLE1                                           
      REAL T0, S0(NS), X(1), T1, S1(NS), DT                             
      REAL TSTOP, E(NS)                                                 
      LOGICAL OK                                                        
      COMMON /A90STK/ NU, NV, K, NX                                     
      INTEGER NU, NV, K, NX                                             
C OUTPUT FILTER IN BPOSTS.                                              
C SCRATCH SPACE ALLOCATED - S(A90STH) = S(HANDLE).                      
C X(NX).                                                                
      CALL HANDLE(T0, S0(NV+1), S0, T1, S1(NV+1), S1, NU, NV, K, X, NX  
     1   , DT, TSTOP, E(NV+1), E, OK, HANLE1)                           
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A90ST0(K, X, NX, U, UT, NU, V, VT, NV, T,        
     1   DT, UOFV, D1234, D, SCALE, DV, D40, D41, D4, DMAT, D50, D51,   
     2   D5, JACDIM, NXMK, DIAG, PIVOT)                                 
      INTEGER JACDIM, NV, NX                                            
      EXTERNAL D1234, D, SCALE                                          
      INTEGER K, NU, NXMK, PIVOT(NV)                                    
      REAL X(NX), U(JACDIM), UT(JACDIM), V(NV), VT(NV), T               
      REAL DT, UOFV(JACDIM,2), DV(NV), D40(NV,NV), D41(NV,NV), D4(NV,NV)
      REAL DMAT(NV, NV), D50(NV, JACDIM), D51(NV, JACDIM), D5(NV,       
     1   JACDIM), DIAG(NV)                                              
      LOGICAL D1234                                                     
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A9OSTT/ TIME, DELTAT                                      
      REAL TIME, DELTAT                                                 
      COMMON /A9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE                                            
      COMMON /A9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      INTEGER ISTKGT, NERROR, I, J, NERR, IS(1000)                      
      INTEGER ITEMP                                                     
      REAL RS(1000), WS(500)                                            
      LOGICAL GETACT, NEESUM, LS(1000)                                  
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C (U,UT)(NX-K,NU),                                                      
C UOFV(NX-K,NU,NV+1).                                                   
      NEESUM = DT .NE. DTJ                                              
      NEESUM = NEESUM .AND. SEPATE                                      
      TEMP1 = SEPATE                                                    
      IF (TEMP1) TEMP1 = GETJAC                                         
      IF (.NOT. TEMP1) GOTO 2                                           
         TIME = TJ                                                      
         IF (.NOT. D1234(K, X, NX, NXMK, U, UT, NU, V, VT, NV, TJ,      
     1      GETJAC, SEPATE, D, D50, D51, DV, D40, D41)) GOTO 1          
            TIME = T                                                    
            A90ST0 = .TRUE.                                             
            RETURN                                                      
   1     TIME = T                                                       
   2  TEMP1 = GETJAC                                                    
      IF (TEMP1) TEMP1 = .NOT. SEPATE                                   
      GETACT = TEMP1                                                    
      IF (.NOT. D1234(K, X, NX, NXMK, U, UT, NU, V, VT, NV, T, GETACT,  
     1   .FALSE., D, D5, D51, DV, D4, D41)) GOTO 3                      
         A90ST0 = .TRUE.                                                
         RETURN                                                         
   3  DO  4 I = 1, NV                                                   
         DV(I) = -DV(I)                                                 
   4     CONTINUE                                                       
      IF (.NOT. NEESUM) GOTO 10                                         
         DO  6 I = 1, NV                                                
            DO  5 J = 1, NV                                             
               D4(I, J) = D40(I, J)+D41(I, J)/DT                        
   5           CONTINUE                                                 
   6        CONTINUE                                                    
         IF (NU .LE. 0) GOTO 9                                          
            DO  8 J = 1, NV                                             
               DO  7 I = 1, JACDIM                                      
                  D5(J, I) = D50(J, I)+D51(J, I)/DT                     
   7              CONTINUE                                              
   8           CONTINUE                                                 
   9     CONTINUE                                                       
  10  TEMP1 = GETJAC                                                    
      IF (.NOT. TEMP1) TEMP1 = NEESUM                                   
      IF (TEMP1) CALL MOVEFR(NV**2, D4, DMAT)                           
      TEMP1 = NEESUM                                                    
      IF (.NOT. TEMP1) TEMP1 = GETJAC                                   
      IF (TEMP1) TEMP1 = NU .GT. 0                                      
      IF (.NOT. TEMP1) GOTO 13                                          
         ITEMP = ISTKGT(NV**2, 3)                                       
C FORM TEMP = D5 * W.                                                   
         CALL MMPY(D5, NV, JACDIM, UOFV(1, 2), NV, WS(ITEMP))           
         DO  12 I = 1, NV                                               
            DO  11 J = 1, NV                                            
               TEMP = ITEMP+I-1+(J-1)*NV                                
               DMAT(I, J) = DMAT(I, J)-WS(TEMP)                         
  11           CONTINUE                                                 
  12        CONTINUE                                                    
         CALL ISTKRL(1)                                                 
  13  TEMP1 = NEESUM                                                    
      IF (.NOT. TEMP1) TEMP1 = GETJAC                                   
      IF (.NOT. TEMP1) GOTO 15                                          
         CALL SCALE(4, 1, DMAT, NV, NV)                                 
C SCALE THE ODE JACOBIAN.                                               
         CALL QRD(NV, NV, DMAT, DIAG, PIVOT)                            
         IF (NERROR(NERR) .EQ. 0) GOTO 14                               
            CALL ERROFF                                                 
            FNUM = 7                                                    
            A90ST0 = .TRUE.                                             
            RETURN                                                      
  14     CONTINUE                                                       
  15  IF (NU .LE. 0) GOTO 17                                            
         ITEMP = ISTKGT(NV, 3)                                          
C FORM DV += D5*W.                                                      
         CALL MMPY(D5, NV, JACDIM, UOFV, 1, WS(ITEMP))                  
         DO  16 I = 1, NV                                               
            TEMP = ITEMP+I                                              
            DV(I) = DV(I)+WS(TEMP-1)                                    
  16        CONTINUE                                                    
         CALL ISTKRL(1)                                                 
C SCALE THE ODE RHS.                                                    
  17  CALL SCALE(4, 2, DV, NV, 1)                                       
      CALL QRQTB(NV, NV, DMAT, DIAG, PIVOT, 1, DV, DV)                  
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
C    1   32HA90STD - SINGULAR DJAC IN  QRQTB, 32, 1, 2)                 
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
     1   'A90STD - SINGULAR DJAC IN  QRQTB', 32, 1, 2)                  
C/                                                                      
C UN-SCALE THE ODE SOLUTION.                                            
      CALL SCALE(4, 3, DV, NV, 1)                                       
      A90ST0 = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A90STA(U, UT, NU, V, VT, NV, T, DT, K, X,        
     1   NX, AF, AF1, BC, BC1, D, D1234, SCALE, DU, ALFA, BETA, GAMMA0  
     2   , GAMMA1, GAMMA, AA, BB, CC, SGAMAD, SGAMAM, BCS, EQS, ORDER, B
     3   , EXCHG, BT, NXMK, NRHS)                                       
      INTEGER NRHS, NXMK, NU, NX                                        
      EXTERNAL AF, AF1, BC, BC1, D, D1234                               
      EXTERNAL SCALE                                                    
      INTEGER NV, K, BCS(NU, 2, 2), EQS(NU, 2, 2), ORDER(NU, NU, 2)     
      REAL U(NXMK, NU), UT(NXMK, NU), V(1), VT(1), T, DT                
      REAL X(NX), DU(NXMK, NU), ALFA(NU, NU, 2), BETA(NU, NU, 2),       
     1   GAMMA0(NU, NRHS, 2), GAMMA1(NU, NRHS, 2)                       
      REAL GAMMA(NU, NRHS, 2), AA(NU, NU, 2), BB(NU, NU, 2), CC(NU, NU  
     1   , 2), SGAMAD(NU, NRHS, 2), SGAMAM(NU, NRHS, 2)                 
      REAL B(NU, NXMK, NRHS), EXCHG(NU, NXMK), BT(NXMK, NU, NRHS)       
      LOGICAL BC                                                        
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /A9OSTT/ TIME, DELTAT                                      
      REAL TIME, DELTAT                                                 
      COMMON /A9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL THETA, EGIVE                                                 
      COMMON /A9OSTJ/ IJP, IB, IAFB, IALFA, IBETA, IGAMMA, ID4, ID5,    
     1   IORDER, IBC, IEQS, IAA, IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT,  
     2   IDMAT, IDIAG, IDPVOT                                           
      INTEGER IJP(3), IB(3), IAFB(3), IALFA(3), IBETA(3), IGAMMA(3)     
      INTEGER ID4(3), ID5(3), IORDER, IBC, IEQS, IAA                    
      INTEGER IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT                      
      INTEGER IDMAT, IDIAG, IDPVOT                                      
      COMMON /A9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      REAL TJ, DTJ                                                      
      LOGICAL GETJAC, SEPATE                                            
      COMMON /A9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /A90STQ/ IXGQ, IWGQ, MGQ                                   
      INTEGER IXGQ, IWGQ, MGQ                                           
      COMMON /A90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS      
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS                         
      INTEGER NNFS, NRS                                                 
      COMMON /A90STB/ IGSSIS, SET                                       
      INTEGER IGSSIS                                                    
      LOGICAL SET                                                       
      INTEGER JACLEN, IEU, SORDER, VORDER, ISTKGT, NERROR               
      INTEGER MAX0, I, J, L, NBCS, IEUX                                 
      INTEGER IEUT, NERR, IS(1000), IEUTX, NRHSD, NRHSG                 
      INTEGER MAXDER                                                    
      REAL WS(500), RS(1000)                                            
      LOGICAL NEESUM, GETACT, LS(1000), GLSIN, GLSBI, GLSBC             
      INTEGER TEMP, TEMP2, TEMP3, TEMP4, TEMP5, TEMP6                   
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C (V,VT)(NV).                                                           
      JACLEN = NU*(NX-K)*(2*K*NU-1)                                     
      NEESUM = DTJ .NE. DT                                              
      NEESUM = NEESUM .AND. SEPATE                                      
      TEMP1 = GETJAC                                                    
      IF (TEMP1) TEMP1 = .NOT. SEPATE                                   
      IF (.NOT. TEMP1) GOTO 1                                           
         NRHSG = NRHS                                                   
         GOTO  2                                                        
   1     NRHSG = 1                                                      
   2  TEMP1 = GETJAC                                                    
      IF (.NOT. TEMP1) TEMP1 = NEESUM                                   
      IF (.NOT. TEMP1) GOTO 3                                           
         NRHSD = NRHS                                                   
         GOTO  4                                                        
   3     NRHSD = 1                                                      
   4  TEMP1 = SEPATE                                                    
      IF (TEMP1) TEMP1 = GETJAC                                         
      IF (.NOT. TEMP1) GOTO 7                                           
         TIME = TJ                                                      
         TEMP4 = IJP(1)                                                 
         TEMP = IJP(2)                                                  
         TEMP3 = IB(1)                                                  
         TEMP5 = IB(2)                                                  
         IF (.NOT. GLSIN(K, X, NX, AF, AF1, NU, NRHS, NRHS, MGQ, WS(    
     1      IXGQ), WS(IWGQ), GETJAC, SEPATE, WS(TEMP4), WS(TEMP), WS(   
     2      TEMP3), WS(TEMP5), ORDER, IGSSIS, SET)) GOTO 5              
            TIME = T                                                    
            A90STA = .TRUE.                                             
            RETURN                                                      
   5     TIME = T                                                       
         IF (NERROR(NERR) .NE. 0) CALL ERROFF                           
C/6S                                                                    
C        IF (NERR .EQ. 14) CALL SETERR(                                 
C    1      33H POST4 - MGQ=K-1 AND ORDER(I,.)=0, 33, 1011, 1)          
C        IF (NERR .EQ. 15) CALL SETERR(26H POST4 - PDE(I) IS VACUOUS,   
C    1      26, 1012, 1)                                                
C/7S                                                                    
         IF (NERR .EQ. 14) CALL SETERR(                                 
     1      ' POST4 - MGQ=K-1 AND ORDER(I,.)=0', 33, 1011, 1)           
         IF (NERR .EQ. 15) CALL SETERR(' POST4 - PDE(I) IS VACUOUS',    
     1      26, 1012, 1)                                                
C/                                                                      
         IF (NERR .EQ. 0) GOTO 6                                        
            A90STA = .TRUE.                                             
            RETURN                                                      
   6     CONTINUE                                                       
   7  IF (.NOT. NEESUM) GOTO 10                                         
         DO  8 I = 1, JACLEN                                            
            TEMP5 = IJP(3)+I                                            
            TEMP3 = IJP(1)+I                                            
            TEMP = IJP(2)+I                                             
            WS(TEMP5-1) = WS(TEMP3-1)+WS(TEMP-1)/DT                     
   8        CONTINUE                                                    
         TEMP = NU*NXMK*NRHS                                            
         DO  9 I = 1, TEMP                                              
            TEMP3 = IB(1)+I                                             
            TEMP5 = IB(2)+I                                             
            B(I, 1, 1) = WS(TEMP3-1)+WS(TEMP5-1)/DT                     
   9        CONTINUE                                                    
  10  TEMP1 = GETJAC                                                    
      IF (TEMP1) TEMP1 = .NOT. SEPATE                                   
      GETACT = TEMP1                                                    
      TEMP = IJP(3)                                                     
      TEMP5 = IJP(2)                                                    
      TEMP3 = IB(3)                                                     
      TEMP4 = IB(2)                                                     
      IF (.NOT. GLSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG, MGQ, WS(IXGQ),
     1   WS(IWGQ), GETACT, .FALSE., WS(TEMP), WS(TEMP5), WS(TEMP3), WS( 
     2   TEMP4), ORDER, IGSSIS, SET)) GOTO 11                           
         A90STA = .TRUE.                                                
         RETURN                                                         
  11  IF (NERROR(NERR) .NE. 0) CALL ERROFF                              
C/6S                                                                    
C     IF (NERR .EQ. 14) CALL SETERR(                                    
C    1   33H POST4 - MGQ=K-1 AND ORDER(I,.)=0, 33, 1011, 1)             
C     IF (NERR .EQ. 15) CALL SETERR(26H POST4 - PDE(I) IS VACUOUS, 26,  
C    1   1012, 1)                                                       
C/7S                                                                    
      IF (NERR .EQ. 14) CALL SETERR(                                    
     1   ' POST4 - MGQ=K-1 AND ORDER(I,.)=0', 33, 1011, 1)              
      IF (NERR .EQ. 15) CALL SETERR(' POST4 - PDE(I) IS VACUOUS', 26,   
     1   1012, 1)                                                       
C/                                                                      
      IF (NERR .EQ. 0) GOTO 12                                          
         A90STA = .TRUE.                                                
         RETURN                                                         
  12  IEU = ISTKGT(8*NU, 3)                                             
      IEUX = IEU+2*NU                                                   
      IEUT = IEUX+2*NU                                                  
      IEUTX = IEUT+2*NU                                                 
      TEMP1 = SEPATE                                                    
      IF (TEMP1) TEMP1 = GETJAC                                         
      IF (.NOT. TEMP1) GOTO 14                                          
         TIME = TJ                                                      
         TEMP4 = IALFA(1)                                               
         TEMP3 = IBETA(1)                                               
         TEMP5 = IALFA(2)                                               
         TEMP = IBETA(2)                                                
         IF (.NOT. BC(U, UT, NU, V, VT, NV, TJ, 0E0, K, X, NX, GETJAC,  
     1      SEPATE, BC1, WS(TEMP4), WS(TEMP3), GAMMA0, WS(TEMP5), WS(   
     2      TEMP), GAMMA1, NX-K, NRHS, NRHS, WS(IEU), WS(IEUX), WS(IEUT)
     3      , WS(IEUTX))) GOTO 13                                       
            TIME = T                                                    
            A90STA = .TRUE.                                             
            RETURN                                                      
  13     TIME = T                                                       
  14  IF (.NOT. NEESUM) GOTO 19                                         
         TEMP = 2*NU**2                                                 
         DO  15 I = 1, TEMP                                             
            TEMP5 = IALFA(1)+I                                          
            TEMP3 = IALFA(2)+I                                          
            ALFA(I, 1, 1) = WS(TEMP5-1)+WS(TEMP3-1)/DT                  
            TEMP3 = IBETA(1)+I                                          
            TEMP5 = IBETA(2)+I                                          
            BETA(I, 1, 1) = WS(TEMP3-1)+WS(TEMP5-1)/DT                  
  15        CONTINUE                                                    
         DO  18 L = 1, 2                                                
            DO  17 I = 1, NU                                            
               DO  16 J = 1, NRHS                                       
                  GAMMA(I, J, L) = GAMMA0(I, J, L)+GAMMA1(I, J, L)/DT   
  16              CONTINUE                                              
  17           CONTINUE                                                 
  18        CONTINUE                                                    
  19  TEMP1 = GETJAC                                                    
      IF (TEMP1) TEMP1 = .NOT. SEPATE                                   
      GETACT = TEMP1                                                    
      TEMP = IALFA(3)                                                   
      TEMP5 = IBETA(3)                                                  
      TEMP3 = IALFA(2)                                                  
      TEMP4 = IBETA(2)                                                  
      IF (.NOT. BC(U, UT, NU, V, VT, NV, T, DT, K, X, NX, GETACT,       
     1   .FALSE., BC1, WS(TEMP), WS(TEMP5), GAMMA, WS(TEMP3), WS(TEMP4),
     2   GAMMA1, NX-K, NRHS, NRHSG, WS(IEU), WS(IEUX), WS(IEUT), WS(    
     3   IEUTX))) GOTO 20                                               
         A90STA = .TRUE.                                                
         RETURN                                                         
  20  CALL ISTKRL(1)                                                    
      DO  22 L = 1, 2                                                   
         CALL GLSSB(ALFA(1, 1, L), BETA(1, 1, L), GAMMA(1, 1, L), NU,   
     1      NRHSD, AA(1, 1, L), BB(1, 1, L), CC(1, 1, L), SGAMAD(1, 1, L
     2      ), SGAMAM(1, 1, L), BCS(1, 1, L))                           
         IF (NERROR(NERR) .NE. 0) CALL ERROFF                           
C/6S                                                                    
C        IF (NERR .EQ. 3) CALL SETERR(                                  
C    1      53H POST4 - MIXED BOUNDARY CONDITIONS ARE OVERDETERMINED,   
C    2      53, 1006, 1)                                                
C/7S                                                                    
         IF (NERR .EQ. 3) CALL SETERR(                                  
     1      ' POST4 - MIXED BOUNDARY CONDITIONS ARE OVERDETERMINED',    
     2      53, 1006, 1)                                                
C/                                                                      
         IF (NERR .EQ. 5) FNUM = 5                                      
C/6S                                                                    
C        IF (NERR .EQ. 4) CALL SETERR(                                  
C    1      57H POST4 - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED
C    2      , 57, 1005, 1)                                              
C/7S                                                                    
         IF (NERR .EQ. 4) CALL SETERR(                                  
     1      ' POST4 - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED' 
     2      , 57, 1005, 1)                                              
C/                                                                      
         IF (NERR .EQ. 6) FNUM = 4                                      
         IF (NERR .EQ. 0) GOTO 21                                       
            A90STA = .TRUE.                                             
            RETURN                                                      
  21     CONTINUE                                                       
  22     CONTINUE                                                       
      TEMP1 = SEPATE                                                    
      IF (TEMP1) TEMP1 = GETJAC                                         
      IF (.NOT. TEMP1) GOTO 24                                          
         TIME = TJ                                                      
         TEMP4 = IAFB(1)                                                
         TEMP3 = IAFB(1)+2*NU**2                                        
         TEMP5 = IAFB(1)+4*NU**2                                        
         TEMP = IAFB(2)                                                 
         TEMP2 = IAFB(2)+2*NU**2                                        
         TEMP6 = IAFB(2)+4*NU**2                                        
         IF (.NOT. GLSBI(K, X, NX, AF, AF1, NU, NRHS, NRHS, GETJAC,     
     1      SEPATE, WS(TEMP4), WS(TEMP3), WS(TEMP5), WS(TEMP), WS(TEMP2)
     2      , WS(TEMP6))) GOTO 23                                       
            TIME = T                                                    
            A90STA = .TRUE.                                             
            RETURN                                                      
  23     TIME = T                                                       
  24  IF (.NOT. NEESUM) GOTO 26                                         
         TEMP6 = 2*NU*(2*NU+NRHS)                                       
         DO  25 I = 1, TEMP6                                            
            TEMP2 = IAFB(3)+I                                           
            TEMP = IAFB(1)+I                                            
            TEMP5 = IAFB(2)+I                                           
            WS(TEMP2-1) = WS(TEMP-1)+WS(TEMP5-1)/DT                     
  25        CONTINUE                                                    
  26  TEMP1 = GETJAC                                                    
      IF (TEMP1) TEMP1 = .NOT. SEPATE                                   
      GETACT = TEMP1                                                    
      TEMP6 = IAFB(3)                                                   
      TEMP5 = IAFB(3)+2*NU**2                                           
      TEMP = IAFB(3)+4*NU**2                                            
      TEMP2 = IAFB(2)                                                   
      TEMP3 = IAFB(2)+2*NU**2                                           
      TEMP4 = IAFB(2)+4*NU**2                                           
      IF (.NOT. GLSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG, GETACT,       
     1   .FALSE., WS(TEMP6), WS(TEMP5), WS(TEMP), WS(TEMP2), WS(TEMP3)  
     2   , WS(TEMP4))) GOTO 27                                          
         A90STA = .TRUE.                                                
         RETURN                                                         
  27  TEMP1 = GETJAC                                                    
      IF (.NOT. TEMP1) TEMP1 = NEESUM                                   
      GETACT = TEMP1                                                    
      TEMP4 = IAFB(3)                                                   
      TEMP3 = IAFB(3)+2*NU**2                                           
      TEMP2 = IAFB(3)+4*NU**2                                           
      TEMP = IJP(3)                                                     
      CALL GLSBT(K, X, NX, NU, NRHS, NRHSD, GETACT, WS(TEMP4), WS(TEMP3)
     1   , WS(TEMP2), AA, BB, SGAMAM, BCS, WS(TEMP), B)                 
      TEMP1 = NEESUM                                                    
      IF (.NOT. TEMP1) TEMP1 = GETJAC                                   
      IF (.NOT. TEMP1) GOTO 39                                          
         IF (GLSBC(NU, ORDER, BCS, EQS)) GOTO 30                        
            CALL GLSBP(NU, ORDER, BCS, EQS)                             
            IF (NERROR(NERR) .EQ. 0) GOTO 28                            
               CALL ERROFF                                              
C/6S                                                                    
C              CALL SETERR(21H POST4 - IMPROPER BCS, 21, 1007, 1)       
C/7S                                                                    
               CALL SETERR(' POST4 - IMPROPER BCS', 21, 1007, 1)        
C/                                                                      
               GOTO  29                                                 
C/6S                                                                    
C 28           IF (.NOT. GLSBC(NU, ORDER, BCS, EQS)) CALL SETERR(       
C    1            26HA90STA -  GLSBP-BC FAILURE, 26, 1, 2)              
C/7S                                                                    
  28           IF (.NOT. GLSBC(NU, ORDER, BCS, EQS)) CALL SETERR(       
     1            'A90STA -  GLSBP-BC FAILURE', 26, 1, 2)               
C/                                                                      
  29        CONTINUE                                                    
C COUNT THE BOUNDARY CONDITIONS.                                        
  30     NBCS = 0                                                       
         DO  32 I = 1, NU                                               
            DO  31 J = 1, 2                                             
               IF (BCS(I, 1, J) .GE. 0) NBCS = NBCS+1                   
               IF (BCS(I, 2, J) .GE. 0) NBCS = NBCS+1                   
  31           CONTINUE                                                 
  32        CONTINUE                                                    
         SORDER = 0                                                     
C COUNT THE TOTAL ORDER OF THE SYSTEM.                                  
         VORDER = 0                                                     
         DO  35 J = 1, NU                                               
            MAXDER = 0                                                  
            DO  33 I = 1, NU                                            
               MAXDER = MAX0(MAXDER, ORDER(I, J, 1), ORDER(I, J, 2))    
  33           CONTINUE                                                 
            SORDER = SORDER+MAXDER                                      
            MAXDER = 0                                                  
            DO  34 I = 1, NU                                            
               MAXDER = MAX0(MAXDER, ORDER(J, I, 1), ORDER(J, I, 2))    
  34           CONTINUE                                                 
            VORDER = VORDER+MAXDER                                      
  35        CONTINUE                                                    
         IF (SORDER .EQ. VORDER) GOTO 36                                
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(                                                
C    1         45H POST4 - PDE SYSTEM NOT IN MINIMAL ORDER FORM, 45,    
C    2         1008, 1)                                                 
C/7S                                                                    
            CALL SETERR(                                                
     1         ' POST4 - PDE SYSTEM NOT IN MINIMAL ORDER FORM', 45,     
     2         1008, 1)                                                 
C/                                                                      
            A90STA = .TRUE.                                             
            RETURN                                                      
  36     IF (NBCS .GE. SORDER) GOTO 37                                  
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(36H POST4 - TOO FEW BOUNDARY CONDITIONS, 36,    
C    1         1009, 1)                                                 
C/7S                                                                    
            CALL SETERR(' POST4 - TOO FEW BOUNDARY CONDITIONS', 36,     
     1         1009, 1)                                                 
C/                                                                      
  37     IF (NBCS .LE. SORDER) GOTO 38                                  
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(37H POST4 - TOO MANY BOUNDARY CONDITIONS, 37,   
C    1         1010, 1)                                                 
C/7S                                                                    
            CALL SETERR(' POST4 - TOO MANY BOUNDARY CONDITIONS', 37,    
     1         1010, 1)                                                 
C/                                                                      
  38     CONTINUE                                                       
  39  IF (NERROR(NERR) .EQ. 0) GOTO 40                                  
         A90STA = .TRUE.                                                
         RETURN                                                         
  40  TEMP1 = GETJAC                                                    
      IF (.NOT. TEMP1) TEMP1 = NEESUM                                   
      GETACT = TEMP1                                                    
      TEMP = IJP(3)                                                     
      CALL GLSBD(K, NX, NU, NRHS, NRHSD, BCS, EQS, CC, SGAMAD, GETACT,  
     1   WS(TEMP), B)                                                   
      IF (KEEJAC .NE. 0) GOTO 41                                        
         IL = ISTKGT((K*NU-1)*(NX-K)*NU, 3)                             
         IPPVOT = ISTKGT(NU*(NX-K), 2)                                  
  41  TEMP1 = NEESUM                                                    
      IF (.NOT. TEMP1) TEMP1 = GETJAC                                   
      IF (.NOT. TEMP1) GOTO 43                                          
         TEMP = IJP(3)                                                  
C SCALE PDE JACOBIAN.                                                   
         CALL SCALE(1, 1, WS(TEMP), (NX-K)*NU, 2*K*NU-1)                
         TEMP = IJP(3)                                                  
         CALL BNDLU((NX-K)*NU, K*NU, 2*K*NU-1, WS(TEMP), WS(IL), IS(    
     1      IPPVOT))                                                    
         IF (NERROR(NERR) .EQ. 0) GOTO 42                               
            CALL ERROFF                                                 
            FNUM = 6                                                    
            A90STA = .TRUE.                                             
            RETURN                                                      
  42     CONTINUE                                                       
C SCALE THE PDE RHS.                                                    
  43  CALL SCALE(1, 2, B, (NX-K)*NU, NRHSD)                             
      TEMP = IJP(3)                                                     
      CALL BNDFB((NX-K)*NU, K*NU, 2*K*NU-1, WS(IL), WS(TEMP), IS(IPPVOT)
     1   , NRHSD, B)                                                    
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
C    1   31HA90STC - SINGULAR JAC IN  BNDFB, 31, 1, 2)                  
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
     1   'A90STC - SINGULAR JAC IN  BNDFB', 31, 1, 2)                   
C/                                                                      
C UN-SCALE THE PDE SOLUTION.                                            
      CALL SCALE(1, 3, B, (NX-K)*NU, NRHSD)                             
      IF (KEEJAC .EQ. 0) CALL ISTKRL(2)                                 
C FORM BT = B-TRANSPOSE.                                                
      DO  46 L = 1, NRHSD                                               
         CALL MOVEFR((NX-K)*NU, B(1, 1, L), EXCHG)                      
         TEMP = NX-K                                                    
         DO  45 I = 1, TEMP                                             
            DO  44 J = 1, NU                                            
               BT(I, J, L) = EXCHG(J, I)                                
  44           CONTINUE                                                 
  45        CONTINUE                                                    
  46     CONTINUE                                                       
      DO  48 J = 1, NU                                                  
         TEMP = NX-K                                                    
         DO  47 I = 1, TEMP                                             
            DU(I, J) = BT(I, J, 1)                                      
  47        CONTINUE                                                    
  48     CONTINUE                                                       
      A90STA = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE BNDFB(N, ML, M, L, U, INT, NB, B)                      
      INTEGER M, N, NB, ML                                              
      INTEGER INT(N)                                                    
      REAL L(N, ML), U(N, M), B(N, NB)                                  
      INTEGER NERR                                                      
C TO SOLVE L*U*X = B, WHERE L AND U RESULT FROM A CALL TO  BNDS.        
C MNEMONIC - BAND FORWARD ELIMINATION AND                               
C            BACK-SOLVE.                                                
C INPUT -                                                               
C   N   - THE ORDER OF THE SYSTEM.                                      
C   ML  - THE NUMBER OF NONZERO ENTRIES OF L ON AND BELOW               
C         THE DIAGONAL.                                                 
C   M   - THE NUMBER OF NONZERO ELEMENTS OF U ON AND ABOVE              
C         THE DIAGONAL.                                                 
C   L   - THE LOWER TRIANGULAR BANDED FACTOR.                           
C   U   - THE UPPER TRIANGULAR BANDED FACTOR.                           
C   INT - THE ORDERING OF THE ROWS OF THE SYSTEM, DUE TO PIVOTING.      
C   NB  - THE NUMBER OF RIGHT-HAND-SIDES.                               
C   B   - THE RIGHT-HAND-SIDES.                                         
C OUTPUT -                                                              
C   B - THE SOLUTION VECTORS.                                           
C SCRATCH SPACE ALLOCATED - NONE.                                       
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - ML.LT.1.                                                        
C   3 - M.LT.ML.                                                        
C   4 - NB.LT.1.                                                        
C L(N,ML-1).                                                            
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15H BNDFB - N.LT.1, 15, 1, 2)           
C     IF (ML .LT. 1) CALL SETERR(16H BNDFB - ML.LT.1, 16, 2, 2)         
C     IF (M .LT. ML) CALL SETERR(16H BNDFB - M.LT.ML, 16, 3, 2)         
C     IF (NB .LT. 1) CALL SETERR(16H BNDFB - NB.LT.1, 16, 4, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' BNDFB - N.LT.1', 15, 1, 2)            
      IF (ML .LT. 1) CALL SETERR(' BNDFB - ML.LT.1', 16, 2, 2)          
      IF (M .LT. ML) CALL SETERR(' BNDFB - M.LT.ML', 16, 3, 2)          
      IF (NB .LT. 1) CALL SETERR(' BNDFB - NB.LT.1', 16, 4, 2)          
C/                                                                      
C PROTECT AGAINST AN EXISTING ERROR STATE.                              
      CALL ENTSRC(NERR, 0)                                              
C DO THE FORWARD-ELIMINATION.                                           
      CALL BNDFE(N, ML, L, INT, NB, B)                                  
C DO THE BACK-SUBSTITUTION.                                             
      CALL BNDBS(N, M, U, NB, B)                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE BNDFE(N, ML, L, INT, NB, B)                            
      INTEGER N, NB, ML                                                 
      INTEGER INT(N)                                                    
      REAL L(N, ML), B(N, NB)                                           
      INTEGER I, J, K, M1, MIN0                                         
      REAL X                                                            
      INTEGER TEMP, TEMP1, TEMP2                                        
C TO SOLVE L*X = B, WHERE L IS A LOWER TRIANGULAR BANDED MATRIX.        
C MNEMONIC - BANDED FORWARD-ELIMINATION.                                
C INPUT -                                                               
C   N   - THE ORDER OF THE SYSTEM.                                      
C   ML  - THE NUMBER OF NONZERO ELEMENTS OF L ON AND BELOW THE DIAGONAL.
C   L   - THE LOWER TRIANGULAR BANDED MATRIX.                           
C   INT - THE ORDERING OF THE ROWS OF THE SYSTEM, DUE TO PIVOTING.      
C   NB  - THE NUMBER OF RIGHT-HAND-SIDES.                               
C   B   - THE RIGHT-HAND-SIDES.                                         
C OUTPUT -                                                              
C   B - THE SOLUTION VECTORS, X.                                        
C SCRATCH STORAGE ALLOCATED - NONE.                                     
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - ML.LT.1.                                                        
C   3 - NB.LT.1.                                                        
C   4 - INT(I) NOT ONE OF 1,...,N.                                      
C L(N,ML-1).                                                            
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15H BNDFE - N.LT.1, 15, 1, 2)           
C     IF (ML .LT. 1) CALL SETERR(16H BNDFE - ML.LT.1, 16, 2, 2)         
C     IF (NB .LT. 1) CALL SETERR(16H BNDFE - NB.LT.1, 16, 3, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' BNDFE - N.LT.1', 15, 1, 2)            
      IF (ML .LT. 1) CALL SETERR(' BNDFE - ML.LT.1', 16, 2, 2)          
      IF (NB .LT. 1) CALL SETERR(' BNDFE - NB.LT.1', 16, 3, 2)          
C/                                                                      
C PROTECT AGAINST AN EXISTING ERROR STATE.                              
      CALL ENTSRC(I, 0)                                                 
      M1 = ML-1                                                         
      DO  5 K = 1, N                                                    
         I = INT(K)                                                     
C/6S                                                                    
C        IF (I .LT. 1 .OR. I .GT. N) CALL SETERR(                       
C    1      34H BNDFE - INT(I) NOT ONE OF 1,...,N, 34, 4, 2)            
C/7S                                                                    
         IF (I .LT. 1 .OR. I .GT. N) CALL SETERR(                       
     1      ' BNDFE - INT(I) NOT ONE OF 1,...,N', 34, 4, 2)             
C/                                                                      
         IF (I .EQ. K) GOTO 2                                           
            DO  1 J = 1, NB                                             
C INTERCHANGE THE ELEMENTS OF B.                                        
               X = B(K, J)                                              
               B(K, J) = B(I, J)                                        
               B(I, J) = X                                              
   1           CONTINUE                                                 
   2     IF (M1 .EQ. 0 .OR. K .EQ. N) GOTO  6                           
         TEMP1 = K+1                                                    
         TEMP = MIN0(M1+K, N)                                           
         DO  4 I = TEMP1, TEMP                                          
            TEMP2 = I-K                                                 
            X = L(K, TEMP2)                                             
            DO  3 J = 1, NB                                             
               B(I, J) = B(I, J)-X*B(K, J)                              
   3           CONTINUE                                                 
   4        CONTINUE                                                    
   5     CONTINUE                                                       
   6  RETURN                                                            
      END                                                               
      SUBROUTINE BNDLU(N, ML, M, G, L, INT)                             
      INTEGER M, N, ML                                                  
      INTEGER INT(N)                                                    
      REAL G(N, M), L(N, ML)                                            
      INTEGER I, J, K, M1, M2, LL                                       
      INTEGER MIN0                                                      
      REAL FLOAT, R1MACH, X, ABS, EPS, NORM                             
      REAL AMAX1                                                        
      LOGICAL SING                                                      
      INTEGER TEMP, TEMP1                                               
C TO OBTAIN THE LU DECOMPOSITION OF A BANDED MATRIX,                    
C USING GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING.                     
C MNEMONIC - BAND LU DECOMPOSITION.                                     
C INPUT -                                                               
C   N   - THE ORDER OF THE MATRIX.                                      
C   ML  - THE NUMBER OF NONZERO ELEMENTS OF A ON AND BELOW THE DIAGONAL.
C   M   - THE NUMBER OF NONZERO ELEMENTS IN EACH ROW OF A.              
C   G   - THE MATRIX A, WITH G(I,J) = A(I,I+J-ML).                      
C OUTPUT -                                                              
C   L   - THE LOWER TRIANGULAR BANDED FACTOR OF A.                      
C   G   - THE UPPER TRIANGULAR BANDED FACTOR OF A.                      
C   INT - THE ROW PIVOTING USED.                                        
C SCRATCH STORAGE ALLOCATED - NONE.                                     
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - ML.LT.1.                                                        
C   3 - M.LT.ML.                                                        
C   4 - SINGULAR MATRIX. (RECOVERABLE)                                  
C L(N,ML-1).                                                            
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15H BNDLU - N.LT.1, 15, 1, 2)           
C     IF (ML .LT. 1) CALL SETERR(16H BNDLU - ML.LT.1, 16, 2, 2)         
C     IF (M .LT. ML) CALL SETERR(16H BNDLU - M.LT.ML, 16, 3, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' BNDLU - N.LT.1', 15, 1, 2)            
      IF (ML .LT. 1) CALL SETERR(' BNDLU - ML.LT.1', 16, 2, 2)          
      IF (M .LT. ML) CALL SETERR(' BNDLU - M.LT.ML', 16, 3, 2)          
C/                                                                      
C PROTECT AGAINST AN EXISTING ERROR STATE.                              
      CALL ENTSRC(I, 0)                                                 
      SING = .FALSE.                                                    
      EPS = R1MACH(4)*FLOAT(N*(M-1)*(ML-1))                             
      M1 = ML-1                                                         
      M2 = M-ML                                                         
      LL = M1                                                           
      I = 1                                                             
         GOTO  2                                                        
   1     I = I+1                                                        
   2     IF (I .GT. MIN0(M1, N)) GOTO  5                                
C SET TO 0 THOSE ELEMENTS                                               
C OF G WHICH ARE UNDEFINED.                                             
         TEMP = ML+1-I                                                  
         DO  3 J = TEMP, M                                              
            TEMP1 = J-LL                                                
            G(I, TEMP1) = G(I, J)                                       
   3        CONTINUE                                                    
         LL = LL-1                                                      
         TEMP = M-LL                                                    
         DO  4 J = TEMP, M                                              
            G(I, J) = 0.0E0                                             
   4        CONTINUE                                                    
         GOTO  1                                                        
   5  I = 1                                                             
         GOTO  7                                                        
   6     I = I+1                                                        
   7     IF (I .GT. MIN0(M2, N)) GOTO  9                                
C ZERO OUT LOWER RHS WART.                                              
         TEMP = ML+I                                                    
         DO  8 J = TEMP, M                                              
            TEMP1 = N+1-I                                               
            G(TEMP1, J) = 0.0E0                                         
   8        CONTINUE                                                    
         GOTO  6                                                        
C GET || A || SUB INFINITY.                                             
   9  NORM = 0.0E0                                                      
      DO  11 I = 1, N                                                   
         INT(I) = I                                                     
         X = 0.0E0                                                      
         DO  10 J = 1, M                                                
            X = X+ABS(G(I, J))                                          
  10        CONTINUE                                                    
         NORM = AMAX1(NORM, X)                                          
  11     CONTINUE                                                       
      DO  20 K = 1, N                                                   
         X = G(K, 1)                                                    
         I = K                                                          
         LL = MIN0(M1+K, N)                                             
         IF (K .GE. LL) GOTO 14                                         
            TEMP = K+1                                                  
C GET THE PIVOT ROW.                                                    
            DO  13 J = TEMP, LL                                         
               IF (ABS(G(J, 1)) .LE. ABS(X)) GOTO 12                    
                  X = G(J, 1)                                           
                  I = J                                                 
  12           CONTINUE                                                 
  13           CONTINUE                                                 
  14     INT(K) = I                                                     
         IF (ABS(X) .GT. NORM*EPS) GOTO 15                              
            SING = .TRUE.                                               
            G(K, 1) = NORM*EPS                                          
  15     IF (ML .EQ. 1 .OR. K .EQ. N) GOTO  20                          
         IF (I .EQ. K) GOTO 17                                          
            DO  16 J = 1, M                                             
C NEED TO INTERCHANGE THE ROWS.                                         
               X = G(K, J)                                              
               G(K, J) = G(I, J)                                        
               G(I, J) = X                                              
  16           CONTINUE                                                 
  17     IF (K .GE. LL) GOTO  20                                        
         TEMP = K+1                                                     
         DO  19 I = TEMP, LL                                            
            X = G(I, 1)/G(K, 1)                                         
            TEMP1 = I-K                                                 
            L(K, TEMP1) = X                                             
            DO  18 J = 2, M                                             
               G(I, J-1) = G(I, J)-X*G(K, J)                            
  18           CONTINUE                                                 
            G(I, M) = 0.0E0                                             
  19        CONTINUE                                                    
  20     CONTINUE                                                       
C/6S                                                                    
C     IF (SING) CALL SETERR(24H BNDLU - SINGULAR MATRIX, 24, 4, 1)      
C/7S                                                                    
      IF (SING) CALL SETERR(' BNDLU - SINGULAR MATRIX', 24, 4, 1)       
C/                                                                      
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION GLSBC(NU, ORDER, BC, E)                          
      INTEGER NU                                                        
      INTEGER ORDER(NU, NU, 2), BC(NU, 2, 2), E(NU, 2, 2)               
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ICOUNT, ISTKGT, I, J, L, IMAORD                           
      INTEGER IS(1000)                                                  
      REAL RS(1000), WS(500)                                            
      LOGICAL FAILED, ALLERO, LS(1000), A6LSBC                          
      LOGICAL TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO CHECK THAT THE BOUNDARY CONDITION PLACEMENT GIVEN BY E             
C IS CORRECT.                                                           
C MNEMONIC - GALERKIN'S METHOD FOR LINEAR SYSTEMS,                      
C            BOUNDARY PLACEMENT CHECK.                                  
C SCRATCH SPACE ALLOCATED - 4*NU INTEGER WORDS.                         
C CHECK THE INPUT.                                                      
C/6S                                                                    
C     IF (NU .LT. 1) CALL SETERR(16H GLSBC - NU.LT.1, 16, 1, 2)         
C/7S                                                                    
      IF (NU .LT. 1) CALL SETERR(' GLSBC - NU.LT.1', 16, 1, 2)          
C/                                                                      
      DO  6 L = 1, 2                                                    
         DO  5 I = 1, NU                                                
            TEMP = E(I, 1, L) .LT. 1                                    
            IF (.NOT. TEMP) TEMP = E(I, 1, L) .GT. NU                   
            IF (TEMP) TEMP = BC(I, 1, L) .EQ. 0                         
            IF (TEMP) GOTO 1                                            
               TEMP1 = E(I, 2, L) .LT. 1                                
               IF (.NOT. TEMP1) TEMP1 = E(I, 2, L) .GT. NU              
               IF (TEMP1) TEMP1 = BC(I, 2, L) .EQ. 1                    
               TEMP = TEMP1                                             
   1        IF (.NOT. TEMP) GOTO 2                                      
               GLSBC = .FALSE.                                          
               RETURN                                                   
C IS ORDER(I,.,L) = (-1, ... , -1)?                                     
   2        ALLERO = .TRUE.                                             
            DO  3 J = 1, NU                                             
               ALLERO = ALLERO .AND. ORDER(I, J, L) .EQ. (-1)           
               TEMP = ORDER(I, J, L) .LT. (-1)                          
               IF (.NOT. TEMP) TEMP = ORDER(I, J, L) .GT. 2             
C/6S                                                                    
C              IF (TEMP) CALL SETERR(                                   
C    1            41H GLSBC - ORDER(I,J,L) NOT ONE OF -1,0,1,2, 41, 3, 2
C    2            )                                                     
C/7S                                                                    
               IF (TEMP) CALL SETERR(                                   
     1            ' GLSBC - ORDER(I,J,L) NOT ONE OF -1,0,1,2', 41, 3, 2 
     2            )                                                     
C/                                                                      
   3           CONTINUE                                                 
            TEMP = BC(I, 1, L) .NE. (-2)                                
            IF (TEMP) TEMP = BC(I, 1, L) .NE. 0                         
            IF (TEMP) GOTO 4                                            
               TEMP1 = BC(I, 2, L) .NE. (-2)                            
               IF (TEMP1) TEMP1 = BC(I, 2, L) .NE. 1                    
               TEMP = TEMP1                                             
C/6S                                                                    
C  4        IF (TEMP) CALL SETERR(                                      
C    1         36H GLSBC - BC(I,.,L) NOT ONE OF -2,0,1, 36, 4, 2)       
C           IF (ALLERO) CALL SETERR(                                    
C    1         33H GLSBC - ORDER(I,.,L)=(-1,...,-1), 33, 5, 2)          
C/7S                                                                    
   4        IF (TEMP) CALL SETERR(                                      
     1         ' GLSBC - BC(I,.,L) NOT ONE OF -2,0,1', 36, 4, 2)        
            IF (ALLERO) CALL SETERR(                                    
     1         ' GLSBC - ORDER(I,.,L)=(-1,...,-1)', 33, 5, 2)           
C/                                                                      
   5        CONTINUE                                                    
   6     CONTINUE                                                       
      CALL ENTER(1)                                                     
      IMAORD = ISTKGT(4*NU, 2)                                          
      ICOUNT = IMAORD+2*NU                                              
      FAILED = A6LSBC(NU, ORDER, BC, E, IS(IMAORD), IS(ICOUNT))         
      CALL LEAVE                                                        
      GLSBC = FAILED                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE GLSBD(K, NX, NU, NRHS, NRHSG, BC, E, CC, GAM,          
     1   GETJAC, G, B)                                                  
      INTEGER NU                                                        
      INTEGER K, NX, NRHS, NRHSG, BC(NU, 2, 2), E(NU, 2, 2)             
      REAL CC(NU, NU, 2), GAM(1, 1, 1), G(1, 1), B(1, 1)                
      LOGICAL GETJAC                                                    
      INTEGER I                                                         
C MNEMONIC - GALERKIN'S METHOD FOR LINEAR SYSTEMS,                      
C            BOUNDARY CONDITIONS ( DIRICHLET ).                         
C SCRATCH SPACE ALLOCATED - NONE.                                       
C REAL GAM((NX-K)*NU,NRHS,2),G((NX-K)*NU,2*K*NU-1),B((NX-K)*NU,NRHS)    
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (K .LT. 2) CALL SETERR(15H GLSBD - K.LT.2, 15, 1, 2)           
C     IF (NX .LT. 2*K) CALL SETERR(18H GLSBD - NX.LT.2*K, 18, 2, 2)     
C     IF (NU .LT. 1) CALL SETERR(16H GLSBD - NU.LT.1, 16, 3, 2)         
C     IF (NRHS .LT. 1) CALL SETERR(18H GLSBD - NRHS.LT.1, 18, 4, 2)     
C     IF (NRHSG .LT. 0) CALL SETERR(19H GLSBD - NRHSG.LT.0, 19, 5, 2)   
C/7S                                                                    
      IF (K .LT. 2) CALL SETERR(' GLSBD - K.LT.2', 15, 1, 2)            
      IF (NX .LT. 2*K) CALL SETERR(' GLSBD - NX.LT.2*K', 18, 2, 2)      
      IF (NU .LT. 1) CALL SETERR(' GLSBD - NU.LT.1', 16, 3, 2)          
      IF (NRHS .LT. 1) CALL SETERR(' GLSBD - NRHS.LT.1', 18, 4, 2)      
      IF (NRHSG .LT. 0) CALL SETERR(' GLSBD - NRHSG.LT.0', 19, 5, 2)    
C/                                                                      
C PROTECT AGAINST AN EXISTING ERROR STATE.                              
      CALL ENTSRC(I, 0)                                                 
      CALL A6LSBD(K, NX, NU, NRHS, NRHSG, BC, E, CC, GAM, GETJAC, G, B  
     1   , (NX-K)*NU, 2*K*NU-1)                                         
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION GLSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG,        
     1   GETJAC, SEPATE, A1, A2, F1, A1T, A2T, F1T)                     
      INTEGER NRHS, NU, NX                                              
      EXTERNAL AF, AF1                                                  
      INTEGER K, NRHSG                                                  
      REAL X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS, 2), A1T(NU,
     1   NU, 2), A2T(NU, NU, 2)                                         
      REAL F1T(NU, NRHS, 2)                                             
      LOGICAL GETJAC, SEPATE                                            
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IAS, IFS, ISTKGT, I, ISBSIS, IS(1000)                     
      REAL RS(1000), WS(500)                                            
      LOGICAL FAILED, LS(1000), A6LSBI                                  
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C MNEMONIC - GALERKIN'S METHOD FOR LINEAR SYSTEMS,                      
C            BOUNDARY INTEGRAL TERMS.                                   
C SCRATCH SPACE ALLOCATED -                                             
C       S( GLSBI) = NU*(8*NU+2*NRHS) + 2*K                              
C   REAL WORDS +                                                        
C       MAX(3*K,S(AF))                                                  
C   INTEGER WORDS.                                                      
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (K .LT. 2) CALL SETERR(15H GLSBI - K.LT.2, 15, 1, 2)           
C     IF (NX .LT. 2*K) CALL SETERR(18H GLSBI - NX.LT.2*K, 18, 2, 2)     
C     IF (NU .LT. 1) CALL SETERR(16H GLSBI - NU.LT.1, 16, 3, 2)         
C     IF (NRHS .LT. 1) CALL SETERR(18H GLSBI - NRHS.LT.1, 18, 4, 2)     
C     IF (NRHSG .LT. 0) CALL SETERR(19H GLSBI - NRHSG.LT.0, 19, 5, 2)   
C/7S                                                                    
      IF (K .LT. 2) CALL SETERR(' GLSBI - K.LT.2', 15, 1, 2)            
      IF (NX .LT. 2*K) CALL SETERR(' GLSBI - NX.LT.2*K', 18, 2, 2)      
      IF (NU .LT. 1) CALL SETERR(' GLSBI - NU.LT.1', 16, 3, 2)          
      IF (NRHS .LT. 1) CALL SETERR(' GLSBI - NRHS.LT.1', 18, 4, 2)      
      IF (NRHSG .LT. 0) CALL SETERR(' GLSBI - NRHSG.LT.0', 19, 5, 2)    
C/                                                                      
C CHECK THAT X IS MONOTONE INCREASING.                                  
      DO  1 I = 2, NX                                                   
C/6S                                                                    
C        IF (X(I-1) .GT. X(I)) CALL SETERR(                             
C    1      37H GLSBI - X IS NOT MONOTONE INCREASING, 37, 6, 2)         
C/7S                                                                    
         IF (X(I-1) .GT. X(I)) CALL SETERR(                             
     1      ' GLSBI - X IS NOT MONOTONE INCREASING', 37, 6, 2)          
C/                                                                      
         IF (I+K .GT. NX) GOTO  1                                       
         TEMP1 = I+K                                                    
C/6S                                                                    
C        IF (X(TEMP1) .LE. X(I)) CALL SETERR(                           
C    1      37H GLSBI - X IS NOT MONOTONE INCREASING, 37, 6, 2)         
C/7S                                                                    
         IF (X(TEMP1) .LE. X(I)) CALL SETERR(                           
     1      ' GLSBI - X IS NOT MONOTONE INCREASING', 37, 6, 2)          
C/                                                                      
   1     CONTINUE                                                       
C CHECK THE MULTIPLICITY OF THE END POINTS.                             
      DO  3 I = 1, K                                                    
         TEMP = X(I) .NE. X(1)                                          
         IF (TEMP) GOTO 2                                               
            TEMP1 = NX-K+I                                              
            TEMP = X(TEMP1) .NE. X(NX)                                  
C/6S                                                                    
C  2     IF (TEMP) CALL SETERR(                                         
C    1      46H GLSBI - END POINTS OF X NOT OF MULTIPLICITY K, 46, 7, 2)
C/7S                                                                    
   2     IF (TEMP) CALL SETERR(                                         
     1      ' GLSBI - END POINTS OF X NOT OF MULTIPLICITY K', 46, 7, 2) 
C/                                                                      
   3     CONTINUE                                                       
      CALL ENTER(1)                                                     
C SCRATCH A AND F VALUES.                                               
      IAS = ISTKGT(4*NU*(2*NU+NRHS), 3)                                 
C SCRATCH F1 AND F2 VALUES.                                             
      IFS = IAS+8*NU**2                                                 
C SPLINE BASIS AND DERIVATIVES AT L AND R.                              
      ISBSIS = ISTKGT(2*K, 3)                                           
      FAILED = A6LSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG, GETJAC,       
     1   SEPATE, A1, A2, F1, A1T, A2T, F1T, WS(IAS), WS(IFS), WS(ISBSIS)
     2   )                                                              
      CALL LEAVE                                                        
      GLSBI = FAILED                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE GLSBP(NU, ORDER, BC, E)                                
      INTEGER NU                                                        
      INTEGER ORDER(NU, NU, 2), BC(NU, 2, 2), E(NU, 2, 2)               
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ICE, ISTKGT, MAX0, I, J, L                                
      INTEGER IPPS, INOW, IMAORD, INOOLD, IS(1000)                      
      REAL RS(1000), WS(500)                                            
      LOGICAL ALLERO, LS(1000)                                          
      INTEGER TEMP1, TEMP2                                              
      LOGICAL TEMP, TEMP3                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO DETERMINE WHICH ODE SHOULD USE WHICH BOUNDARY CONDITION.           
C MNEMONIC - GALERKIN'S METHOD FOR LINEAR SYSTEMS,                      
C            BOUNDARY CONDITION PLACEMENT.                              
C SCRATCH SPACE ALLOCATED -                                             
C       S( GLSBP) <= NU*(4*NU+15)                                       
C INTEGER WORDS.                                                        
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (NU .LT. 1) CALL SETERR(16H GLSBP - NU.LT.1, 16, 1, 2)         
C/7S                                                                    
      IF (NU .LT. 1) CALL SETERR(' GLSBP - NU.LT.1', 16, 1, 2)          
C/                                                                      
      DO  4 L = 1, 2                                                    
         DO  3 I = 1, NU                                                
C IS ORDER(I,.,L) = (-1, ... , -1)?                                     
            ALLERO = .TRUE.                                             
            DO  1 J = 1, NU                                             
               ALLERO = ALLERO .AND. ORDER(I, J, L) .EQ. (-1)           
               TEMP = ORDER(I, J, L) .LT. (-1)                          
               IF (.NOT. TEMP) TEMP = ORDER(I, J, L) .GT. 2             
C/6S                                                                    
C              IF (TEMP) CALL SETERR(                                   
C    1            41H GLSBP - ORDER(I,J,L) NOT ONE OF -1,0,1,2, 41, 2, 2
C    2            )                                                     
C/7S                                                                    
               IF (TEMP) CALL SETERR(                                   
     1            ' GLSBP - ORDER(I,J,L) NOT ONE OF -1,0,1,2', 41, 2, 2 
     2            )                                                     
C/                                                                      
   1           CONTINUE                                                 
            TEMP = BC(I, 1, L) .NE. (-2)                                
            IF (TEMP) TEMP = BC(I, 1, L) .NE. 0                         
            IF (TEMP) GOTO 2                                            
               TEMP3 = BC(I, 2, L) .NE. (-2)                            
               IF (TEMP3) TEMP3 = BC(I, 2, L) .NE. 1                    
               TEMP = TEMP3                                             
C/6S                                                                    
C  2        IF (TEMP) CALL SETERR(                                      
C    1         36H GLSBP - BC(I,.,L) NOT ONE OF -2,0,1, 36, 3, 2)       
C           IF (ALLERO) CALL SETERR(                                    
C    1         33H GLSBP - ORDER(I,.,L)=(-1,...,-1), 33, 4, 2)          
C/7S                                                                    
   2        IF (TEMP) CALL SETERR(                                      
     1         ' GLSBP - BC(I,.,L) NOT ONE OF -2,0,1', 36, 3, 2)        
            IF (ALLERO) CALL SETERR(                                    
     1         ' GLSBP - ORDER(I,.,L)=(-1,...,-1)', 33, 4, 2)           
C/                                                                      
   3        CONTINUE                                                    
   4     CONTINUE                                                       
      CALL ENTER(1)                                                     
C COMPLEMENT OF E.                                                      
      ICE = ISTKGT(NU, 2)                                               
C MAXORD(I,L) = MAX OVER J = 1, ... , NU ORDER(I,J,L).                  
      IMAORD = ISTKGT(2*NU, 2)                                          
      CALL SETI(2*NU, -1, IS(IMAORD))                                   
      DO  7 L = 1, 2                                                    
         DO  6 I = 1, NU                                                
            DO  5 J = 1, NU                                             
               TEMP2 = IMAORD+I-1+(L-1)*NU                              
               TEMP1 = IMAORD+I-1+(L-1)*NU                              
               IS(TEMP2) = MAX0(IS(TEMP1), ORDER(I, J, L))              
   5           CONTINUE                                                 
   6        CONTINUE                                                    
   7     CONTINUE                                                       
      I = 0                                                             
      IPPS = 1                                                          
      INOW = 0                                                          
   8  TEMP = I .GE. 4*NU                                                
      IF (TEMP) TEMP = IPPS .EQ. 1                                      
      IF (TEMP) GOTO  16                                                
         GOTO  13                                                       
C MAKE A NODE.                                                          
   9        INOOLD = INOW                                               
            I = I+1                                                     
            INOW = ISTKGT(NU+3, 2)                                      
            IS(INOW) = INOOLD                                           
C       GET THE CANDIDATES FOR E(I).                                    
            CALL A6LSBP(I, NU, ORDER, BC, E, IS(IMAORD), IS(ICE), IS(   
     1         INOW+3), IS(INOW+1))                                     
            IS(INOW+2) = 0                                              
            IPPS = 0                                                    
            GOTO  14                                                    
C SEARCHING A NODE.                                                     
  10        IS(INOW+2) = IS(INOW+2)+1                                   
            IF (IS(INOW+2) .LE. IS(INOW+1)) GOTO 11                     
               IPPS = -1                                                
C BACK-UP.                                                              
               GOTO  8                                                  
  11        TEMP1 = INOW+2+IS(INOW+2)-1                                 
            E(I, 1, 1) = IS(TEMP1+1)                                    
            IPPS = 1                                                    
            GOTO  14                                                    
C BACKING UP A NODE.                                                    
  12        INOW = IS(INOW)                                             
            CALL ISTKRL(1)                                              
            I = I-1                                                     
            IPPS = 0                                                    
            GOTO  14                                                    
  13        TEMP1 = IPPS+2                                              
            IF (TEMP1 .GT. 0 .AND. TEMP1 .LE. 3) GOTO ( 12,  10,  9),   
     1         TEMP1                                                    
C END SWITCH.                                                           
  14     IF (I .NE. 0) GOTO 15                                          
C/6S                                                                    
C           CALL SETERR(37H GLSBP - IMPROPER BOUNDARY CONDITIONS, 37, 5,
C    1         1)                                                       
C/7S                                                                    
            CALL SETERR(' GLSBP - IMPROPER BOUNDARY CONDITIONS', 37, 5, 
     1         1)                                                       
C/                                                                      
            GOTO  16                                                    
  15     CONTINUE                                                       
         GOTO  8                                                        
C END WHILE.                                                            
  16  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE GLSBT(K, X, NX, NU, NRHS, NRHSG, GETJAC, A1, A2        
     1   , F1, AA, BB, GAM, BC, G, B)                                   
      INTEGER NRHS, NU, NX                                              
      INTEGER K, NRHSG, BC(NU, 2, 2)                                    
      REAL X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS, 2), AA(NU  
     1   , NU, 2), BB(NU, NU, 2)                                        
      REAL GAM(NU, NRHS, 2), G(1, 1), B(1)                              
      LOGICAL GETJAC                                                    
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IDF, IDG, ISTKGT, I, IDGI, ISBSIS                         
      INTEGER IS(1000)                                                  
      REAL RS(1000), WS(500)                                            
      LOGICAL LS(1000)                                                  
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C MNEMONIC - GALERKIN'S METHOD FOR LINEAR SYSTEMS,                      
C            BOUNDARY TERMS.                                            
C SCRATCH SPACE ALLOCATED -                                             
C       S( GLSBT) = NU*(4*NU+2*NRHS) + 2*K                              
C   REAL WORDS +                                                        
C       3*K                                                             
C   INTEGER WORDS.                                                      
C G(NU*(NX-K),2*K*NU-1),B(NU*(NX-K),NRHS).                              
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (K .LT. 2) CALL SETERR(15H GLSBT - K.LT.2, 15, 1, 2)           
C     IF (NX .LT. 2*K) CALL SETERR(18H GLSBT - NX.LT.2*K, 18, 2, 2)     
C     IF (NU .LT. 1) CALL SETERR(16H GLSBT - NU.LT.1, 16, 3, 2)         
C     IF (NRHS .LT. 1) CALL SETERR(18H GLSBT - NRHS.LT.1, 18, 4, 2)     
C     IF (NRHSG .LT. 0) CALL SETERR(19H GLSBT - NRHSG.LT.0, 19, 5, 2)   
C/7S                                                                    
      IF (K .LT. 2) CALL SETERR(' GLSBT - K.LT.2', 15, 1, 2)            
      IF (NX .LT. 2*K) CALL SETERR(' GLSBT - NX.LT.2*K', 18, 2, 2)      
      IF (NU .LT. 1) CALL SETERR(' GLSBT - NU.LT.1', 16, 3, 2)          
      IF (NRHS .LT. 1) CALL SETERR(' GLSBT - NRHS.LT.1', 18, 4, 2)      
      IF (NRHSG .LT. 0) CALL SETERR(' GLSBT - NRHSG.LT.0', 19, 5, 2)    
C/                                                                      
C CHECK THAT X IS MONOTONE INCREASING.                                  
      DO  1 I = 2, NX                                                   
C/6S                                                                    
C        IF (X(I-1) .GT. X(I)) CALL SETERR(                             
C    1      37H GLSBT - X IS NOT MONOTONE INCREASING, 37, 6, 2)         
C/7S                                                                    
         IF (X(I-1) .GT. X(I)) CALL SETERR(                             
     1      ' GLSBT - X IS NOT MONOTONE INCREASING', 37, 6, 2)          
C/                                                                      
         IF (I+K .GT. NX) GOTO  1                                       
         TEMP1 = I+K                                                    
C/6S                                                                    
C        IF (X(TEMP1) .LE. X(I)) CALL SETERR(                           
C    1      37H GLSBT - X IS NOT MONOTONE INCREASING, 37, 6, 2)         
C/7S                                                                    
         IF (X(TEMP1) .LE. X(I)) CALL SETERR(                           
     1      ' GLSBT - X IS NOT MONOTONE INCREASING', 37, 6, 2)          
C/                                                                      
   1     CONTINUE                                                       
C CHECK THE MULTIPLICITY OF THE END POINTS.                             
      DO  3 I = 1, K                                                    
         TEMP = X(I) .NE. X(1)                                          
         IF (TEMP) GOTO 2                                               
            TEMP1 = NX-K+I                                              
            TEMP = X(TEMP1) .NE. X(NX)                                  
C/6S                                                                    
C  2     IF (TEMP) CALL SETERR(                                         
C    1      46H GLSBT - END POINTS OF X NOT OF MULTIPLICITY K, 46, 7, 2)
C/7S                                                                    
   2     IF (TEMP) CALL SETERR(                                         
     1      ' GLSBT - END POINTS OF X NOT OF MULTIPLICITY K', 46, 7, 2) 
C/                                                                      
   3     CONTINUE                                                       
      CALL ENTER(1)                                                     
      IDG = ISTKGT(NU, 3)                                               
      IDGI = ISTKGT(NU, 3)                                              
      IDF = ISTKGT(NRHS, 3)                                             
C SPLINE BASIS AND DERIVATIVES AT L AND R.                              
      ISBSIS = ISTKGT(2*K, 3)                                           
      CALL A6LSBT(K, X, NX, NU, NRHS, NRHSG, GETJAC, A1, A2, F1, AA, BB,
     1   GAM, BC, G, B, NU*(NX-K), WS(IDG), WS(IDGI), WS(IDF), WS(      
     2   ISBSIS))                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION GLSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG,        
     1   MQ, XQ, WQ, GETJAC, SEPATE, G, GT, B, BT, ORDER, IGSSIS, SET)  
      INTEGER MQ, NU, NX                                                
      EXTERNAL AF, AF1                                                  
      INTEGER K, NRHS, NRHSG, ORDER(NU, NU, 2), IGSSIS                  
      REAL X(NX), XQ(MQ), WQ(MQ), G(1, 1), GT(1, 1), B(1, 1)            
      REAL BT(1, 1)                                                     
      LOGICAL GETJAC, SEPATE, SET                                       
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ITQ, ISTKGT, IA1T, IA2T, IA3T, IA4T                       
      INTEGER IF1T, IF2T, I, ISBSIS, IQ, IS(1000)                       
      INTEGER IA1, IA2, IA3, IA4, IF1, IF2                              
      INTEGER IQ0, IZERO                                                
      REAL RS(1000), WS(500)                                            
      LOGICAL FAILED, LS(1000), A6LSIN                                  
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C MNEMONIC - GALERKIN'S METHOD FOR LINEAR SYSTEMS,                      
C            INTEGRALS.                                                 
C SCRATCH SPACE ALLOCATED -                                             
C       S( GLSIN) = MQ*( 2*K+1 + 8*NU**2 + 2*NU*NRHS ) +                
C                   MAX( IF ( SET ) { 0 } ELSE { 3*K }, S(AF) )         
C   REAL WORDS +                                                        
C                   NU                                                  
C   LOGICAL WORDS.                                                      
C REAL G((NX-K)*NU,2*K*NU-1),B((NX-K)*NU,NRHS),BT((NX-K)*NU,NRHS)       
C CHECK THE DATA FOR ERRORS.                                            
C/6S                                                                    
C     IF (K .LT. 2) CALL SETERR(15H GLSIN - K.LT.2, 15, 1, 2)           
C     IF (NX .LT. 2*K) CALL SETERR(18H GLSIN - NX.LT.2*K, 18, 2, 2)     
C     IF (NU .LT. 1) CALL SETERR(16H GLSIN - NU.LT.1, 16, 3, 2)         
C     IF (NRHS .LT. 1) CALL SETERR(18H GLSIN - NRHS.LT.1, 18, 4, 2)     
C     IF (NRHSG .LT. 0) CALL SETERR(19H GLSIN - NRHSG.LT.0, 19, 5, 2)   
C     IF (MQ .LT. K-1) CALL SETERR(18H GLSIN - MQ.LT.K-1, 18, 6, 2)     
C     IF (XQ(1) .LT. (-1.)) CALL SETERR(20H GLSIN - XQ(1).LT.-1, 20, 7  
C    1   , 2)                                                           
C/7S                                                                    
      IF (K .LT. 2) CALL SETERR(' GLSIN - K.LT.2', 15, 1, 2)            
      IF (NX .LT. 2*K) CALL SETERR(' GLSIN - NX.LT.2*K', 18, 2, 2)      
      IF (NU .LT. 1) CALL SETERR(' GLSIN - NU.LT.1', 16, 3, 2)          
      IF (NRHS .LT. 1) CALL SETERR(' GLSIN - NRHS.LT.1', 18, 4, 2)      
      IF (NRHSG .LT. 0) CALL SETERR(' GLSIN - NRHSG.LT.0', 19, 5, 2)    
      IF (MQ .LT. K-1) CALL SETERR(' GLSIN - MQ.LT.K-1', 18, 6, 2)      
      IF (XQ(1) .LT. (-1.)) CALL SETERR(' GLSIN - XQ(1).LT.-1', 20, 7   
     1   , 2)                                                           
C/                                                                      
      IF (WQ(1) .EQ. 0.) GOTO 1                                         
         IQ0 = 1                                                        
         GOTO  2                                                        
   1     IQ0 = MQ+1                                                     
   2  IQ = 2                                                            
         GOTO  4                                                        
   3     IQ = IQ+1                                                      
   4     IF (IQ .GT. MQ) GOTO  5                                        
C CHECK XQ FOR MONOTONICITY                                             
C AND WQ BEING 0.                                                       
C/6S                                                                    
C        IF (XQ(IQ-1) .GE. XQ(IQ)) CALL SETERR(                         
C    1      44H GLSIN - XQ NOT STRICTLY MONOTONE INCREASING, 44, 8, 2)  
C/7S                                                                    
         IF (XQ(IQ-1) .GE. XQ(IQ)) CALL SETERR(                         
     1      ' GLSIN - XQ NOT STRICTLY MONOTONE INCREASING', 44, 8, 2)   
C/                                                                      
         IF (WQ(IQ) .NE. 0.) IQ0 = IQ                                   
         GOTO  3                                                        
C/6S                                                                    
C  5  IF (IQ0 .GT. MQ) CALL SETERR(31H GLSIN - WQ IS IDENTICALLY ZERO,  
C    1   31, 9, 2)                                                      
C     IF (XQ(MQ) .GT. 1.) CALL SETERR(20H GLSIN - XQ(MQ).GT.1, 20, 10, 2
C    1   )                                                              
C/7S                                                                    
   5  IF (IQ0 .GT. MQ) CALL SETERR(' GLSIN - WQ IS IDENTICALLY ZERO',   
     1   31, 9, 2)                                                      
      IF (XQ(MQ) .GT. 1.) CALL SETERR(' GLSIN - XQ(MQ).GT.1', 20, 10, 2 
     1   )                                                              
C/                                                                      
      DO  7 I = 1, K                                                    
         TEMP = X(I) .NE. X(1)                                          
         IF (TEMP) GOTO 6                                               
            TEMP1 = NX-K+I                                              
            TEMP = X(TEMP1) .NE. X(NX)                                  
C/6S                                                                    
C  6     IF (TEMP) CALL SETERR(                                         
C    1      46H GLSIN - END POINTS OF X NOT OF MULTIPLICITY K, 46, 11, 2
C    2      )                                                           
C/7S                                                                    
   6     IF (TEMP) CALL SETERR(                                         
     1      ' GLSIN - END POINTS OF X NOT OF MULTIPLICITY K', 46, 11, 2 
     2      )                                                           
C/                                                                      
   7     CONTINUE                                                       
C/6S                                                                    
C     IF (X(1) .GE. X(NX)) CALL SETERR(                                 
C    1   34H GLSIN - X NOT MONOTONE INCREASING, 34, 12, 2)              
C/7S                                                                    
      IF (X(1) .GE. X(NX)) CALL SETERR(                                 
     1   ' GLSIN - X NOT MONOTONE INCREASING', 34, 12, 2)               
C/                                                                      
      TEMP = SET                                                        
      IF (TEMP) TEMP = IGSSIS .LE. 1                                    
C/6S                                                                    
C     IF (TEMP) CALL SETERR(34H GLSIN - SET=.T. AND IGSBASIS.LE.1, 34,  
C    1   13, 2)                                                         
C/7S                                                                    
      IF (TEMP) CALL SETERR(' GLSIN - SET=.T. AND IGSBASIS.LE.1', 34,   
     1   13, 2)                                                         
C/                                                                      
      CALL ENTER(1)                                                     
      ITQ = ISTKGT(MQ, 3)                                               
      ISBSIS = ISTKGT(2*K*MQ, 3)                                        
      IA1 = ISTKGT(4*MQ*NU*(2*NU+NRHS), 3)                              
      IA1T = IA1+MQ*NU**2                                               
      IA2 = IA1T+MQ*NU**2                                               
      IA2T = IA2+MQ*NU**2                                               
      IA3 = IA2T+MQ*NU**2                                               
      IA3T = IA3+MQ*NU**2                                               
      IA4 = IA3T+MQ*NU**2                                               
      IA4T = IA4+MQ*NU**2                                               
      IF1 = IA4T+MQ*NU**2                                               
      IF1T = IF1+MQ*NU*NRHS                                             
      IF2 = IF1T+MQ*NU*NRHS                                             
      IF2T = IF2+MQ*NU*NRHS                                             
      IZERO = ISTKGT(NU, 1)                                             
      FAILED = A6LSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG, MQ, XQ, WQ,   
     1   GETJAC, SEPATE, G, GT, B, BT, ORDER, IGSSIS, SET, WS(ITQ), WS( 
     2   ISBSIS), WS(IA1), WS(IA1T), WS(IA2), WS(IA2T), WS(IA3), WS(    
     3   IA3T), WS(IA4), WS(IA4T), WS(IF1), WS(IF1T), WS(IF2), WS(IF2T),
     4   LS(IZERO), WS(IGSSIS), (NX-K)*NU, 2*K*NU-1, 2*MQ*K)            
      CALL LEAVE                                                        
      GLSIN = FAILED                                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE GLSSB(ALFA, BETA, GAMMA, NU, NRHS, AA, BB, CC,         
     1   SGAMAD, SGAMAM, BC)                                            
      INTEGER NRHS, NU                                                  
      INTEGER BC(NU, 2)                                                 
      REAL ALFA(NU, NU), BETA(NU, NU), GAMMA(NU, NRHS), AA(NU, NU), BB( 
     1   NU, NU), CC(NU, NU)                                            
      REAL SGAMAD(NU, NRHS), SGAMAM(NU, NRHS)                           
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IED, IEM, ISTKGT, NERROR, IPIVOT, I                       
      INTEGER J, NERR, IA, IB, IC, IG                                   
      INTEGER IDIAG, IS(1000), NDEQS, NMEQS                             
      REAL RS(1000), WS(500)                                            
      LOGICAL ALLERO, ALFERO, LS(1000)                                  
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO CONVERT GENERAL LINEAR BC'S INTO STANDARD BC'S.                    
C MNEMONIC - GALERKIN'S METHOD FOR LINEAR SYSTEMS,                      
C            STANDARD BOUNDARY CONDITIONS.                              
C SCRATCH SPACE ALLOCATED -                                             
C       S( GLSSB) <= 4*NU                                               
C   INTEGER WORDS +                                                     
C       NU*(2*NU+NRHS+2)                                                
C   REAL WORDS.                                                         
C/6S                                                                    
C     IF (NU .LT. 1) CALL SETERR(16H GLSSB - NU.LT.1, 16, 1, 2)         
C     IF (NRHS .LT. 1) CALL SETERR(18H GLSSB - NRHS.LT.1, 18, 2, 2)     
C/7S                                                                    
      IF (NU .LT. 1) CALL SETERR(' GLSSB - NU.LT.1', 16, 1, 2)          
      IF (NRHS .LT. 1) CALL SETERR(' GLSSB - NRHS.LT.1', 18, 2, 2)      
C/                                                                      
      CALL ENTER(1)                                                     
C THE DEFAULT IS NO BOUNDARY CONDITIONS AT ALL.                         
      CALL SETR(NU**2, 0E0, AA)                                         
      CALL SETR(NU**2, 0E0, BB)                                         
      CALL SETR(NU**2, 0E0, CC)                                         
      CALL SETR(NU*NRHS, 0E0, SGAMAD)                                   
      CALL SETR(NU*NRHS, 0E0, SGAMAM)                                   
      CALL SETI(2*NU, -2, BC)                                           
C SPACE FOR THE DIRICHLET EQUATION NUMBERS.                             
      IED = ISTKGT(NU, 2)                                               
C SPACE FOR THE MIXED EQUATION NUMBERS.                                 
      IEM = ISTKGT(NU, 2)                                               
      NDEQS = 0                                                         
      NMEQS = 0                                                         
C FIND THE EQUATIONS FOR EACH.                                          
      DO  5 I = 1, NU                                                   
         ALLERO = .TRUE.                                                
         DO  3 J = 1, NU                                                
            TEMP = ALFA(I, J) .EQ. 0.                                   
            IF (TEMP) TEMP = BETA(I, J) .EQ. 0.                         
            ALFERO = TEMP                                               
            ALLERO = ALLERO .AND. ALFERO                                
            IF (BETA(I, J) .EQ. 0.) GOTO 1                              
               NMEQS = NMEQS+1                                          
C THEN MIXED.                                                           
               TEMP1 = IEM-1+NMEQS                                      
               IS(TEMP1) = I                                            
               GOTO  4                                                  
   1        TEMP = J .EQ. NU                                            
            IF (TEMP) TEMP = .NOT. ALLERO                               
            IF (.NOT. TEMP) GOTO 2                                      
               NDEQS = NDEQS+1                                          
C THEN DIRICHLET.                                                       
               TEMP1 = IED-1+NDEQS                                      
               IS(TEMP1) = I                                            
   2        CONTINUE                                                    
   3        CONTINUE                                                    
C END J.                                                                
   4     CONTINUE                                                       
   5     CONTINUE                                                       
C END I.                                                                
      IF (NMEQS .LE. 0) GOTO 7                                          
         IA = ISTKGT(NMEQS*NU, 3)                                       
C CONVERT THE MIXED BC'S.                                               
         IB = ISTKGT(NMEQS*NU, 3)                                       
         IG = ISTKGT(NMEQS*NRHS, 3)                                     
         IPIVOT = ISTKGT(NU, 2)                                         
         IDIAG = ISTKGT(NU, 3)                                          
         CALL A6LSSM(ALFA, BETA, GAMMA, NU, NRHS, AA, BB, SGAMAM, BC(1  
     1      , 2), WS(IA), WS(IB), WS(IG), IS(IEM), NMEQS, IS(IPIVOT),   
     2      WS(IDIAG))                                                  
         CALL ISTKRL(5)                                                 
         IF (NERROR(NERR) .EQ. 0) GOTO 6                                
            CALL LEAVE                                                  
            RETURN                                                      
   6     CONTINUE                                                       
   7  IF (NDEQS .LE. 0) GOTO 8                                          
         IC = ISTKGT(NDEQS*NU, 3)                                       
C CONVERT THE DIRICHLET EQUATIONS.                                      
         IG = ISTKGT(NDEQS*NRHS, 3)                                     
         IPIVOT = ISTKGT(NU, 2)                                         
         IDIAG = ISTKGT(NU, 3)                                          
         CALL A6LSSD(ALFA, GAMMA, NU, NRHS, CC, SGAMAD, BC(1, 1), WS(IC)
     1      , WS(IG), IS(IED), NDEQS, IS(IPIVOT), WS(IDIAG))            
   8  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE MMPY(A, MA, NA, B, NB, C)                              
      INTEGER MA, NA, NB                                                
      REAL A(MA, NA), B(NA, NB), C(MA, NB)                              
      INTEGER I, J, K                                                   
      REAL T                                                            
      LOGICAL TEMP                                                      
C  TO MULTIPLY THE MATRICES A*B AND PUT THE RESULT IN C.                
C  INPUT -                                                              
C    A  - THE MATRIX A                                                  
C    MA - IS MA BY                                                      
C    NA - NA.                                                           
C    B  - THE MATRIX B                                                  
C    NB - IS NA BY NB.                                                  
C  OUTPUT -                                                             
C    C - C=A*B IS MA BY NB.                                             
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C  ERROR STATES -                                                       
C    1 - BAD DIMENSIONS FOR A AND/OR B.                                 
      TEMP = MA .LT. 1                                                  
      IF (.NOT. TEMP) TEMP = NA .LT. 1                                  
      IF (.NOT. TEMP) TEMP = NB .LT. 1                                  
C/6S                                                                    
C     IF (TEMP) CALL SETERR(37H MMPY - BAD DIMENSIONS FOR A AND/OR B,   
C    1   37, 1, 2)                                                      
C/7S                                                                    
      IF (TEMP) CALL SETERR(' MMPY - BAD DIMENSIONS FOR A AND/OR B',    
     1   37, 1, 2)                                                      
C/                                                                      
      DO  3 I = 1, MA                                                   
         DO  2 J = 1, NB                                                
            T = 0                                                       
            DO  1 K = 1, NA                                             
               T = T+A(I, K)*B(K, J)                                    
   1           CONTINUE                                                 
            C(I, J) = T                                                 
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A6LSBC(NU, ORDER, BC, E, MAXORD, COUNT)          
      INTEGER NU                                                        
      INTEGER ORDER(NU, NU, 2), BC(NU, 2, 2), E(NU, 2, 2), MAXORD(NU, 2)
     1   , COUNT(NU, 2)                                                 
      INTEGER MAX0, I, J, L                                             
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      CALL SETI(2*NU, -1, MAXORD)                                       
      CALL SETI(2*NU, 0, COUNT)                                         
      DO  3 L = 1, 2                                                    
         DO  2 I = 1, NU                                                
            DO  1 J = 1, NU                                             
               MAXORD(I, L) = MAX0(MAXORD(I, L), ORDER(I, J, L))        
   1           CONTINUE                                                 
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      DO  9 L = 1, 2                                                    
         DO  8 I = 1, NU                                                
            IF (BC(I, 1, L) .NE. 0) GOTO 5                              
               TEMP1 = E(I, 1, L)                                       
               COUNT(TEMP1, L) = COUNT(TEMP1, L)+1                      
               TEMP1 = E(I, 1, L)                                       
               IF (MAXORD(TEMP1, L) .GT. BC(I, 1, L)) GOTO 4            
                  A6LSBC = .FALSE.                                      
                  RETURN                                                
   4           CONTINUE                                                 
   5        IF (BC(I, 2, L) .NE. 1) GOTO 7                              
               TEMP1 = E(I, 2, L)                                       
               COUNT(TEMP1, L) = COUNT(TEMP1, L)+1                      
               TEMP1 = E(I, 2, L)                                       
               IF (ORDER(TEMP1, I, L) .GT. BC(I, 2, L)) GOTO 6          
                  A6LSBC = .FALSE.                                      
                  RETURN                                                
   6           CONTINUE                                                 
   7        CONTINUE                                                    
   8        CONTINUE                                                    
   9     CONTINUE                                                       
      DO  12 I = 1, NU                                                  
         TEMP = COUNT(I, 1) .GT. 1                                      
         IF (.NOT. TEMP) TEMP = COUNT(I, 2) .GT. 1                      
         IF (.NOT. TEMP) GOTO 10                                        
            A6LSBC = .FALSE.                                            
            RETURN                                                      
  10     IF (COUNT(I, 1)+COUNT(I, 2) .LE. MAX0(MAXORD(I, 1), MAXORD(I, 2
     1      ))) GOTO 11                                                 
            A6LSBC = .FALSE.                                            
            RETURN                                                      
  11     CONTINUE                                                       
  12     CONTINUE                                                       
      A6LSBC = .TRUE.                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE A6LSBD(K, NX, NU, NRHS, NRHSG, BC, E, CC, GAM,         
     1   GETJAC, G, B, GDIM1, GDIM2)                                    
      INTEGER NRHS, GDIM1, GDIM2, NU                                    
      INTEGER K, NX, NRHSG, BC(NU, 2, 2), E(NU, 2, 2)                   
      REAL CC(NU, NU, 2), GAM(NU, NRHS, 2), G(GDIM1, GDIM2), B(GDIM1,   
     1   NRHS)                                                          
      LOGICAL GETJAC                                                    
      INTEGER JLR, I, J, L, GIDX1                                       
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
      DO  7 JLR = 1, 2                                                  
         DO  6 I = 1, NU                                                
            TEMP1 = BC(I, 1, JLR) .NE. (-2)                             
            IF (TEMP1) TEMP1 = BC(I, 1, JLR) .NE. 0                     
C/6S                                                                    
C           IF (TEMP1) CALL SETERR(                                     
C    1         36H GLSBD - BC(I,1,JLR) NOT ONE OF -2,0, 36, 6, 2)       
C/7S                                                                    
            IF (TEMP1) CALL SETERR(                                     
     1         ' GLSBD - BC(I,1,JLR) NOT ONE OF -2,0', 36, 6, 2)        
C/                                                                      
            TEMP1 = BC(I, 2, JLR) .NE. (-2)                             
            IF (TEMP1) TEMP1 = BC(I, 2, JLR) .NE. 1                     
C/6S                                                                    
C           IF (TEMP1) CALL SETERR(                                     
C    1         36H GLSBD - BC(I,2,JLR) NOT ONE OF -2,1, 36, 7, 2)       
C/7S                                                                    
            IF (TEMP1) CALL SETERR(                                     
     1         ' GLSBD - BC(I,2,JLR) NOT ONE OF -2,1', 36, 7, 2)        
C/                                                                      
            IF (BC(I, 1, JLR) .NE. 0) GOTO  6                           
C ONLY DO DIRICHLET B.C.'S.                                             
            TEMP1 = E(I, 1, JLR) .LT. 1                                 
            IF (.NOT. TEMP1) TEMP1 = E(I, 1, JLR) .GT. NU               
C/6S                                                                    
C           IF (TEMP1) CALL SETERR(                                     
C    1         39H GLSBD - E(I,1,JLR) NOT ONE OF 1,...,NU, 39, 8, 2)    
C/7S                                                                    
            IF (TEMP1) CALL SETERR(                                     
     1         ' GLSBD - E(I,1,JLR) NOT ONE OF 1,...,NU', 39, 8, 2)     
C/                                                                      
            GIDX1 = E(I, 1, JLR)+(JLR-1)*(NX-K-1)*NU                    
            L = 1                                                       
               GOTO  2                                                  
   1           L = L+1                                                  
   2           IF (L .GT. NRHSG) GOTO  3                                
               B(GIDX1, L) = GAM(I, L, JLR)                             
               GOTO  1                                                  
   3        IF (.NOT. GETJAC) GOTO  6                                   
            DO  4 J = 1, GDIM2                                          
               G(GIDX1, J) = 0                                          
   4           CONTINUE                                                 
            DO  5 J = 1, NU                                             
               TEMP1 = CC(I, J, JLR) .NE. 0.                            
               IF (TEMP1) TEMP1 = BC(J, 1, JLR) .EQ. 0                  
C/6S                                                                    
C              IF (TEMP1) CALL SETERR(                                  
C    1        55H GLSBD - CC(I,J,JLR).NE.0 AND BC(I,1,JLR)=0=BC(J,1,JLR)
C    2            , 55, 9, 2)                                           
C/7S                                                                    
               IF (TEMP1) CALL SETERR(                                  
     1        ' GLSBD - CC(I,J,JLR).NE.0 AND BC(I,1,JLR)=0=BC(J,1,JLR)' 
     2            , 55, 9, 2)                                           
C/                                                                      
               TEMP = J-E(I, 1, JLR)+K*NU                               
               G(GIDX1, TEMP) = -CC(I, J, JLR)                          
   5           CONTINUE                                                 
            TEMP = I-E(I, 1, JLR)+K*NU                                  
            G(GIDX1, TEMP) = 1                                          
   6        CONTINUE                                                    
   7     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A6LSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG        
     1   , GETJAC, SEPATE, A1, A2, F1, A1T, A2T, F1T, AS, FS, SBASIS)   
      INTEGER K, NRHS, NU, NX                                           
      EXTERNAL AF, AF1                                                  
      INTEGER NRHSG                                                     
      REAL X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS, 2), A1T(NU,
     1   NU, 2), A2T(NU, NU, 2)                                         
      REAL F1T(NU, NRHS, 2), AS(NU, NU, 8), FS(NU, NRHS, 4), SBASIS(K, 2
     1   )                                                              
      LOGICAL AF, GETJAC, SEPATE                                        
      INTEGER JLR, ID(2)                                                
      REAL LR(2)                                                        
      LOGICAL TEMP                                                      
      DATA ID(1)/0/                                                     
      DATA ID(2)/1/                                                     
      LR(1) = X(1)                                                      
      LR(2) = X(NX)                                                     
C DO L AND R TERMS.                                                     
      DO  4 JLR = 1, 2                                                  
C   GET THE BASIS SPLINES AT L OR R.                                    
         CALL BSPL1(K, X, NX, LR(JLR), 1, K+(JLR-1)*(NX-2*K), ID, 2,    
     1      SBASIS)                                                     
C DEFAULT A AND F VALUES.                                               
         CALL SETR(4*NU*(2*NU+NRHS), 0E0, AS)                           
         IF (.NOT. AF(LR(JLR), 1, NU, NRHS, NRHSG, GETJAC, SEPATE, AS(1,
     1      1, 1), AS(1, 1, 2), AS(1, 1, 3), AS(1, 1, 4), AS(1, 1, 5),  
     2      AS(1, 1, 6), AS(1, 1, 7), AS(1, 1, 8), FS(1, 1, 1), FS(1, 1,
     3      2), FS(1, 1, 3), FS(1, 1, 4), K+(JLR-1)*(NX-2*K), SBASIS, K,
     4      X, NX, AF1)) GOTO 1                                         
            A6LSBI = .TRUE.                                             
            RETURN                                                      
   1     IF (.NOT. GETJAC) GOTO 3                                       
            CALL MOVEFR(NU**2, AS(1, 1, 1), A1(1, 1, JLR))              
            CALL MOVEFR(NU**2, AS(1, 1, 3), A2(1, 1, JLR))              
            IF (.NOT. SEPATE) GOTO 2                                    
               CALL MOVEFR(NU**2, AS(1, 1, 2), A1T(1, 1, JLR))          
               CALL MOVEFR(NU**2, AS(1, 1, 4), A2T(1, 1, JLR))          
   2        CONTINUE                                                    
   3     IF (NRHSG .GT. 0) CALL MOVEFR(NU*NRHSG, FS(1, 1, 1), F1(1, 1,  
     1      JLR))                                                       
         TEMP = SEPATE                                                  
         IF (TEMP) TEMP = NRHSG .GT. 0                                  
         IF (TEMP) CALL MOVEFR(NU*NRHSG, FS(1, 1, 2), F1T(1, 1, JLR))   
   4     CONTINUE                                                       
C END JLR.                                                              
      A6LSBI = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE A6LSBP(I, NU, ORDER, BC, E, MAXORD, CE, R, N)          
      INTEGER NU                                                        
      INTEGER I, ORDER(NU, NU, 2), BC(1), E(1), MAXORD(NU, 2), CE(NU)   
      INTEGER R(NU), N                                                  
      INTEGER MOD, MAX0, J, L, NBCS, DM                                 
      INTEGER II, LR                                                    
      INTEGER TEMP2                                                     
      LOGICAL TEMP, TEMP1                                               
C BC(NU,2,2),E(NU,2,2),                                                 
C E(I-1),R(N).                                                          
      IF (BC(I) .GE. 0) GOTO 1                                          
         N = 1                                                          
         R(N) = 0                                                       
         RETURN                                                         
C LR = 1 FOR LEFT, LR = 2 FOR RIGHT.                                    
   1  LR = (I-1)/(2*NU)+1                                               
C DM = 1 FOR DIRICHLET, DM = 2 FOR MIXED BOUNDARY CONDITIONS.           
      DM = MOD((I-1)/NU, 2)+1                                           
      II = MOD(I, NU)                                                   
      IF (II .EQ. 0) II = NU                                            
C B(I) = B(II,DM,LR).                                                   
      N = 0                                                             
      DO  2 J = 1, NU                                                   
         CE(J) = J                                                      
   2     CONTINUE                                                       
C CE = COMPLEMENT OF E.                                                 
      IF (I .GT. 2*NU) GOTO 7                                           
         J = 1                                                          
            GOTO  4                                                     
   3        J = J+1                                                     
   4        IF (J .GE. I) GOTO  6                                       
            IF (BC(J) .LT. 0) GOTO 5                                    
               TEMP2 = E(J)                                             
               CE(TEMP2) = 0                                            
   5        CONTINUE                                                    
            GOTO  3                                                     
   6     CONTINUE                                                       
         GOTO  12                                                       
   7     J = 2*NU+1                                                     
            GOTO  9                                                     
   8        J = J+1                                                     
   9        IF (J .GE. I) GOTO  11                                      
            IF (BC(J) .LT. 0) GOTO 10                                   
               TEMP2 = E(J)                                             
               CE(TEMP2) = 0                                            
  10        CONTINUE                                                    
            GOTO  8                                                     
  11     CONTINUE                                                       
  12  DO  19 J = 1, NU                                                  
         IF (CE(J) .EQ. 0) GOTO  19                                     
         NBCS = 0                                                       
         L = 1                                                          
            GOTO  14                                                    
  13        L = L+1                                                     
  14        IF (L .GE. I) GOTO  15                                      
            TEMP = E(L) .EQ. J                                          
            IF (TEMP) TEMP = BC(L) .GE. 0                               
            IF (TEMP) NBCS = NBCS+1                                     
            GOTO  13                                                    
  15     TEMP = DM .EQ. 1                                               
         IF (TEMP) TEMP = MAXORD(J, LR) .GT. BC(I)                      
         IF (TEMP) GOTO 16                                              
            TEMP1 = DM .EQ. 2                                           
            IF (TEMP1) TEMP1 = ORDER(J, II, LR) .GT. BC(I)              
            TEMP = TEMP1                                                
  16     IF (.NOT. TEMP) GOTO 18                                        
            IF (NBCS .GE. MAX0(MAXORD(J, 1), MAXORD(J, 2))) GOTO 17     
               N = N+1                                                  
               R(N) = J                                                 
  17        CONTINUE                                                    
  18     CONTINUE                                                       
  19     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE A6LSBT(K, X, NX, NU, NRHS, NRHSG, GETJAC, A1, A2,      
     1   F1, AA, BB, GAM, BC, G, B, GDIM, DG, DGI, DF, SBASIS)          
      INTEGER K, GDIM, NRHS, NU, NX                                     
      INTEGER NRHSG, BC(NU, 2, 2)                                       
      REAL X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS, 2), AA(NU  
     1   , NU, 2), BB(NU, NU, 2)                                        
      REAL GAM(NU, NRHS, 2), G(GDIM, 1), B(GDIM, NRHS), DG(NU), DGI(NU),
     1   DF(NRHS)                                                       
      REAL SBASIS(K, 2)                                                 
      LOGICAL GETJAC                                                    
      INTEGER JLR, I, J, L, GIDX, ISINLR                                
      INTEGER ID(2)                                                     
      REAL BBP, BIP, SIGNLR                                             
      INTEGER TEMP                                                      
      LOGICAL TEMP1, TEMP2                                              
      DATA ID(1)/0/                                                     
      DATA ID(2)/1/                                                     
C DO L AND R TERMS.                                                     
      DO  18 JLR = 1, 2                                                 
         ISINLR = (-1)**(JLR+1)                                         
         SIGNLR = ISINLR                                                
C   GET THE BASIS SPLINES AT L OR R.                                    
         TEMP = (JLR-1)*(NX-1)                                          
         CALL BSPL1(K, X, NX, X(TEMP+1), 1, K+(JLR-1)*(NX-2*K), ID, 2,  
     1      SBASIS)                                                     
         TEMP = (JLR-1)*(K-1)                                           
         BBP = SBASIS(TEMP+1, 2)                                        
         TEMP = (JLR-1)*(K-3)                                           
         BIP = SBASIS(TEMP+2, 2)                                        
         DO  17 I = 1, NU                                               
            GIDX = I+(JLR-1)*(NX-K-1)*NU                                
            CALL SETR(NU, 0E0, DG)                                      
            CALL SETR(NU, 0E0, DGI)                                     
            IF (NRHSG .GT. 0) CALL SETR(NRHSG, 0E0, DF)                 
            DO  11 J = 1, NU                                            
C NOT MIXED.                                                            
               TEMP1 = BC(J, 1, JLR) .EQ. 0                             
               IF (TEMP1) GOTO 1                                        
                  TEMP2 = BC(J, 1, JLR) .EQ. (-2)                       
                  IF (TEMP2) TEMP2 = BC(J, 2, JLR) .EQ. (-2)            
                  TEMP1 = TEMP2                                         
   1           IF (.NOT. TEMP1) GOTO 3                                  
                  IF (.NOT. GETJAC) GOTO 2                              
                     DG(J) = DG(J)+SIGNLR*(A1(I, J, JLR)*BBP+A2(I, J,   
     1                  JLR))                                           
                     DGI(J) = DGI(J)+SIGNLR*A1(I, J, JLR)*BIP           
   2              CONTINUE                                              
   3           IF (BC(J, 2, JLR) .NE. 1) GOTO 9                         
                  IF (.NOT. GETJAC) GOTO 5                              
                     DO  4 L = 1, NU                                    
C MIXED.                                                                
                        TEMP1 = BC(L, 2, JLR) .EQ. 1                    
                        IF (TEMP1) TEMP1 = BB(J, L, JLR) .NE. 0.        
C/6S                                                                    
C                       IF (TEMP1) CALL SETERR(                         
C    1       56H GLSBT - BC(L,2,JLR)=1=BC(J,2,JLR) WHEN BB(J,L,JLR).NE.0
C    2                     , 56, 8, 2)                                  
C/7S                                                                    
                        IF (TEMP1) CALL SETERR(                         
     1       ' GLSBT - BC(L,2,JLR)=1=BC(J,2,JLR) WHEN BB(J,L,JLR).NE.0' 
     2                     , 56, 8, 2)                                  
C/                                                                      
                        DG(L) = DG(L)+SIGNLR*A1(I, J, JLR)*(AA(J, L,    
     1                     JLR)+BB(J, L, JLR)*BBP)                      
                        DGI(L) = DGI(L)+SIGNLR*A1(I, J, JLR)*BB(J, L,   
     1                     JLR)*BIP                                     
   4                    CONTINUE                                        
                     DG(J) = DG(J)+SIGNLR*A2(I, J, JLR)                 
   5              L = 1                                                 
                     GOTO  7                                            
   6                 L = L+1                                            
   7                 IF (L .GT. NRHSG) GOTO  8                          
                     DF(L) = DF(L)-SIGNLR*A1(I, J, JLR)*GAM(J, L, JLR)  
                     GOTO  6                                            
   8              CONTINUE                                              
C BAD BC.                                                               
   9           TEMP1 = BC(J, 1, JLR) .NE. (-2)                          
               IF (TEMP1) TEMP1 = BC(J, 1, JLR) .NE. 0                  
               IF (TEMP1) GOTO 10                                       
                  TEMP2 = BC(J, 2, JLR) .NE. (-2)                       
                  IF (TEMP2) TEMP2 = BC(J, 2, JLR) .NE. 1               
                  TEMP1 = TEMP2                                         
C/6S                                                                    
C 10           IF (TEMP1) CALL SETERR(                                  
C    1            38H GLSBT - BC(J,.,JLR) NOT ONE OF -2,0,1, 38, 9, 2)  
C/7S                                                                    
  10           IF (TEMP1) CALL SETERR(                                  
     1            ' GLSBT - BC(J,.,JLR) NOT ONE OF -2,0,1', 38, 9, 2)   
C/                                                                      
  11           CONTINUE                                                 
C END J.                                                                
            IF (.NOT. GETJAC) GOTO 13                                   
               DO  12 J = 1, NU                                         
                  TEMP = J-I+K*NU                                       
                  G(GIDX, TEMP) = G(GIDX, TEMP)+DG(J)                   
                  TEMP = J-I+(K+ISINLR)*NU                              
                  G(GIDX, TEMP) = G(GIDX, TEMP)+DGI(J)                  
  12              CONTINUE                                              
  13        L = 1                                                       
               GOTO  15                                                 
  14           L = L+1                                                  
  15           IF (L .GT. NRHSG) GOTO  16                               
               B(GIDX, L) = B(GIDX, L)+DF(L)+SIGNLR*F1(I, L, JLR)       
               GOTO  14                                                 
  16        CONTINUE                                                    
  17        CONTINUE                                                    
C END I.                                                                
  18     CONTINUE                                                       
C END JLR.                                                              
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION A6LSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG        
     1   , MQ, XQ, WQ, GETJAC, SEPATE, G, GT, B, BT, ORDER, IGSSIS, SET,
     2   TQ, SBASIS, A1, A1T, A2, A2T, A3, A3T, A4, A4T, F1, F1T, F2,   
     3   F2T, ZERO, GSBSIS, GDIM1, GDIM2, GSDIM1)                       
      INTEGER K, NRHS, GDIM1, GDIM2, MQ, NU                             
      INTEGER NX, GSDIM1                                                
      EXTERNAL AF, AF1                                                  
      INTEGER NRHSG, ORDER(NU, NU, 2), IGSSIS                           
      REAL X(NX), XQ(MQ), WQ(MQ), G(GDIM1, GDIM2), GT(GDIM1, GDIM2), B( 
     1   GDIM1, NRHS)                                                   
      REAL BT(GDIM1, NRHS), TQ(MQ), SBASIS(MQ, K, 2), A1(MQ, NU, NU),   
     1   A1T(MQ, NU, NU), A2(MQ, NU, NU)                                
      REAL A2T(MQ, NU, NU), A3(MQ, NU, NU), A3T(MQ, NU, NU), A4(MQ, NU  
     1   , NU), A4T(MQ, NU, NU), F1(MQ, NU, NRHS)                       
      REAL F1T(MQ, NU, NRHS), F2(MQ, NU, NRHS), F2T(MQ, NU, NRHS),      
     1   GSBSIS(GSDIM1, 1)                                              
      LOGICAL AF, GETJAC, SEPATE, SET, ZERO(NU)                         
      INTEGER ILR, INT, MAX0, I, J, P                                   
      INTEGER Q, JRHS, GIDX1, GIDX2, ERRATE, ID(2)                      
      INTEGER IQ, NSPLN                                                 
      REAL T, SCALE, TT                                                 
      LOGICAL A12ERO, A40, A1230, A4ZERO                                
      INTEGER TEMP4                                                     
      LOGICAL TEMP, TEMP1, TEMP2, TEMP3                                 
      DATA ID(1)/0/                                                     
      DATA ID(2)/1/                                                     
      ERRATE = 0                                                        
      NSPLN = NX-K                                                      
      IF (GETJAC) CALL SETR(GDIM1*GDIM2, 0E0, G)                        
C INITIALIZE G.                                                         
      TEMP = GETJAC                                                     
      IF (TEMP) TEMP = SEPATE                                           
      IF (TEMP) CALL SETR(GDIM1*GDIM2, 0E0, GT)                         
C GT TOO.                                                               
      IF (NRHSG .GT. 0) CALL SETR(GDIM1*NRHSG, 0E0, B)                  
C INITIALIZE B.                                                         
      TEMP = SEPATE                                                     
      IF (TEMP) TEMP = NRHSG .GT. 0                                     
      IF (TEMP) CALL SETR(GDIM1*NRHSG, 0E0, BT)                         
C INITIALIZE BT.                                                        
      IF (GETJAC) CALL SETI(2*NU**2, -1, ORDER)                         
C INITIALIZE ORDER.                                                     
C DO INTEGRALS OVER EACH MESH INTERVAL.                                 
      DO  33 INT = K, NSPLN                                             
C/6S                                                                    
C        IF (X(INT) .GT. X(INT+1)) CALL SETERR(                         
C    1      37H GLSIN - X IS NOT MONOTONE INCREASING, 37, 12, 2)        
C/7S                                                                    
         IF (X(INT) .GT. X(INT+1)) CALL SETERR(                         
     1      ' GLSIN - X IS NOT MONOTONE INCREASING', 37, 12, 2)         
C/                                                                      
         IF (INT+K .GT. NX) GOTO 1                                      
            TEMP4 = INT+K                                               
C/6S                                                                    
C           IF (X(TEMP4) .LE. X(INT)) CALL SETERR(                      
C    1         37H GLSIN - X IS NOT MONOTONE INCREASING, 37, 12, 2)     
C/7S                                                                    
            IF (X(TEMP4) .LE. X(INT)) CALL SETERR(                      
     1         ' GLSIN - X IS NOT MONOTONE INCREASING', 37, 12, 2)      
C/                                                                      
   1     IF (X(INT) .EQ. X(INT+1)) GOTO  33                             
C GET THE QUADRATURE POINTS ON (X(INT),X(INT+1)).                       
         DO  2 IQ = 1, MQ                                               
            TQ(IQ) = 0.5E0*((X(INT+1)+X(INT))+(X(INT+1)-X(INT))*XQ(IQ)) 
   2        CONTINUE                                                    
         SCALE = 0.5E0*(X(INT+1)-X(INT))                                
         IF (.NOT. SET) GOTO 3                                          
            TEMP4 = INT-K                                               
            CALL MOVEFR(2*MQ*K, GSBSIS(1, TEMP4+1), SBASIS)             
            GOTO  5                                                     
   3        CALL BSPL1(K, X, NX, TQ, MQ, INT, ID, 2, SBASIS)            
C GET BASIS SPLINES.                                                    
            IF (IGSSIS .LE. 1) GOTO 4                                   
               TEMP4 = INT-K                                            
               CALL MOVEFR(2*MQ*K, SBASIS, GSBSIS(1, TEMP4+1))          
   4        CONTINUE                                                    
C DEFAULT A AND F VALUES.                                               
   5     CALL SETR(4*MQ*NU*(2*NU+NRHS), 0E0, A1)                        
C GET A AND F.                                                          
         IF (.NOT. AF(TQ, MQ, NU, NRHS, NRHSG, GETJAC, SEPATE, A1, A1T  
     1      , A2, A2T, A3, A3T, A4, A4T, F1, F1T, F2, F2T, INT, SBASIS  
     2      , K, X, NX, AF1)) GOTO 6                                    
            A6LSIN = .TRUE.                                             
            RETURN                                                      
C DO THE I-TH ODE.                                                      
   6     DO  32 I = 1, NU                                               
            IF (.NOT. GETJAC) GOTO 18                                   
               A12ERO = .TRUE.                                          
               A4ZERO = .TRUE.                                          
               DO  17 J = 1, NU                                         
                  DO  16 IQ = 1, MQ                                     
                     TEMP = A1(IQ, I, J) .EQ. 0.                        
                     IF (TEMP) TEMP = A1T(IQ, I, J) .EQ. 0.             
                     IF (TEMP) TEMP = A2(IQ, I, J) .EQ. 0.              
                     IF (TEMP) TEMP = A2T(IQ, I, J) .EQ. 0.             
                     IF (TEMP) TEMP = A3(IQ, I, J) .EQ. 0.              
                     IF (TEMP) TEMP = A3T(IQ, I, J) .EQ. 0.             
                     A1230 = TEMP                                       
                     TEMP = A4(IQ, I, J) .EQ. 0.                        
                     IF (TEMP) TEMP = A4T(IQ, I, J) .EQ. 0.             
                     A40 = TEMP                                         
                     A12ERO = A12ERO .AND. A1230                        
                     A4ZERO = A4ZERO .AND. A40                          
                     TEMP = INT .EQ. K                                  
                     IF (TEMP) TEMP = IQ .EQ. 1                         
                     IF (TEMP) GOTO 7                                   
                        TEMP1 = INT .EQ. NSPLN                          
                        IF (TEMP1) TEMP1 = IQ .EQ. MQ                   
                        TEMP = TEMP1                                    
   7                 IF (.NOT. TEMP) GOTO 14                            
                        IF (INT .NE. K) GOTO 8                          
                           ILR = 1                                      
C SET ORDER.                                                            
                           GOTO  9                                      
   8                       ILR = 2                                      
   9                    TEMP1 = A1(IQ, I, J) .NE. 0.                    
                        IF (.NOT. TEMP1) TEMP1 = A1T(IQ, I, J) .NE. 0.  
                        IF (.NOT. TEMP1) GOTO 10                        
                           ORDER(I, J, ILR) = MAX0(ORDER(I, J, ILR), 2) 
                           GOTO  13                                     
  10                       TEMP2 = A2(IQ, I, J) .NE. 0.                 
                           IF (.NOT. TEMP2) TEMP2 = A2T(IQ, I, J) .NE.  
     1                        0.                                        
                           IF (.NOT. TEMP2) TEMP2 = A3(IQ, I, J) .NE.   
     1                        0.                                        
                           IF (.NOT. TEMP2) TEMP2 = A3T(IQ, I, J) .NE.  
     1                        0.                                        
                           IF (.NOT. TEMP2) GOTO 11                     
                              ORDER(I, J, ILR) = MAX0(ORDER(I, J, ILR)  
     1                           , 1)                                   
                              GOTO  12                                  
  11                          TEMP3 = A4(IQ, I, J) .NE. 0.              
                              IF (.NOT. TEMP3) TEMP3 = A4T(IQ, I, J)    
     1                            .NE. 0.                               
                              IF (TEMP3) ORDER(I, J, ILR) = MAX0(ORDER(I
     1                           , J, ILR), 0)                          
  12                    CONTINUE                                        
  13                    TEMP1 = K .EQ. NSPLN                            
                        IF (TEMP1) TEMP1 = I .EQ. NU                    
                        IF (TEMP1) TEMP1 = J .EQ. NU                    
                        IF (TEMP1) TEMP1 = IQ .EQ. MQ                   
                        IF (TEMP1) CALL MOVEFI(NU**2, ORDER, ORDER(1, 1,
     1                     2))                                          
  14                 A1(IQ, I, J) = A1(IQ, I, J)*WQ(IQ)                 
                     A2(IQ, I, J) = A2(IQ, I, J)*WQ(IQ)                 
                     A3(IQ, I, J) = A3(IQ, I, J)*WQ(IQ)                 
                     A4(IQ, I, J) = A4(IQ, I, J)*WQ(IQ)                 
                     IF (.NOT. SEPATE) GOTO 15                          
                        A1T(IQ, I, J) = A1T(IQ, I, J)*WQ(IQ)            
                        A2T(IQ, I, J) = A2T(IQ, I, J)*WQ(IQ)            
                        A3T(IQ, I, J) = A3T(IQ, I, J)*WQ(IQ)            
                        A4T(IQ, I, J) = A4T(IQ, I, J)*WQ(IQ)            
  15                 CONTINUE                                           
  16                 CONTINUE                                           
C END IQ.                                                               
C DOES U(J) NOT APPEAR IN ODE(I)?                                       
                  TEMP = A12ERO                                         
                  IF (TEMP) TEMP = A4ZERO                               
                  ZERO(J) = TEMP                                        
  17              CONTINUE                                              
C END J.                                                                
               TEMP = .NOT. A4ZERO                                      
               IF (TEMP) TEMP = A12ERO                                  
               IF (TEMP) TEMP = MQ .EQ. K-1                             
               IF (TEMP) ERRATE = 1                                     
               TEMP = A12ERO                                            
               IF (TEMP) TEMP = A4ZERO                                  
               IF (TEMP) ERRATE = 2                                     
C MAKE THE I-TH EQUATION'S RESIDUAL                                     
  18        DO  25 P = 1, K                                             
C ORTHOGONAL TO B-SUB-P.                                                
               GIDX1 = I+(INT-K-1+P)*NU                                 
C       GET THE RIGHT-HAND-SIDE B.                                      
               JRHS = 1                                                 
                  GOTO  20                                              
  19              JRHS = JRHS+1                                         
  20              IF (JRHS .GT. NRHSG) GOTO  24                         
                  T = 0                                                 
                  DO  21 IQ = 1, MQ                                     
                     T = T+(F1(IQ, I, JRHS)*SBASIS(IQ, P, 2)-F2(IQ, I,  
     1                  JRHS)*SBASIS(IQ, P, 1))*WQ(IQ)                  
  21                 CONTINUE                                           
                  B(GIDX1, JRHS) = B(GIDX1, JRHS)+SCALE*T               
                  IF (.NOT. SEPATE) GOTO 23                             
                     TT = 0                                             
                     DO  22 IQ = 1, MQ                                  
                        TT = TT+(F1T(IQ, I, JRHS)*SBASIS(IQ, P, 2)-F2T( 
     1                     IQ, I, JRHS)*SBASIS(IQ, P, 1))*WQ(IQ)        
  22                    CONTINUE                                        
                     BT(GIDX1, JRHS) = BT(GIDX1, JRHS)+SCALE*TT         
  23              CONTINUE                                              
                  GOTO  19                                              
  24           CONTINUE                                                 
  25           CONTINUE                                                 
            IF (.NOT. GETJAC) GOTO  32                                  
C GET THE J-TH COMPONENT OF (I,P) RELATION.                             
            DO  31 J = 1, NU                                            
               IF (ZERO(J)) GOTO  31                                    
C MAKE THE I-TH EQUATION'S RESIDUAL                                     
               DO  30 P = 1, K                                          
C ORTHOGONAL TO B-SUB-P.                                                
                  GIDX1 = I+(INT-K-1+P)*NU                              
                  GIDX2 = J-I+(K+1-P)*NU                                
C Q-TH COEFFICIENT OF THE (I,P,J) RELATION.                             
                  DO  29 Q = 1, K                                       
                     T = 0                                              
                     DO  26 IQ = 1, MQ                                  
                        T = T+(A1(IQ, I, J)*SBASIS(IQ, Q, 2)+A2(IQ, I, J
     1                     )*SBASIS(IQ, Q, 1))*SBASIS(IQ, P, 2)+(A3(IQ  
     2                     , I, J)*SBASIS(IQ, Q, 2)+A4(IQ, I, J)*SBASIS(
     3                     IQ, Q, 1))*SBASIS(IQ, P, 1)                  
  26                    CONTINUE                                        
                     G(GIDX1, GIDX2) = G(GIDX1, GIDX2)+T*SCALE          
                     IF (.NOT. SEPATE) GOTO 28                          
                        TT = 0                                          
                        DO  27 IQ = 1, MQ                               
                           TT = TT+(A1T(IQ, I, J)*SBASIS(IQ, Q, 2)+A2T( 
     1                        IQ, I, J)*SBASIS(IQ, Q, 1))*SBASIS(IQ, P  
     2                        , 2)+(A3T(IQ, I, J)*SBASIS(IQ, Q, 2)+A4T( 
     3                        IQ, I, J)*SBASIS(IQ, Q, 1))*SBASIS(IQ, P  
     4                        , 1)                                      
  27                       CONTINUE                                     
                        GT(GIDX1, GIDX2) = GT(GIDX1, GIDX2)+TT*SCALE    
  28                 GIDX2 = GIDX2+NU                                   
  29                 CONTINUE                                           
C END Q.                                                                
  30              CONTINUE                                              
C END P.                                                                
  31           CONTINUE                                                 
C END J.                                                                
  32        CONTINUE                                                    
C END I.                                                                
  33     CONTINUE                                                       
C END INT.                                                              
      TEMP = .NOT. SET                                                  
      IF (TEMP) TEMP = IGSSIS .GT. 1                                    
      IF (TEMP) SET = .TRUE.                                            
C/6S                                                                    
C     IF (ERRATE .EQ. 1) CALL SETERR(                                   
C    1   33H GLSIN - MQ=K-1 WHEN ORDER(I,.)=0, 33, 14, 1)               
C     IF (ERRATE .EQ. 2) CALL SETERR(26H GLSIN - ODE(I) IS VACUOUS, 26  
C    1   , 15, 1)                                                       
C/7S                                                                    
      IF (ERRATE .EQ. 1) CALL SETERR(                                   
     1   ' GLSIN - MQ=K-1 WHEN ORDER(I,.)=0', 33, 14, 1)                
      IF (ERRATE .EQ. 2) CALL SETERR(' GLSIN - ODE(I) IS VACUOUS', 26   
     1   , 15, 1)                                                       
C/                                                                      
      A6LSIN = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE A6LSSD(ALFA, GAMMA, NU, NRHS, CC, SGAMMA, BC, C        
     1   , G, ED, NDEQS, PIVOT, DIAG)                                   
      INTEGER NRHS, NU, NDEQS                                           
      INTEGER BC(NU), ED(NU), PIVOT(NU)                                 
      REAL ALFA(NU, NU), GAMMA(NU, NRHS), CC(NU, NU), SGAMMA(NU, NRHS)  
     1   , C(NDEQS, NU), G(NDEQS, NRHS)                                 
      REAL DIAG(NU)                                                     
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, NERROR, IPIVOT, I, J, NERR                        
      INTEGER IS(1000), NDVAR                                           
      REAL RS(1000), WS(500)                                            
      LOGICAL LS(1000)                                                  
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C SCRATCH SPACE ALLOCATED - S(A6LSSD) <= NU REAL WORDS +                
C                                        NU INTEGER WORDS.              
      CALL ENTER(1)                                                     
C COPY ALFA AND GAMMA INTO                                              
      DO  3 I = 1, NDEQS                                                
C PROPERLY DIMENSIONED ARRAYS.                                          
         DO  1 J = 1, NU                                                
            TEMP1 = ED(I)                                               
            C(I, J) = ALFA(TEMP1, J)                                    
   1        CONTINUE                                                    
         DO  2 J = 1, NRHS                                              
            TEMP1 = ED(I)                                               
            G(I, J) = GAMMA(TEMP1, J)                                   
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      NDVAR = 0                                                         
C COUNT THE VARIABLES.                                                  
      DO  7 J = 1, NU                                                   
         DO  5 I = 1, NDEQS                                             
            IF (C(I, J) .EQ. 0.) GOTO 4                                 
               NDVAR = NDVAR+1                                          
               GOTO  6                                                  
   4        CONTINUE                                                    
   5        CONTINUE                                                    
   6     CONTINUE                                                       
   7     CONTINUE                                                       
      IF (NDVAR .GE. NDEQS) GOTO 8                                      
C/6S                                                                    
C        CALL SETERR(                                                   
C    1      57H GLSSB - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED
C    2      , 57, 4, 1)                                                 
C/7S                                                                    
         CALL SETERR(                                                   
     1      ' GLSSB - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED' 
     2      , 57, 4, 1)                                                 
C/                                                                      
         CALL LEAVE                                                     
         RETURN                                                         
C GET THE QR DECOMPOSITION OF C.                                        
   8  CALL QRD(NDEQS, NU, C, DIAG, PIVOT)                               
      IF (NERROR(NERR) .EQ. 0) GOTO 9                                   
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(37H GLSSB - SINGULAR DIRICHLET SUBSYSTEM, 37, 6, 1)
C/7S                                                                    
         CALL SETERR(' GLSSB - SINGULAR DIRICHLET SUBSYSTEM', 37, 6, 1) 
C/                                                                      
         CALL LEAVE                                                     
         RETURN                                                         
C FORM G = Q*G.                                                         
   9  CALL QRQTV(NDEQS, NU, C, DIAG, NRHS, G)                           
C GET A PHONY PIVOT(I) = I ARRAY.                                       
      IPIVOT = ISTKGT(NU, 2)                                            
      DO  10 I = 1, NU                                                  
         TEMP1 = IPIVOT-1+I                                             
         IS(TEMP1) = I                                                  
  10     CONTINUE                                                       
      CALL QRBS(NDEQS, NDEQS, C, DIAG, IS(IPIVOT), NRHS, G, G)          
      IF (NDEQS .LT. NU) CALL QRBS(NDEQS, NDEQS, C, DIAG, IS(IPIVOT),   
     1   NU-NDEQS, C(1, NDEQS+1), C(1, NDEQS+1))                        
C PUT THE STANDARD BC'S INTO CC AND SGAMMA.                             
      DO  15 I = 1, NDEQS                                               
         J = NDEQS+1                                                    
            GOTO  12                                                    
  11        J = J+1                                                     
  12        IF (J .GT. NU) GOTO  13                                     
            TEMP1 = PIVOT(I)                                            
            TEMP = PIVOT(J)                                             
            CC(TEMP1, TEMP) = -C(I, J)                                  
            GOTO  11                                                    
  13     DO  14 J = 1, NRHS                                             
            TEMP = PIVOT(I)                                             
            SGAMMA(TEMP, J) = G(I, J)                                   
  14        CONTINUE                                                    
         TEMP = PIVOT(I)                                                
         BC(TEMP) = 0                                                   
  15     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE A6LSSM(ALFA, BETA, GAMMA, NU, NRHS, AA, BB,            
     1   SGAMMA, BC, A, B, G, EM, NMEQS, PIVOT, DIAG)                   
      INTEGER NRHS, NU, NMEQS                                           
      INTEGER BC(NU), EM(NU), PIVOT(NU)                                 
      REAL ALFA(NU, NU), BETA(NU, NU), GAMMA(NU, NRHS), AA(NU, NU), BB( 
     1   NU, NU), SGAMMA(NU, NRHS)                                      
      REAL A(NMEQS, NU), B(NMEQS, NU), G(NMEQS, NRHS), DIAG(NU)         
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, NERROR, IPIVOT, I, J, NERR                        
      INTEGER IS(1000), NMVAR                                           
      REAL RS(1000), WS(500)                                            
      LOGICAL LS(1000)                                                  
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C SCRATCH SPACE ALLOCATED - S(A6LSSM) <= NU REAL WORDS +                
C                                        NU INTEGER WORDS.              
      CALL ENTER(1)                                                     
C COPY ALFA, BETA AND GAMMA INTO                                        
      DO  3 I = 1, NMEQS                                                
C PROPERLY DIMENSIONED ARRAYS.                                          
         DO  1 J = 1, NU                                                
            TEMP1 = EM(I)                                               
            A(I, J) = ALFA(TEMP1, J)                                    
            TEMP1 = EM(I)                                               
            B(I, J) = BETA(TEMP1, J)                                    
   1        CONTINUE                                                    
         DO  2 J = 1, NRHS                                              
            TEMP1 = EM(I)                                               
            G(I, J) = GAMMA(TEMP1, J)                                   
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      NMVAR = 0                                                         
C COUNT THE VARIABLES.                                                  
      DO  7 J = 1, NU                                                   
         DO  5 I = 1, NMEQS                                             
            IF (B(I, J) .EQ. 0.) GOTO 4                                 
               NMVAR = NMVAR+1                                          
               GOTO  6                                                  
   4        CONTINUE                                                    
   5        CONTINUE                                                    
   6     CONTINUE                                                       
   7     CONTINUE                                                       
      IF (NMVAR .GE. NMEQS) GOTO 8                                      
C/6S                                                                    
C        CALL SETERR(                                                   
C    1      53H GLSSB - MIXED BOUNDARY CONDITIONS ARE OVERDETERMINED,   
C    2      53, 3, 1)                                                   
C/7S                                                                    
         CALL SETERR(                                                   
     1      ' GLSSB - MIXED BOUNDARY CONDITIONS ARE OVERDETERMINED',    
     2      53, 3, 1)                                                   
C/                                                                      
         CALL LEAVE                                                     
         RETURN                                                         
C GET THE QR DECOMPOSITION OF B.                                        
   8  CALL QRD(NMEQS, NU, B, DIAG, PIVOT)                               
      IF (NERROR(NERR) .EQ. 0) GOTO 9                                   
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(33H GLSSB - SINGULAR MIXED SUBSYSTEM, 33, 5, 1)    
C/7S                                                                    
         CALL SETERR(' GLSSB - SINGULAR MIXED SUBSYSTEM', 33, 5, 1)     
C/                                                                      
         CALL LEAVE                                                     
         RETURN                                                         
C FORM G = Q*G.                                                         
   9  CALL QRQTV(NMEQS, NU, B, DIAG, NRHS, G)                           
C FORM Q*A.                                                             
      CALL QRQTV(NMEQS, NU, B, DIAG, NU, A)                             
C FORM PHONY PIVOT(I) = I.                                              
      IPIVOT = ISTKGT(NMEQS, 2)                                         
      DO  10 I = 1, NMEQS                                               
         TEMP1 = IPIVOT-1+I                                             
         IS(TEMP1) = I                                                  
  10     CONTINUE                                                       
      CALL QRBS(NMEQS, NMEQS, B, DIAG, IS(IPIVOT), NRHS, G, G)          
      CALL QRBS(NMEQS, NMEQS, B, DIAG, IS(IPIVOT), NU, A, A)            
      IF (NMEQS .LT. NU) CALL QRBS(NMEQS, NMEQS, B, DIAG, IS(IPIVOT),   
     1   NU-NMEQS, B(1, NMEQS+1), B(1, NMEQS+1))                        
C PUT THE STANDARD FORM INTO AA,BB,SGAMMA AND BC.                       
      DO  14 I = 1, NMEQS                                               
         DO  12 J = 1, NU                                               
            TEMP1 = PIVOT(I)                                            
            AA(TEMP1, J) = -A(I, J)                                     
            IF (J .LE. NMEQS) GOTO 11                                   
               TEMP1 = PIVOT(I)                                         
               TEMP = PIVOT(J)                                          
               BB(TEMP1, TEMP) = -B(I, J)                               
  11        CONTINUE                                                    
  12        CONTINUE                                                    
         DO  13 J = 1, NRHS                                             
            TEMP = PIVOT(I)                                             
            SGAMMA(TEMP, J) = G(I, J)                                   
  13        CONTINUE                                                    
         TEMP = PIVOT(I)                                                
         BC(TEMP) = 1                                                   
  14     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE QRQTV(M, N, QR, ALFA, NB, B)                           
      INTEGER M, N, NB                                                  
      REAL QR(M, N), ALFA(N), B(M, NB)                                  
      INTEGER MIN0, I, J, JB                                            
      REAL SDOT, GAMMA                                                  
      INTEGER TEMP                                                      
C TO FORM Q*B.                                                          
C MNEMONIC - QR FACTOR Q TIMES A VECTOR.                                
C INPUT -                                                               
C   M    - THE NUMBER OF ROWS IN THE MATRIX.                            
C   N    - THE NUMBER OF COLUMNS IN THE MATRIX.                         
C   QR   - THE QR FACTORIZATION, AS DESCRIBED IN  QRD.                  
C   ALFA - THE DIAGONAL OF R, AS DESCRIBED IN  QRD.                     
C   NB   - THE NUMBER OF RIGHT-HAND-SIDES.                              
C   B    - THE RIGHT-HAND-SIDES.                                        
C OUTPUT -                                                              
C   B - B = Q*B.                                                        
C SCRATCH SPACE ALLOCATED - NONE.                                       
C ERROR STATES -                                                        
C   1 - M.LT.1.                                                         
C   2 - N.LT.1.                                                         
C   3 - NB.LT.1.                                                        
C   4 - ALFA(J)=0.                                                      
C   5 - QR(J,J)=0.                                                      
C ALFA(MIN(M,N)).                                                       
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(15H QRQTV - M.LT.1, 15, 1, 2)           
C     IF (N .LT. 1) CALL SETERR(15H QRQTV - N.LT.1, 15, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(16H QRQTV - NB.LT.1, 16, 3, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR(' QRQTV - M.LT.1', 15, 1, 2)            
      IF (N .LT. 1) CALL SETERR(' QRQTV - N.LT.1', 15, 2, 2)            
      IF (NB .LT. 1) CALL SETERR(' QRQTV - NB.LT.1', 16, 3, 2)          
C/                                                                      
C MULTIPLY ALL THE VECTORS.                                             
      DO  3 JB = 1, NB                                                  
C APPLY THE J-TH TRANSFORMATION.                                        
         TEMP = MIN0(M, N)                                              
         DO  2 J = 1, TEMP                                              
C/6S                                                                    
C           IF (ALFA(J) .EQ. 0.) CALL SETERR(18H QRQTV - ALFA(J)=0, 18  
C    1         , 4, 2)                                                  
C           IF (QR(J, J) .EQ. 0.) CALL SETERR(18H QRQTV - QR(J,J)=0, 18,
C    1         5, 2)                                                    
C/7S                                                                    
            IF (ALFA(J) .EQ. 0.) CALL SETERR(' QRQTV - ALFA(J)=0', 18   
     1         , 4, 2)                                                  
            IF (QR(J, J) .EQ. 0.) CALL SETERR(' QRQTV - QR(J,J)=0', 18, 
     1         5, 2)                                                    
C/                                                                      
            GAMMA = SDOT(M-J+1, QR(J, J), 1, B(J, JB), 1)/(ALFA(J)*QR(J,
     1         J))                                                      
            DO  1 I = J, M                                              
               B(I, JB) = B(I, JB)+GAMMA*QR(I, J)                       
   1           CONTINUE                                                 
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE BNDBS(N, M, U, NB, B)                                  
      INTEGER M, N, NB                                                  
      REAL U(N, M), B(N, NB)                                            
      INTEGER I, J, K, L, MIN0                                          
      REAL X                                                            
      INTEGER TEMP                                                      
C TO SOLVE U*X = B, WHERE U IS AN UPPER TRIANGULAR BANDED MATRIX.       
C MNEMONIC - BANDED BACK-SOLVE.                                         
C INPUT -                                                               
C   N  - THE ORDER OF THE SYSTEM.                                       
C   M  - THE NUMBER OF NONZERO ENTRIES ON AND ABOVE                     
C        THE DIAGONAL OF U.                                             
C   U  - THE UPPER TRIANGULAR BAND MATRIX.                              
C   NB - THE NUMBER OF RIGHT-HAND-SIDES.                                
C   B  - THE RIGHT-HAND-SIDES.                                          
C OUTPUT -                                                              
C   B - THE SOLUTION VECTORS, X.                                        
C SCRATCH SPACE ALLOCATED - NONE.                                       
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - M.LT.1.                                                         
C   3 - NB.LT.1.                                                        
C   4 - U(I,1)=0.                                                       
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15H BNDBS - N.LT.1, 15, 1, 2)           
C     IF (M .LT. 1) CALL SETERR(15H BNDBS - M.LT.1, 15, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(16H BNDBS - NB.LT.1, 16, 3, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' BNDBS - N.LT.1', 15, 1, 2)            
      IF (M .LT. 1) CALL SETERR(' BNDBS - M.LT.1', 15, 2, 2)            
      IF (NB .LT. 1) CALL SETERR(' BNDBS - NB.LT.1', 16, 3, 2)          
C/                                                                      
C PROTECT AGAINST AN EXISTING ERROR STATE.                              
      CALL ENTSRC(I, 0)                                                 
      DO  6 J = 1, NB                                                   
         L = 1                                                          
         I = N                                                          
            GOTO  2                                                     
   1        I = I-1                                                     
   2        IF (I .LT. 1) GOTO  5                                       
            X = B(I, J)                                                 
            IF (L .LE. 1) GOTO 4                                        
               DO  3 K = 2, L                                           
                  TEMP = I-1+K                                          
                  X = X-U(I, K)*B(TEMP, J)                              
   3              CONTINUE                                              
C/6S                                                                    
C  4        IF (U(I, 1) .EQ. 0.0E0) CALL SETERR(17H BNDBS - U(I,1)=0,   
C    1         17, 4, 2)                                                
C/7S                                                                    
   4        IF (U(I, 1) .EQ. 0.0E0) CALL SETERR(' BNDBS - U(I,1)=0',    
     1         17, 4, 2)                                                
C/                                                                      
            B(I, J) = X/U(I, 1)                                         
            L = MIN0(L+1, M)                                            
            GOTO  1                                                     
   5     CONTINUE                                                       
   6     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE QRBS(M, N, QR, ALFA, PIVOT, NB, B, X)                  
      INTEGER M, N, NB                                                  
      INTEGER PIVOT(N)                                                  
      REAL QR(M, N), ALFA(N), B(M, NB), X(N, NB)                        
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, I, JB, IS(1000), IZ                               
      REAL SDOT, RS(1000), WS(1000)                                     
      LOGICAL LS(1000)                                                  
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO SOLVE R*X = B.                                                     
C MNEMONIC - QR BACKSOLVE.                                              
C INPUT -                                                               
C   M     - THE NUMBER OF ROWS IN THE MATRIX.                           
C   N     - THE NUMBER OF COLUMNS IN THE MATRIX.                        
C   QR    - THE QR FACTORIZATION, AS DESCRIBED IN  QRD.                 
C   ALFA  - THE DIAGONAL OF R, AS DESCRIBED IN  QRD.                    
C   PIVOT - THE PIVOTING VECTOR, AS DESCRIBED IN  QRD.                  
C   NB    - THE NUMBER OF RIGHT-HAND-SIDES.                             
C   B     - THE RIGHT-HAND-SIDES.                                       
C OUTPUT -                                                              
C   X - THE SOLUTION VECTORS.                                           
C SCRATCH STORAGE ALLOCATED - N*MU WORDS.                               
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - M.LT.N.                                                         
C   3 - NB.LT.1.                                                        
C   4 - ALFA(I)=0.                                                      
C   5 - PIVOT(I) NOT ONE OF 1,...,N.                                    
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
C DEFINE Z(J) WS(IZ-1+J)                                                
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(14H QRBS - N.LT.1, 14, 1, 2)            
C     IF (M .LT. N) CALL SETERR(14H QRBS - M.LT.N, 14, 2, 2)            
C     IF (NB .LT. 1) CALL SETERR(15H QRBS - NB.LT.1, 15, 3, 2)          
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR(' QRBS - N.LT.1', 14, 1, 2)             
      IF (M .LT. N) CALL SETERR(' QRBS - M.LT.N', 14, 2, 2)             
      IF (NB .LT. 1) CALL SETERR(' QRBS - NB.LT.1', 15, 3, 2)           
C/                                                                      
      DO  1 I = 1, N                                                    
C/6S                                                                    
C        IF (ALFA(I) .EQ. 0.) CALL SETERR(17H QRBS - ALFA(I)=0, 17, 4, 2
C    1      )                                                           
C        IF (PIVOT(I) .LT. 1 .OR. PIVOT(I) .GT. N) CALL SETERR(         
C    1      35H QRBS - PIVOT(I) NOT ONE OF 1,...,N, 35, 5, 2)           
C/7S                                                                    
         IF (ALFA(I) .EQ. 0.) CALL SETERR(' QRBS - ALFA(I)=0', 17, 4, 2 
     1      )                                                           
         IF (PIVOT(I) .LT. 1 .OR. PIVOT(I) .GT. N) CALL SETERR(         
     1      ' QRBS - PIVOT(I) NOT ONE OF 1,...,N', 35, 5, 2)            
C/                                                                      
   1     CONTINUE                                                       
      IZ = ISTKGT(N, 3)                                                 
C DO ALL THE RIGHT-HAND-SIDES.                                          
      DO  6 JB = 1, NB                                                  
         TEMP = IZ+N                                                    
         WS(TEMP-1) = B(N, JB)/ALFA(N)                                  
         I = N-1                                                        
            GOTO  3                                                     
   2        I = I-1                                                     
   3        IF (I .LT. 1) GOTO  4                                       
            TEMP = IZ-1+I                                               
            TEMP1 = IZ+I                                                
            WS(TEMP) = (-(SDOT(N-I, QR(I, I+1), M, WS(TEMP1), 1)-B(I,   
     1         JB)))/ALFA(I)                                            
            GOTO  2                                                     
   4     DO  5 I = 1, N                                                 
            TEMP1 = PIVOT(I)                                            
            TEMP = IZ+I                                                 
            X(TEMP1, JB) = WS(TEMP-1)                                   
   5        CONTINUE                                                    
   6     CONTINUE                                                       
      CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      REAL FUNCTION EXPL(X)                                             
      REAL X                                                            
      REAL SMALL, R1MACH, LOGBIG, BIG, EXP, LOGALL                      
      REAL ALOG                                                         
      DATA SMALL/0E0/                                                   
      DATA BIG/0E0/                                                     
      DATA LOGALL/0E0/                                                  
      DATA LOGBIG/0E0/                                                  
C  EXPL(X) = EXP(X) EXCEPT WHEN IT WILL -                               
C   UNDERFLOW - 0 IS RETURNED,                                          
C     OR                                                                
C   OVERFLOW - +BIG IS RETURNED, WITH AN ERROR STATE.                   
      IF (SMALL .NE. 0.) GOTO 1                                         
         SMALL = R1MACH(1)                                              
         BIG = R1MACH(2)                                                
         LOGALL = ALOG(SMALL*(50.*R1MACH(4)+1.))                        
         LOGBIG = ALOG(BIG*(1.-50.*R1MACH(4)))                          
   1  IF (X .GT. LOGALL) GOTO 2                                         
         EXPL = 0                                                       
         RETURN                                                         
   2     IF (X .LT. LOGBIG) GOTO 3                                      
C/6S                                                                    
C           CALL SETERR(25H EXPL - EXPONENT OVERFLOW, 25, 1, 2)         
C/7S                                                                    
            CALL SETERR(' EXPL - EXPONENT OVERFLOW', 25, 1, 2)          
C/                                                                      
            EXPL = BIG                                                  
            RETURN                                                      
   3  CONTINUE                                                          
   4  EXPL = EXP(X)                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOST(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT        
     1   , AF, BC, D, ERRPAR, HANDLE)                                   
      EXTERNAL AF, BC, D, HANDLE                                        
      INTEGER NU, K, NX, NV                                             
      REAL ERRPAR(2)                                                    
      DOUBLE PRECISION U(1), X(1), V(1), TSTART, TSTOP, DT              
      EXTERNAL DPOSTE, DPOSTN, DPOSTP                                   
C THE FIRST LEVEL OF DPOST.                                             
C U(NX-K,NU),X(NX),V(NV).                                               
      CALL DPOSTR(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT, AF, BC, D  
     1   , DPOSTE, ERRPAR, DPOSTN, DPOSTP, HANDLE)                      
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTS(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,      
     1   AF, BC, D, ERRPAR, HANDLE)                                     
      EXTERNAL AF, BC, D, HANDLE                                        
      INTEGER NU, K, NX, NV                                             
      REAL ERRPAR(2)                                                    
      DOUBLE PRECISION U(1), X(1), V(1), TSTART, TSTOP, DT              
      EXTERNAL DPOSTE                                                   
      LOGICAL ERPUTS                                                    
C THE FIRST LEVEL OF DPOSTS.                                            
C U(NX-K,NU),X(NX),V(NV).                                               
      ERPUTS = .FALSE.                                                  
      CALL DPOST1(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT, AF, BC, D  
     1   , DPOSTE, ERRPAR, ERPUTS, HANDLE)                              
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOST1(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,      
     1   AF, BC, D, ERROR, ERRPAR, ERPUTS, HANDLE)                      
      EXTERNAL AF, BC, D, ERROR, HANDLE                                 
      INTEGER NU, K, NX, NV                                             
      REAL ERRPAR(2)                                                    
      LOGICAL ERPUTS                                                    
      DOUBLE PRECISION U(1), X(1), V(1), TSTART, TSTOP, DT              
      INTEGER KMAX, KINIT                                               
      LOGICAL EQUIL, XPOLY                                              
C THE SECOND LEVEL OF DPOSTS.                                           
C U(NX-K,NU),X(NX),V(NV).                                               
      KMAX = 10                                                         
      XPOLY = .FALSE.                                                   
      KINIT = 2                                                         
      EQUIL = .TRUE.                                                    
      CALL DPOST2(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT, AF, BC, D  
     1   , EQUIL, KMAX, XPOLY, KINIT, ERROR, ERRPAR, ERPUTS, HANDLE)    
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOST2(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,      
     1   AF, BC, D, EQUIL, KMAX, XPOLY, KINIT, ERROR, ERRPAR, ERPUTS,   
     2   HANDLE)                                                        
      EXTERNAL AF, BC, D, ERROR, HANDLE                                 
      INTEGER NU, K, NX, NV, KMAX, KINIT                                
      REAL ERRPAR(2)                                                    
      LOGICAL EQUIL, XPOLY, ERPUTS                                      
      DOUBLE PRECISION U(1), X(1), V(1), TSTART, TSTOP, DT              
      EXTERNAL DPOSTN, DPOSTP                                           
      INTEGER MGQ, NERROR, KEEJAC, NERR, MINIT, MAXIT                   
      DOUBLE PRECISION T0, T1, THETA                                    
C THE THIRD LEVEL OF DPOSTS.                                            
C U(NX-K,NU),X(NX),V(NV).                                               
C CHECK THE INPUT FOR ERRORS.                                           
      CALL ENTER(1)                                                     
      IF (.NOT. EQUIL) GOTO 1                                           
         THETA = 1                                                      
         GOTO  2                                                        
   1     THETA = 0.5D0                                                  
   2  KEEJAC = 0                                                        
      IF (.NOT. EQUIL) GOTO 3                                           
         MINIT = 1                                                      
         MAXIT = 1                                                      
         GOTO  4                                                        
   3     MINIT = 10                                                     
         MAXIT = 50                                                     
   4  T0 = TSTART                                                       
      T1 = TSTOP                                                        
      MGQ = K-1                                                         
         GOTO  6                                                        
   5     MGQ = MGQ+1                                                    
   6     IF (MGQ .GT. K) GOTO  7                                        
C LOOP UNTIL GQ WORKS.                                                  
         CALL DPOST3(U, NU, K, X, NX, V, NV, T0, T1, DT, AF, BC, D,     
     1      THETA, KEEJAC, MINIT, MAXIT, MGQ, KMAX, XPOLY, KINIT, ERROR,
     2      ERRPAR, ERPUTS, DPOSTN, DPOSTP, HANDLE)                     
         IF (NERROR(NERR) .NE. 1011) GOTO  7                            
         CALL ERROFF                                                    
         T0 = T1                                                        
         T1 = TSTOP                                                     
         GOTO  5                                                        
   7  TSTOP = T1                                                        
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOST3(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,      
     1   AF, BC, D, THETA, KEEJAC, MINIT, MAXIT, MGQ, KMAX, XPOLY,      
     2   KINIT, ERROR, ERRPAR, ERPUTS, INMI, SCALE, HANDLE)             
      EXTERNAL AF, BC, D, ERROR, INMI, SCALE                            
      EXTERNAL HANDLE                                                   
      INTEGER NU, K, NX, NV, KEEJAC, MINIT                              
      INTEGER MAXIT, MGQ, KMAX, KINIT                                   
      REAL ERRPAR(2)                                                    
      LOGICAL XPOLY, ERPUTS                                             
      DOUBLE PRECISION U(1), X(1), V(1), TSTART, TSTOP, DT              
      DOUBLE PRECISION THETA                                            
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, I, MMAX, IN, IS(1000), SAVEB                      
      REAL HFRACT, EGIVE, RS(1000)                                      
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION BETA, GAMMA, DELTA, WS(500)                      
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE FOURTH LEVEL OF POSTS.                                            
C SCRATCH SPACE ALLOCATED -                                             
C     S(DPOST3) = 10*NU**2 + 8*NU + 2*NU*(NV+1) + S(D9OSTS) +           
C                 IF ( KEEPJAC > 0 ) { 2*K*MGQ*(NX-2*K+1) }             
C LONG REAL WORDS +                                                     
C                 MMAX                                                  
C INTEGER WORDS.                                                        
C U(NX-K,NU),X(NX),V(NV).                                               
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      CALL ENTER(1)                                                     
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
C DEFAULT.                                                              
   7  SAVEB = 0                                                         
      IF (THETA .NE. 0.5) GOTO 9                                        
         HFRACT = 0.5                                                   
         DO  8 I = 1, MMAX                                              
            TEMP = IN+I                                                 
            IS(TEMP-1) = 2*IS(TEMP-1)                                   
   8        CONTINUE                                                    
         GOTO  10                                                       
   9     HFRACT = 1                                                     
  10  EGIVE = 1E+2                                                      
      CALL DPOST4(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT, AF, BC, D  
     1   , THETA, KEEJAC, MINIT, MAXIT, MGQ, BETA, GAMMA, DELTA, IS(IN),
     2   MMAX, HFRACT, EGIVE, SAVEB, KMAX, XPOLY, KINIT, ERROR, ERRPAR  
     3   , ERPUTS, INMI, SCALE, HANDLE)                                 
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTR(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,      
     1   AF, BC, D, ERROR, ERRPAR, INMI, SCALE, HANDLE)                 
      EXTERNAL AF, BC, D, ERROR, INMI, SCALE                            
      EXTERNAL HANDLE                                                   
      INTEGER NU, K, NX, NV                                             
      REAL ERRPAR(2)                                                    
      DOUBLE PRECISION U(1), X(1), V(1), TSTART, TSTOP, DT              
      INTEGER MGQ, NERROR, MAX0, I, N(100), KEEJAC                      
      INTEGER IMGQ, KMAX, MMAX, IZAP, NERR, SAVEB                       
      INTEGER KINIT, MINIT, MAXIT                                       
      REAL HFRACT, RZAP, EGIVE                                          
      LOGICAL USENFD, USENGJ, USENNS, ERPUTS, LZAP, XPOLY               
      DOUBLE PRECISION BETA, FZAP, T0, T1, GAMMA, DELTA                 
      DOUBLE PRECISION THETA                                            
C THE ROUTINE LEVEL OF POST.                                            
C SCRATCH SPACE ALLOCATED -                                             
C     S(DPOST3) = 10*NU**2 + 8*NU + 2*NU*(NV+1) + S(D9OSTS) +           
C                 IF ( KEEPJAC > 0 ) { 2*K*MGQ*(NX-2*K+1) }             
C LONG REAL WORDS +                                                     
C                 MMAX                                                  
C INTEGER WORDS.                                                        
C U(NX-K,NU),X(NX),V(NV).                                               
      CALL ENTER(1)                                                     
C RETRIEVE THE VALUES TO BE USED.                                       
      CALL DPOSTW(-1, THETA, RZAP, IZAP, LZAP)                          
      CALL DPOSTW(-2, BETA, RZAP, IZAP, LZAP)                           
      CALL DPOSTW(-3, GAMMA, RZAP, IZAP, LZAP)                          
      CALL DPOSTW(-4, DELTA, RZAP, IZAP, LZAP)                          
      CALL DPOSTW(-1001, FZAP, HFRACT, IZAP, LZAP)                      
      CALL DPOSTW(-1002, FZAP, EGIVE, IZAP, LZAP)                       
      CALL DPOSTW(-2001, FZAP, RZAP, KEEJAC, LZAP)                      
      CALL DPOSTW(-2002, FZAP, RZAP, MINIT, LZAP)                       
      CALL DPOSTW(-2003, FZAP, RZAP, MAXIT, LZAP)                       
      CALL DPOSTW(-2004, FZAP, RZAP, KMAX, LZAP)                        
      CALL DPOSTW(-2005, FZAP, RZAP, KINIT, LZAP)                       
      CALL DPOSTW(-2006, FZAP, RZAP, MMAX, LZAP)                        
      CALL DPOSTW(-2007, FZAP, RZAP, MGQ, LZAP)                         
      CALL DPOSTW(-2008, FZAP, RZAP, SAVEB, LZAP)                       
      CALL DPOSTW(-3001, FZAP, RZAP, IZAP, XPOLY)                       
      CALL DPOSTW(-3002, FZAP, RZAP, IZAP, ERPUTS)                      
      CALL DPOSTW(-3003, FZAP, RZAP, IZAP, USENGJ)                      
      CALL DPOSTW(-3004, FZAP, RZAP, IZAP, USENNS)                      
      CALL DPOSTW(-3005, FZAP, RZAP, IZAP, USENFD)                      
C TEST FOR ERRORS.                                                      
C/6S                                                                    
C     IF (KMAX .LT. 1) CALL SETERR(18HDPOST4 - KMAX.LT.1, 18, 13, 2)    
C     IF (MMAX .LT. KMAX+2) CALL SETERR(23HDPOST4 - MMAX.LT.KMAX+2, 23  
C    1   , 23, 2)                                                       
C/7S                                                                    
      IF (KMAX .LT. 1) CALL SETERR('DPOST4 - KMAX.LT.1', 18, 13, 2)     
      IF (MMAX .LT. KMAX+2) CALL SETERR('DPOST4 - MMAX.LT.KMAX+2', 23   
     1   , 23, 2)                                                       
C/                                                                      
      DO  1 I = 1, MMAX                                                 
         CALL DPOSTW(-(I+4000), FZAP, RZAP, N(I), LZAP)                 
   1     CONTINUE                                                       
C TEST N FOR MONOTONICITY.                                              
      DO  2 I = 2, MMAX                                                 
C/6S                                                                    
C        IF (N(I) .LE. N(I-1)) CALL SETERR(                             
C    1      37HDPOST4 - N IS NOT MONOTONE INCREASING, 37, 25, 2)        
C/7S                                                                    
         IF (N(I) .LE. N(I-1)) CALL SETERR(                             
     1      'DPOST4 - N IS NOT MONOTONE INCREASING', 37, 25, 2)         
C/                                                                      
   2     CONTINUE                                                       
      T0 = TSTART                                                       
      T1 = TSTOP                                                        
      IMGQ = MAX0(K-1, MGQ)                                             
         GOTO  4                                                        
   3     IMGQ = IMGQ+1                                                  
   4     IF (IMGQ .GT. MAX0(K, MGQ)) GOTO  5                            
C LOOP TILL GQ WORKS.                                                   
         CALL DPOST4(U, NU, K, X, NX, V, NV, T0, T1, DT, AF, BC, D,     
     1      THETA, KEEJAC, MINIT, MAXIT, IMGQ, BETA, GAMMA, DELTA, N,   
     2      MMAX, HFRACT, EGIVE, SAVEB, KMAX, XPOLY, KINIT, ERROR,      
     3      ERRPAR, ERPUTS, INMI, SCALE, HANDLE)                        
         IF (NERROR(NERR) .NE. 1011) GOTO  5                            
         CALL ERROFF                                                    
         T0 = T1                                                        
         T1 = TSTOP                                                     
         GOTO  3                                                        
   5  TSTOP = T1                                                        
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOST4(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,      
     1   AF, BC, D, THETA, KEEJAC, MINIT, MAXIT, MGQ, BETA, GAMMA,      
     2   DELTA, N, MMAX, HFRACT, EGIVE, SAVEB, KMAX, XPOLY, KINIT,      
     3   ERROR, ERRPAR, ERPUTS, INMI, SCALE, HANDLE)                    
      INTEGER MMAX                                                      
      EXTERNAL AF, BC, D, ERROR, INMI, SCALE                            
      EXTERNAL HANDLE                                                   
      INTEGER NU, K, NX, NV, KEEJAC, MINIT                              
      INTEGER MAXIT, MGQ, N(MMAX), SAVEB, KMAX, KINIT                   
      REAL HFRACT, EGIVE, ERRPAR(2)                                     
      LOGICAL XPOLY, ERPUTS                                             
      DOUBLE PRECISION U(1), X(1), V(1), TSTART, TSTOP, DT              
      DOUBLE PRECISION THETA, BETA, GAMMA, DELTA                        
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /DPOSTF/ FAILED                                            
      LOGICAL FAILED                                                    
      COMMON /D90STY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL RV(30)                                                       
      LOGICAL LV(20)                                                    
      DOUBLE PRECISION WV(30)                                           
      COMMON /D90STV/ IEU                                               
      INTEGER IEU                                                       
      COMMON /D90STT/ TGOOD                                             
      DOUBLE PRECISION TGOOD                                            
      COMMON /D90STR/ STATS                                             
      INTEGER STATS(8)                                                  
      COMMON /D90STQ/ IXGQ, IWGQ, MGQQ                                  
      INTEGER IXGQ, IWGQ, MGQQ                                          
      COMMON /D90STK/ NUU, NVV, KK, NXX                                 
      INTEGER NUU, NVV, KK, NXX                                         
      COMMON /D90STB/ IGSSIS, SET                                       
      INTEGER IGSSIS                                                    
      LOGICAL SET                                                       
      COMMON /D9OSTT/ TC, DTC                                           
      DOUBLE PRECISION TC, DTC                                          
      COMMON /D9OSTS/ IMEM                                              
      INTEGER IMEM(4)                                                   
      COMMON /D9OSTM/ THETAC, EGIVEC, MINITC, MAXITC, KEEACC            
      INTEGER MINITC, MAXITC, KEEACC                                    
      REAL EGIVEC                                                       
      DOUBLE PRECISION THETAC                                           
      COMMON /D9OSTL/ ERPTSC                                            
      LOGICAL ERPTSC                                                    
      COMMON /D9OSTK/ IUTETA, IVTETA, IUT, IVT                          
      INTEGER IUTETA, IVTETA, IUT, IVT                                  
      COMMON /D9OSTJ/ IJP, IB, IAFB, IALFA, IBETA, IGAMMA, ID4, ID5,    
     1   IORDER, IBC, IEQS, IAA, IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT,  
     2   IDMAT, IDIAG, IDPVOT                                           
      INTEGER IJP(3), IB(3), IAFB(3), IALFA(3), IBETA(3), IGAMMA(3)     
      INTEGER ID4(3), ID5(3), IORDER, IBC, IEQS, IAA                    
      INTEGER IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT                      
      INTEGER IDMAT, IDIAG, IDPVOT                                      
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      EXTERNAL D9OSTH, D9OSTN, D9OSTP, D9OSTA, D9OSTB, D9OSTD           
      EXTERNAL D9OSTE                                                   
      INTEGER ISTKGT, NERROR, I, NERR, IS(1000)                         
      REAL RS(1000)                                                     
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION DABS, WS(500)                                    
      INTEGER TEMP, TEMP2                                               
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE BOTTOM LEVEL OF POSTS.                                            
C SCRATCH SPACE ALLOCATED -                                             
C     S(DPOST4) = 10*NU**2 + 8*NU + 2*NU*(NV+1) + S(D9OSTS) +           
C                 IF ( KEEPJAC > 0 ) { 2*K*MGQ*(NX-2*K+1) }             
C LONG REAL WORDS +                                                     
C                 MMAX                                                  
C INTEGER WORDS.                                                        
C U(NX-K,NU),X(NX),V(NV).                                               
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      IF (TSTART .EQ. TSTOP) RETURN                                     
      CALL ENTER(1)                                                     
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (NU .LT. 0) CALL SETERR(16HDPOST4 - NU.LT.0, 16, 1, 2)         
C     IF (NV .LT. 0) CALL SETERR(16HDPOST4 - NV.LT.0, 16, 2, 2)         
C/7S                                                                    
      IF (NU .LT. 0) CALL SETERR('DPOST4 - NU.LT.0', 16, 1, 2)          
      IF (NV .LT. 0) CALL SETERR('DPOST4 - NV.LT.0', 16, 2, 2)          
C/                                                                      
      TEMP1 = NU .EQ. NV                                                
      IF (TEMP1) TEMP1 = NV .EQ. 0                                      
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(16HDPOST4 - NU=NV=0, 16, 3, 2)             
C/7S                                                                    
      IF (TEMP1) CALL SETERR('DPOST4 - NU=NV=0', 16, 3, 2)              
C/                                                                      
      TEMP1 = NU .GT. 0                                                 
      IF (TEMP1) TEMP1 = K .LT. 2                                       
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(32HDPOST4 - K.LT.2 WHEN NU POSITIVE, 32, 4,
C    1   2)                                                             
C/7S                                                                    
      IF (TEMP1) CALL SETERR('DPOST4 - K.LT.2 WHEN NU POSITIVE', 32, 4, 
     1   2)                                                             
C/                                                                      
      TEMP1 = NU .GT. 0                                                 
      IF (TEMP1) TEMP1 = NX .LT. 2*K                                    
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(35HDPOST4 - NX.LT.2*K WHEN NU POSITIVE, 35,
C    1   5, 2)                                                          
C     IF (TSTART+DT .EQ. TSTART) CALL SETERR(                           
C    1   31HDPOST4 - INPUT VALUE OF DT IS 0, 31, 6, 2)                  
C     IF ((DT/DABS(DT))*(TSTOP-TSTART) .LE. 0D0) CALL SETERR(           
C    1   45HDPOST4 - INPUT VALUE OF DT HAS THE WRONG SIGN, 45, 7, 2)    
C/7S                                                                    
      IF (TEMP1) CALL SETERR('DPOST4 - NX.LT.2*K WHEN NU POSITIVE', 35, 
     1   5, 2)                                                          
      IF (TSTART+DT .EQ. TSTART) CALL SETERR(                           
     1   'DPOST4 - INPUT VALUE OF DT IS 0', 31, 6, 2)                   
      IF ((DT/DABS(DT))*(TSTOP-TSTART) .LE. 0D0) CALL SETERR(           
     1   'DPOST4 - INPUT VALUE OF DT HAS THE WRONG SIGN', 45, 7, 2)     
C/                                                                      
C ???                                                                   
      TEMP1 = THETA .LT. 0D0                                            
      IF (.NOT. TEMP1) TEMP1 = THETA .GT. 1D0                           
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(27HDPOST4 - THETA NOT IN (0,1), 27, 8, 2)  
C/7S                                                                    
      IF (TEMP1) CALL SETERR('DPOST4 - THETA NOT IN (0,1)', 27, 8, 2)   
C/                                                                      
C ???                                                                   
      TEMP1 = KEEJAC .LT. 0                                             
      IF (.NOT. TEMP1) TEMP1 = KEEJAC .GT. 5                            
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(37HDPOST4 - KEEPJAC NOT ONE OF (0,...,5),  
C    1   37, 9, 2)                                                      
C     IF (MINIT .LT. 1) CALL SETERR(19HDPOST4 - MINIT.LT.1, 19, 10, 2)  
C     IF (MAXIT .LT. 1) CALL SETERR(19HDPOST4 - MAXIT.LT.1, 19, 11, 2)  
C/7S                                                                    
      IF (TEMP1) CALL SETERR('DPOST4 - KEEPJAC NOT ONE OF (0,...,5)',   
     1   37, 9, 2)                                                      
      IF (MINIT .LT. 1) CALL SETERR('DPOST4 - MINIT.LT.1', 19, 10, 2)   
      IF (MAXIT .LT. 1) CALL SETERR('DPOST4 - MAXIT.LT.1', 19, 11, 2)   
C/                                                                      
      TEMP1 = NU .GT. 0                                                 
      IF (TEMP1) TEMP1 = MGQ .LT. 1                                     
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(37HDPOST4 - MGQ.LT.1 WHEN NU IS POSITIVE,  
C    1   37, 12, 2)                                                     
C     IF (KMAX .LT. 1) CALL SETERR(18HDPOST4 - KMAX.LT.1, 18, 13, 2)    
C     IF (KINIT .LT. 1) CALL SETERR(19HDPOST4 - KINIT.LT.1, 19, 14, 2)  
C/7S                                                                    
      IF (TEMP1) CALL SETERR('DPOST4 - MGQ.LT.1 WHEN NU IS POSITIVE',   
     1   37, 12, 2)                                                     
      IF (KMAX .LT. 1) CALL SETERR('DPOST4 - KMAX.LT.1', 18, 13, 2)     
      IF (KINIT .LT. 1) CALL SETERR('DPOST4 - KINIT.LT.1', 19, 14, 2)   
C/                                                                      
      IF (NU .LE. 0) GOTO 3                                             
         DO  1 I = 1, K                                                 
C/6S                                                                    
C           IF (X(I) .NE. X(1)) CALL SETERR(                            
C    1         35HDPOST4 - X(1) NOT OF MULTIPLICITY K, 35, 15, 2)       
C/7S                                                                    
            IF (X(I) .NE. X(1)) CALL SETERR(                            
     1         'DPOST4 - X(1) NOT OF MULTIPLICITY K', 35, 15, 2)        
C/                                                                      
            TEMP = NX-K+I                                               
C/6S                                                                    
C           IF (X(TEMP) .NE. X(NX)) CALL SETERR(                        
C    1         36HDPOST4 - X(NX) NOT OF MULTIPLICITY K, 36, 16, 2)      
C/7S                                                                    
            IF (X(TEMP) .NE. X(NX)) CALL SETERR(                        
     1         'DPOST4 - X(NX) NOT OF MULTIPLICITY K', 36, 16, 2)       
C/                                                                      
   1        CONTINUE                                                    
         TEMP = NX-K                                                    
         DO  2 I = K, TEMP                                              
C/6S                                                                    
C           IF (X(I) .GT. X(I+1)) CALL SETERR(                          
C    1         34HDPOST4 - X NOT MONOTONE INCREASING, 34, 17, 2)        
C/7S                                                                    
            IF (X(I) .GT. X(I+1)) CALL SETERR(                          
     1         'DPOST4 - X NOT MONOTONE INCREASING', 34, 17, 2)         
C/                                                                      
            IF (I+K .GT. NX) GOTO  2                                    
            TEMP2 = I+K                                                 
C/6S                                                                    
C           IF (X(TEMP2) .LE. X(I)) CALL SETERR(                        
C    1         34HDPOST4 - X NOT MONOTONE INCREASING, 34, 17, 2)        
C/7S                                                                    
            IF (X(TEMP2) .LE. X(I)) CALL SETERR(                        
     1         'DPOST4 - X NOT MONOTONE INCREASING', 34, 17, 2)         
C/                                                                      
   2        CONTINUE                                                    
C/6S                                                                    
C  3  IF (BETA .LE. 0D0) CALL SETERR(19HDPOST4 - BETA .LE.0, 19, 19, 2) 
C     IF (GAMMA .LE. 0D0) CALL SETERR(20HDPOST4 - GAMMA .LE.0, 20, 20, 2
C    1   )                                                              
C     IF (DELTA .LT. 0D0) CALL SETERR(20HDPOST4 - DELTA .LT.0, 20, 21, 2
C    1   )                                                              
C     IF (BETA+GAMMA-DELTA .LE. 0D0) CALL SETERR(                       
C    1   30HDPOST4 - BETA+GAMMA-DELTA.LE.0, 30, 22, 2)                  
C     IF (MMAX .LT. KMAX+2) CALL SETERR(23HDPOST4 - MMAX.LT.KMAX+2, 23  
C    1   , 23, 2)                                                       
C     IF (N(1) .LT. 1) CALL SETERR(18HDPOST4 - N(1).LT.1, 18, 24, 2)    
C/7S                                                                    
   3  IF (BETA .LE. 0D0) CALL SETERR('DPOST4 - BETA .LE.0', 19, 19, 2)  
      IF (GAMMA .LE. 0D0) CALL SETERR('DPOST4 - GAMMA .LE.0', 20, 20, 2 
     1   )                                                              
      IF (DELTA .LT. 0D0) CALL SETERR('DPOST4 - DELTA .LT.0', 20, 21, 2 
     1   )                                                              
      IF (BETA+GAMMA-DELTA .LE. 0D0) CALL SETERR(                       
     1   'DPOST4 - BETA+GAMMA-DELTA.LE.0', 30, 22, 2)                   
      IF (MMAX .LT. KMAX+2) CALL SETERR('DPOST4 - MMAX.LT.KMAX+2', 23   
     1   , 23, 2)                                                       
      IF (N(1) .LT. 1) CALL SETERR('DPOST4 - N(1).LT.1', 18, 24, 2)     
C/                                                                      
      DO  4 I = 2, MMAX                                                 
C/6S                                                                    
C        IF (N(I) .LE. N(I-1)) CALL SETERR(                             
C    1      37HDPOST4 - N IS NOT MONOTONE INCREASING, 37, 25, 2)        
C/7S                                                                    
         IF (N(I) .LE. N(I-1)) CALL SETERR(                             
     1      'DPOST4 - N IS NOT MONOTONE INCREASING', 37, 25, 2)         
C/                                                                      
   4     CONTINUE                                                       
C/6S                                                                    
C     IF (HFRACT .LE. 0.) CALL SETERR(20HDPOST4 - HFRACT.LE.0, 20, 26, 2
C    1   )                                                              
C     IF (EGIVE .LT. 0.) CALL SETERR(21HDPOST4 - EGIVE .LT. 0, 21, 27, 2
C    1   )                                                              
C/7S                                                                    
      IF (HFRACT .LE. 0.) CALL SETERR('DPOST4 - HFRACT.LE.0', 20, 26, 2 
     1   )                                                              
      IF (EGIVE .LT. 0.) CALL SETERR('DPOST4 - EGIVE .LT. 0', 21, 27, 2 
     1   )                                                              
C/                                                                      
      ERPTSC = ERPUTS                                                   
      THETAC = THETA                                                    
      MINITC = MINIT                                                    
      MAXITC = MAXIT                                                    
      KEEACC = KEEJAC                                                   
      TEMP1 = KEEJAC .EQ. 1                                             
      IF (TEMP1) TEMP1 = MAXIT .EQ. 1                                   
      IF (TEMP1) KEEACC = 0                                             
      IF (SAVEB .LE. 0) GOTO 5                                          
         IGSSIS = ISTKGT(2*K*MGQ*(NX-2*K+1), 4)                         
         GOTO  10                                                       
   5     IF (SAVEB .GE. 0) GOTO 6                                       
            IGSSIS = 1                                                  
            GOTO  9                                                     
   6        TEMP1 = NU .GT. 0                                           
            IF (TEMP1) TEMP1 = KEEACC .GT. 1                            
            IF (.NOT. TEMP1) GOTO 7                                     
               IGSSIS = ISTKGT(2*K*MGQ*(NX-2*K+1), 4)                   
               GOTO  8                                                  
   7           IGSSIS = 1                                               
   8        CONTINUE                                                    
   9  CONTINUE                                                          
  10  SET = .FALSE.                                                     
      IF (KEEACC .LE. 1) GOTO 11                                        
         SEPATE = .TRUE.                                                
         GOTO  12                                                       
  11     SEPATE = .FALSE.                                               
  12  IF (KEEACC .LT. 3) GOTO 13                                        
         GETJAC = .TRUE.                                                
         TJ = TSTART                                                    
         GOTO  18                                                       
  13     IF (KEEACC .NE. 2) GOTO 14                                     
            TJ = TSTOP                                                  
C CANNOT BE TSTART.                                                     
            GOTO  17                                                    
  14        IF (THETA .LE. 0.5) GOTO 15                                 
               TJ = TSTART                                              
C CANNOT BE TSTART+THETA*DT/N.                                          
               GOTO  16                                                 
  15           TJ = TSTOP                                               
  16     CONTINUE                                                       
C CANNOT BE TSTART.                                                     
  17     CONTINUE                                                       
  18  DTJ = 0                                                           
C START WITH NO ERROR STATES.                                           
      FNUM = 0                                                          
      IORDER = ISTKGT(2*NU**2, 2)                                       
      IBC = ISTKGT(8*NU, 2)                                             
      IEQS = IBC+4*NU                                                   
      IF (NU .LE. 0) GOTO 19                                            
         CALL SETI(4*NU, 1, IS(IEQS))                                   
         CALL SETI(4*NU, -2, IS(IBC))                                   
  19  IAFB(1) = ISTKGT(6*NU*(2*NU+NV+1), 4)                             
      IAFB(2) = IAFB(1)+2*NU*(2*NU+NV+1)                                
      IAFB(3) = IAFB(2)+2*NU*(2*NU+NV+1)                                
      IDMAT = ISTKGT(NV**2, 4)                                          
      IALFA(1) = ISTKGT(6*NU*(2*NU+NV+1), 4)                            
      IBETA(1) = IALFA(1)+2*NU**2                                       
      IGAMMA(1) = IBETA(1)+2*NU**2                                      
      IALFA(2) = IGAMMA(1)+2*NU*(NV+1)                                  
      IBETA(2) = IALFA(2)+2*NU**2                                       
      IGAMMA(2) = IBETA(2)+2*NU**2                                      
      IALFA(3) = IGAMMA(2)+2*NU*(NV+1)                                  
      IBETA(3) = IALFA(3)+2*NU**2                                       
      IGAMMA(3) = IBETA(3)+2*NU**2                                      
      IL = 1                                                            
      IPPVOT = 1                                                        
      IDIAG = ISTKGT(NV, 4)                                             
      IDPVOT = ISTKGT(NV, 2)                                            
      IAA = ISTKGT(2*NU*(3*NU+2*(NV+1)), 4)                             
      IBB = IAA+2*NU**2                                                 
      ICC = IBB+2*NU**2                                                 
      ISGMAD = ICC+2*NU**2                                              
      ISGMAM = ISGMAD+2*NU*(NV+1)                                       
      ID4(1) = ISTKGT(3*NV**2, 4)                                       
      ID4(2) = ID4(1)+NV**2                                             
      ID4(3) = ID4(2)+NV**2                                             
      IF (.NOT. SEPATE) GOTO 20                                         
         IJP(1) = ISTKGT(3*(2*K*NU-1)*(NX-K)*NU, 4)                     
         IJP(2) = IJP(1)+(2*K*NU-1)*(NX-K)*NU                           
         IJP(3) = IJP(2)+(2*K*NU-1)*(NX-K)*NU                           
         IB(1) = ISTKGT(3*NU*(NX-K)*(NV+1), 4)                          
         IB(2) = IB(1)+NU*(NX-K)*(NV+1)                                 
         IB(3) = IB(2)+NU*(NX-K)*(NV+1)                                 
         ID5(1) = ISTKGT(3*NU*(NX-K)*NV, 4)                             
         ID5(2) = ID5(1)+NU*(NX-K)*NV                                   
         ID5(3) = ID5(2)+NU*(NX-K)*NV                                   
         IL = ISTKGT((K*NU-1)*(NX-K)*NU, 4)                             
         IPPVOT = ISTKGT((NX-K)*NU, 2)                                  
         GOTO  22                                                       
  20     CALL SETI(3, 1, IJP)                                           
         CALL SETI(3, 1, IB)                                            
         IF (KEEACC .NE. 1) GOTO 21                                     
            IJP(1) = ISTKGT(NU*(NX-K)*(2*K*NU-1), 4)                    
            IJP(3) = IJP(1)                                             
            IL = ISTKGT(NU*(NX-K)*(K*NU-1), 4)                          
            IB(1) = ISTKGT(NU*(NX-K)*(NV+1), 4)                         
            IB(3) = IB(1)                                               
            IPPVOT = ISTKGT(NU*(NX-K), 2)                               
  21     IAFB(3) = IAFB(1)                                              
         IALFA(3) = IALFA(1)                                            
         IBETA(3) = IBETA(1)                                            
         IGAMMA(3) = IGAMMA(1)                                          
         ID4(3) = ID4(1)                                                
         ID5(1) = ISTKGT(2*NU*(NX-K)*NV, 4)                             
         ID5(2) = ID5(1)+NU*(NX-K)*NV                                   
         ID5(3) = ID5(1)                                                
C FLAG SCALING WORK-SPACE AS UN-ALLOCATED.                              
  22  CALL SETI(4, 0, IMEM)                                             
C GET SPACE FOR PDE SCALING.                                            
      TEMP = IJP(3)                                                     
      CALL SCALE(1, 1, WS(TEMP), NU*(NX-K), 2*K*NU-1)                   
C GET SPACE FOR DIRICHLET BC SCALING.                                   
      CALL SCALE(2, 1, WS(IAA), NU, NU)                                 
C GET SPACE FOR NEUMAN BC SCALING.                                      
      CALL SCALE(3, 1, WS(IAA), NU, NU)                                 
C GET SPACE FOR ODE SCALING.                                            
      TEMP = ID4(3)                                                     
      CALL SCALE(4, 1, WS(TEMP), NV, NV)                                
      DO  23 I = 1, 4                                                   
C/6S                                                                    
C        IF (IMEM(I) .LE. 0) CALL SETERR(                               
C    1      51HDPOST4 - SCALE FAILED TO INITIALIZE COMMON /D9OSTS/, 51  
C    2      , 18, 2)                                                    
C/7S                                                                    
         IF (IMEM(I) .LE. 0) CALL SETERR(                               
     1      'DPOST4 - SCALE FAILED TO INITIALIZE COMMON /D9OSTS/', 51   
     2      , 18, 2)                                                    
C/                                                                      
  23     CONTINUE                                                       
      EGIVEC = EGIVE                                                    
      TGOOD = TSTART                                                    
      IF (NU .LE. 0) GOTO 24                                            
         IXGQ = ISTKGT(2*MGQ, 4)                                        
         IWGQ = IXGQ+MGQ                                                
         MGQQ = MGQ                                                     
         CALL DGQ1(MGQ, WS(IXGQ), WS(IWGQ))                             
         GOTO  25                                                       
  24     IXGQ = 1                                                       
         IWGQ = 1                                                       
         MGQQ = 0                                                       
  25  NUU = NU                                                          
      NVV = NV                                                          
      IF (NU .NE. 0) GOTO 26                                            
         KK = 2                                                         
         NXX = 2*KK                                                     
         GOTO  27                                                       
  26     KK = K                                                         
         NXX = NX                                                       
C TELL STATS ROUTINE IN POST.                                           
  27  CALL D9OSTX(STATS, 1)                                             
      CALL D90STS(U, NU, KK, X, NXX, V, NV, TSTART, TSTOP, DT, D9OSTA,  
     1   AF, D9OSTB, BC, D9OSTD, D, D9OSTE, ERROR, ERRPAR, INMI, SCALE  
     2   , D9OSTH, HANDLE, BETA, GAMMA, DELTA, N, KMAX, MMAX, XPOLY,    
     3   KINIT, HFRACT, D9OSTP, D9OSTN)                                 
C TELL STATS ROUTINE OUT OF POST.                                       
      CALL D9OSTX(STATS, -1)                                            
      TSTOP = TGOOD                                                     
C CAPTURE THE ERROR NUMBER, IF ANY.                                     
      NERR = NERROR(NERR)                                               
      IF (NERR .NE. 15) GOTO 28                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(13HDPOST4 - DT=0, 13, 1000, 1)                     
C/7S                                                                    
         CALL SETERR('DPOST4 - DT=0', 13, 1000, 1)                      
C/                                                                      
  28  IF (NERR .NE. 16) GOTO 29                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(32HDPOST4 - DT=0 RETURNED BY HANDLE, 32, 1001, 1)  
C/7S                                                                    
         CALL SETERR('DPOST4 - DT=0 RETURNED BY HANDLE', 32, 1001, 1)   
C/                                                                      
  29  IF (NERR .NE. 17) GOTO 30                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(45HDPOST4 - DT RETURNED BY HANDLE HAS WRONG SIGN,  
C    1      45, 1002, 1)                                                
C/7S                                                                    
         CALL SETERR('DPOST4 - DT RETURNED BY HANDLE HAS WRONG SIGN',   
     1      45, 1002, 1)                                                
C/                                                                      
  30  IF (NERR .NE. 18) GOTO 31                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(46HDPOST4 - CANNOT RAISE DT IN HANDLE WHEN FAILED  
C    1      , 46, 1003, 1)                                              
C/7S                                                                    
         CALL SETERR('DPOST4 - CANNOT RAISE DT IN HANDLE WHEN FAILED'   
     1      , 46, 1003, 1)                                              
C/                                                                      
  31  IF (NERR .NE. 19) GOTO 32                                         
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(36HDPOST4 - E(I).LE.0 RETURNED BY ERROR, 36, 1004  
C    1      , 1)                                                        
C/7S                                                                    
         CALL SETERR('DPOST4 - E(I).LE.0 RETURNED BY ERROR', 36, 1004   
     1      , 1)                                                        
C/                                                                      
  32  IF (NERR .NE. 15) GOTO 42                                         
         IF (FNUM .NE. 1) GOTO 33                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(19HDPOST4 - AF FAILURE, 19, 1013, 1)            
C/7S                                                                    
            CALL SETERR('DPOST4 - AF FAILURE', 19, 1013, 1)             
C/                                                                      
  33     IF (FNUM .NE. 2) GOTO 34                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(19HDPOST4 - BC FAILURE, 19, 1014, 1)            
C/7S                                                                    
            CALL SETERR('DPOST4 - BC FAILURE', 19, 1014, 1)             
C/                                                                      
  34     IF (FNUM .NE. 3) GOTO 35                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(18HDPOST4 - D FAILURE, 18, 1015, 1)             
C/7S                                                                    
            CALL SETERR('DPOST4 - D FAILURE', 18, 1015, 1)              
C/                                                                      
  35     IF (FNUM .NE. 4) GOTO 36                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(                                                
C    1         47HDPOST4 - SINGULAR DIRICHLET BOUNDARY CONDITIONS, 47,  
C    2         1016, 1)                                                 
C/7S                                                                    
            CALL SETERR(                                                
     1         'DPOST4 - SINGULAR DIRICHLET BOUNDARY CONDITIONS', 47,   
     2         1016, 1)                                                 
C/                                                                      
  36     IF (FNUM .NE. 5) GOTO 37                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(43HDPOST4 - SINGULAR MIXED BOUNDARY CONDITIONS  
C    1         , 43, 1017, 1)                                           
C/7S                                                                    
            CALL SETERR('DPOST4 - SINGULAR MIXED BOUNDARY CONDITIONS'   
     1         , 43, 1017, 1)                                           
C/                                                                      
  37     IF (FNUM .NE. 6) GOTO 38                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(30HDPOST4 - SINGULAR PDE JACOBIAN, 30, 1018, 1) 
C/7S                                                                    
            CALL SETERR('DPOST4 - SINGULAR PDE JACOBIAN', 30, 1018, 1)  
C/                                                                      
  38     IF (FNUM .NE. 7) GOTO 39                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(30HDPOST4 - SINGULAR ODE JACOBIAN, 30, 1019, 1) 
C/7S                                                                    
            CALL SETERR('DPOST4 - SINGULAR ODE JACOBIAN', 30, 1019, 1)  
C/                                                                      
  39     IF (FNUM .NE. 8) GOTO 40                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(                                                
C    1         45HDPOST4 - TOO MANY NEWTON ITERATIONS PREDICTED, 45,    
C    2         1020, 1)                                                 
C/7S                                                                    
            CALL SETERR(                                                
     1         'DPOST4 - TOO MANY NEWTON ITERATIONS PREDICTED', 45,     
     2         1020, 1)                                                 
C/                                                                      
  40     IF (FNUM .NE. 9) GOTO 41                                       
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(42HDPOST4 - TOO MANY NEWTON ITERATIONS NEEDED,  
C    1         42, 1021, 1)                                             
C/7S                                                                    
            CALL SETERR('DPOST4 - TOO MANY NEWTON ITERATIONS NEEDED',   
     1         42, 1021, 1)                                             
C/                                                                      
  41     CONTINUE                                                       
  42  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTH(T0, U0, V0, T, U, V, NU, NXMK, NV, K, X,        
     1   NX, DT, TSTOP)                                                 
      INTEGER NXMK, NX                                                  
      INTEGER NU, NV, K                                                 
      DOUBLE PRECISION T0, U0(NXMK, 1), V0(1), T, U(NXMK, 1), V(1)      
      DOUBLE PRECISION X(NX), DT, TSTOP                                 
C DEFAULT HANDLE PROCEDURE FOR POSTS.                                   
C SCRATCH SPACE ALLOCATED - NONE.                                       
C LONG REAL (U0,U)(NXMK,NU),(V0,V)(NV).                                 
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION DPOSTE(U, NU, NXMK, K, X, NX, V, NV, T, DT,      
     1   ERRPAR, ERPUTS, EU, EV)                                        
      INTEGER NXMK, NX                                                  
      INTEGER NU, K, NV                                                 
      REAL ERRPAR(2), EU(NXMK, 1), EV(1)                                
      LOGICAL ERPUTS                                                    
      DOUBLE PRECISION U(NXMK, 1), X(NX), V(1), T, DT                   
      INTEGER I, J                                                      
      REAL ABS, EMAX, TEMP, AMAX1, DTPOW                                
      LOGICAL CONGED                                                    
      DOUBLE PRECISION DABS, DMAX1, UNORM                               
C THE STANDARD ERROR PROCEDURE FOR DPOSTS.                              
C SCRATCH SPACE ALLOCATED - NONE.                                       
C U(NXMK,NU),V(NV).                                                     
C EU(NXMK,NU),EV(NV).                                                   
      IF (.NOT. ERPUTS) GOTO 1                                          
         DTPOW = DABS(DT)                                               
         GOTO  2                                                        
   1     DTPOW = 1                                                      
   2  CONGED = .TRUE.                                                   
      I = 1                                                             
         GOTO  4                                                        
   3     I = I+1                                                        
   4     IF (I .GT. NV) GOTO  5                                         
C ERROR FOR V.                                                          
         TEMP = DTPOW*(ERRPAR(1)*DABS(V(I))+ERRPAR(2))                  
         IF (TEMP .LT. EV(I)) CONGED = .FALSE.                          
         EV(I) = TEMP                                                   
         GOTO  3                                                        
   5  J = 1                                                             
         GOTO  7                                                        
   6     J = J+1                                                        
   7     IF (J .GT. NU) GOTO  9                                         
C FIND || U(J) || AND || EU(J) || .                                     
         EMAX = 0                                                       
         UNORM = 0                                                      
         DO  8 I = 1, NXMK                                              
            EMAX = AMAX1(EMAX, ABS(EU(I, J)))                           
            UNORM = DMAX1(UNORM, DABS(U(I, J)))                         
   8        CONTINUE                                                    
         TEMP = DTPOW*(ERRPAR(1)*UNORM+ERRPAR(2))                       
         IF (TEMP .LT. EMAX) CONGED = .FALSE.                           
         CALL SETR(NXMK, TEMP, EU(1, J))                                
         GOTO  6                                                        
   9  DPOSTE = CONGED                                                   
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION DPOSTC(U, NU, NXMK, K, X, NX, V, NV, T, DT,      
     1   ERRPAR, ERPUTS, EU, EV)                                        
      INTEGER NXMK, NX                                                  
      INTEGER NU, K, NV                                                 
      REAL ERRPAR(2), EU(NXMK, 1), EV(1)                                
      LOGICAL ERPUTS                                                    
      DOUBLE PRECISION U(NXMK, 1), X(NX), V(1), T, DT                   
      INTEGER I, J                                                      
      REAL TEMP, DTPOW                                                  
      LOGICAL CONGED                                                    
      DOUBLE PRECISION DABS                                             
C THE COMPONENT ERROR PROCEDURE FOR DPOSTS.                             
C SCRATCH SPACE ALLOCATED - NONE.                                       
C U(NXMK,NU),V(NV).                                                     
C EU(NXMK,NU),EV(NV).                                                   
      IF (.NOT. ERPUTS) GOTO 1                                          
         DTPOW = DABS(DT)                                               
         GOTO  2                                                        
   1     DTPOW = 1                                                      
   2  CONGED = .TRUE.                                                   
      I = 1                                                             
         GOTO  4                                                        
   3     I = I+1                                                        
   4     IF (I .GT. NV) GOTO  5                                         
C ERROR FOR V.                                                          
         TEMP = DTPOW*(ERRPAR(1)*DABS(V(I))+ERRPAR(2))                  
         IF (TEMP .LT. EV(I)) CONGED = .FALSE.                          
         EV(I) = TEMP                                                   
         GOTO  3                                                        
   5  J = 1                                                             
         GOTO  7                                                        
   6     J = J+1                                                        
   7     IF (J .GT. NU) GOTO  9                                         
C ERROR FOR U.                                                          
         DO  8 I = 1, NXMK                                              
            TEMP = DTPOW*(ERRPAR(1)*DABS(U(I, J))+ERRPAR(2))            
            IF (TEMP .LT. EU(I, J)) CONGED = .FALSE.                    
            EU(I, J) = TEMP                                             
   8        CONTINUE                                                    
         GOTO  6                                                        
   9  DPOSTC = CONGED                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTN(NU, NV, NXMK, K, X, NX, T, DT, UOLD, VOLD,      
     1   U, UT, V, VT)                                                  
      INTEGER NXMK, NX                                                  
      INTEGER NU, NV, K                                                 
      DOUBLE PRECISION X(NX), T, DT, UOLD(NXMK, 1), VOLD(1), U(NXMK, 1) 
      DOUBLE PRECISION UT(NXMK, 1), V(1), VT(1)                         
C THE DEFAULT NEWTON ITERATION INITIALIZER FOR POSTS.                   
C UOLD(NXMK,NU),VOLD(NV).                                               
C (U,UT)(NXMK,NU).                                                      
C (V,VT)(NV).                                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTP(I, J, A, M, N)                                  
      INTEGER I, J, M, N                                                
      DOUBLE PRECISION A(1)                                             
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D9OSTS/ IMEM                                              
      INTEGER IMEM(4)                                                   
      INTEGER ISTKGT, II, IS(1000)                                      
      REAL RS(1000)                                                     
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP1, TEMP2, TEMP3, TEMP4, TEMP5, TEMP6                  
      INTEGER TEMP7, TEMP8                                              
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE DEFAULT LINEAR SYSTEM PRE-CONDITIONING ROUTINE FOR POSTS.         
C A(M,N)                                                                
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      II = I                                                            
      IF (IMEM(I) .NE. 0) GOTO 3                                        
         IMEM(I) = ISTKGT(5, 2)                                         
C INITIALIZE.                                                           
C R, CA, RCA, CB AND RCB.                                               
CR.                                                                     
         TEMP3 = IMEM(II)                                               
         IS(TEMP3) = ISTKGT(M, 4)                                       
C CA.                                                                   
         TEMP3 = IMEM(II)                                               
         IS(TEMP3+1) = ISTKGT(M, 4)                                     
C RCA.                                                                  
         TEMP3 = IMEM(II)                                               
         IS(TEMP3+2) = ISTKGT(M, 2)                                     
         IF (I .NE. 4) GOTO 2                                           
            TEMP3 = IMEM(II)                                            
            IS(TEMP3+3) = ISTKGT(1, 4)                                  
            TEMP3 = IMEM(II)                                            
            IS(TEMP3+4) = ISTKGT(1, 2)                                  
            DO  1 II = 1, 3                                             
               TEMP3 = IMEM(II)                                         
               IS(TEMP3+3) = ISTKGT(M+1, 4)                             
               TEMP3 = IMEM(II)                                         
               IS(TEMP3+4) = ISTKGT(M+1, 2)                             
   1           CONTINUE                                                 
   2     CONTINUE                                                       
         GOTO  10                                                       
   3     TEMP = I .EQ. 1                                                
C SCALE.                                                                
         IF (TEMP) TEMP = J .EQ. 1                                      
         IF (.NOT. TEMP) GOTO 4                                         
            TEMP3 = IMEM(II)                                            
C SCALE THE BANDED PDE JACOBIAN.                                        
            TEMP4 = IS(TEMP3)                                           
            TEMP5 = IMEM(II)                                            
            TEMP6 = IS(TEMP5+1)                                         
            TEMP7 = IMEM(II)                                            
            TEMP8 = IS(TEMP7+2)                                         
            CALL DRCSBA(A, M, N, WS(TEMP4), WS(TEMP6), IS(TEMP8))       
            GOTO  9                                                     
   4        IF (J .NE. 1) GOTO 5                                        
               TEMP8 = IMEM(II)                                         
C SCALE THE DENSE JACOBIAN.                                             
               TEMP7 = IS(TEMP8)                                        
               TEMP6 = IMEM(II)                                         
               TEMP5 = IS(TEMP6+1)                                      
               TEMP4 = IMEM(II)                                         
               TEMP3 = IS(TEMP4+2)                                      
               CALL DRCSA(A, M, N, WS(TEMP7), WS(TEMP5), IS(TEMP3))     
               GOTO  8                                                  
   5           IF (J .NE. 2) GOTO 6                                     
                  TEMP3 = IMEM(II)                                      
C SCALE THE RHS OF THE EQUATIONS.                                       
                  TEMP4 = IS(TEMP3)                                     
                  TEMP5 = IMEM(II)                                      
                  TEMP6 = IS(TEMP5+3)                                   
                  TEMP7 = IMEM(II)                                      
                  TEMP8 = IS(TEMP7+4)                                   
                  CALL DRCSB(A, M, N, WS(TEMP4), WS(TEMP6), IS(TEMP8))  
                  GOTO  7                                               
   6              TEMP8 = IMEM(II)                                      
C SCALE THE SOLUTION X.                                                 
                  TEMP7 = IS(TEMP8+1)                                   
                  TEMP6 = IMEM(II)                                      
                  TEMP5 = IS(TEMP6+2)                                   
                  TEMP4 = IMEM(II)                                      
                  TEMP3 = IS(TEMP4+3)                                   
                  TEMP2 = IMEM(II)                                      
                  TEMP1 = IS(TEMP2+4)                                   
                  CALL DRCSX(A, M, N, WS(TEMP7), IS(TEMP5), WS(TEMP3),  
     1               IS(TEMP1))                                         
   7        CONTINUE                                                    
   8     CONTINUE                                                       
   9     CONTINUE                                                       
  10  RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTW(J, F, R, I, L)                                  
      INTEGER J, I                                                      
      REAL R                                                            
      LOGICAL L                                                         
      DOUBLE PRECISION F                                                
      INTEGER MGQ, MAX0, K, M, N(100), IABS                             
      INTEGER KEEJAC, KMAX, MMAX, SAVEB, KINIT, MINIT                   
      INTEGER MAXIT                                                     
      REAL HFRACT, EGIVE, FLOAT                                         
      LOGICAL USENFD, USENGJ, USENNS, ERPUTS, XPOLY                     
      DOUBLE PRECISION BETA, DBLE, GAMMA, DELTA, THETA, DSQRT           
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
C/7                                                                     
      SAVE K                                                            
C/                                                                      
      DATA K/0/                                                         
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
      DATA MGQ/0/                                                       
      DATA SAVEB/0/                                                     
      DATA XPOLY/.FALSE./                                               
      DATA ERPUTS/.FALSE./                                              
      DATA USENGJ/.FALSE./                                              
      DATA USENNS/.FALSE./                                              
      DATA USENFD/.FALSE./                                              
      DATA N(1)/1/, N(2)/0/, N(3)/0/                                    
C THE PARAMETER SETTING ROUTINE FOR POST.                               
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
C J = 2007. 0 IMPLIES MGQ = K-1 BY DEFAULT.                             
C J = 2008. -1 DO NOT SAVE, 0 DEFAULT, +1 SAVE.                         
C J = 3001.                                                             
C J = 3002.                                                             
C J = 3003.                                                             
C J = 3004.                                                             
C J = 3005.                                                             
C J = 4001, ... , 4100.                                                 
      GOTO  62                                                          
C   EXPORT THE VARIABLES.                                               
   1     F = THETA                                                      
         GOTO  63                                                       
   2     F = BETA                                                       
         GOTO  63                                                       
   3     F = GAMMA                                                      
         GOTO  63                                                       
   4     F = DELTA                                                      
         GOTO  63                                                       
   5     R = HFRACT                                                     
         GOTO  63                                                       
   6     R = EGIVE                                                      
         GOTO  63                                                       
   7     I = KEEJAC                                                     
         GOTO  63                                                       
   8     I = MINIT                                                      
         GOTO  63                                                       
   9     I = MAXIT                                                      
         GOTO  63                                                       
  10     I = KMAX                                                       
         GOTO  63                                                       
  11     I = KINIT                                                      
         GOTO  63                                                       
  12     I = MMAX                                                       
         GOTO  63                                                       
  13     IF (MGQ .NE. 0) GOTO 14                                        
            I = K-1                                                     
            GOTO  15                                                    
  14        I = MGQ                                                     
  15     GOTO  63                                                       
  16     I = SAVEB                                                      
         GOTO  63                                                       
  17     L = XPOLY                                                      
         GOTO  63                                                       
  18     L = ERPUTS                                                     
         GOTO  63                                                       
  19     L = USENGJ                                                     
         GOTO  63                                                       
  20     L = USENNS                                                     
         GOTO  63                                                       
  21     L = USENFD                                                     
         GOTO  63                                                       
C POST VERSION NUMBER.                                                  
  22     F = 3D0                                                        
         GOTO  63                                                       
C SET THE VARIABLES TO THE DEFAULTS.                                    
  23     THETA = 1D0                                                    
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
C 0 IMPLIES MGQ = K-1, BY DEFAULT.                                      
         MGQ = 0                                                        
         SAVEB = 0                                                      
         XPOLY = .FALSE.                                                
         ERPUTS = .FALSE.                                               
         USENGJ = .FALSE.                                               
         USENNS = .FALSE.                                               
         USENFD = .FALSE.                                               
         CALL SETI(100, 0, N)                                           
         N(1) = 1                                                       
C   IMPORT THE VARIABLES.                                               
         GOTO  63                                                       
  24     THETA = F                                                      
         IF (THETA .EQ. 0.5) GOTO 25                                    
            GAMMA = 1                                                   
            HFRACT = 1                                                  
            GOTO  29                                                    
  25        GAMMA = 2                                                   
            HFRACT = 0.5                                                
            N(1) = 2                                                    
            N(2) = 4                                                    
            N(3) = 6                                                    
            M = 4                                                       
               GOTO  27                                                 
  26           M = M+1                                                  
  27           IF (M .GT. MMAX) GOTO  28                                
               N(M) = 2*N(M-2)                                          
               GOTO  26                                                 
  28        CONTINUE                                                    
  29     GOTO  63                                                       
  30     BETA = F                                                       
         GOTO  63                                                       
  31     GAMMA = F                                                      
         GOTO  63                                                       
  32     DELTA = F                                                      
         GOTO  63                                                       
  33     HFRACT = R                                                     
         GOTO  63                                                       
  34     EGIVE = R                                                      
         GOTO  63                                                       
  35     KEEJAC = I                                                     
         GOTO  63                                                       
  36     MINIT = I                                                      
         GOTO  63                                                       
  37     MAXIT = I                                                      
         GOTO  63                                                       
  38     KMAX = I                                                       
         MMAX = KMAX+5                                                  
         GOTO  63                                                       
  39     KINIT = I                                                      
         GOTO  63                                                       
  40     MMAX = I                                                       
         GOTO  63                                                       
  41     MGQ = I                                                        
         GOTO  63                                                       
  42     SAVEB = I                                                      
         GOTO  63                                                       
  43     XPOLY = L                                                      
         GOTO  63                                                       
  44     ERPUTS = L                                                     
         IF (.NOT. ERPUTS) GOTO 45                                      
            DELTA = 1                                                   
            GOTO  46                                                    
  45        DELTA = 0                                                   
  46     GOTO  63                                                       
  47     USENGJ = L                                                     
         GOTO  63                                                       
  48     USENNS = L                                                     
         GOTO  63                                                       
  49     USENFD = L                                                     
         GOTO  63                                                       
  50     TEMP1 = IABS(J) .GT. 4100                                      
         IF (.NOT. TEMP1) TEMP1 = IABS(J) .LT. 4001                     
C/6S                                                                    
C        IF (TEMP1) CALL SETERR(24HDPOSTW - J OUT OF BOUNDS, 24, 1, 2)  
C/7S                                                                    
         IF (TEMP1) CALL SETERR('DPOSTW - J OUT OF BOUNDS', 24, 1, 2)   
C/                                                                      
         IF (J .GE. 0) GOTO 60                                          
            IF (N(2) .NE. 0) GOTO 51                                    
               N(2) = DSQRT(2D0)*DBLE(FLOAT(N(1)))                      
C EXPORT N(ABS(J)-4000)                                                 
C ONLY N(1) IS GIVEN, USE SQRT(2) INCREASE.                             
               IF (N(2) .EQ. N(1)) N(2) = N(2)+1                        
               N(3) = DSQRT(2D0)*DBLE(FLOAT(N(2)))                      
               IF (N(3) .EQ. N(2)) N(3) = N(3)+1                        
               N(4) = 0                                                 
  51        TEMP = IABS(J)                                              
            IF (N(TEMP-4000) .NE. 0) GOTO 59                            
               DO  57 K = 1, MMAX                                       
C FILL IN THE MISSING N(M).                                             
                  IF (N(K) .NE. 0) GOTO 56                              
                     IF (K .NE. 3) GOTO 53                              
                        DO  52 M = K, MMAX                              
                           N(M) = (N(2)*N(M-1))/MAX0(1, N(1))           
  52                       CONTINUE                                     
                        GOTO  55                                        
  53                    DO  54 M = K, MMAX                              
                           N(M) = 2*N(M-2)                              
  54                       CONTINUE                                     
  55                 GOTO  58                                           
  56              CONTINUE                                              
  57              CONTINUE                                              
  58           CONTINUE                                                 
  59        TEMP = IABS(J)                                              
            I = N(TEMP-4000)                                            
            GOTO  61                                                    
  60        N(J-4000) = I                                               
C IMPORT N(J-4000)                                                      
            IF (J-4000 .LT. 100) N(J-3999) = 0                          
  61     CONTINUE                                                       
         GOTO  63                                                       
  62     IF (J .EQ. 3005) GOTO  49                                      
         IF (J .EQ. 3004) GOTO  48                                      
         IF (J .EQ. 3003) GOTO  47                                      
         IF (J .EQ. 3002) GOTO  44                                      
         IF (J .EQ. 3001) GOTO  43                                      
         IF (J .EQ. 2008) GOTO  42                                      
         IF (J .EQ. 2007) GOTO  41                                      
         IF (J .EQ. 2006) GOTO  40                                      
         IF (J .EQ. 2005) GOTO  39                                      
         IF (J .EQ. 2004) GOTO  38                                      
         IF (J .EQ. 2003) GOTO  37                                      
         IF (J .EQ. 2002) GOTO  36                                      
         IF (J .EQ. 2001) GOTO  35                                      
         IF (J .EQ. 1002) GOTO  34                                      
         IF (J .EQ. 1001) GOTO  33                                      
         IF (J .EQ. 4) GOTO  32                                         
         IF (J .EQ. 3) GOTO  31                                         
         IF (J .EQ. 2) GOTO  30                                         
         IF (J .EQ. 1) GOTO  24                                         
         IF (J .EQ. 0) GOTO  23                                         
         IF (J .EQ. (-6000)) GOTO  22                                   
         IF (J .EQ. (-3005)) GOTO  21                                   
         IF (J .EQ. (-3004)) GOTO  20                                   
         IF (J .EQ. (-3003)) GOTO  19                                   
         IF (J .EQ. (-3002)) GOTO  18                                   
         IF (J .EQ. (-3001)) GOTO  17                                   
         IF (J .EQ. (-2008)) GOTO  16                                   
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
         GOTO  50                                                       
  63  RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTA(T, X, NX, U, UX, UT, UTX, NU, V, VT, NV, A      
     1   , AU, AUX, AUT, AUTX, AV, AVT, F, FU, FUX, FUT, FUTX, FV, FVT) 
      INTEGER NU, NV, NX                                                
      DOUBLE PRECISION T, X(NX), U(NX, NU), UX(NX, NU), UT(NX, NU), UTX(
     1   NX, NU)                                                        
      DOUBLE PRECISION V(NV), VT(NV), A(NX, NU), AU(NX, NU, NU), AUX(NX,
     1   NU, NU), AUT(NX, NU, NU)                                       
      DOUBLE PRECISION AUTX(NX, NU, NU), AV(NX, NU, NV), AVT(NX, NU, NV)
     1   , F(NX, NU), FU(NX, NU, NU), FUX(NX, NU, NU)                   
      DOUBLE PRECISION FUT(NX, NU, NU), FUTX(NX, NU, NU), FV(NX, NU, NV)
     1   , FVT(NX, NU, NV)                                              
C THE DEFAULT, NULL AF ROUTINE FOR POST.                                
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTB(T, L, R, U, UX, UT, UTX, NU, V, VT, NV, B,      
     1   BU, BUX, BUT, BUTX, BV, BVT)                                   
      INTEGER NU, NV                                                    
      DOUBLE PRECISION T, L, R, U(NU, 2), UX(NU, 2), UT(NU, 2)          
      DOUBLE PRECISION UTX(NU, 2), V(NV), VT(NV), B(NU, 2), BU(NU, NU, 2
     1   ), BUX(NU, NU, 2)                                              
      DOUBLE PRECISION BUT(NU, NU, 2), BUTX(NU, NU, 2), BV(NU, NV, 2),  
     1   BVT(NU, NV, 2)                                                 
C THE DEFAULT, NULL BC ROUTINE FOR POST.                                
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTD(T, K, X, NX, U, UT, NU, NXMK, V, VT, NV, D      
     1   , DU, DUT, DV, DVT)                                            
      INTEGER NXMK, NV, NX                                              
      INTEGER K, NU                                                     
      DOUBLE PRECISION T, X(NX), U(NXMK, 1), UT(NXMK, 1), V(NV), VT(NV) 
      DOUBLE PRECISION D(NV), DU(NV, NXMK, 1), DUT(NV, NXMK, 1), DV(NV  
     1   , NV), DVT(NV, NV)                                             
C SCRATCH SPACE ALLOCATED - NONE.                                       
C (U,UT)(NXMK,NU).                                                      
C (DU,DUT)(NV,NXMK,NU).                                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTI(XI, X, XT, XXI, XV, XTV, XXIV, XVV, NX,         
     1   UX, UT, NU, V, VT, NV, IV, NVM, A, AX, AU, AUX, AUT, AUTX, AV  
     2   , AVT, F, FX, FU, FUX, FUT, FUTX, FV, FVT)                     
      INTEGER NVM, NU, NV, NX                                           
      INTEGER IV                                                        
      DOUBLE PRECISION XI(NX), X(NX), XT(NX), XXI(NX), XV(NX, NVM), XTV(
     1   NX, NVM)                                                       
      DOUBLE PRECISION XXIV(NX, NVM), XVV(NX, NVM, NVM), UX(NX, NU), UT(
     1   NX, NU), V(NV), VT(NV)                                         
      DOUBLE PRECISION A(NX, NU), AX(NX, NU), AU(NX, NU, NU), AUX(NX,   
     1   NU, NU), AUT(NX, NU, NU), AUTX(NX, NU, NU)                     
      DOUBLE PRECISION AV(NX, NU, NV), AVT(NX, NU, NV), F(NX, NU), FX(  
     1   NX, NU), FU(NX, NU, NU), FUX(NX, NU, NU)                       
      DOUBLE PRECISION FUT(NX, NU, NU), FUTX(NX, NU, NU), FV(NX, NU, NV)
     1   , FVT(NX, NU, NV)                                              
      INTEGER I, J, L, I1, IX                                           
      DOUBLE PRECISION TERMAT, TERMAX, TERMFT, TERMFX, TERMVV, TERM     
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
C TO PERFORM INTERVAL MAPPING FROM USER COORDINATES BACK INTO INTERNAL  
C POST COORDINATES.                                                     
C/6S                                                                    
C     IF (NX .LT. 1) CALL SETERR(18HDPOSTI - NX .LT. 1, 18, 1, 2)       
C     IF (NU .LT. 1) CALL SETERR(18HDPOSTI - NU .LT. 1, 18, 2, 2)       
C     IF (NV .LT. 0) CALL SETERR(18HDPOSTI - NV .LT. 0, 18, 3, 2)       
C/7S                                                                    
      IF (NX .LT. 1) CALL SETERR('DPOSTI - NX .LT. 1', 18, 1, 2)        
      IF (NU .LT. 1) CALL SETERR('DPOSTI - NU .LT. 1', 18, 2, 2)        
      IF (NV .LT. 0) CALL SETERR('DPOSTI - NV .LT. 0', 18, 3, 2)        
C/                                                                      
      TEMP1 = IV .LT. 0                                                 
      IF (.NOT. TEMP1) TEMP1 = IV .GT. NV                               
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(29HDPOSTI - IV MUST BE IN (0,NV), 29, 4, 2)
C/7S                                                                    
      IF (TEMP1) CALL SETERR('DPOSTI - IV MUST BE IN (0,NV)', 29, 4, 2) 
C/                                                                      
      TEMP1 = NVM .LT. 0                                                
      IF (.NOT. TEMP1) TEMP1 = IV+NVM-1 .GT. NV                         
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(30HDPOSTI - NVM MUST BE IN (0,NV), 30, 5, 2
C    1   )                                                              
C/7S                                                                    
      IF (TEMP1) CALL SETERR('DPOSTI - NVM MUST BE IN (0,NV)', 30, 5, 2 
     1   )                                                              
C/                                                                      
      DO  13 IX = 1, NX                                                 
         TERM = XT(IX)                                                  
         J = 1                                                          
            GOTO  2                                                     
   1        J = J+1                                                     
   2        IF (J .GT. NVM) GOTO  3                                     
            TEMP = J+IV                                                 
            TERM = TERM+XV(IX, J)*VT(TEMP-1)                            
            GOTO  1                                                     
   3     DO  12 I = 1, NU                                               
            DO  4 J = 1, NV                                             
               FV(IX, I, J) = FV(IX, I, J)*XXI(IX)                      
               FVT(IX, I, J) = FVT(IX, I, J)*XXI(IX)                    
   4           CONTINUE                                                 
            TERMAX = 0                                                  
            TERMFX = 0                                                  
            TERMAT = 0                                                  
            TERMFT = 0                                                  
            DO  5 J = 1, NU                                             
               TERMAX = TERMAX+AUX(IX, I, J)*UX(IX, J)                  
               TERMFX = TERMFX+FUX(IX, I, J)*UX(IX, J)                  
               TERMAT = TERMAT+AUT(IX, I, J)*UX(IX, J)                  
               TERMFT = TERMFT+FUT(IX, I, J)*UX(IX, J)                  
               AUX(IX, I, J) = (AUX(IX, I, J)-TERM*AUT(IX, I, J))/XXI(  
     1            IX)                                                   
               FUX(IX, I, J) = FUX(IX, I, J)-TERM*FUT(IX, I, J)         
               FUT(IX, I, J) = FUT(IX, I, J)*XXI(IX)                    
               FU(IX, I, J) = FU(IX, I, J)*XXI(IX)                      
               TEMP1 = AUTX(IX, I, J) .NE. 0D0                          
               IF (.NOT. TEMP1) TEMP1 = FUTX(IX, I, J) .NE. 0D0         
C/6S                                                                    
C              IF (TEMP1) CALL SETERR(                                  
C    1            34HDPOSTI - MUST HAVE AUTX = 0 = FUTX, 34, 6, 2)      
C/7S                                                                    
               IF (TEMP1) CALL SETERR(                                  
     1            'DPOSTI - MUST HAVE AUTX = 0 = FUTX', 34, 6, 2)       
C/                                                                      
   5           CONTINUE                                                 
            J = 1                                                       
               GOTO  7                                                  
   6           J = J+1                                                  
   7           IF (J .GT. NVM) GOTO  11                                 
               I1 = J+IV-1                                              
               TERMVV = XTV(IX, J)                                      
               L = 1                                                    
                  GOTO  9                                               
   8              L = L+1                                               
   9              IF (L .GT. NVM) GOTO  10                              
                  TEMP = L+IV                                           
                  TERMVV = TERMVV+XVV(IX, L, J)*VT(TEMP-1)              
                  GOTO  8                                               
  10           AV(IX, I, I1) = AV(IX, I, I1)+AX(IX, I)*XV(IX, J)-(      
     1            TERMAX*XXIV(IX, J)+TERMAT*(TERMVV*XXI(IX)-TERM*XXIV(  
     2            IX, J)))/XXI(IX)                                      
               FV(IX, I, I1) = FV(IX, I, I1)+FX(IX, I)*XXI(IX)*XV(IX, J)
     1            -(TERMFX*XXIV(IX, J)+TERMFT*(TERMVV*XXI(IX)-TERM*XXIV(
     2            IX, J)))+XXIV(IX, J)*F(IX, I)                         
               AVT(IX, I, I1) = AVT(IX, I, I1)-TERMAT*XV(IX, J)         
               FVT(IX, I, I1) = FVT(IX, I, I1)-TERMFT*XV(IX, J)*XXI(IX) 
               GOTO  6                                                  
  11        F(IX, I) = F(IX, I)*XXI(IX)                                 
  12        CONTINUE                                                    
  13     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTJ(XI, X, XT, XXI, XV, XTV, XXIV, XVV, UX,         
     1   UT, NU, V, VT, NV, IV, NVM, B, BX, BU, BUX, BUT, BUTX, BV, BVT)
      INTEGER NVM, NU, NV                                               
      INTEGER IV                                                        
      DOUBLE PRECISION XI(2), X(2), XT(2), XXI(2), XV(2, NVM), XTV(2,   
     1   NVM)                                                           
      DOUBLE PRECISION XXIV(2, NVM), XVV(2, NVM, NVM), UX(NU, 2), UT(NU,
     1   2), V(NV), VT(NV)                                              
      DOUBLE PRECISION B(NU, 2), BX(NU, 2), BU(NU, NU, 2), BUX(NU, NU, 2
     1   ), BUT(NU, NU, 2), BUTX(NU, NU, 2)                             
      DOUBLE PRECISION BV(NU, NV, 2), BVT(NU, NV, 2)                    
      INTEGER I, J, L, I1, IX                                           
      DOUBLE PRECISION TERMBT, TERMBX, TERMVV, TERM                     
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
C TO PERFORM INTERVAL MAPPING FROM USER COORDINATES BACK INTO INTERNAL  
C POST COORDINATES.                                                     
C/6S                                                                    
C     IF (NU .LT. 1) CALL SETERR(18HDPOSTJ - NU .LT. 1, 18, 1, 2)       
C     IF (NV .LT. 0) CALL SETERR(18HDPOSTJ - NV .LT. 0, 18, 2, 2)       
C/7S                                                                    
      IF (NU .LT. 1) CALL SETERR('DPOSTJ - NU .LT. 1', 18, 1, 2)        
      IF (NV .LT. 0) CALL SETERR('DPOSTJ - NV .LT. 0', 18, 2, 2)        
C/                                                                      
      TEMP1 = IV .LT. 0                                                 
      IF (.NOT. TEMP1) TEMP1 = IV .GT. NV                               
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(29HDPOSTJ - IV MUST BE IN (0,NV), 29, 3, 2)
C/7S                                                                    
      IF (TEMP1) CALL SETERR('DPOSTJ - IV MUST BE IN (0,NV)', 29, 3, 2) 
C/                                                                      
      TEMP1 = NVM .LT. 0                                                
      IF (.NOT. TEMP1) TEMP1 = IV+NVM-1 .GT. NV                         
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(30HDPOSTJ - NVM MUST BE IN (0,NV), 30, 4, 2
C    1   )                                                              
C/7S                                                                    
      IF (TEMP1) CALL SETERR('DPOSTJ - NVM MUST BE IN (0,NV)', 30, 4, 2 
     1   )                                                              
C/                                                                      
      DO  12 IX = 1, 2                                                  
         TERM = XT(IX)                                                  
         J = 1                                                          
            GOTO  2                                                     
   1        J = J+1                                                     
   2        IF (J .GT. NVM) GOTO  3                                     
            TEMP = J+IV                                                 
            TERM = TERM+XV(IX, J)*VT(TEMP-1)                            
            GOTO  1                                                     
   3     DO  11 I = 1, NU                                               
            TERMBX = 0                                                  
            TERMBT = 0                                                  
            DO  4 J = 1, NU                                             
               TERMBX = TERMBX+BUX(I, J, IX)*UX(J, IX)                  
               TERMBT = TERMBT+BUT(I, J, IX)*UX(J, IX)                  
               BUX(I, J, IX) = BUX(I, J, IX)-TERM*BUT(I, J, IX)         
               BUX(I, J, IX) = BUX(I, J, IX)/XXI(IX)                    
C/6S                                                                    
C              IF (BUTX(I, J, IX) .NE. 0D0) CALL SETERR(                
C    1            27HDPOSTJ - MUST HAVE BUTX = 0, 27, 5, 2)             
C/7S                                                                    
               IF (BUTX(I, J, IX) .NE. 0D0) CALL SETERR(                
     1            'DPOSTJ - MUST HAVE BUTX = 0', 27, 5, 2)              
C/                                                                      
   4           CONTINUE                                                 
            J = 1                                                       
               GOTO  6                                                  
   5           J = J+1                                                  
   6           IF (J .GT. NVM) GOTO  10                                 
               I1 = J+IV-1                                              
               TERMVV = XTV(IX, J)                                      
               L = 1                                                    
                  GOTO  8                                               
   7              L = L+1                                               
   8              IF (L .GT. NVM) GOTO  9                               
                  TEMP = L+IV                                           
                  TERMVV = TERMVV+XVV(IX, L, J)*VT(TEMP-1)              
                  GOTO  7                                               
   9           BV(I, I1, IX) = BV(I, I1, IX)+BX(I, IX)*XV(IX, J)-(      
     1            TERMBX*XXIV(IX, J)+TERMBT*(TERMVV*XXI(IX)-TERM*XXIV(  
     2            IX, J)))/XXI(IX)                                      
               BVT(I, I1, IX) = BVT(I, I1, IX)-TERMBT*XV(IX, J)         
               GOTO  5                                                  
  10        CONTINUE                                                    
  11        CONTINUE                                                    
  12     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTU(XI, X, XT, XXI, XV, VT, NX, NV, UX, UT,         
     1   NU, AX, FX)                                                    
      INTEGER NU, NV, NX                                                
      DOUBLE PRECISION XI(NX), X(NX), XT(NX), XXI(NX), XV(NX, NV), VT(  
     1   NV)                                                            
      DOUBLE PRECISION UX(NX, NU), UT(NX, NU), AX(NX, NU), FX(NX, NU)   
      INTEGER I, J, IX                                                  
      DOUBLE PRECISION XVSUM                                            
C TO PERFORM INTERVAL MAPPING FROM INTERNAL POST COORDINATES            
C INTO USER COORDINATES.                                                
C/6S                                                                    
C     IF (NX .LT. 1) CALL SETERR(18HDPOSTU - NX .LT. 1, 18, 1, 2)       
C     IF (NV .LT. 0) CALL SETERR(18HDPOSTU - NV .LT. 0, 18, 2, 2)       
C     IF (NU .LT. 1) CALL SETERR(18HDPOSTU - NU .LT. 1, 18, 3, 2)       
C/7S                                                                    
      IF (NX .LT. 1) CALL SETERR('DPOSTU - NX .LT. 1', 18, 1, 2)        
      IF (NV .LT. 0) CALL SETERR('DPOSTU - NV .LT. 0', 18, 2, 2)        
      IF (NU .LT. 1) CALL SETERR('DPOSTU - NU .LT. 1', 18, 3, 2)        
C/                                                                      
C MAP INTO USER SYSTEM.                                                 
      DO  5 IX = 1, NX                                                  
         XVSUM = 0                                                      
         J = 1                                                          
            GOTO  2                                                     
   1        J = J+1                                                     
   2        IF (J .GT. NV) GOTO  3                                      
            XVSUM = XVSUM+VT(J)*XV(IX, J)                               
            GOTO  1                                                     
   3     DO  4 I = 1, NU                                                
            UX(IX, I) = UX(IX, I)/XXI(IX)                               
            UT(IX, I) = UT(IX, I)-UX(IX, I)*(XT(IX)+XVSUM)              
   4        CONTINUE                                                    
   5     CONTINUE                                                       
      CALL SETD(NX*NU, 0D0, AX)                                         
C AX = 0 = FX BY DEFAULT.                                               
      CALL SETD(NX*NU, 0D0, FX)                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTV(XI, X, XT, XXI, XV, VT, NV, UX, UT, NU,         
     1   BX)                                                            
      INTEGER NU, NV                                                    
      DOUBLE PRECISION XI(2), X(2), XT(2), XXI(2), XV(2, NV), VT(NV)    
      DOUBLE PRECISION UX(NU, 2), UT(NU, 2), BX(NU, 2)                  
      INTEGER I, J, IX                                                  
      DOUBLE PRECISION XVSUM                                            
C TO PERFORM INTERVAL MAPPING FROM INTERNAL POST COORDINATES            
C INTO USER COORDINATES.                                                
C/6S                                                                    
C     IF (NV .LT. 0) CALL SETERR(18HDPOSTV - NV .LT. 0, 18, 1, 2)       
C     IF (NU .LT. 1) CALL SETERR(18HDPOSTV - NU .LT. 1, 18, 2, 2)       
C/7S                                                                    
      IF (NV .LT. 0) CALL SETERR('DPOSTV - NV .LT. 0', 18, 1, 2)        
      IF (NU .LT. 1) CALL SETERR('DPOSTV - NU .LT. 1', 18, 2, 2)        
C/                                                                      
C MAP INTO USER SYSTEM.                                                 
      DO  5 IX = 1, 2                                                   
         XVSUM = 0                                                      
         J = 1                                                          
            GOTO  2                                                     
   1        J = J+1                                                     
   2        IF (J .GT. NV) GOTO  3                                      
            XVSUM = XVSUM+VT(J)*XV(IX, J)                               
            GOTO  1                                                     
   3     DO  4 I = 1, NU                                                
            UX(I, IX) = UX(I, IX)/XXI(IX)                               
            UT(I, IX) = UT(I, IX)-UX(I, IX)*(XT(IX)+XVSUM)              
   4        CONTINUE                                                    
   5     CONTINUE                                                       
C BX = 0 BY DEFAULT.                                                    
      CALL SETD(2*NU, 0D0, BX)                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DLPLMX(XI, NX, V, NV, X, XV)                           
      INTEGER NV, NX                                                    
      DOUBLE PRECISION XI(NX), V(NV), X(NX), XV(NX, NV)                 
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IXT, ISTKGT, IXXI, IXTV, IXVV, IS(1000)                   
      INTEGER IXXIV                                                     
      REAL RS(1000)                                                     
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION WS(500)                                          
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C X(XI,V) IS A PIECEWISE LINEAR MAP IN XI, V == BREAKPOINTS.            
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (NX .LT. 1) CALL SETERR(18HDLPLMX - NX .LT. 1, 18, 1, 2)       
C     IF (NV .LT. 2) CALL SETERR(18HDLPLMX - NV .LT. 2, 18, 2, 2)       
C/7S                                                                    
      IF (NX .LT. 1) CALL SETERR('DLPLMX - NX .LT. 1', 18, 1, 2)        
      IF (NV .LT. 2) CALL SETERR('DLPLMX - NV .LT. 2', 18, 2, 2)        
C/                                                                      
      IXXI = ISTKGT(NX, 4)                                              
      IXXIV = ISTKGT(NX*NV, 4)                                          
      IXVV = ISTKGT(NX*NV**2, 4)                                        
      IXT = ISTKGT(NX, 4)                                               
      IXTV = ISTKGT(NX*NV, 4)                                           
      CALL DLPLM(XI, NX, V, NV, X, WS(IXXI), WS(IXXIV), XV, WS(IXVV),   
     1   WS(IXT), WS(IXTV))                                             
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DLPLM(XI, NX, V, NV, X, XXI, XXIV, XV, XVV, XT,        
     1   XTV)                                                           
      INTEGER NV, NX                                                    
      DOUBLE PRECISION XI(NX), V(NV), X(NX), XXI(NX), XXIV(NX, NV), XV( 
     1   NX, NV)                                                        
      DOUBLE PRECISION XVV(NX, NV, NV), XT(NX), XTV(NX, NV)             
      INTEGER MIN0, I, IX                                               
      REAL FLOAT                                                        
      DOUBLE PRECISION DBLE                                             
      LOGICAL TEMP                                                      
C X(XI,V) IS A PIECEWISE LINEAR MAP IN XI, V == BREAKPOINTS.            
C/6S                                                                    
C     IF (NX .LT. 1) CALL SETERR(27HDLPLM - MUST HAVE NX .GE. 1, 27, 1  
C    1   , 2)                                                           
C     IF (NV .LT. 2) CALL SETERR(27HDLPLM - MUST HAVE NV .GE. 2, 27, 2  
C    1   , 2)                                                           
C/7S                                                                    
      IF (NX .LT. 1) CALL SETERR('DLPLM - MUST HAVE NX .GE. 1', 27, 1   
     1   , 2)                                                           
      IF (NV .LT. 2) CALL SETERR('DLPLM - MUST HAVE NV .GE. 2', 27, 2   
     1   , 2)                                                           
C/                                                                      
      CALL SETD(NX*NV, 0D0, XXIV)                                       
      CALL SETD(NX*NV, 0D0, XV)                                         
      CALL SETD(NX*NV**2, 0D0, XVV)                                     
      CALL SETD(NX, 0D0, XT)                                            
      CALL SETD(NX*NV, 0D0, XTV)                                        
      DO  1 IX = 1, NX                                                  
         TEMP = XI(IX) .LT. 0D0                                         
         IF (.NOT. TEMP) TEMP = XI(IX) .GT. DBLE(FLOAT(NV-1))           
C/6S                                                                    
C        IF (TEMP) CALL SETERR(30HDLPLM - XI(IX) NOT IN (0,NV-1), 30, 3,
C    1      2)                                                          
C/7S                                                                    
         IF (TEMP) CALL SETERR('DLPLM - XI(IX) NOT IN (0,NV-1)', 30, 3, 
     1      2)                                                          
C/                                                                      
         I = XI(IX)                                                     
C THE INDEX OF XI(IX) IN V.                                             
         I = I+1                                                        
C DO NOT RUN OFF THE END.                                               
         I = MIN0(NV-1, I)                                              
         X(IX) = V(I)+(V(I+1)-V(I))*(XI(IX)-DBLE(FLOAT(I-1)))           
         XV(IX, I) = DBLE(FLOAT(I))-XI(IX)                              
         XV(IX, I+1) = XI(IX)-DBLE(FLOAT(I-1))                          
         XXI(IX) = V(I+1)-V(I)                                          
         XXIV(IX, I) = -1                                               
         XXIV(IX, I+1) = 1                                              
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DLPLMG(NV, X, V)                                       
      INTEGER NV                                                        
      DOUBLE PRECISION X(NV), V(NV)                                     
      INTEGER I                                                         
C FORCE X(I-1,V) = X(I), I = 1,..., NV, FOR THE LPLM MAPS.              
C/6S                                                                    
C     IF (NV .LT. 2) CALL SETERR(18HDLPLMG - NV .LT. 2, 18, 1, 2)       
C/7S                                                                    
      IF (NV .LT. 2) CALL SETERR('DLPLMG - NV .LT. 2', 18, 1, 2)        
C/                                                                      
      DO  1 I = 2, NV                                                   
C/6S                                                                    
C        IF (X(I) .LE. X(I-1)) CALL SETERR(                             
C    1      46HDLPLMG - X IS NOT STRICTLY MONOTONE INCREASING, 46, 2, 2)
C/7S                                                                    
         IF (X(I) .LE. X(I-1)) CALL SETERR(                             
     1      'DLPLMG - X IS NOT STRICTLY MONOTONE INCREASING', 46, 2, 2) 
C/                                                                      
   1     CONTINUE                                                       
C GET V.                                                                
      DO  2 I = 1, NV                                                   
         V(I) = X(I)                                                    
   2     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DLPLMI(NV, V, X, NX, XI)                               
      INTEGER NV, NX                                                    
      DOUBLE PRECISION V(NV), X(NX), XI(NX)                             
      INTEGER I, IX                                                     
      REAL FLOAT                                                        
      DOUBLE PRECISION DBLE                                             
      LOGICAL TEMP                                                      
C GET XI(X) FROM X(XI) = X.                                             
C/6S                                                                    
C     IF (NV .LT. 2) CALL SETERR(18HDLPLMI - NV .LT. 2, 18, 1, 2)       
C     IF (NX .LT. 1) CALL SETERR(18HDLPLMI - NX .LT. 1, 18, 2, 2)       
C/7S                                                                    
      IF (NV .LT. 2) CALL SETERR('DLPLMI - NV .LT. 2', 18, 1, 2)        
      IF (NX .LT. 1) CALL SETERR('DLPLMI - NX .LT. 1', 18, 2, 2)        
C/                                                                      
      DO  4 IX = 1, NX                                                  
         I = 1                                                          
            GOTO  2                                                     
   1        I = I+1                                                     
   2        IF (I .GE. NV) GOTO  3                                      
C FIND X(IX) IN RANGE OF X.                                             
            TEMP = X(IX) .GE. V(I)                                      
            IF (TEMP) TEMP = X(IX) .LE. V(I+1)                          
            IF (TEMP) GOTO  3                                           
            GOTO  1                                                     
   3     IF (I .GE. NV) I = NV-1                                        
C X NOT IN RANGE, USE LAST INTERVAL.                                    
         XI(IX) = (X(IX)-V(I))/(V(I+1)-V(I))+DBLE(FLOAT(I))-1D0         
   4     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DLPLMT(T1, V1, NV, T2, V2, F, DT)       
      INTEGER NV                                                        
      DOUBLE PRECISION T1, V1(NV), T2, V2(NV), F, DT                    
      INTEGER I, J                                                      
      DOUBLE PRECISION DTT, DABS, SGNDT, SGNT21, SGNV21                 
      LOGICAL TEMP                                                      
C TO RETURN THE VALUE OF DT THAT WILL CAUSE THE BREAK-POINTS TO MOVE    
C NO MORE THAN A FACTOR OF F CLOSER TOGETHER OVER THE NEXT TIME-STEP.   
C/6S                                                                    
C     IF (T1 .EQ. T2) CALL SETERR(17HDLPLMT - T1 == T2, 17, 1, 2)       
C     IF (NV .LT. 2) CALL SETERR(18HDLPLMT - NV .LT. 2, 18, 2, 2)       
C/7S                                                                    
      IF (T1 .EQ. T2) CALL SETERR('DLPLMT - T1 == T2', 17, 1, 2)        
      IF (NV .LT. 2) CALL SETERR('DLPLMT - NV .LT. 2', 18, 2, 2)        
C/                                                                      
      TEMP = F .LE. 0D0                                                 
      IF (.NOT. TEMP) TEMP = F .GE. 1D0                                 
C/6S                                                                    
C     IF (TEMP) CALL SETERR(23HDLPLMT - F NOT IN (0,1), 23, 3, 2)       
C     IF (DT .EQ. 0D0) CALL SETERR(16HDLPLMT - DT == 0, 16, 4, 2)       
C/7S                                                                    
      IF (TEMP) CALL SETERR('DLPLMT - F NOT IN (0,1)', 23, 3, 2)        
      IF (DT .EQ. 0D0) CALL SETERR('DLPLMT - DT == 0', 16, 4, 2)        
C/                                                                      
      DTT = DT                                                          
      SGNDT = DT/DABS(DT)                                               
      SGNT21 = (T2-T1)/DABS(T2-T1)                                      
      DO  3 I = 1, NV                                                   
         IF (V2(I) .EQ. V1(I)) GOTO  3                                  
         SGNV21 = (V2(I)-V1(I))/DABS(V2(I)-V1(I))                       
         DO  2 J = 1, NV                                                
            IF (I .EQ. J) GOTO  2                                       
            IF (V2(I) .NE. V2(J)) GOTO 1                                
               DTT = 0                                                  
               DLPLMT = DTT                                             
               RETURN                                                   
   1        TEMP = SGNDT*SGNT21*((V2(I)-V1(I))/DABS(V2(I)-V1(I)))*((V2(I
     1         )-V2(J))/DABS(V2(I)-V2(J))) .LT. 0D0                     
            IF (TEMP) TEMP = SGNDT*SGNV21*(DTT*(V2(I)-V1(I))-(T2-T1)*(  
     1         V2(I)-V2(J))*(F-1D0)) .GT. 0D0                           
            IF (TEMP) DTT = (T2-T1)*(V2(I)-V2(J))*(F-1D0)/(V2(I)-V1(I)) 
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      DLPLMT = DTT                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSA(A, M, N, R, C, RC)                               
      INTEGER M, N                                                      
      INTEGER RC(N)                                                     
      DOUBLE PRECISION A(M, N), R(M), C(N)                              
      INTEGER NERROR, NERR, IROLD                                       
C TO GET THE ROW AND COLUMN SCALE FACTORS FOR A                         
C AND PERFORM THE SCALING.                                              
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16HDRCSA - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16HDRCSA - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DRCSA - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR('DRCSA - N .LT. 1', 16, 2, 2)           
C/                                                                      
C TURN ERROR RECOVERY ON AND SAVE OLD VALUE.                            
      CALL ENTSRC(IROLD, 1)                                             
C GET ROW AND COLUMN SCALE FACTORS.                                     
      CALL DRCSM(A, M, N, R, C, RC)                                     
C APPLY THE SCALE FACTORS TO A.                                         
      CALL DRCSS(A, M, N, R, C, RC)                                     
      IF (NERROR(NERR) .EQ. 0) GOTO 1                                   
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(42HDRCSA - MUST HAVE 1/(S*L) IN MACHINE RANGE, 42  
C    1      , 3, 1)                                                     
C/7S                                                                    
         CALL SETERR('DRCSA - MUST HAVE 1/(S*L) IN MACHINE RANGE', 42   
     1      , 3, 1)                                                     
C/                                                                      
         RETURN                                                         
C RESTORE OLD RECOVERY VALUE.                                           
   1  CALL RETSRC(IROLD)                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSBA(A, M, N, R, C, RC)                              
      INTEGER M, N                                                      
      INTEGER RC(M)                                                     
      DOUBLE PRECISION A(M, N), R(M), C(M)                              
      INTEGER NERROR, NERR                                              
C TO GET THE ROW AND COLUMN SCALE FACTORS FOR A                         
C AND PERFORM THE SCALING.                                              
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(17HDRCSBA - M .LT. 1, 17, 1, 2)         
C     IF (N .LT. 1) CALL SETERR(17HDRCSBA - N .LT. 1, 17, 2, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DRCSBA - M .LT. 1', 17, 1, 2)          
      IF (N .LT. 1) CALL SETERR('DRCSBA - N .LT. 1', 17, 2, 2)          
C/                                                                      
      CALL ENTER(1)                                                     
C GET ROW AND COLUMN SCALE FACTORS.                                     
      CALL DRCSBM(A, M, N, R, C, RC)                                    
C APPLY THE SCALE FACTORS TO A.                                         
      CALL DRCSBS(A, M, N, R, C, RC)                                    
      IF (NERROR(NERR) .EQ. 0) GOTO 1                                   
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(43HDRCSBA - MUST HAVE 1/(S*L) IN MACHINE RANGE, 43,
C    1      3, 1)                                                       
C/7S                                                                    
         CALL SETERR('DRCSBA - MUST HAVE 1/(S*L) IN MACHINE RANGE', 43, 
     1      3, 1)                                                       
C/                                                                      
         RETURN                                                         
   1  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSBM(A, M, N, R, C, RC)                              
      INTEGER M, N                                                      
      INTEGER RC(M)                                                     
      DOUBLE PRECISION A(M, N), R(M), C(M)                              
C TO GET THE ROW AND COLUMN SCALE FACTORS FOR A.                        
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(17HDRCSBM - M .LT. 1, 17, 1, 2)         
C     IF (N .LT. 1) CALL SETERR(17HDRCSBM - N .LT. 1, 17, 2, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DRCSBM - M .LT. 1', 17, 1, 2)          
      IF (N .LT. 1) CALL SETERR('DRCSBM - N .LT. 1', 17, 2, 2)          
C/                                                                      
C GET THE ROW FACTOR.                                                   
      CALL DRCSBR(A, M, N, R)                                           
C GET THE COLUMN SCALE FACTOR.                                          
      CALL DRCSBC(A, M, N, R, C, RC)                                    
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSBR(A, M, N, R)                                     
      INTEGER M, N                                                      
      DOUBLE PRECISION A(M, N), R(M)                                    
      INTEGER I, J, N2, MIN0, MAX0                                      
      DOUBLE PRECISION DABS, DMAX1                                      
      INTEGER TEMP, TEMP1                                               
C TO GET THE ROW SCALE FACTOR FOR A.                                    
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(17HDRCSBR - M .LT. 1, 17, 1, 2)         
C     IF (N .LT. 1) CALL SETERR(17HDRCSBR - N .LT. 1, 17, 2, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DRCSBR - M .LT. 1', 17, 1, 2)          
      IF (N .LT. 1) CALL SETERR('DRCSBR - N .LT. 1', 17, 2, 2)          
C/                                                                      
      N2 = (N+1)/2                                                      
      DO  2 I = 1, M                                                    
         R(I) = 0                                                       
         TEMP1 = MAX0(1, N2+1-I)                                        
         TEMP = MIN0(N, M+N2-I)                                         
         DO  1 J = TEMP1, TEMP                                          
            R(I) = DMAX1(DABS(A(I, J)), R(I))                           
   1        CONTINUE                                                    
   2     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSBS(A, M, N, R, C, RC)                              
      INTEGER M, N                                                      
      INTEGER RC(M)                                                     
      DOUBLE PRECISION A(M, N), R(M), C(M)                              
      INTEGER I, J, N2, RD2, MIN0, MAX0                                 
      LOGICAL BADNGE                                                    
      DOUBLE PRECISION D1MACH, L, S, D1, D2                             
      INTEGER TEMP, TEMP1, TEMP2                                        
C TO SCALE ((1/R)*A)*(1/C).                                             
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(17HDRCSBS - M .LT. 1, 17, 1, 2)         
C     IF (N .LT. 1) CALL SETERR(17HDRCSBS - N .LT. 1, 17, 2, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DRCSBS - M .LT. 1', 17, 1, 2)          
      IF (N .LT. 1) CALL SETERR('DRCSBS - N .LT. 1', 17, 2, 2)          
C/                                                                      
      CALL ENTER(1)                                                     
      S = D1MACH(1)                                                     
      L = D1MACH(2)                                                     
      DO  1 I = 1, M                                                    
         IF (R(I) .EQ. 0D0) GOTO  1                                     
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      37HDRCSBS - MUST HAVE S .LE. R(I) .LE. L, 37, 3, 2)         
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      'DRCSBS - MUST HAVE S .LE. R(I) .LE. L', 37, 3, 2)          
C/                                                                      
   1     CONTINUE                                                       
      DO  2 I = 1, M                                                    
         IF (C(I) .EQ. 0D0) GOTO  2                                     
C/6S                                                                    
C        IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
C    1      37HDRCSBS - MUST HAVE S .LE. C(I) .LE. L, 37, 4, 2)         
C        IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
C    1      36HDRCSS - MUST HAVE RC(I) IN (-1,0,+1), 36, 5, 2)          
C/7S                                                                    
         IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(                 
     1      'DRCSBS - MUST HAVE S .LE. C(I) .LE. L', 37, 4, 2)          
         IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(            
     1      'DRCSS - MUST HAVE RC(I) IN (-1,0,+1)', 36, 5, 2)           
C/                                                                      
   2     CONTINUE                                                       
C CHECK 1/(S*L) RANGE.                                                  
      BADNGE = .FALSE.                                                  
      IF (S*L .GT. 1D0) GOTO 3                                          
         IF (1D0/L .GT. S*L) BADNGE = .TRUE.                            
         GOTO  4                                                        
   3     IF (S*L .GT. 1D0/S) BADNGE = .TRUE.                            
C S*L > 1.                                                              
C/6S                                                                    
C  4  IF (BADNGE) CALL SETERR(                                          
C    1   43HDRCSBX - MUST HAVE 1/(S*L) IN MACHINE RANGE, 43, 6, 1)      
C/7S                                                                    
   4  IF (BADNGE) CALL SETERR(                                          
     1   'DRCSBX - MUST HAVE 1/(S*L) IN MACHINE RANGE', 43, 6, 1)       
C/                                                                      
      N2 = (N+1)/2                                                      
      DO  32 I = 1, M                                                   
         D1 = R(I)                                                      
         IF (D1 .EQ. 0D0) GOTO  32                                      
         TEMP1 = MAX0(1, N2+1-I)                                        
         TEMP = MIN0(N, M+N2-I)                                         
         DO  31 J = TEMP1, TEMP                                         
            TEMP2 = I+J-N2                                              
            D2 = C(TEMP2)                                               
            TEMP2 = I+J-N2                                              
            RD2 = RC(TEMP2)                                             
            IF (A(I, J) .NE. 0D0 .AND. D2 .NE. 0D0) GOTO 5              
               GOTO  31                                                 
   5           IF (D1 .LT. 1D0) GOTO 18                                 
                  IF (RD2 .LE. 0) GOTO 10                               
                     IF (D2 .LT. 1D0) GOTO 6                            
                        A(I, J) = S*((A(I, J)/D1)/D2)                   
C D2 OVERFLOWED.                                                        
                        GOTO  9                                         
   6                    IF (D1*D2 .LT. 1D0) GOTO 7                      
                           A(I, J) = S*(A(I, J)/(D1*D2))                
C D2 < 1.                                                               
                           GOTO  8                                      
   7                       A(I, J) = A(I, J)*(S/(D1*D2))                
   8                    CONTINUE                                        
   9                 CONTINUE                                           
                     GOTO  17                                           
  10                 IF (D2 .LT. 1D0) GOTO 11                           
                        A(I, J) = (A(I, J)/D1)/D2                       
                        GOTO  16                                        
  11                    IF (RD2 .GE. 0) GOTO 14                         
                           IF (D2 .LT. 1D0/D1) GOTO 12                  
                              A(I, J) = A(I, J)*((L/D1)/D2)             
C D2 UNDERFLOWED.                                                       
                              GOTO  13                                  
  12                          A(I, J) = L*(A(I, J)/(D1*D2))             
  13                       CONTINUE                                     
                           GOTO  15                                     
  14                       A(I, J) = A(I, J)/(D1*D2)                    
C D2 < 1.                                                               
  15                 CONTINUE                                           
  16              CONTINUE                                              
  17              CONTINUE                                              
                  GOTO  29                                              
  18              IF (RD2 .LE. 0) GOTO 21                               
                     IF (D1*D2 .LT. 1D0) GOTO 19                        
                        A(I, J) = S*(A(I, J)/(D1*D2))                   
C D1 < 1.                                                               
C D2 OVERFLOWED.                                                        
                        GOTO  20                                        
  19                    A(I, J) = A(I, J)*((S/D1)/D2)                   
  20                 CONTINUE                                           
                     GOTO  28                                           
  21                 IF (D2 .LT. 1D0) GOTO 22                           
                        A(I, J) = A(I, J)/(D1*D2)                       
                        GOTO  27                                        
  22                    IF (RD2 .GE. 0) GOTO 25                         
                           IF (D1*D2 .GT. 1D0) GOTO 23                  
                              A(I, J) = L*(A(I, J)/(D1*D2))             
C D2 UNDERFLOWED.                                                       
                              GOTO  24                                  
  23                          A(I, J) = A(I, J)*(L/(D1*D2))             
  24                       CONTINUE                                     
                           GOTO  26                                     
  25                       A(I, J) = (A(I, J)/D1)/D2                    
C D2 < 1.                                                               
  26                 CONTINUE                                           
  27              CONTINUE                                              
  28              CONTINUE                                              
  29        CONTINUE                                                    
  30        CONTINUE                                                    
  31        CONTINUE                                                    
  32     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSM(A, M, N, R, C, RC)                               
      INTEGER M, N                                                      
      INTEGER RC(N)                                                     
      DOUBLE PRECISION A(M, N), R(M), C(N)                              
C TO GET THE ROW AND COLUMN SCALE FACTORS FOR A.                        
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16HDRCSM - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16HDRCSM - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DRCSM - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR('DRCSM - N .LT. 1', 16, 2, 2)           
C/                                                                      
C GET THE ROW FACTOR.                                                   
      CALL DRCSR(A, M, N, R)                                            
C GET THE COLUMN SCALE FACTOR.                                          
      CALL DRCSC(A, M, N, R, C, RC)                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSR(A, M, N, R)                                      
      INTEGER M, N                                                      
      DOUBLE PRECISION A(M, N), R(M)                                    
      INTEGER I, J                                                      
      DOUBLE PRECISION DABS, DMAX1                                      
C TO GET THE ROW SCALE FACTOR FOR A.                                    
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(16HDRCSR - M .LT. 1, 16, 1, 2)          
C     IF (N .LT. 1) CALL SETERR(16HDRCSR - N .LT. 1, 16, 2, 2)          
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DRCSR - M .LT. 1', 16, 1, 2)           
      IF (N .LT. 1) CALL SETERR('DRCSR - N .LT. 1', 16, 2, 2)           
C/                                                                      
      DO  2 I = 1, M                                                    
         R(I) = 0                                                       
         DO  1 J = 1, N                                                 
            R(I) = DMAX1(DABS(A(I, J)), R(I))                           
   1        CONTINUE                                                    
   2     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DRCSBC(A, M, N, R, C, RC)                              
      INTEGER M, N                                                      
      INTEGER RC(M)                                                     
      DOUBLE PRECISION A(M, N), R(M), C(M)                              
      INTEGER I, J, N2, RD2, MIN0, MAX0                                 
      DOUBLE PRECISION D1MACH, L, S, D1, D2, AIJ                        
      DOUBLE PRECISION AAIJ, DABS, DMAX1                                
      INTEGER TEMP, TEMP1, TEMP2                                        
C TO GET THE COLUMN SCALE FACTOR FOR (1/R)*A.                           
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(17HDRCSBC - M .LT. 1, 17, 1, 2)         
C     IF (N .LT. 1) CALL SETERR(17HDRCSBC - N .LT. 1, 17, 2, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DRCSBC - M .LT. 1', 17, 1, 2)          
      IF (N .LT. 1) CALL SETERR('DRCSBC - N .LT. 1', 17, 2, 2)          
C/                                                                      
      S = D1MACH(1)                                                     
      L = D1MACH(2)                                                     
      DO  1 I = 1, M                                                    
         IF (R(I) .EQ. 0D0) GOTO  1                                     
C/6S                                                                    
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
C    1      37HDRCSBC - MUST HAVE S .LE. R(I) .LE. L, 37, 3, 2)         
C/7S                                                                    
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(                 
     1      'DRCSBC - MUST HAVE S .LE. R(I) .LE. L', 37, 3, 2)          
C/                                                                      
   1     CONTINUE                                                       
      N2 = (N+1)/2                                                      
      DO  17 J = 1, M                                                   
         D2 = 0                                                         
C -1 = UNDERFLOW, 0 = IN-RANGE, +1 = OVERFLOW.                          
         RD2 = -1                                                       
         TEMP1 = MAX0(1, J+N2-M)                                        
         TEMP = MIN0(N, J+N2-1)                                         
         DO  16 I = TEMP1, TEMP                                         
            TEMP2 = J+N2-I                                              
            AIJ = A(TEMP2, I)                                           
            AAIJ = DABS(AIJ)                                            
            TEMP2 = J+N2-I                                              
            D1 = R(TEMP2)                                               
            IF (AIJ .EQ. 0D0 .OR. D1 .EQ. 0D0) GOTO  16                 
            IF (D1 .GE. 1D0) GOTO 8                                     
               IF (AAIJ .LE. D1*L) GOTO 2                               
                  IF (RD2 .LT. 1) D2 = 0                                
C CHECK FOR OVERFLOW.                                                   
C OVERFLOW.                                                             
                  RD2 = 1                                               
                  D2 = DMAX1(D2, AAIJ*(S/D1))                           
                  GOTO  7                                               
   2              IF (RD2 .LE. 0) GOTO 3                                
                     GOTO  16                                           
C THIS ELEMENT IS IN-RANGE.                                             
C ALREADY OVERFLOWED, NO EFFECT.                                        
   3                 IF (RD2 .NE. 0) GOTO 4                             
                        D2 = DMAX1(D2, AAIJ/D1)                         
                        GOTO  5                                         
   4                    RD2 = 0                                         
C RD2 = -1.                                                             
                        D2 = AAIJ/D1                                    
   5              CONTINUE                                              
   6              CONTINUE                                              
   7           CONTINUE                                                 
               GOTO  15                                                 
   8           IF (AAIJ .GE. D1*S) GOTO 9                               
                  IF (RD2 .GE. 0) GOTO  16                              
C ELEMENT UNDERFLOW, D1 >= 1.                                           
C NO-EFFECT.                                                            
                  D2 = DMAX1(D2, AAIJ*(L/D1))                           
                  GOTO  14                                              
   9              IF (RD2 .LE. 0) GOTO 10                               
                     GOTO  16                                           
C IN-RANGE.                                                             
C NO-EFFECT.                                                            
  10                 IF (RD2 .NE. 0) GOTO 11                            
                        D2 = DMAX1(D2, AAIJ/D1)                         
                        GOTO  12                                        
  11                    RD2 = 0                                         
C UNDERFLOWED SO FAR.                                                   
                        D2 = AAIJ/D1                                    
  12              CONTINUE                                              
  13              CONTINUE                                              
  14        CONTINUE                                                    
  15        CONTINUE                                                    
  16        CONTINUE                                                    
         C(J) = D2                                                      
         RC(J) = RD2                                                    
  17     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE D90STS(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,      
     1   AF, AF1, BC, BC1, D, D1, ERROR, ERROR1, ERRPAR, INMI, SCALE,   
     2   HANDLE, HANLE1, BETA, GAMMA, DELTA, N, KMAX, MMAX, XPOLY,      
     3   KINIT, HFRACT, STEPS, NLEQS)                                   
      INTEGER MMAX                                                      
      EXTERNAL AF, AF1, BC, BC1, D, D1                                  
      EXTERNAL ERROR, ERROR1, INMI, SCALE, HANDLE, HANLE1               
      EXTERNAL STEPS, NLEQS                                             
      INTEGER NU, K, NX, NV, N(MMAX), KMAX                              
      INTEGER KINIT                                                     
      REAL ERRPAR(2), HFRACT                                            
      LOGICAL XPOLY                                                     
      DOUBLE PRECISION U(1), X(1), V(1), TSTART, TSTOP, DT              
      DOUBLE PRECISION BETA, GAMMA, DELTA                               
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      EXTERNAL D90STE, D90STH                                           
      INTEGER ISTKGT, IS(1000), IUOLD, IVOLD                            
      REAL RS(1000)                                                     
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION WS(500)                                          
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C THE BASIC POSTS SOLVER.                                               
C SCRATCH SPACE ALLOCATED -                                             
C      S(D90STS) <= 2*MGQ +                                             
C                   MAX ( 17*MGQ, NU*(NX-K) + NV + S(D90STX) )          
C LONG REAL WORDS.                                                      
C U(NX-K,NU),X(NX),V(NV),                                               
      CALL ENTER(1)                                                     
C THE SOLUTION IS (V,U).                                                
      IVOLD = ISTKGT(NU*(NX-K)+NV, 4)                                   
      IUOLD = IVOLD+NV                                                  
      IF (NU .GT. 0) CALL MOVEFD(NU*(NX-K), U, WS(IUOLD))               
      IF (NV .GT. 0) CALL MOVEFD(NV, V, WS(IVOLD))                      
      CALL D90STX(TSTART, TSTOP, STEPS, NLEQS, X, AF, AF1, BC, BC1, D,  
     1   D1, BETA, GAMMA, DELTA, WS(IVOLD), NU*(NX-K)+NV, DT, N, KMAX,  
     2   MMAX, XPOLY, D90STE, ERROR, ERROR1, ERRPAR, INMI, SCALE,       
     3   D90STH, HANDLE, HANLE1, 0.9E0, HFRACT, KINIT)                  
      IF (NU .GT. 0) CALL MOVEFD(NU*(NX-K), WS(IUOLD), U)               
      IF (NV .GT. 0) CALL MOVEFD(NV, WS(IVOLD), V)                      
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE D90STX(TSTART, TSTOP, XA, F, MESH, AF, AF1, BC,        
     1   BC1, D, D1, BETA, GAMMA, DELTA, X, NX, DT, N, KMAX, MMAX,      
     2   XPOLY, SERROR, ERROR1, ERROR2, ERRPAR, INMI, SCALE, SOUT,      
     3   OUTUT1, OUTUT2, PESPAR, HFRACT, KINIT)                         
      INTEGER MMAX, NX                                                  
      EXTERNAL XA, F, AF, AF1, BC, BC1                                  
      EXTERNAL D, D1, SERROR, ERROR1, ERROR2, INMI                      
      EXTERNAL SCALE, SOUT, OUTUT1, OUTUT2                              
      INTEGER N(MMAX), KMAX, KINIT                                      
      REAL ERRPAR(2), PESPAR, HFRACT                                    
      LOGICAL XPOLY, SERROR                                             
      DOUBLE PRECISION TSTART, TSTOP, MESH(1), BETA, GAMMA, DELTA       
      DOUBLE PRECISION X(NX), DT                                        
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D90STY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL RV(30)                                                       
      LOGICAL LV(20)                                                    
      DOUBLE PRECISION WV(30)                                           
      COMMON /D9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION TJ, DTJ                                          
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
C     S(D90STX) <= 2*MMAX + 1 + NX*(KMAX+1) +                           
C     ( 5*KMAX + 2*MMAX + 3 ) INTEGER +                                 
C     MAX ( S(XA), NX*(KMAX+1) REAL +                                   
C           MAX ( KMAX + KMAX INTEGER, S(SERROR) ),                     
C           NX REAL + S(SOUT) )                                         
C LONG REAL.                                                            
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
            CALL XA(T0, X, T1, WS(IX1), NX, N(M), MESH, F, AF, AF1, BC  
     1         , BC1, D, D1, OK, ERROR2, ERRPAR, INMI, SCALE)           
            IF (OK) GOTO 4                                              
               IF (NERROR(NERR) .EQ. 0) GOTO 3                          
                  CALL LEAVE                                            
                  RETURN                                                
   3           CONTINUE                                                 
C     EXTRAPOLATE THE RESULTS.                                          
   4        IF (D4SSOX(WV, RV, IV, LV, N, M)) GOTO  6                   
            IF (M .GT. 1) DONE = SERROR(WS(IX1), NX, MESH, T1, DT,      
     1         ERRPAR, DELTA, RS(IE), ERROR1, ERROR2)                   
C CHECK FOR CONVERGENCE.                                                
C     CHECK FOR A RESTART.                                              
            IF (D4SSOR(WV, RV, IV, LV, ERRPAR)) GOTO  6                 
   5        CONTINUE                                                    
C   GET OPTIMAL DT AND ORDER ( LOZENGE SIZE ).                          
   6     IF (D4SSOM(WV, RV, IV, LV, DT)) GOTO  7                        
C   OUTPUT THE RESULTS FOR THIS TIME-STEP.                              
         CALL SOUT(T0, X, MESH, T1, WS(IX1), NX, DT, TSTOP, OK, RS(IE)  
     1      , OUTUT1, OUTUT2)                                           
C   WIND-UP THIS TIME-STEP.                                             
         IF (D4SSOE(WV, RV, IV, LV, X, TSTOP, DT)) GOTO  7              
         GOTO  1                                                        
   7  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D9OSTA(X, NX, NU, NRHS, NRHSG, GETJAC,           
     1   SEPATE, A1, A1T, A2, A2T, A3, A3T, A4, A4T, F1, F1T, F2, F2T,  
     2   INTVAL, SBASIS, K, MESH, NMESH, AF)                            
      INTEGER K, NRHS, NU, NX, NMESH                                    
      EXTERNAL AF                                                       
      INTEGER NRHSG, INTVAL                                             
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION X(NX), A1(NX, NU, NU), A1T(NX, NU, NU), A2(NX,   
     1   NU, NU), A2T(NX, NU, NU), A3(NX, NU, NU)                       
      DOUBLE PRECISION A3T(NX, NU, NU), A4(NX, NU, NU), A4T(NX, NU, NU),
     1   F1(NX, NU, NRHS), F1T(NX, NU, NRHS), F2(NX, NU, NRHS)          
      DOUBLE PRECISION F2T(NX, NU, NRHS), SBASIS(NX, K, 2), MESH(NMESH) 
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /DPOSTF/ FAILED                                            
      LOGICAL FAILED                                                    
      COMMON /D9OSTT/ T, DT                                             
      DOUBLE PRECISION T, DT                                            
      COMMON /D9OSTM/ THETA, EGIVE, IZAP                                
      INTEGER IZAP(3)                                                   
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      COMMON /D9OSTK/ IUTETA, IVTETA, IUT, IVT                          
      INTEGER IUTETA, IVTETA, IUT, IVT                                  
      COMMON /D9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /D90STV/ IEU                                               
      INTEGER IEU                                                       
      COMMON /D90STK/ IZAP1, NV, IZAP2                                  
      INTEGER IZAP1, NV, IZAP2(2)                                       
      INTEGER I, J, IS(1000), IX                                        
      REAL RS(1000)                                                     
      LOGICAL BEULER, LS(1000)                                          
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP, TEMP1, TEMP2                                        
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO LINEARIZE THE AF RESULTS.                                          
C SCRATCH SPACE ALLOCATED - S(D9OSTA) =                                 
C                           S(AF) + 2*NX*NU*(NV+2)                      
C LONG REAL WORDS.                                                      
      BEULER = THETA .EQ. 1D0                                           
      CALL DQSPLN(WS(IUTETA), NU, NMESH-K, K, X, NX, INTVAL, SBASIS, 2  
     1   , WS(IEU))                                                     
      TEMP2 = IEU+2*NX*NU                                               
      CALL DQSPLN(WS(IUT), NU, NMESH-K, K, X, NX, INTVAL, SBASIS, 2, WS(
     1   TEMP2))                                                        
      FAILED = .FALSE.                                                  
      TEMP2 = IEU+NX*NU                                                 
      TEMP1 = IEU+2*NX*NU                                               
      TEMP = IEU+3*NX*NU                                                
      CALL AF(T, X, NX, WS(IEU), WS(TEMP2), WS(TEMP1), WS(TEMP), NU, WS(
     1   IVTETA), WS(IVT), NV, F1, A2, A1, A2T, A1T, F1(1, 1, 2), F1T(1,
     2   1, 2), F2, A4, A3, A4T, A3T, F2(1, 1, 2), F2T(1, 1, 2))        
      IF (.NOT. FAILED) GOTO 1                                          
         FNUM = 1                                                       
         D9OSTA = .TRUE.                                                
         RETURN                                                         
   1  DO  10 IX = 1, NX                                                 
         DO  9 I = 1, NU                                                
            F1(IX, I, 1) = -F1(IX, I, 1)                                
            IF (.NOT. GETJAC) GOTO  9                                   
            DO  4 J = 1, NU                                             
               IF (BEULER) GOTO 2                                       
                  A1(IX, I, J) = A1(IX, I, J)*THETA                     
                  A2(IX, I, J) = A2(IX, I, J)*THETA                     
                  A3(IX, I, J) = A3(IX, I, J)*THETA                     
                  A4(IX, I, J) = A4(IX, I, J)*THETA                     
   2           IF (SEPATE) GOTO 3                                       
                  A1(IX, I, J) = A1(IX, I, J)+A1T(IX, I, J)/DT          
                  A2(IX, I, J) = A2(IX, I, J)+A2T(IX, I, J)/DT          
                  A3(IX, I, J) = A3(IX, I, J)+A3T(IX, I, J)/DT          
                  A4(IX, I, J) = A4(IX, I, J)+A4T(IX, I, J)/DT          
   3           CONTINUE                                                 
   4           CONTINUE                                                 
            IF (NV .EQ. 0) GOTO  9                                      
            TEMP = NV+1                                                 
            DO  8 J = 2, TEMP                                           
               IF (BEULER) GOTO 5                                       
                  F1(IX, I, J) = F1(IX, I, J)*THETA                     
                  F2(IX, I, J) = F2(IX, I, J)*THETA                     
   5           IF (SEPATE) GOTO 6                                       
                  F1(IX, I, J) = -(F1(IX, I, J)+F1T(IX, I, J)/DT)       
                  F2(IX, I, J) = F2(IX, I, J)+F2T(IX, I, J)/DT          
                  GOTO  7                                               
   6              F1(IX, I, J) = -F1(IX, I, J)                          
                  F1T(IX, I, J) = -F1T(IX, I, J)                        
   7           CONTINUE                                                 
   8           CONTINUE                                                 
   9        CONTINUE                                                    
  10     CONTINUE                                                       
      D9OSTA = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D9OSTB(U, UT, NU, V, VT, NV, T, DT, K, X,        
     1   NX, GETJAC, SEPATE, BC, ALFA, BETA, GAMMA, ALFAT, BETAT,       
     2   GAMMAT, NXMK, NRHS, NRHSG, EU, EUX, EUT, EUTX)                 
      INTEGER NRHS, NXMK, NU, NX                                        
      EXTERNAL BC                                                       
      INTEGER NV, K, NRHSG                                              
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION U(NXMK, NU), UT(NXMK, NU), V(1), VT(1), T, DT    
      DOUBLE PRECISION X(NX), ALFA(NU, NU, 2), BETA(NU, NU, 2), GAMMA(  
     1   NU, NRHS, 2), ALFAT(NU, NU, 2), BETAT(NU, NU, 2)               
      DOUBLE PRECISION GAMMAT(NU, NRHS, 2), EU(NU, 2), EUX(NU, 2), EUT( 
     1   NU, 2), EUTX(NU, 2)                                            
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D9OSTM/ THETA, EGIVE, IZAP                                
      INTEGER IZAP(3)                                                   
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      COMMON /D9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /DPOSTF/ FAILED                                            
      LOGICAL FAILED                                                    
      INTEGER IGAMMA, ISTKGT, IGAMA0, IGAMA1, I, J                      
      INTEGER L, ID(1), IALFA, IBETA, IS(1000), IALFA1                  
      INTEGER IBETA1                                                    
      REAL RS(1000)                                                     
      LOGICAL BEULER, LS(1000)                                          
      DOUBLE PRECISION LR(2), WS(500)                                   
      INTEGER TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
      DATA ID(1)/1/                                                     
C SCRATCH SPACE ALLOCATED - S(D9OSTB) = MAX ( 6*K, S(BC) )              
C                                       LONG REAL WORDS.                
C (V,VT)(NV).                                                           
      CALL ENTER(1)                                                     
      BEULER = THETA .EQ. 1D0                                           
      LR(1) = X(1)                                                      
C SO THAT BC CAN CLOBBER L AND R.                                       
      LR(2) = X(NX)                                                     
      DO  1 I = 1, NU                                                   
         EU(I, 1) = U(1, I)                                             
         EU(I, 2) = U(NXMK, I)                                          
         CALL DSPLN1(K, X, NX, U(1, I), X(1), 1, ID, 1, EUX(I, 1))      
         CALL DSPLN1(K, X, NX, U(1, I), X(NX), 1, ID, 1, EUX(I, 2))     
         EUT(I, 1) = UT(1, I)                                           
         EUT(I, 2) = UT(NXMK, I)                                        
         CALL DSPLN1(K, X, NX, UT(1, I), X(1), 1, ID, 1, EUTX(I, 1))    
         CALL DSPLN1(K, X, NX, UT(1, I), X(NX), 1, ID, 1, EUTX(I, 2))   
   1     CONTINUE                                                       
      FAILED = .FALSE.                                                  
      IF (.NOT. GETJAC) GOTO 3                                          
         IGAMMA = ISTKGT(2*NU, 4)                                       
         CALL SETD(2*NU**2, 0D0, ALFA)                                  
         CALL SETD(2*NU**2, 0D0, BETA)                                  
         CALL SETD(2*NU, 0D0, WS(IGAMMA))                               
         CALL SETD(2*NU**2, 0D0, ALFAT)                                 
         CALL SETD(2*NU**2, 0D0, BETAT)                                 
         CALL SETD(2*NU*NRHS, 0D0, GAMMA)                               
         CALL SETD(2*NU*NRHS, 0D0, GAMMAT)                              
         CALL BC(T, LR(1), LR(2), EU, EUX, EUT, EUTX, NU, V, VT, NV, WS(
     1      IGAMMA), ALFA, BETA, ALFAT, BETAT, GAMMA(1, 2, 1), GAMMAT(1,
     2      2, 1))                                                      
         IF (NV .LE. 0) GOTO 2                                          
            CALL MOVEBD(NU*NV, GAMMA(1, 1, 2), GAMMA(1, 2, 2))          
            CALL MOVEBD(NU*NV, GAMMAT(1, 1, 2), GAMMAT(1, 2, 2))        
   2     CALL SETD(NU, 0D0, GAMMAT(1, 1, 1))                            
         CALL SETD(NU, 0D0, GAMMAT(1, 1, 2))                            
         GOTO  4                                                        
   3     IGAMMA = ISTKGT(2*NU*(4*NU+1+2*NV), 4)                         
         IALFA = IGAMMA+2*NU                                            
         IBETA = IALFA+2*NU**2                                          
         IALFA1 = IBETA+2*NU**2                                         
         IBETA1 = IALFA1+2*NU**2                                        
         IGAMA0 = IBETA1+2*NU**2                                        
         IGAMA1 = IGAMA0+2*NU*NV                                        
         CALL SETD(2*NU*(4*NU+1+2*NV), 0D0, WS(IGAMMA))                 
         CALL BC(T, LR(1), LR(2), EU, EUX, EUT, EUTX, NU, V, VT, NV, WS(
     1      IGAMMA), WS(IALFA), WS(IBETA), WS(IALFA1), WS(IBETA1), WS(  
     2      IGAMA0), WS(IGAMA1))                                        
   4  IF (.NOT. FAILED) GOTO 5                                          
         FNUM = 2                                                       
         CALL LEAVE                                                     
         D9OSTB = .TRUE.                                                
         RETURN                                                         
   5  IF (NRHSG .LE. 0) GOTO 6                                          
         CALL MOVEFD(NU, WS(IGAMMA), GAMMA)                             
         TEMP = IGAMMA+NU                                               
         CALL MOVEFD(NU, WS(TEMP), GAMMA(1, 1, 2))                      
   6  DO  14 L = 1, 2                                                   
         DO  13 I = 1, NU                                               
            GAMMA(I, 1, L) = -GAMMA(I, 1, L)                            
            IF (.NOT. GETJAC) GOTO  13                                  
            DO  9 J = 1, NU                                             
               IF (BEULER) GOTO 7                                       
                  ALFA(I, J, L) = ALFA(I, J, L)*THETA                   
                  BETA(I, J, L) = BETA(I, J, L)*THETA                   
   7           IF (SEPATE) GOTO 8                                       
                  ALFA(I, J, L) = ALFA(I, J, L)+ALFAT(I, J, L)/DT       
                  BETA(I, J, L) = BETA(I, J, L)+BETAT(I, J, L)/DT       
   8           CONTINUE                                                 
   9           CONTINUE                                                 
            IF (NV .EQ. 0) GOTO  13                                     
            TEMP = NV+1                                                 
            DO  12 J = 2, TEMP                                          
               IF (.NOT. BEULER) GAMMA(I, J, L) = GAMMA(I, J, L)*THETA  
               IF (SEPATE) GOTO 10                                      
                  GAMMA(I, J, L) = -(GAMMA(I, J, L)+GAMMAT(I, J, L)/DT) 
                  GOTO  11                                              
  10              GAMMA(I, J, L) = -GAMMA(I, J, L)                      
                  GAMMAT(I, J, L) = -GAMMAT(I, J, L)                    
  11           CONTINUE                                                 
  12           CONTINUE                                                 
  13        CONTINUE                                                    
  14     CONTINUE                                                       
      CALL LEAVE                                                        
      D9OSTB = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D9OSTD(K, X, NX, NXMK, U, UT, NU, V, VT,         
     1   NV, T, GETJAC, SEPATE, D, D1, D1T, D2, D3, D3T)                
      INTEGER NXMK, NV, NX                                              
      EXTERNAL D                                                        
      INTEGER K, NU                                                     
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION X(NX), U(NXMK, 1), UT(NXMK, 1), V(NV), VT(NV), T 
      DOUBLE PRECISION D1(NV, NXMK, 1), D1T(NV, NXMK, 1), D2(NV), D3(NV,
     1   NV), D3T(NV, NV)                                               
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D9OSTT/ TT, DT                                            
      DOUBLE PRECISION TT, DT                                           
      COMMON /D9OSTM/ THETA, EGIVE, IZAP                                
      INTEGER IZAP(3)                                                   
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      COMMON /D9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /DPOSTF/ FAILED                                            
      LOGICAL FAILED                                                    
      INTEGER ISTKGT, ID1T, ID3T, I, J, L                               
      INTEGER IS(1000), ID1, ID3                                        
      REAL RS(1000)                                                     
      LOGICAL BEULER, LS(1000)                                          
      DOUBLE PRECISION WS(500)                                          
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO LINEARIZE THE D RESULTS.                                           
C SCRATCH SPACE ALLOCATED - S(D9OSTD) = S(D) LONG REAL WORDS.           
C (U,UT)(NXMK,NU).                                                      
C           (D1,D1T)(NV,NXMK,NU).                                       
      BEULER = THETA .EQ. 1D0                                           
      FAILED = .FALSE.                                                  
      CALL SETD(NV, 0D0, D2)                                            
      IF (.NOT. GETJAC) GOTO 1                                          
         IF (NU .GT. 0) CALL SETD(2*NV*NU*NXMK, 0D0, D1)                
         CALL SETD(2*NV**2, 0D0, D3)                                    
         CALL D(T, K, X, NX, U, UT, NU, NX-K, V, VT, NV, D2, D1, D1T,   
     1      D3, D3T)                                                    
         GOTO  2                                                        
   1     ID1 = ISTKGT(2*NV*(NU*NXMK+NV), 4)                             
         ID1T = ID1+NV*NU*NXMK                                          
         ID3 = ID1T+NV*NU*NXMK                                          
         ID3T = ID3+NV**2                                               
C DEFAULT VALUES.                                                       
         CALL SETD(2*NV*(NU*NXMK+NV), 0D0, WS(ID1))                     
         CALL D(T, K, X, NX, U, UT, NU, NX-K, V, VT, NV, D2, WS(ID1),   
     1      WS(ID1T), WS(ID3), WS(ID3T))                                
         CALL ISTKRL(1)                                                 
   2  IF (.NOT. FAILED) GOTO 3                                          
         FNUM = 3                                                       
         D9OSTD = .TRUE.                                                
         RETURN                                                         
   3  DO  9 I = 1, NV                                                   
         D2(I) = -D2(I)                                                 
         IF (.NOT. GETJAC) GOTO  9                                      
         DO  6 J = 1, NV                                                
            IF (.NOT. BEULER) D3(I, J) = D3(I, J)*THETA                 
            IF (SEPATE) GOTO 4                                          
               D3(I, J) = -(D3(I, J)+D3T(I, J)/DT)                      
               GOTO  5                                                  
   4           D3(I, J) = -D3(I, J)                                     
               D3T(I, J) = -D3T(I, J)                                   
   5        CONTINUE                                                    
   6        CONTINUE                                                    
         IF (NU .EQ. 0) GOTO  9                                         
         DO  8 J = 1, NU                                                
            DO  7 L = 1, NXMK                                           
               IF (.NOT. BEULER) D1(I, L, J) = D1(I, L, J)*THETA        
               IF (.NOT. SEPATE) D1(I, L, J) = D1(I, L, J)+D1T(I, L, J)/
     1            DT                                                    
   7           CONTINUE                                                 
   8        CONTINUE                                                    
   9     CONTINUE                                                       
      D9OSTD = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D9OSTE(U, NU, K, X, NX, V, NV, T, DT,            
     1   ERRPAR, ERPUTS, EU, EV, ERROR)                                 
      INTEGER NX                                                        
      EXTERNAL ERROR                                                    
      INTEGER NU, K, NV                                                 
      REAL ERRPAR(2), EU(1), EV(1)                                      
      LOGICAL ERPUTS, ERROR                                             
      DOUBLE PRECISION U(1), X(NX), V(1), T, DT                         
C THE ERROR FILTER FOR NLEQS.                                           
C SCRATCH SPACE ALLOCATED - S(D9OSTE) = S(ERROR).                       
C U(NX-K,NU),V(NV).                                                     
C EU(NX-K,NU),EV(NV).                                                   
      D9OSTE = ERROR(U, NU, NX-K, K, X, NX, V, NV, T, DT, ERRPAR,       
     1   ERPUTS, EU, EV)                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE D9OSTH(T0, U0, V0, T, U, V, NU, NV, K, X, NX, DT,      
     1   TSTOP, EU, EV, OK, HANDLE)                                     
      INTEGER NX                                                        
      EXTERNAL HANDLE                                                   
      INTEGER NU, NV, K                                                 
      REAL EU(1), EV(1)                                                 
      LOGICAL OK                                                        
      DOUBLE PRECISION T0, U0(1), V0(1), T, U(1), V(1)                  
      DOUBLE PRECISION X(NX), DT, TSTOP                                 
      COMMON /D9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /D90STT/ TGOOD                                             
      DOUBLE PRECISION TGOOD                                            
      COMMON /D90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS      
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS                         
      INTEGER NNFS, NRS                                                 
      LOGICAL TEMP                                                      
C OUTPUT FILTER FOR POSTS.                                              
C SCRATCH SPACE ALLOCATED - S(D9OSTH) = S(HANDLE).                      
C (U0,U)(NX-K,NU),(V0,V)(NV).                                           
C EU(NX-K,NU),EV(NV).                                                   
      IF (T0 .EQ. T) GOTO 1                                             
         FNUM = 0                                                       
         TGOOD = T                                                      
         GOTO  2                                                        
   1     NRS = NRS+1                                                    
   2  TEMP = T0 .EQ. T                                                  
      IF (TEMP) TEMP = KEEJAC .EQ. 3                                    
      IF (.NOT. TEMP) GOTO 3                                            
         GETJAC = T0 .NE. TJ                                            
         TJ = T0                                                        
   3  NTSS = NTSS+1                                                     
      CALL HANDLE(T0, U0, V0, T, U, V, NU, NX-K, NV, K, X, NX, DT,      
     1   TSTOP)                                                         
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D9OSTN(U, NU, V, NV, T, DT, K, X, NX, AF,        
     1   AF1, BC, BC1, D, D1234, ERROR, INMI, SCALE, ERRPAR)            
      INTEGER NX                                                        
      EXTERNAL AF, AF1, BC, BC1, D, D1234                               
      EXTERNAL ERROR, INMI, SCALE                                       
      INTEGER NU, NV, K                                                 
      REAL ERRPAR(2)                                                    
      DOUBLE PRECISION U(1), V(1), T, DT, X(NX)                         
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      COMMON /D9OSTK/ IUTETA, IVTETA, IUT, IVT                          
      INTEGER IUTETA, IVTETA, IUT, IVT                                  
      INTEGER IDU, IEU, IEV, IDV, ISTKGT, IEU1                          
      INTEGER IEV1, IEU2, IEV2, IS(1000), IUOLD, IVOLD                  
      REAL RS(1000)                                                     
      LOGICAL D9OSTO, DONE, LS(1000)                                    
      DOUBLE PRECISION WS(500)                                          
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C NONLINEAR EQUATION SOLVER FOR POSTS.                                  
C SCRATCH SPACE ALLOCATED -                                             
C     S(D9OSTN) = IF ( THETA ~= 1 ) { NU*(NX-K)+NV } +                  
C                 3*(NU*(NX-K)+NV) +                                    
C                 ( NU*(NX-K)+NV ) INTEGER +                            
C                 MAX ( S(D90STC), S(ERROR) )                           
C LONG REAL WORDS.                                                      
C U(NX-K,NU),V(NV).                                                     
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
      CALL ENTER(1)                                                     
      IVOLD = ISTKGT(NU*(NX-K)+NV, 4)                                   
      IUOLD = IVOLD+NV                                                  
      IVTETA = ISTKGT(NU*(NX-K)+NV, 4)                                  
      IUTETA = IVTETA+NV                                                
      IVT = ISTKGT(NU*(NX-K)+NV, 4)                                     
      IUT = IVT+NV                                                      
      IEV = ISTKGT(NU*(NX-K)+NV, 3)                                     
      IEU = IEV+NV                                                      
      IDV = ISTKGT(NU*(NX-K)+NV, 4)                                     
      IDU = IDV+NV                                                      
      IEV1 = ISTKGT(NU*(NX-K)+NV, 3)                                    
      IEU1 = IEV1+NV                                                    
      IEV2 = ISTKGT(NU*(NX-K)+NV, 3)                                    
      IEU2 = IEV2+NV                                                    
      DONE = D9OSTO(U, NU, V, NV, T, DT, K, X, NX, AF, AF1, BC, BC1, D  
     1   , D1234, ERROR, INMI, SCALE, ERRPAR, WS(IUTETA), WS(IUT), WS(  
     2   IVTETA), WS(IVT), WS(IUOLD), WS(IVOLD), RS(IEU), RS(IEV), RS(  
     3   IEU1), RS(IEV1), RS(IEU2), RS(IEV2), WS(IDU), WS(IDV), NX-K,   
     4   THETA, MINIT, MAXIT, KEEJAC, EGIVE)                            
      CALL LEAVE                                                        
      D9OSTN = DONE                                                     
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D9OSTO(U, NU, V, NV, T, DT, K, X, NX, AF,        
     1   AF1, BC, BC1, D, D1234, ERROR, INMI, SCALE, ERRPAR, UTHETA, UT,
     2   VTHETA, VT, UOLD, VOLD, EU, EV, EU1, EV1, EU2, EV2, DU, DV,    
     3   NXMK, THETA, MINIT, MAXIT, KEEJAC, EGIVE)                      
      INTEGER NXMK, NX                                                  
      EXTERNAL AF, AF1, BC, BC1, D, D1234                               
      EXTERNAL ERROR, INMI, SCALE                                       
      INTEGER NU, NV, K, MINIT, MAXIT, KEEJAC                           
      REAL ERRPAR(2), EU(NXMK, 1), EV(1), EU1(NXMK, 1), EV1(1), EU2(    
     1   NXMK, 1)                                                       
      REAL EV2(1), EGIVE                                                
      LOGICAL ERROR                                                     
      DOUBLE PRECISION U(NXMK, 1), V(1), T, DT, X(NX), UTHETA(NXMK, 1)  
      DOUBLE PRECISION UT(NXMK, 1), VTHETA(1), VT(1), UOLD(NXMK, 1),    
     1   VOLD(1), DU(NXMK, 1)                                           
      DOUBLE PRECISION DV(1), THETA                                     
      COMMON /D90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS      
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS                         
      INTEGER NNFS, NRS                                                 
      COMMON /D9OSTL/ ERPUTS                                            
      LOGICAL ERPUTS                                                    
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      INTEGER I, J, ITER                                                
      REAL RHO, PROD, TEMP, POWER, R1MACH                               
      LOGICAL DONE, NCGCE, D90STC                                       
      DOUBLE PRECISION DABS                                             
      INTEGER TEMP2, TEMP3                                              
      LOGICAL TEMP1                                                     
C U(NXMK,NU),V(NV).                                                     
C           (UTHETA,UT)(NXMK,NU),(VTHETA,VT)(NV).                       
C UOLD(NXMK,NU),VOLD(NV).                                               
C DU(NXMK,NU),DV(NV).                                                   
C REAL EU(NXMK,NU),EV(NV),EU1(NXMK,NU),EV1(NV),EU2(NXMK,NU),EV2(NV).    
      IF (NU .LE. 0) GOTO 1                                             
         CALL MOVEFD(NU*(NX-K), U, UOLD)                                
         CALL SETD(NU*(NX-K), 0D0, UT)                                  
   1  IF (NV .LE. 0) GOTO 2                                             
         CALL MOVEFD(NV, V, VOLD)                                       
         CALL SETD(NV, 0D0, VT)                                         
C GET INITIAL NEWTON METHOD GUESS.                                      
   2  CALL INMI(NU, NV, NX-K, K, X, NX, T, DT, UOLD, VOLD, U, UT, V, VT)
      DO  49 ITER = 1, MAXIT                                            
         IF (KEEJAC .NE. 0) GOTO 3                                      
            GETJAC = .TRUE.                                             
            TJ = T                                                      
   3     IF (GETJAC) NJS = NJS+1                                        
         NNITS = NNITS+1                                                
         J = 1                                                          
            GOTO  5                                                     
   4        J = J+1                                                     
   5        IF (J .GT. NU) GOTO  7                                      
            DO  6 I = 1, NXMK                                           
               UTHETA(I, J) = THETA*(U(I, J)-UOLD(I, J))+UOLD(I, J)     
   6           CONTINUE                                                 
            GOTO  4                                                     
   7     I = 1                                                          
            GOTO  9                                                     
   8        I = I+1                                                     
   9        IF (I .GT. NV) GOTO  10                                     
            VTHETA(I) = THETA*(V(I)-VOLD(I))+VOLD(I)                    
            GOTO  8                                                     
  10     DONE = D90STC(UTHETA, UT, NU, VTHETA, VT, NV, T, DT, K, X, NX  
     1      , AF, AF1, BC, BC1, D, D1234, SCALE, DU, DV)                
         IF (.NOT. DONE) GOTO 11                                        
            DONE = .FALSE.                                              
            GOTO  50                                                    
  11     J = 1                                                          
            GOTO  13                                                    
  12        J = J+1                                                     
  13        IF (J .GT. NU) GOTO  15                                     
            DO  14 I = 1, NXMK                                          
               U(I, J) = U(I, J)+DU(I, J)                               
               EU(I, J) = EGIVE*DABS(DU(I, J))                          
  14           CONTINUE                                                 
            GOTO  12                                                    
  15     I = 1                                                          
            GOTO  17                                                    
  16        I = I+1                                                     
  17        IF (I .GT. NV) GOTO  18                                     
            V(I) = V(I)+DV(I)                                           
            EV(I) = EGIVE*DABS(DV(I))                                   
            GOTO  16                                                    
  18     IF (MAXIT .NE. 1) GOTO 19                                      
            DONE = .TRUE.                                               
            GOTO  50                                                    
  19     CALL MOVEFR(NU*NXMK+NV, EV, EV2)                               
         DONE = ERROR(U, NU, NX-K, K, X, NX, V, NV, T, DT, ERRPAR,      
     1      ERPUTS, EU, EV)                                             
C CHECK FOR NEGATIVE ERROR REQUESTS.                                    
         TEMP2 = NU*NXMK+NV                                             
         DO  21 I = 1, TEMP2                                            
            TEMP1 = EV(I) .EQ. 0.                                       
            IF (TEMP1) TEMP1 = EV2(I) .NE. 0.                           
            IF (.NOT. TEMP1) TEMP1 = EV(I) .LT. 0.                      
            IF (.NOT. TEMP1) GOTO 20                                    
C/6S                                                                    
C              CALL SETERR(37HDESSOM - E(I).LE.0 RETURNED BY SERROR, 37,
C    1            19, 1)                                                
C/7S                                                                    
               CALL SETERR('DESSOM - E(I).LE.0 RETURNED BY SERROR', 37, 
     1            19, 1)                                                
C/                                                                      
               D9OSTO = .FALSE.                                         
               RETURN                                                   
  20        CONTINUE                                                    
  21        CONTINUE                                                    
         IF (.NOT. DONE) GOTO 22                                        
            GOTO  50                                                    
  22        IF (ITER .NE. MAXIT) GOTO 23                                
               FNUM = 9                                                 
               NNFS = NNFS+1                                            
               GOTO  50                                                 
  23     CONTINUE                                                       
C NO CONVERGENCE.                                                       
  24     NCGCE = .FALSE.                                                
         TEMP2 = NU*NXMK+NV                                             
         DO  34 I = 1, TEMP2                                            
            TEMP1 = ITER .GT. MINIT                                     
            IF (TEMP1) TEMP1 = EV(I) .LT. EV2(I)                        
            IF (.NOT. TEMP1) GOTO 33                                    
               IF (EV1(I) .LE. EV2(I)) GOTO 25                          
                  RHO = EV2(I)/EV1(I)                                   
C CAN CHECK CONVERGENCE RATE.                                           
                  GOTO  26                                              
  25              RHO = 1                                               
  26           IF (RHO .LT. 1.) GOTO 27                                 
                  NCGCE = .TRUE.                                        
C DIVERGING.                                                            
                  GOTO  32                                              
  27              IF (KEEJAC .NE. 0) GOTO 30                            
                     PROD = 1                                           
C CONVERGING.                                                           
C CHECK QUADRATIC CONVERGENCE RATE.                                     
                     POWER = RHO**2                                     
C < 1.                                                                  
                     TEMP = EV(I)/EV2(I)                                
                     TEMP3 = MAXIT-ITER                                 
                     DO  28 J = 1, TEMP3                                
                        PROD = PROD*POWER                               
                        POWER = POWER**2                                
                        IF (PROD .LE. TEMP) GOTO  29                    
  28                    CONTINUE                                        
  29                 IF (PROD .GT. TEMP) NCGCE = .TRUE.                 
C SLOW CONVERGENCE.                                                     
                     GOTO  31                                           
  30                 IF (RHO**(MAXIT-ITER)*EV2(I) .GT. EV(I)) NCGCE =   
     1                  .TRUE.                                          
C KEEPJAC > 0 AND SHOULD CHECK LINEAR CONVERGENCE RATE.                 
C SLOW CONVERGENCE.                                                     
  31              CONTINUE                                              
  32           IF (NCGCE) GOTO  35                                      
  33        EV1(I) = EV2(I)                                             
  34        CONTINUE                                                    
  35     TEMP1 = NCGCE                                                  
         IF (TEMP1) TEMP1 = KEEJAC .EQ. 4                               
         IF (.NOT. TEMP1) GOTO 39                                       
            IF (T .NE. TJ) GOTO 36                                      
               NNDS = NNDS+1                                            
               FNUM = 8                                                 
               D9OSTO = DONE                                            
               RETURN                                                   
C HAVE NEW JACOBIAN, DIE.                                               
  36        IF (NU .LE. 0) GOTO 37                                      
               CALL MOVEFD(NU*(NX-K), UOLD, U)                          
               CALL SETD(NU*(NX-K), 0D0, UT)                            
  37        IF (NV .LE. 0) GOTO 38                                      
               CALL MOVEFD(NV, VOLD, V)                                 
               CALL SETD(NV, 0D0, VT)                                   
C     GET INITIAL NEWTON METHOD GUESS.                                  
  38        CALL INMI(NU, NV, NX-K, K, X, NX, T, DT, UOLD, VOLD, U, UT  
     1         , V, VT)                                                 
            TJ = T                                                      
            GETJAC = .TRUE.                                             
            FNUM = 0                                                    
C EV1 == BIG FOR NEXT ITERATION.                                        
            CALL SETR(NU*NXMK+NV, R1MACH(2), EV1)                       
            GOTO  49                                                    
  39        IF (.NOT. NCGCE) GOTO 40                                    
               NNDS = NNDS+1                                            
               FNUM = 8                                                 
               D9OSTO = DONE                                            
               RETURN                                                   
  40     CONTINUE                                                       
  41     J = 1                                                          
            GOTO  43                                                    
  42        J = J+1                                                     
  43        IF (J .GT. NU) GOTO  45                                     
            DO  44 I = 1, NXMK                                          
               UT(I, J) = UT(I, J)+DU(I, J)/DT                          
  44           CONTINUE                                                 
            GOTO  42                                                    
  45     I = 1                                                          
            GOTO  47                                                    
  46        I = I+1                                                     
  47        IF (I .GT. NV) GOTO  48                                     
            VT(I) = VT(I)+DV(I)/DT                                      
            GOTO  46                                                    
  48     CONTINUE                                                       
  49     CONTINUE                                                       
  50  D9OSTO = DONE                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE D9OSTP(T0, S0, T1, S1, NS, N, X, NLEQS, AF, AF1        
     1   , BC, BC1, D, D1234, OK, ERROR, ERRPAR, INMI, SCALE)           
      INTEGER NS                                                        
      EXTERNAL NLEQS, AF, AF1, BC, BC1, D                               
      EXTERNAL D1234, ERROR, INMI, SCALE                                
      INTEGER N                                                         
      REAL ERRPAR(2)                                                    
      LOGICAL NLEQS, OK                                                 
      DOUBLE PRECISION T0, S0(NS), T1, S1(NS), X(1)                     
      COMMON /D90STY/ WV, RV, IV, LV                                    
      INTEGER IV(40)                                                    
      REAL RV(30)                                                       
      LOGICAL LV(20)                                                    
      DOUBLE PRECISION WV(30)                                           
      COMMON /D90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS      
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS                         
      INTEGER NNFS, NRS                                                 
      COMMON /D90STK/ NU, NV, K, NX                                     
      INTEGER NU, NV, K, NX                                             
      COMMON /D9OSTT/ TC, DTC                                           
      DOUBLE PRECISION TC, DTC                                          
      COMMON /D9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      INTEGER ISTEP                                                     
      REAL FLOAT                                                        
      DOUBLE PRECISION TSTART, DBLE, T, DT                              
      LOGICAL TEMP, TEMP1                                               
      EQUIVALENCE (TSTART, WV(1))                                       
C TIME-STEPPING SCHEME FOR POSTS.                                       
C SCRATCH SPACE ALLOCATED -                                             
C     S(D9OSTP) = NU*(NX-K)*(2*K*NU-1) + NV**2 + S(NLEQS).              
C LONG REAL WORDS.                                                      
      CALL ENTER(1)                                                     
      DT = (T1-T0)/DBLE(FLOAT(N))                                       
      DTC = DT                                                          
C INITIAL APPROXIMATION FOR S1.                                         
      CALL MOVEFD(NS, S0, S1)                                           
      DO  4 ISTEP = 1, N                                                
         T = T0+(DBLE(FLOAT(ISTEP-1))+THETA)*DT                         
         TC = T                                                         
         NSSS = NSSS+1                                                  
         IF (KEEJAC .NE. 1) GOTO 1                                      
            GETJAC = .TRUE.                                             
            TJ = T                                                      
   1     TEMP = DT .GT. 0D0                                             
         IF (TEMP) TEMP = T .GT. T1                                     
         IF (TEMP) GOTO 2                                               
            TEMP1 = DT .LT. 0D0                                         
            IF (TEMP1) TEMP1 = T .LT. T1                                
            TEMP = TEMP1                                                
   2     IF (.NOT. TEMP) GOTO 3                                         
            T = T1                                                      
            TC = T                                                      
   3     OK = NLEQS(S1(NV+1), NU, S1, NV, T, DT, K, X, NX, AF, AF1, BC  
     1      , BC1, D, D1234, ERROR, INMI, SCALE, ERRPAR)                
         IF (.NOT. OK) GOTO  5                                          
   4     CONTINUE                                                       
   5  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DPOSTX                                                 
      COMMON /D90STR/ STATS                                             
      INTEGER STATS(8)                                                  
      INTEGER I1MACH                                                    
      INTEGER TEMP                                                      
C TO PRINT THE RUN-TIME STATISTICS FOR POST.                            
      CALL D9OSTX(STATS, 0)                                             
      TEMP = I1MACH(2)                                                  
      WRITE (TEMP,  1) STATS                                            
   1  FORMAT (31H DPOST(J,F,TS,SS,NIT,ND,NF,R) =, 8(I5))                
      RETURN                                                            
      END                                                               
      SUBROUTINE D9OSTX(XSTATS, IFLAG)                                  
      INTEGER XSTATS(8), IFLAG                                          
      INTEGER STATS(8)                                                  
      LOGICAL INPOST                                                    
      DATA STATS(1)/0/                                                  
      DATA STATS(2)/0/                                                  
      DATA STATS(3)/0/                                                  
      DATA STATS(4)/0/                                                  
      DATA STATS(5)/0/                                                  
      DATA STATS(6)/0/                                                  
      DATA STATS(7)/0/                                                  
      DATA STATS(8)/0/                                                  
      DATA INPOST/.FALSE./                                              
C INTERNAL SAVING OF STATISTICS FOR POST.                               
C FOR IFLAG = 0, THE STATS ARE SIMPLY REPORTED.                         
C FOR IFLAG > 0, IT ENTERS POST.                                        
C FOR IFLAG < 0, IT EXITS POST.                                         
      IF (IFLAG .NE. 0) GOTO 1                                          
         IF (.NOT. INPOST) CALL MOVEFI(8, STATS, XSTATS)                
         GOTO  4                                                        
   1     IF (IFLAG .LE. 0) GOTO 2                                       
            INPOST = .TRUE.                                             
            CALL SETI(8, 0, STATS)                                      
            CALL MOVEFI(8, STATS, XSTATS)                               
            GOTO  3                                                     
   2        INPOST = .FALSE.                                            
C IFLAG < 0.                                                            
            CALL MOVEFI(8, XSTATS, STATS)                               
   3  CONTINUE                                                          
   4  RETURN                                                            
      END                                                               
      SUBROUTINE DQSPLN(A, NA, NXMK, K, Y, NY, INTVAL, SBASIS, ND,      
     1   SPLINE)                                                        
      INTEGER K, NXMK, NA, ND, NY                                       
      INTEGER INTVAL                                                    
      DOUBLE PRECISION A(NXMK, NA), Y(NY), SBASIS(NY, K, ND), SPLINE(NY,
     1   NA, ND)                                                        
      INTEGER MIN0, MAX0, J, IA, ID, IY                                 
      DOUBLE PRECISION T                                                
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
C TO EVALUATE A B-SPLINE WHEN THE BASIS SPLINES ARE KNOWN.              
C SCRATCH SPACE ALLOCATED - NONE.                                       
C/6S                                                                    
C     IF (NA .LT. 1) CALL SETERR(16HDQSPLN - NA.LT.1, 16, 1, 2)         
C     IF (NXMK .LE. 0) CALL SETERR(18HDQSPLN - NXMK.LE.0, 18, 2, 2)     
C     IF (K .LT. 2) CALL SETERR(15HDQSPLN - K.LT.2, 15, 3, 2)           
C     IF (NY .LT. 1) CALL SETERR(16HDQSPLN - NY.LT.1, 16, 4, 2)         
C/7S                                                                    
      IF (NA .LT. 1) CALL SETERR('DQSPLN - NA.LT.1', 16, 1, 2)          
      IF (NXMK .LE. 0) CALL SETERR('DQSPLN - NXMK.LE.0', 18, 2, 2)      
      IF (K .LT. 2) CALL SETERR('DQSPLN - K.LT.2', 15, 3, 2)            
      IF (NY .LT. 1) CALL SETERR('DQSPLN - NY.LT.1', 16, 4, 2)          
C/                                                                      
      TEMP1 = INTVAL .LT. 1                                             
      IF (.NOT. TEMP1) TEMP1 = INTVAL .GT. NXMK+K-1                     
C/6S                                                                    
C     IF (TEMP1) CALL SETERR(                                           
C    1   45HDQSPLN - INTERVAL POINTS OUTSIDE (X(1),X(NX)), 45, 5, 2)    
C     IF (ND .LT. 1) CALL SETERR(16HDQSPLN - ND.LT.1, 16, 6, 2)         
C/7S                                                                    
      IF (TEMP1) CALL SETERR(                                           
     1   'DQSPLN - INTERVAL POINTS OUTSIDE (X(1),X(NX))', 45, 5, 2)     
      IF (ND .LT. 1) CALL SETERR('DQSPLN - ND.LT.1', 16, 6, 2)          
C/                                                                      
      DO  6 IY = 1, NY                                                  
         DO  5 ID = 1, ND                                               
            DO  4 IA = 1, NA                                            
               T = 0                                                    
               J = MAX0(INTVAL-K+1, 1)                                  
                  GOTO  2                                               
   1              J = J+1                                               
   2              IF (J .GT. MIN0(INTVAL, NXMK)) GOTO  3                
                  TEMP = J+K-INTVAL                                     
                  T = T+A(J, IA)*SBASIS(IY, TEMP, ID)                   
                  GOTO  1                                               
   3           SPLINE(IY, IA, ID) = T                                   
   4           CONTINUE                                                 
   5        CONTINUE                                                    
   6     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D90STC(U, UT, NU, V, VT, NV, T, DT, K, X,        
     1   NX, AF, AF1, BC, BC1, D, D1234, SCALE, DU, DV)                 
      INTEGER NX                                                        
      EXTERNAL AF, AF1, BC, BC1, D, D1234                               
      EXTERNAL SCALE                                                    
      INTEGER NU, NV, K                                                 
      DOUBLE PRECISION U(1), UT(1), V(1), VT(1), T, DT                  
      DOUBLE PRECISION X(NX), DU(1), DV(1)                              
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      COMMON /D9OSTJ/ IJP, IB, IAFB, IALFA, IBETA, IGAMMA, ID4, ID5,    
     1   IORDER, IBC, IEQS, IAA, IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT,  
     2   IDMAT, IDIAG, IDPVOT                                           
      INTEGER IJP(3), IB(3), IAFB(3), IALFA(3), IBETA(3), IGAMMA(3)     
      INTEGER ID4(3), ID5(3), IORDER, IBC, IEQS, IAA                    
      INTEGER IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT                      
      INTEGER IDMAT, IDIAG, IDPVOT                                      
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D90STV/ IEU                                               
      INTEGER IEU                                                       
      COMMON /D90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS      
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS                         
      INTEGER NNFS, NRS                                                 
      COMMON /D90STQ/ IXGQ, IWGQ, MGQ                                   
      INTEGER IXGQ, IWGQ, MGQ                                           
      INTEGER IEXCHG, ISTKGT, MAX0, IS(1000)                            
      REAL RS(1000)                                                     
      LOGICAL FAILED, LS(1000), D90STA, D90STD                          
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP, TEMP1, TEMP2, TEMP3, TEMP4, TEMP5                   
      INTEGER TEMP6                                                     
      LOGICAL TEMP7                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO COMPUTE DU AND DV FOR EACH TIME-STEP.                              
C SCRATCH SPACE ALLOCATED - S(D90STC) =                                 
C     2*NU*(3*NU+2*(NV+1)) + NU*(NX-K)*(NV+2) +                         
C     IF ( NU > 0 )                                                     
C       MAX ( 4*NU*(NU+NV+2) + S(BC), S(DGLSSB), S(DGLSIN),             
C             2*NU*(NV+1) + S(DGLSBT), S(DGLSBC),                       
C             S(DGLSBP), NU*(NX-K)*(K*NU-1) + NU*(NX-K) INTEGER ) +     
C     IF ( NV > 0 ) S(D90STD)                                           
C LONG REAL WORDS.                                                      
C (U,UT)(NX-K,NU).                                                      
C (V,VT)(NV).                                                           
C DU(NX-K,NU),DV(NV).                                                   
      CALL ENTER(1)                                                     
      IF (GETJAC) DTJ = 0                                               
      IF (KEEJAC .NE. 0) GOTO 5                                         
         IF (NU .LE. 0) GOTO 1                                          
            IJP(1) = ISTKGT(NU*(NX-K)*(2*K*NU-1), 4)                    
            GOTO  2                                                     
   1        IJP(1) = 1                                                  
   2     IJP(3) = IJP(1)                                                
         IF (NU .LE. 0) GOTO 3                                          
            IB(1) = ISTKGT(NU*(NX-K)*(NV+1), 4)                         
            GOTO  4                                                     
   3        IB(1) = 1                                                   
   4     IB(3) = IB(1)                                                  
   5  IEXCHG = ISTKGT((NX-K)*NU, 4)                                     
      TEMP7 = DT .NE. DTJ                                               
      IF (TEMP7) TEMP7 = SEPATE                                         
      IF (.NOT. TEMP7) TEMP7 = GETJAC                                   
      IF (TEMP7) NFS = NFS+1                                            
      IF (NU .LE. 0) GOTO 7                                             
         IEU = ISTKGT(4*MAX0(MGQ, 2)*NU, 4)                             
         TEMP6 = IALFA(3)                                               
         TEMP5 = IBETA(3)                                               
         TEMP4 = IGAMMA(1)                                              
         TEMP3 = IGAMMA(2)                                              
         TEMP2 = IGAMMA(3)                                              
         TEMP = IB(3)                                                   
         TEMP1 = IB(3)                                                  
         FAILED = D90STA(U, UT, NU, V, VT, NV, T, DT, K, X, NX, AF, AF1,
     1      BC, BC1, D, D1234, SCALE, DU, WS(TEMP6), WS(TEMP5), WS(     
     2      TEMP4), WS(TEMP3), WS(TEMP2), WS(IAA), WS(IBB), WS(ICC), WS(
     3      ISGMAD), WS(ISGMAM), IS(IBC), IS(IEQS), IS(IORDER), WS(TEMP)
     4      , WS(IEXCHG), WS(TEMP1), NX-K, NV+1)                        
         IF (.NOT. FAILED) GOTO 6                                       
            CALL LEAVE                                                  
            D90STC = FAILED                                             
            RETURN                                                      
   6     CONTINUE                                                       
   7  IF (NV .LE. 0) GOTO 9                                             
         TEMP1 = IB(3)                                                  
         TEMP = IB(3)                                                   
         FAILED = D90STD(K, X, NX, U, UT, NU, V, VT, NV, T, DT, WS(     
     1      TEMP1), WS(TEMP), MAX0(1, NU*(NX-K)), D, D1234, SCALE, DU,  
     2      DV)                                                         
         IF (.NOT. FAILED) GOTO 8                                       
            CALL LEAVE                                                  
            D90STC = .TRUE.                                             
            RETURN                                                      
   8     CONTINUE                                                       
   9  CALL LEAVE                                                        
      GETJAC = .FALSE.                                                  
      DTJ = DT                                                          
      D90STC = FAILED                                                   
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D90STD(K, X, NX, U, UT, NU, V, VT, NV, T,        
     1   DT, UOFV, UOFVT, JACDIM, D1234, D, SCALE, DU, DV)              
      INTEGER JACDIM, NV                                                
      EXTERNAL D1234, D, SCALE                                          
      INTEGER K, NX, NU                                                 
      DOUBLE PRECISION X(1), U(1), UT(1), V(NV), VT(NV), T              
      DOUBLE PRECISION DT, UOFV(JACDIM, 1), UOFVT(JACDIM, 1), DU(1), DV(
     1   NV)                                                            
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D9OSTJ/ IZAP, ID4, ID5, IZAP1, ID, IDIAG, IPIVOT          
      INTEGER IZAP(18), ID4(3), ID5(3), IZAP1(10), ID, IDIAG            
      INTEGER IPIVOT                                                    
      INTEGER I, L, IS(1000)                                            
      REAL RS(1000)                                                     
      LOGICAL FAILED, D90ST0, LS(1000)                                  
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP, TEMP1, TEMP2, TEMP3, TEMP4, TEMP5                   
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C SCRATCH SPACE ALLOCATED - S(D90STD) =                                 
C     2*K*(K+NU*(NV+1)) + NV**2 +  NV*(2*NV+1) +                        
C     MAX ( S(D1234), 3*K, 2*NV + NV INTEGER )                          
C LONG REAL WORDS.                                                      
C X(NX),(U,UT)(NX-K,NU),                                                
C           (UOFV,UOFVT)(JACDIM,NV+1), DU(NX-K,NU).                     
      CALL ENTER(1)                                                     
      CALL SETD(NV, 0D0, DV)                                            
      TEMP5 = ID4(1)                                                    
      TEMP4 = ID4(2)                                                    
      TEMP3 = ID4(3)                                                    
      TEMP2 = ID5(1)                                                    
      TEMP1 = ID5(2)                                                    
      TEMP = ID5(3)                                                     
      FAILED = D90ST0(K, X, NX, U, UT, NU, V, VT, NV, T, DT, UOFV,      
     1   D1234, D, SCALE, DV, WS(TEMP5), WS(TEMP4), WS(TEMP3), WS(ID),  
     2   WS(TEMP2), WS(TEMP1), WS(TEMP), JACDIM, NX-K, WS(IDIAG), IS(   
     3   IPIVOT))                                                       
      IF (NU .LE. 0) GOTO 3                                             
         DO  2 L = 1, NV                                                
            DO  1 I = 1, JACDIM                                         
               DU(I) = DU(I)+DV(L)*UOFVT(I, L+1)                        
   1           CONTINUE                                                 
   2        CONTINUE                                                    
   3  CALL LEAVE                                                        
      D90STD = FAILED                                                   
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D90STE(Y, NY, X, T, DT, ERRPAR, DELTA, E,        
     1   ERROR, ERROR1)                                                 
      INTEGER NY                                                        
      EXTERNAL ERROR, ERROR1                                            
      REAL ERRPAR(2), E(NY)                                             
      LOGICAL ERROR                                                     
      DOUBLE PRECISION Y(NY), X(1), T, DT, DELTA                        
      COMMON /D90STK/ NU, NV, K, NX                                     
      INTEGER NU, NV, K, NX                                             
      LOGICAL ERPUTS                                                    
C THE ERROR FILTER FOR DESSOM.                                          
C SCRATCH SPACE ALLOCATED - S(D90STE) = S(ERROR).                       
C X(NX).                                                                
      ERPUTS = DELTA .EQ. 1D0                                           
      D90STE = ERROR(Y(NV+1), NU, K, X, NX, Y, NV, T, DT, ERRPAR,       
     1   ERPUTS, E(NV+1), E, ERROR1)                                    
      RETURN                                                            
      END                                                               
      SUBROUTINE D90STH(T0, S0, X, T1, S1, NS, DT, TSTOP, OK, E,        
     1   HANDLE, HANLE1)                                                
      INTEGER NS                                                        
      EXTERNAL HANDLE, HANLE1                                           
      REAL E(NS)                                                        
      LOGICAL OK                                                        
      DOUBLE PRECISION T0, S0(NS), X(1), T1, S1(NS), DT                 
      DOUBLE PRECISION TSTOP                                            
      COMMON /D90STK/ NU, NV, K, NX                                     
      INTEGER NU, NV, K, NX                                             
C OUTPUT FILTER IN BPOSTS.                                              
C SCRATCH SPACE ALLOCATED - S(D90STH) = S(HANDLE).                      
C X(NX).                                                                
      CALL HANDLE(T0, S0(NV+1), S0, T1, S1(NV+1), S1, NU, NV, K, X, NX  
     1   , DT, TSTOP, E(NV+1), E, OK, HANLE1)                           
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D90ST0(K, X, NX, U, UT, NU, V, VT, NV, T,        
     1   DT, UOFV, D1234, D, SCALE, DV, D40, D41, D4, DMAT, D50, D51,   
     2   D5, JACDIM, NXMK, DIAG, PIVOT)                                 
      INTEGER JACDIM, NV, NX                                            
      EXTERNAL D1234, D, SCALE                                          
      INTEGER K, NU, NXMK, PIVOT(NV)                                    
      LOGICAL D1234                                                     
      DOUBLE PRECISION X(NX), U(JACDIM), UT(JACDIM), V(NV), VT(NV), T   
      DOUBLE PRECISION DT, UOFV(JACDIM, 2), DV(NV), D40(NV, NV), D41(NV,
     1   NV), D4(NV, NV)                                                
      DOUBLE PRECISION DMAT(NV, NV), D50(NV, JACDIM), D51(NV, JACDIM),  
     1   D5(NV, JACDIM), DIAG(NV)                                       
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D9OSTT/ TIME, DELTAT                                      
      DOUBLE PRECISION TIME, DELTAT                                     
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      INTEGER ISTKGT, NERROR, I, J, NERR, IS(1000)                      
      INTEGER ITEMP                                                     
      REAL RS(1000)                                                     
      LOGICAL GETACT, NEESUM, LS(1000)                                  
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C (U,UT)(NX-K,NU),                                                      
C UOFV(NX-K,NU,NV+1).                                                   
      NEESUM = DT .NE. DTJ                                              
      NEESUM = NEESUM .AND. SEPATE                                      
      TEMP1 = SEPATE                                                    
      IF (TEMP1) TEMP1 = GETJAC                                         
      IF (.NOT. TEMP1) GOTO 2                                           
         TIME = TJ                                                      
         IF (.NOT. D1234(K, X, NX, NXMK, U, UT, NU, V, VT, NV, TJ,      
     1      GETJAC, SEPATE, D, D50, D51, DV, D40, D41)) GOTO 1          
            TIME = T                                                    
            D90ST0 = .TRUE.                                             
            RETURN                                                      
   1     TIME = T                                                       
   2  TEMP1 = GETJAC                                                    
      IF (TEMP1) TEMP1 = .NOT. SEPATE                                   
      GETACT = TEMP1                                                    
      IF (.NOT. D1234(K, X, NX, NXMK, U, UT, NU, V, VT, NV, T, GETACT,  
     1   .FALSE., D, D5, D51, DV, D4, D41)) GOTO 3                      
         D90ST0 = .TRUE.                                                
         RETURN                                                         
   3  DO  4 I = 1, NV                                                   
         DV(I) = -DV(I)                                                 
   4     CONTINUE                                                       
      IF (.NOT. NEESUM) GOTO 10                                         
         DO  6 I = 1, NV                                                
            DO  5 J = 1, NV                                             
               D4(I, J) = D40(I, J)+D41(I, J)/DT                        
   5           CONTINUE                                                 
   6        CONTINUE                                                    
         IF (NU .LE. 0) GOTO 9                                          
            DO  8 J = 1, NV                                             
               DO  7 I = 1, JACDIM                                      
                  D5(J, I) = D50(J, I)+D51(J, I)/DT                     
   7              CONTINUE                                              
   8           CONTINUE                                                 
   9     CONTINUE                                                       
  10  TEMP1 = GETJAC                                                    
      IF (.NOT. TEMP1) TEMP1 = NEESUM                                   
      IF (TEMP1) CALL MOVEFD(NV**2, D4, DMAT)                           
      TEMP1 = NEESUM                                                    
      IF (.NOT. TEMP1) TEMP1 = GETJAC                                   
      IF (TEMP1) TEMP1 = NU .GT. 0                                      
      IF (.NOT. TEMP1) GOTO 13                                          
         ITEMP = ISTKGT(NV**2, 4)                                       
C FORM TEMP = D5 * W.                                                   
         CALL DMMPY(D5, NV, JACDIM, UOFV(1, 2), NV, WS(ITEMP))          
         DO  12 I = 1, NV                                               
            DO  11 J = 1, NV                                            
               TEMP = ITEMP+I-1+(J-1)*NV                                
               DMAT(I, J) = DMAT(I, J)-WS(TEMP)                         
  11           CONTINUE                                                 
  12        CONTINUE                                                    
         CALL ISTKRL(1)                                                 
  13  TEMP1 = NEESUM                                                    
      IF (.NOT. TEMP1) TEMP1 = GETJAC                                   
      IF (.NOT. TEMP1) GOTO 15                                          
         CALL SCALE(4, 1, DMAT, NV, NV)                                 
C SCALE THE ODE JACOBIAN.                                               
         CALL DQRD(NV, NV, DMAT, DIAG, PIVOT)                           
         IF (NERROR(NERR) .EQ. 0) GOTO 14                               
            CALL ERROFF                                                 
            FNUM = 7                                                    
            D90ST0 = .TRUE.                                             
            RETURN                                                      
  14     CONTINUE                                                       
  15  IF (NU .LE. 0) GOTO 17                                            
         ITEMP = ISTKGT(NV, 4)                                          
C FORM DV += D5*W.                                                      
         CALL DMMPY(D5, NV, JACDIM, UOFV, 1, WS(ITEMP))                 
         DO  16 I = 1, NV                                               
            TEMP = ITEMP+I                                              
            DV(I) = DV(I)+WS(TEMP-1)                                    
  16        CONTINUE                                                    
         CALL ISTKRL(1)                                                 
C SCALE THE ODE RHS.                                                    
  17  CALL SCALE(4, 2, DV, NV, 1)                                       
      CALL DQRQTB(NV, NV, DMAT, DIAG, PIVOT, 1, DV, DV)                 
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
C    1   32HD90STD - SINGULAR DJAC IN DQRQTB, 32, 1, 2)                 
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
     1   'D90STD - SINGULAR DJAC IN DQRQTB', 32, 1, 2)                  
C/                                                                      
C UN-SCALE THE ODE SOLUTION.                                            
      CALL SCALE(4, 3, DV, NV, 1)                                       
      D90ST0 = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D90STA(U, UT, NU, V, VT, NV, T, DT, K, X,        
     1   NX, AF, AF1, BC, BC1, D, D1234, SCALE, DU, ALFA, BETA, GAMMA0  
     2   , GAMMA1, GAMMA, AA, BB, CC, SGAMAD, SGAMAM, BCS, EQS, ORDER, B
     3   , EXCHG, BT, NXMK, NRHS)                                       
      INTEGER NRHS, NXMK, NU, NX                                        
      EXTERNAL AF, AF1, BC, BC1, D, D1234                               
      EXTERNAL SCALE                                                    
      INTEGER NV, K, BCS(NU, 2, 2), EQS(NU, 2, 2), ORDER(NU, NU, 2)     
      LOGICAL BC                                                        
      DOUBLE PRECISION U(NXMK, NU), UT(NXMK, NU), V(1), VT(1), T, DT    
      DOUBLE PRECISION X(NX), DU(NXMK, NU), ALFA(NU, NU, 2), BETA(NU,   
     1   NU, 2), GAMMA0(NU, NRHS, 2), GAMMA1(NU, NRHS, 2)               
      DOUBLE PRECISION GAMMA(NU, NRHS, 2), AA(NU, NU, 2), BB(NU, NU, 2),
     1   CC(NU, NU, 2), SGAMAD(NU, NRHS, 2), SGAMAM(NU, NRHS, 2)        
      DOUBLE PRECISION B(NU, NXMK, NRHS), EXCHG(NU, NXMK), BT(NXMK, NU  
     1   , NRHS)                                                        
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      COMMON /D9OSTT/ TIME, DELTAT                                      
      DOUBLE PRECISION TIME, DELTAT                                     
      COMMON /D9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC                
      INTEGER MINIT, MAXIT, KEEJAC                                      
      REAL EGIVE                                                        
      DOUBLE PRECISION THETA                                            
      COMMON /D9OSTJ/ IJP, IB, IAFB, IALFA, IBETA, IGAMMA, ID4, ID5,    
     1   IORDER, IBC, IEQS, IAA, IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT,  
     2   IDMAT, IDIAG, IDPVOT                                           
      INTEGER IJP(3), IB(3), IAFB(3), IALFA(3), IBETA(3), IGAMMA(3)     
      INTEGER ID4(3), ID5(3), IORDER, IBC, IEQS, IAA                    
      INTEGER IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT                      
      INTEGER IDMAT, IDIAG, IDPVOT                                      
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE                           
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION TJ, DTJ                                          
      COMMON /D9OSTF/ FNUM                                              
      INTEGER FNUM                                                      
      COMMON /D90STQ/ IXGQ, IWGQ, MGQ                                   
      INTEGER IXGQ, IWGQ, MGQ                                           
      COMMON /D90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS      
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS                         
      INTEGER NNFS, NRS                                                 
      COMMON /D90STB/ IGSSIS, SET                                       
      INTEGER IGSSIS                                                    
      LOGICAL SET                                                       
      INTEGER JACLEN, IEU, SORDER, VORDER, ISTKGT, NERROR               
      INTEGER MAX0, I, J, L, NBCS, IEUX                                 
      INTEGER IEUT, NERR, IS(1000), IEUTX, NRHSD, NRHSG                 
      INTEGER MAXDER                                                    
      REAL RS(1000)                                                     
      LOGICAL DGLSBC, DGLSBI, DGLSIN, NEESUM, GETACT, LS(1000)          
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP, TEMP2, TEMP3, TEMP4, TEMP5, TEMP6                   
      LOGICAL TEMP1                                                     
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C (V,VT)(NV).                                                           
      JACLEN = NU*(NX-K)*(2*K*NU-1)                                     
      NEESUM = DTJ .NE. DT                                              
      NEESUM = NEESUM .AND. SEPATE                                      
      TEMP1 = GETJAC                                                    
      IF (TEMP1) TEMP1 = .NOT. SEPATE                                   
      IF (.NOT. TEMP1) GOTO 1                                           
         NRHSG = NRHS                                                   
         GOTO  2                                                        
   1     NRHSG = 1                                                      
   2  TEMP1 = GETJAC                                                    
      IF (.NOT. TEMP1) TEMP1 = NEESUM                                   
      IF (.NOT. TEMP1) GOTO 3                                           
         NRHSD = NRHS                                                   
         GOTO  4                                                        
   3     NRHSD = 1                                                      
   4  TEMP1 = SEPATE                                                    
      IF (TEMP1) TEMP1 = GETJAC                                         
      IF (.NOT. TEMP1) GOTO 7                                           
         TIME = TJ                                                      
         TEMP4 = IJP(1)                                                 
         TEMP = IJP(2)                                                  
         TEMP3 = IB(1)                                                  
         TEMP5 = IB(2)                                                  
         IF (.NOT. DGLSIN(K, X, NX, AF, AF1, NU, NRHS, NRHS, MGQ, WS(   
     1      IXGQ), WS(IWGQ), GETJAC, SEPATE, WS(TEMP4), WS(TEMP), WS(   
     2      TEMP3), WS(TEMP5), ORDER, IGSSIS, SET)) GOTO 5              
            TIME = T                                                    
            D90STA = .TRUE.                                             
            RETURN                                                      
   5     TIME = T                                                       
         IF (NERROR(NERR) .NE. 0) CALL ERROFF                           
C/6S                                                                    
C        IF (NERR .EQ. 14) CALL SETERR(                                 
C    1      33HDPOST4 - MGQ=K-1 AND ORDER(I,.)=0, 33, 1011, 1)          
C        IF (NERR .EQ. 15) CALL SETERR(26HDPOST4 - PDE(I) IS VACUOUS,   
C    1      26, 1012, 1)                                                
C/7S                                                                    
         IF (NERR .EQ. 14) CALL SETERR(                                 
     1      'DPOST4 - MGQ=K-1 AND ORDER(I,.)=0', 33, 1011, 1)           
         IF (NERR .EQ. 15) CALL SETERR('DPOST4 - PDE(I) IS VACUOUS',    
     1      26, 1012, 1)                                                
C/                                                                      
         IF (NERR .EQ. 0) GOTO 6                                        
            D90STA = .TRUE.                                             
            RETURN                                                      
   6     CONTINUE                                                       
   7  IF (.NOT. NEESUM) GOTO 10                                         
         DO  8 I = 1, JACLEN                                            
            TEMP5 = IJP(3)+I                                            
            TEMP3 = IJP(1)+I                                            
            TEMP = IJP(2)+I                                             
            WS(TEMP5-1) = WS(TEMP3-1)+WS(TEMP-1)/DT                     
   8        CONTINUE                                                    
         TEMP = NU*NXMK*NRHS                                            
         DO  9 I = 1, TEMP                                              
            TEMP3 = IB(1)+I                                             
            TEMP5 = IB(2)+I                                             
            B(I, 1, 1) = WS(TEMP3-1)+WS(TEMP5-1)/DT                     
   9        CONTINUE                                                    
  10  TEMP1 = GETJAC                                                    
      IF (TEMP1) TEMP1 = .NOT. SEPATE                                   
      GETACT = TEMP1                                                    
      TEMP = IJP(3)                                                     
      TEMP5 = IJP(2)                                                    
      TEMP3 = IB(3)                                                     
      TEMP4 = IB(2)                                                     
      IF (.NOT. DGLSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG, MGQ, WS(IXGQ)
     1   , WS(IWGQ), GETACT, .FALSE., WS(TEMP), WS(TEMP5), WS(TEMP3),   
     2   WS(TEMP4), ORDER, IGSSIS, SET)) GOTO 11                        
         D90STA = .TRUE.                                                
         RETURN                                                         
  11  IF (NERROR(NERR) .NE. 0) CALL ERROFF                              
C/6S                                                                    
C     IF (NERR .EQ. 14) CALL SETERR(                                    
C    1   33HDPOST4 - MGQ=K-1 AND ORDER(I,.)=0, 33, 1011, 1)             
C     IF (NERR .EQ. 15) CALL SETERR(26HDPOST4 - PDE(I) IS VACUOUS, 26,  
C    1   1012, 1)                                                       
C/7S                                                                    
      IF (NERR .EQ. 14) CALL SETERR(                                    
     1   'DPOST4 - MGQ=K-1 AND ORDER(I,.)=0', 33, 1011, 1)              
      IF (NERR .EQ. 15) CALL SETERR('DPOST4 - PDE(I) IS VACUOUS', 26,   
     1   1012, 1)                                                       
C/                                                                      
      IF (NERR .EQ. 0) GOTO 12                                          
         D90STA = .TRUE.                                                
         RETURN                                                         
  12  IEU = ISTKGT(8*NU, 4)                                             
      IEUX = IEU+2*NU                                                   
      IEUT = IEUX+2*NU                                                  
      IEUTX = IEUT+2*NU                                                 
      TEMP1 = SEPATE                                                    
      IF (TEMP1) TEMP1 = GETJAC                                         
      IF (.NOT. TEMP1) GOTO 14                                          
         TIME = TJ                                                      
         TEMP4 = IALFA(1)                                               
         TEMP3 = IBETA(1)                                               
         TEMP5 = IALFA(2)                                               
         TEMP = IBETA(2)                                                
         IF (.NOT. BC(U, UT, NU, V, VT, NV, TJ, 0D0, K, X, NX, GETJAC,  
     1      SEPATE, BC1, WS(TEMP4), WS(TEMP3), GAMMA0, WS(TEMP5), WS(   
     2      TEMP), GAMMA1, NX-K, NRHS, NRHS, WS(IEU), WS(IEUX), WS(IEUT)
     3      , WS(IEUTX))) GOTO 13                                       
            TIME = T                                                    
            D90STA = .TRUE.                                             
            RETURN                                                      
  13     TIME = T                                                       
  14  IF (.NOT. NEESUM) GOTO 19                                         
         TEMP = 2*NU**2                                                 
         DO  15 I = 1, TEMP                                             
            TEMP5 = IALFA(1)+I                                          
            TEMP3 = IALFA(2)+I                                          
            ALFA(I, 1, 1) = WS(TEMP5-1)+WS(TEMP3-1)/DT                  
            TEMP3 = IBETA(1)+I                                          
            TEMP5 = IBETA(2)+I                                          
            BETA(I, 1, 1) = WS(TEMP3-1)+WS(TEMP5-1)/DT                  
  15        CONTINUE                                                    
         DO  18 L = 1, 2                                                
            DO  17 I = 1, NU                                            
               DO  16 J = 1, NRHS                                       
                  GAMMA(I, J, L) = GAMMA0(I, J, L)+GAMMA1(I, J, L)/DT   
  16              CONTINUE                                              
  17           CONTINUE                                                 
  18        CONTINUE                                                    
  19  TEMP1 = GETJAC                                                    
      IF (TEMP1) TEMP1 = .NOT. SEPATE                                   
      GETACT = TEMP1                                                    
      TEMP = IALFA(3)                                                   
      TEMP5 = IBETA(3)                                                  
      TEMP3 = IALFA(2)                                                  
      TEMP4 = IBETA(2)                                                  
      IF (.NOT. BC(U, UT, NU, V, VT, NV, T, DT, K, X, NX, GETACT,       
     1   .FALSE., BC1, WS(TEMP), WS(TEMP5), GAMMA, WS(TEMP3), WS(TEMP4),
     2   GAMMA1, NX-K, NRHS, NRHSG, WS(IEU), WS(IEUX), WS(IEUT), WS(    
     3   IEUTX))) GOTO 20                                               
         D90STA = .TRUE.                                                
         RETURN                                                         
  20  CALL ISTKRL(1)                                                    
      DO  22 L = 1, 2                                                   
         CALL DGLSSB(ALFA(1, 1, L), BETA(1, 1, L), GAMMA(1, 1, L), NU,  
     1      NRHSD, AA(1, 1, L), BB(1, 1, L), CC(1, 1, L), SGAMAD(1, 1, L
     2      ), SGAMAM(1, 1, L), BCS(1, 1, L))                           
         IF (NERROR(NERR) .NE. 0) CALL ERROFF                           
C/6S                                                                    
C        IF (NERR .EQ. 3) CALL SETERR(                                  
C    1      53HDPOST4 - MIXED BOUNDARY CONDITIONS ARE OVERDETERMINED,   
C    2      53, 1006, 1)                                                
C/7S                                                                    
         IF (NERR .EQ. 3) CALL SETERR(                                  
     1      'DPOST4 - MIXED BOUNDARY CONDITIONS ARE OVERDETERMINED',    
     2      53, 1006, 1)                                                
C/                                                                      
         IF (NERR .EQ. 5) FNUM = 5                                      
C/6S                                                                    
C        IF (NERR .EQ. 4) CALL SETERR(                                  
C    1      57HDPOST4 - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED
C    2      , 57, 1005, 1)                                              
C/7S                                                                    
         IF (NERR .EQ. 4) CALL SETERR(                                  
     1      'DPOST4 - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED' 
     2      , 57, 1005, 1)                                              
C/                                                                      
         IF (NERR .EQ. 6) FNUM = 4                                      
         IF (NERR .EQ. 0) GOTO 21                                       
            D90STA = .TRUE.                                             
            RETURN                                                      
  21     CONTINUE                                                       
  22     CONTINUE                                                       
      TEMP1 = SEPATE                                                    
      IF (TEMP1) TEMP1 = GETJAC                                         
      IF (.NOT. TEMP1) GOTO 24                                          
         TIME = TJ                                                      
         TEMP4 = IAFB(1)                                                
         TEMP3 = IAFB(1)+2*NU**2                                        
         TEMP5 = IAFB(1)+4*NU**2                                        
         TEMP = IAFB(2)                                                 
         TEMP2 = IAFB(2)+2*NU**2                                        
         TEMP6 = IAFB(2)+4*NU**2                                        
         IF (.NOT. DGLSBI(K, X, NX, AF, AF1, NU, NRHS, NRHS, GETJAC,    
     1      SEPATE, WS(TEMP4), WS(TEMP3), WS(TEMP5), WS(TEMP), WS(TEMP2)
     2      , WS(TEMP6))) GOTO 23                                       
            TIME = T                                                    
            D90STA = .TRUE.                                             
            RETURN                                                      
  23     TIME = T                                                       
  24  IF (.NOT. NEESUM) GOTO 26                                         
         TEMP6 = 2*NU*(2*NU+NRHS)                                       
         DO  25 I = 1, TEMP6                                            
            TEMP2 = IAFB(3)+I                                           
            TEMP = IAFB(1)+I                                            
            TEMP5 = IAFB(2)+I                                           
            WS(TEMP2-1) = WS(TEMP-1)+WS(TEMP5-1)/DT                     
  25        CONTINUE                                                    
  26  TEMP1 = GETJAC                                                    
      IF (TEMP1) TEMP1 = .NOT. SEPATE                                   
      GETACT = TEMP1                                                    
      TEMP6 = IAFB(3)                                                   
      TEMP5 = IAFB(3)+2*NU**2                                           
      TEMP = IAFB(3)+4*NU**2                                            
      TEMP2 = IAFB(2)                                                   
      TEMP3 = IAFB(2)+2*NU**2                                           
      TEMP4 = IAFB(2)+4*NU**2                                           
      IF (.NOT. DGLSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG, GETACT,      
     1   .FALSE., WS(TEMP6), WS(TEMP5), WS(TEMP), WS(TEMP2), WS(TEMP3)  
     2   , WS(TEMP4))) GOTO 27                                          
         D90STA = .TRUE.                                                
         RETURN                                                         
  27  TEMP1 = GETJAC                                                    
      IF (.NOT. TEMP1) TEMP1 = NEESUM                                   
      GETACT = TEMP1                                                    
      TEMP4 = IAFB(3)                                                   
      TEMP3 = IAFB(3)+2*NU**2                                           
      TEMP2 = IAFB(3)+4*NU**2                                           
      TEMP = IJP(3)                                                     
      CALL DGLSBT(K, X, NX, NU, NRHS, NRHSD, GETACT, WS(TEMP4), WS(     
     1   TEMP3), WS(TEMP2), AA, BB, SGAMAM, BCS, WS(TEMP), B)           
      TEMP1 = NEESUM                                                    
      IF (.NOT. TEMP1) TEMP1 = GETJAC                                   
      IF (.NOT. TEMP1) GOTO 39                                          
         IF (DGLSBC(NU, ORDER, BCS, EQS)) GOTO 30                       
            CALL DGLSBP(NU, ORDER, BCS, EQS)                            
            IF (NERROR(NERR) .EQ. 0) GOTO 28                            
               CALL ERROFF                                              
C/6S                                                                    
C              CALL SETERR(21HDPOST4 - IMPROPER BCS, 21, 1007, 1)       
C/7S                                                                    
               CALL SETERR('DPOST4 - IMPROPER BCS', 21, 1007, 1)        
C/                                                                      
               GOTO  29                                                 
C/6S                                                                    
C 28           IF (.NOT. DGLSBC(NU, ORDER, BCS, EQS)) CALL SETERR(      
C    1            26HD90STA - DGLSBP-BC FAILURE, 26, 1, 2)              
C/7S                                                                    
  28           IF (.NOT. DGLSBC(NU, ORDER, BCS, EQS)) CALL SETERR(      
     1            'D90STA - DGLSBP-BC FAILURE', 26, 1, 2)               
C/                                                                      
  29        CONTINUE                                                    
C COUNT THE BOUNDARY CONDITIONS.                                        
  30     NBCS = 0                                                       
         DO  32 I = 1, NU                                               
            DO  31 J = 1, 2                                             
               IF (BCS(I, 1, J) .GE. 0) NBCS = NBCS+1                   
               IF (BCS(I, 2, J) .GE. 0) NBCS = NBCS+1                   
  31           CONTINUE                                                 
  32        CONTINUE                                                    
         SORDER = 0                                                     
C COUNT THE TOTAL ORDER OF THE SYSTEM.                                  
         VORDER = 0                                                     
         DO  35 J = 1, NU                                               
            MAXDER = 0                                                  
            DO  33 I = 1, NU                                            
               MAXDER = MAX0(MAXDER, ORDER(I, J, 1), ORDER(I, J, 2))    
  33           CONTINUE                                                 
            SORDER = SORDER+MAXDER                                      
            MAXDER = 0                                                  
            DO  34 I = 1, NU                                            
               MAXDER = MAX0(MAXDER, ORDER(J, I, 1), ORDER(J, I, 2))    
  34           CONTINUE                                                 
            VORDER = VORDER+MAXDER                                      
  35        CONTINUE                                                    
         IF (SORDER .EQ. VORDER) GOTO 36                                
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(                                                
C    1         45HDPOST4 - PDE SYSTEM NOT IN MINIMAL ORDER FORM, 45,    
C    2         1008, 1)                                                 
C/7S                                                                    
            CALL SETERR(                                                
     1         'DPOST4 - PDE SYSTEM NOT IN MINIMAL ORDER FORM', 45,     
     2         1008, 1)                                                 
C/                                                                      
            D90STA = .TRUE.                                             
            RETURN                                                      
  36     IF (NBCS .GE. SORDER) GOTO 37                                  
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(36HDPOST4 - TOO FEW BOUNDARY CONDITIONS, 36,    
C    1         1009, 1)                                                 
C/7S                                                                    
            CALL SETERR('DPOST4 - TOO FEW BOUNDARY CONDITIONS', 36,     
     1         1009, 1)                                                 
C/                                                                      
  37     IF (NBCS .LE. SORDER) GOTO 38                                  
            CALL ERROFF                                                 
C/6S                                                                    
C           CALL SETERR(37HDPOST4 - TOO MANY BOUNDARY CONDITIONS, 37,   
C    1         1010, 1)                                                 
C/7S                                                                    
            CALL SETERR('DPOST4 - TOO MANY BOUNDARY CONDITIONS', 37,    
     1         1010, 1)                                                 
C/                                                                      
  38     CONTINUE                                                       
  39  IF (NERROR(NERR) .EQ. 0) GOTO 40                                  
         D90STA = .TRUE.                                                
         RETURN                                                         
  40  TEMP1 = GETJAC                                                    
      IF (.NOT. TEMP1) TEMP1 = NEESUM                                   
      GETACT = TEMP1                                                    
      TEMP = IJP(3)                                                     
      CALL DGLSBD(K, NX, NU, NRHS, NRHSD, BCS, EQS, CC, SGAMAD, GETACT  
     1   , WS(TEMP), B)                                                 
      IF (KEEJAC .NE. 0) GOTO 41                                        
         IL = ISTKGT((K*NU-1)*(NX-K)*NU, 4)                             
         IPPVOT = ISTKGT(NU*(NX-K), 2)                                  
  41  TEMP1 = NEESUM                                                    
      IF (.NOT. TEMP1) TEMP1 = GETJAC                                   
      IF (.NOT. TEMP1) GOTO 43                                          
         TEMP = IJP(3)                                                  
C SCALE PDE JACOBIAN.                                                   
         CALL SCALE(1, 1, WS(TEMP), (NX-K)*NU, 2*K*NU-1)                
         TEMP = IJP(3)                                                  
         CALL DBNDLU((NX-K)*NU, K*NU, 2*K*NU-1, WS(TEMP), WS(IL), IS(   
     1      IPPVOT))                                                    
         IF (NERROR(NERR) .EQ. 0) GOTO 42                               
            CALL ERROFF                                                 
            FNUM = 6                                                    
            D90STA = .TRUE.                                             
            RETURN                                                      
  42     CONTINUE                                                       
C SCALE THE PDE RHS.                                                    
  43  CALL SCALE(1, 2, B, (NX-K)*NU, NRHSD)                             
      TEMP = IJP(3)                                                     
      CALL DBNDFB((NX-K)*NU, K*NU, 2*K*NU-1, WS(IL), WS(TEMP), IS(      
     1   IPPVOT), NRHSD, B)                                             
C/6S                                                                    
C     IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
C    1   31HD90STC - SINGULAR JAC IN DBNDFB, 31, 1, 2)                  
C/7S                                                                    
      IF (NERROR(NERR) .NE. 0) CALL SETERR(                             
     1   'D90STC - SINGULAR JAC IN DBNDFB', 31, 1, 2)                   
C/                                                                      
C UN-SCALE THE PDE SOLUTION.                                            
      CALL SCALE(1, 3, B, (NX-K)*NU, NRHSD)                             
      IF (KEEJAC .EQ. 0) CALL ISTKRL(2)                                 
C FORM BT = B-TRANSPOSE.                                                
      DO  46 L = 1, NRHSD                                               
         CALL MOVEFD((NX-K)*NU, B(1, 1, L), EXCHG)                      
         TEMP = NX-K                                                    
         DO  45 I = 1, TEMP                                             
            DO  44 J = 1, NU                                            
               BT(I, J, L) = EXCHG(J, I)                                
  44           CONTINUE                                                 
  45        CONTINUE                                                    
  46     CONTINUE                                                       
      DO  48 J = 1, NU                                                  
         TEMP = NX-K                                                    
         DO  47 I = 1, TEMP                                             
            DU(I, J) = BT(I, J, 1)                                      
  47        CONTINUE                                                    
  48     CONTINUE                                                       
      D90STA = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE DBNDFB(N, ML, M, L, U, INT, NB, B)                     
      INTEGER M, N, NB, ML                                              
      INTEGER INT(N)                                                    
      DOUBLE PRECISION L(N, ML), U(N, M), B(N, NB)                      
      INTEGER NERR                                                      
C TO SOLVE L*U*X = B, WHERE L AND U RESULT FROM A CALL TO DBNDS.        
C MNEMONIC - DOUBLE PRECISION BAND FORWARD ELIMINATION AND              
C            BACK-SOLVE.                                                
C INPUT -                                                               
C   N   - THE ORDER OF THE SYSTEM.                                      
C   ML  - THE NUMBER OF NONZERO ENTRIES OF L ON AND BELOW               
C         THE DIAGONAL.                                                 
C   M   - THE NUMBER OF NONZERO ELEMENTS OF U ON AND ABOVE              
C         THE DIAGONAL.                                                 
C   L   - THE LOWER TRIANGULAR BANDED FACTOR.                           
C   U   - THE UPPER TRIANGULAR BANDED FACTOR.                           
C   INT - THE ORDERING OF THE ROWS OF THE SYSTEM, DUE TO PIVOTING.      
C   NB  - THE NUMBER OF RIGHT-HAND-SIDES.                               
C   B   - THE RIGHT-HAND-SIDES.                                         
C OUTPUT -                                                              
C   B - THE SOLUTION VECTORS.                                           
C SCRATCH SPACE ALLOCATED - NONE.                                       
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - ML.LT.1.                                                        
C   3 - M.LT.ML.                                                        
C   4 - NB.LT.1.                                                        
C L(N,ML-1).                                                            
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15HDBNDFB - N.LT.1, 15, 1, 2)           
C     IF (ML .LT. 1) CALL SETERR(16HDBNDFB - ML.LT.1, 16, 2, 2)         
C     IF (M .LT. ML) CALL SETERR(16HDBNDFB - M.LT.ML, 16, 3, 2)         
C     IF (NB .LT. 1) CALL SETERR(16HDBNDFB - NB.LT.1, 16, 4, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DBNDFB - N.LT.1', 15, 1, 2)            
      IF (ML .LT. 1) CALL SETERR('DBNDFB - ML.LT.1', 16, 2, 2)          
      IF (M .LT. ML) CALL SETERR('DBNDFB - M.LT.ML', 16, 3, 2)          
      IF (NB .LT. 1) CALL SETERR('DBNDFB - NB.LT.1', 16, 4, 2)          
C/                                                                      
C PROTECT AGAINST AN EXISTING ERROR STATE.                              
      CALL ENTSRC(NERR, 0)                                              
C DO THE FORWARD-ELIMINATION.                                           
      CALL DBNDFE(N, ML, L, INT, NB, B)                                 
C DO THE BACK-SUBSTITUTION.                                             
      CALL DBNDBS(N, M, U, NB, B)                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DBNDFE(N, ML, L, INT, NB, B)                           
      INTEGER N, NB, ML                                                 
      INTEGER INT(N)                                                    
      DOUBLE PRECISION L(N, ML), B(N, NB)                               
      INTEGER I, J, K, M1, MIN0                                         
      DOUBLE PRECISION X                                                
      INTEGER TEMP, TEMP1, TEMP2                                        
C TO SOLVE L*X = B, WHERE L IS A LOWER TRIANGULAR BANDED MATRIX.        
C MNEMONIC - DOUBLE PRECISION BANDED FORWARD-ELIMINATION.               
C INPUT -                                                               
C   N   - THE ORDER OF THE SYSTEM.                                      
C   ML  - THE NUMBER OF NONZERO ELEMENTS OF L ON AND BELOW THE DIAGONAL.
C   L   - THE LOWER TRIANGULAR BANDED MATRIX.                           
C   INT - THE ORDERING OF THE ROWS OF THE SYSTEM, DUE TO PIVOTING.      
C   NB  - THE NUMBER OF RIGHT-HAND-SIDES.                               
C   B   - THE RIGHT-HAND-SIDES.                                         
C OUTPUT -                                                              
C   B - THE SOLUTION VECTORS, X.                                        
C SCRATCH STORAGE ALLOCATED - NONE.                                     
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - ML.LT.1.                                                        
C   3 - NB.LT.1.                                                        
C   4 - INT(I) NOT ONE OF 1,...,N.                                      
C L(N,ML-1).                                                            
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15HDBNDFE - N.LT.1, 15, 1, 2)           
C     IF (ML .LT. 1) CALL SETERR(16HDBNDFE - ML.LT.1, 16, 2, 2)         
C     IF (NB .LT. 1) CALL SETERR(16HDBNDFE - NB.LT.1, 16, 3, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DBNDFE - N.LT.1', 15, 1, 2)            
      IF (ML .LT. 1) CALL SETERR('DBNDFE - ML.LT.1', 16, 2, 2)          
      IF (NB .LT. 1) CALL SETERR('DBNDFE - NB.LT.1', 16, 3, 2)          
C/                                                                      
C PROTECT AGAINST AN EXISTING ERROR STATE.                              
      CALL ENTSRC(I, 0)                                                 
      M1 = ML-1                                                         
      DO  5 K = 1, N                                                    
         I = INT(K)                                                     
C/6S                                                                    
C        IF (I .LT. 1 .OR. I .GT. N) CALL SETERR(                       
C    1      34HDBNDFE - INT(I) NOT ONE OF 1,...,N, 34, 4, 2)            
C/7S                                                                    
         IF (I .LT. 1 .OR. I .GT. N) CALL SETERR(                       
     1      'DBNDFE - INT(I) NOT ONE OF 1,...,N', 34, 4, 2)             
C/                                                                      
         IF (I .EQ. K) GOTO 2                                           
            DO  1 J = 1, NB                                             
C INTERCHANGE THE ELEMENTS OF B.                                        
               X = B(K, J)                                              
               B(K, J) = B(I, J)                                        
               B(I, J) = X                                              
   1           CONTINUE                                                 
   2     IF (M1 .EQ. 0 .OR. K .EQ. N) GOTO  6                           
         TEMP1 = K+1                                                    
         TEMP = MIN0(M1+K, N)                                           
         DO  4 I = TEMP1, TEMP                                          
            TEMP2 = I-K                                                 
            X = L(K, TEMP2)                                             
            DO  3 J = 1, NB                                             
               B(I, J) = B(I, J)-X*B(K, J)                              
   3           CONTINUE                                                 
   4        CONTINUE                                                    
   5     CONTINUE                                                       
   6  RETURN                                                            
      END                                                               
      SUBROUTINE DBNDLU(N, ML, M, G, L, INT)                            
      INTEGER M, N, ML                                                  
      INTEGER INT(N)                                                    
      DOUBLE PRECISION G(N, M), L(N, ML)                                
      INTEGER I, J, K, M1, M2, LL                                       
      INTEGER MIN0                                                      
      REAL FLOAT                                                        
      LOGICAL SING                                                      
      DOUBLE PRECISION D1MACH, X, EPS, DABS, NORM, DMAX1                
      INTEGER TEMP, TEMP1                                               
C TO OBTAIN THE LU DECOMPOSITION OF A BANDED MATRIX,                    
C USING GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING.                     
C MNEMONIC - DOUBLE PRECISION BAND LU DECOMPOSITION.                    
C INPUT -                                                               
C   N   - THE ORDER OF THE MATRIX.                                      
C   ML  - THE NUMBER OF NONZERO ELEMENTS OF A ON AND BELOW THE DIAGONAL.
C   M   - THE NUMBER OF NONZERO ELEMENTS IN EACH ROW OF A.              
C   G   - THE MATRIX A, WITH G(I,J) = A(I,I+J-ML).                      
C OUTPUT -                                                              
C   L   - THE LOWER TRIANGULAR BANDED FACTOR OF A.                      
C   G   - THE UPPER TRIANGULAR BANDED FACTOR OF A.                      
C   INT - THE ROW PIVOTING USED.                                        
C SCRATCH STORAGE ALLOCATED - NONE.                                     
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - ML.LT.1.                                                        
C   3 - M.LT.ML.                                                        
C   4 - SINGULAR MATRIX. (RECOVERABLE)                                  
C L(N,ML-1).                                                            
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15HDBNDLU - N.LT.1, 15, 1, 2)           
C     IF (ML .LT. 1) CALL SETERR(16HDBNDLU - ML.LT.1, 16, 2, 2)         
C     IF (M .LT. ML) CALL SETERR(16HDBNDLU - M.LT.ML, 16, 3, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DBNDLU - N.LT.1', 15, 1, 2)            
      IF (ML .LT. 1) CALL SETERR('DBNDLU - ML.LT.1', 16, 2, 2)          
      IF (M .LT. ML) CALL SETERR('DBNDLU - M.LT.ML', 16, 3, 2)          
C/                                                                      
C PROTECT AGAINST AN EXISTING ERROR STATE.                              
      CALL ENTSRC(I, 0)                                                 
      SING = .FALSE.                                                    
      EPS = D1MACH(4)*FLOAT(N*(M-1)*(ML-1))                             
      M1 = ML-1                                                         
      M2 = M-ML                                                         
      LL = M1                                                           
      I = 1                                                             
         GOTO  2                                                        
   1     I = I+1                                                        
   2     IF (I .GT. MIN0(M1, N)) GOTO  5                                
C SET TO 0 THOSE ELEMENTS                                               
C OF G WHICH ARE UNDEFINED.                                             
         TEMP = ML+1-I                                                  
         DO  3 J = TEMP, M                                              
            TEMP1 = J-LL                                                
            G(I, TEMP1) = G(I, J)                                       
   3        CONTINUE                                                    
         LL = LL-1                                                      
         TEMP = M-LL                                                    
         DO  4 J = TEMP, M                                              
            G(I, J) = 0.0D0                                             
   4        CONTINUE                                                    
         GOTO  1                                                        
   5  I = 1                                                             
         GOTO  7                                                        
   6     I = I+1                                                        
   7     IF (I .GT. MIN0(M2, N)) GOTO  9                                
C ZERO OUT LOWER RHS WART.                                              
         TEMP = ML+I                                                    
         DO  8 J = TEMP, M                                              
            TEMP1 = N+1-I                                               
            G(TEMP1, J) = 0.0D0                                         
   8        CONTINUE                                                    
         GOTO  6                                                        
C GET || A || SUB INFINITY.                                             
   9  NORM = 0.0D0                                                      
      DO  11 I = 1, N                                                   
         INT(I) = I                                                     
         X = 0.0D0                                                      
         DO  10 J = 1, M                                                
            X = X+DABS(G(I, J))                                         
  10        CONTINUE                                                    
         NORM = DMAX1(NORM, X)                                          
  11     CONTINUE                                                       
      DO  20 K = 1, N                                                   
         X = G(K, 1)                                                    
         I = K                                                          
         LL = MIN0(M1+K, N)                                             
         IF (K .GE. LL) GOTO 14                                         
            TEMP = K+1                                                  
C GET THE PIVOT ROW.                                                    
            DO  13 J = TEMP, LL                                         
               IF (DABS(G(J, 1)) .LE. DABS(X)) GOTO 12                  
                  X = G(J, 1)                                           
                  I = J                                                 
  12           CONTINUE                                                 
  13           CONTINUE                                                 
  14     INT(K) = I                                                     
         IF (DABS(X) .GT. NORM*EPS) GOTO 15                             
            SING = .TRUE.                                               
            G(K, 1) = NORM*EPS                                          
  15     IF (ML .EQ. 1 .OR. K .EQ. N) GOTO  20                          
         IF (I .EQ. K) GOTO 17                                          
            DO  16 J = 1, M                                             
C NEED TO INTERCHANGE THE ROWS.                                         
               X = G(K, J)                                              
               G(K, J) = G(I, J)                                        
               G(I, J) = X                                              
  16           CONTINUE                                                 
  17     IF (K .GE. LL) GOTO  20                                        
         TEMP = K+1                                                     
         DO  19 I = TEMP, LL                                            
            X = G(I, 1)/G(K, 1)                                         
            TEMP1 = I-K                                                 
            L(K, TEMP1) = X                                             
            DO  18 J = 2, M                                             
               G(I, J-1) = G(I, J)-X*G(K, J)                            
  18           CONTINUE                                                 
            G(I, M) = 0.0D0                                             
  19        CONTINUE                                                    
  20     CONTINUE                                                       
C/6S                                                                    
C     IF (SING) CALL SETERR(24HDBNDLU - SINGULAR MATRIX, 24, 4, 1)      
C/7S                                                                    
      IF (SING) CALL SETERR('DBNDLU - SINGULAR MATRIX', 24, 4, 1)       
C/                                                                      
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION DGLSBC(NU, ORDER, BC, E)                         
      INTEGER NU                                                        
      INTEGER ORDER(NU, NU, 2), BC(NU, 2, 2), E(NU, 2, 2)               
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ICOUNT, ISTKGT, I, J, L, IMAORD                           
      INTEGER IS(1000)                                                  
      REAL RS(1000)                                                     
      LOGICAL FAILED, ALLERO, LS(1000), D6LSBC                          
      DOUBLE PRECISION WS(500)                                          
      LOGICAL TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO CHECK THAT THE BOUNDARY CONDITION PLACEMENT GIVEN BY E             
C IS CORRECT.                                                           
C MNEMONIC - DOUBLE PRECISION GALERKIN'S METHOD FOR LINEAR SYSTEMS,     
C            BOUNDARY PLACEMENT CHECK.                                  
C SCRATCH SPACE ALLOCATED - 4*NU INTEGER WORDS.                         
C CHECK THE INPUT.                                                      
C/6S                                                                    
C     IF (NU .LT. 1) CALL SETERR(16HDGLSBC - NU.LT.1, 16, 1, 2)         
C/7S                                                                    
      IF (NU .LT. 1) CALL SETERR('DGLSBC - NU.LT.1', 16, 1, 2)          
C/                                                                      
      DO  6 L = 1, 2                                                    
         DO  5 I = 1, NU                                                
            TEMP = E(I, 1, L) .LT. 1                                    
            IF (.NOT. TEMP) TEMP = E(I, 1, L) .GT. NU                   
            IF (TEMP) TEMP = BC(I, 1, L) .EQ. 0                         
            IF (TEMP) GOTO 1                                            
               TEMP1 = E(I, 2, L) .LT. 1                                
               IF (.NOT. TEMP1) TEMP1 = E(I, 2, L) .GT. NU              
               IF (TEMP1) TEMP1 = BC(I, 2, L) .EQ. 1                    
               TEMP = TEMP1                                             
   1        IF (.NOT. TEMP) GOTO 2                                      
               DGLSBC = .FALSE.                                         
               RETURN                                                   
C IS ORDER(I,.,L) = (-1, ... , -1)?                                     
   2        ALLERO = .TRUE.                                             
            DO  3 J = 1, NU                                             
               ALLERO = ALLERO .AND. ORDER(I, J, L) .EQ. (-1)           
               TEMP = ORDER(I, J, L) .LT. (-1)                          
               IF (.NOT. TEMP) TEMP = ORDER(I, J, L) .GT. 2             
C/6S                                                                    
C              IF (TEMP) CALL SETERR(                                   
C    1            41HDGLSBC - ORDER(I,J,L) NOT ONE OF -1,0,1,2, 41, 3, 2
C    2            )                                                     
C/7S                                                                    
               IF (TEMP) CALL SETERR(                                   
     1            'DGLSBC - ORDER(I,J,L) NOT ONE OF -1,0,1,2', 41, 3, 2 
     2            )                                                     
C/                                                                      
   3           CONTINUE                                                 
            TEMP = BC(I, 1, L) .NE. (-2)                                
            IF (TEMP) TEMP = BC(I, 1, L) .NE. 0                         
            IF (TEMP) GOTO 4                                            
               TEMP1 = BC(I, 2, L) .NE. (-2)                            
               IF (TEMP1) TEMP1 = BC(I, 2, L) .NE. 1                    
               TEMP = TEMP1                                             
C/6S                                                                    
C  4        IF (TEMP) CALL SETERR(                                      
C    1         36HDGLSBC - BC(I,.,L) NOT ONE OF -2,0,1, 36, 4, 2)       
C           IF (ALLERO) CALL SETERR(                                    
C    1         33HDGLSBC - ORDER(I,.,L)=(-1,...,-1), 33, 5, 2)          
C/7S                                                                    
   4        IF (TEMP) CALL SETERR(                                      
     1         'DGLSBC - BC(I,.,L) NOT ONE OF -2,0,1', 36, 4, 2)        
            IF (ALLERO) CALL SETERR(                                    
     1         'DGLSBC - ORDER(I,.,L)=(-1,...,-1)', 33, 5, 2)           
C/                                                                      
   5        CONTINUE                                                    
   6     CONTINUE                                                       
      CALL ENTER(1)                                                     
      IMAORD = ISTKGT(4*NU, 2)                                          
      ICOUNT = IMAORD+2*NU                                              
      FAILED = D6LSBC(NU, ORDER, BC, E, IS(IMAORD), IS(ICOUNT))         
      CALL LEAVE                                                        
      DGLSBC = FAILED                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE DGLSBD(K, NX, NU, NRHS, NRHSG, BC, E, CC, GAM,         
     1   GETJAC, G, B)                                                  
      INTEGER NU                                                        
      INTEGER K, NX, NRHS, NRHSG, BC(NU, 2, 2), E(NU, 2, 2)             
      LOGICAL GETJAC                                                    
      DOUBLE PRECISION CC(NU, NU, 2), GAM(1, 1, 1), G(1, 1), B(1, 1)    
      INTEGER I                                                         
C MNEMONIC - DOUBLE PRECISION GALERKIN'S METHOD FOR LINEAR SYSTEMS,     
C            BOUNDARY CONDITIONS ( DIRICHLET ).                         
C SCRATCH SPACE ALLOCATED - NONE.                                       
C LONG REAL GAM((NX-K)*NU,NRHS,2),G((NX-K)*NU,2*K*NU-1),B((NX-K)*NU,NRHS
C)                                                                      
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (K .LT. 2) CALL SETERR(15HDGLSBD - K.LT.2, 15, 1, 2)           
C     IF (NX .LT. 2*K) CALL SETERR(18HDGLSBD - NX.LT.2*K, 18, 2, 2)     
C     IF (NU .LT. 1) CALL SETERR(16HDGLSBD - NU.LT.1, 16, 3, 2)         
C     IF (NRHS .LT. 1) CALL SETERR(18HDGLSBD - NRHS.LT.1, 18, 4, 2)     
C     IF (NRHSG .LT. 0) CALL SETERR(19HDGLSBD - NRHSG.LT.0, 19, 5, 2)   
C/7S                                                                    
      IF (K .LT. 2) CALL SETERR('DGLSBD - K.LT.2', 15, 1, 2)            
      IF (NX .LT. 2*K) CALL SETERR('DGLSBD - NX.LT.2*K', 18, 2, 2)      
      IF (NU .LT. 1) CALL SETERR('DGLSBD - NU.LT.1', 16, 3, 2)          
      IF (NRHS .LT. 1) CALL SETERR('DGLSBD - NRHS.LT.1', 18, 4, 2)      
      IF (NRHSG .LT. 0) CALL SETERR('DGLSBD - NRHSG.LT.0', 19, 5, 2)    
C/                                                                      
C PROTECT AGAINST AN EXISTING ERROR STATE.                              
      CALL ENTSRC(I, 0)                                                 
      CALL D6LSBD(K, NX, NU, NRHS, NRHSG, BC, E, CC, GAM, GETJAC, G, B  
     1   , (NX-K)*NU, 2*K*NU-1)                                         
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION DGLSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG        
     1   , GETJAC, SEPATE, A1, A2, F1, A1T, A2T, F1T)                   
      INTEGER NRHS, NU, NX                                              
      EXTERNAL AF, AF1                                                  
      INTEGER K, NRHSG                                                  
      LOGICAL GETJAC, SEPATE                                            
      DOUBLE PRECISION X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS,
     1   2), A1T(NU, NU, 2), A2T(NU, NU, 2)                             
      DOUBLE PRECISION F1T(NU, NRHS, 2)                                 
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IAS, IFS, ISTKGT, I, ISBSIS, IS(1000)                     
      REAL RS(1000)                                                     
      LOGICAL FAILED, LS(1000), D6LSBI                                  
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C MNEMONIC - DOUBLE PRECISION GALERKIN'S METHOD FOR LINEAR SYSTEMS,     
C            BOUNDARY INTEGRAL TERMS.                                   
C SCRATCH SPACE ALLOCATED -                                             
C       S(DGLSBI) = NU*(8*NU+2*NRHS) + 2*K                              
C   LONG REAL WORDS +                                                   
C       MAX(3*K,S(AF))                                                  
C   INTEGER WORDS.                                                      
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (K .LT. 2) CALL SETERR(15HDGLSBI - K.LT.2, 15, 1, 2)           
C     IF (NX .LT. 2*K) CALL SETERR(18HDGLSBI - NX.LT.2*K, 18, 2, 2)     
C     IF (NU .LT. 1) CALL SETERR(16HDGLSBI - NU.LT.1, 16, 3, 2)         
C     IF (NRHS .LT. 1) CALL SETERR(18HDGLSBI - NRHS.LT.1, 18, 4, 2)     
C     IF (NRHSG .LT. 0) CALL SETERR(19HDGLSBI - NRHSG.LT.0, 19, 5, 2)   
C/7S                                                                    
      IF (K .LT. 2) CALL SETERR('DGLSBI - K.LT.2', 15, 1, 2)            
      IF (NX .LT. 2*K) CALL SETERR('DGLSBI - NX.LT.2*K', 18, 2, 2)      
      IF (NU .LT. 1) CALL SETERR('DGLSBI - NU.LT.1', 16, 3, 2)          
      IF (NRHS .LT. 1) CALL SETERR('DGLSBI - NRHS.LT.1', 18, 4, 2)      
      IF (NRHSG .LT. 0) CALL SETERR('DGLSBI - NRHSG.LT.0', 19, 5, 2)    
C/                                                                      
C CHECK THAT X IS MONOTONE INCREASING.                                  
      DO  1 I = 2, NX                                                   
C/6S                                                                    
C        IF (X(I-1) .GT. X(I)) CALL SETERR(                             
C    1      37HDGLSBI - X IS NOT MONOTONE INCREASING, 37, 6, 2)         
C/7S                                                                    
         IF (X(I-1) .GT. X(I)) CALL SETERR(                             
     1      'DGLSBI - X IS NOT MONOTONE INCREASING', 37, 6, 2)          
C/                                                                      
         IF (I+K .GT. NX) GOTO  1                                       
         TEMP1 = I+K                                                    
C/6S                                                                    
C        IF (X(TEMP1) .LE. X(I)) CALL SETERR(                           
C    1      37HDGLSBI - X IS NOT MONOTONE INCREASING, 37, 6, 2)         
C/7S                                                                    
         IF (X(TEMP1) .LE. X(I)) CALL SETERR(                           
     1      'DGLSBI - X IS NOT MONOTONE INCREASING', 37, 6, 2)          
C/                                                                      
   1     CONTINUE                                                       
C CHECK THE MULTIPLICITY OF THE END POINTS.                             
      DO  3 I = 1, K                                                    
         TEMP = X(I) .NE. X(1)                                          
         IF (TEMP) GOTO 2                                               
            TEMP1 = NX-K+I                                              
            TEMP = X(TEMP1) .NE. X(NX)                                  
C/6S                                                                    
C  2     IF (TEMP) CALL SETERR(                                         
C    1      46HDGLSBI - END POINTS OF X NOT OF MULTIPLICITY K, 46, 7, 2)
C/7S                                                                    
   2     IF (TEMP) CALL SETERR(                                         
     1      'DGLSBI - END POINTS OF X NOT OF MULTIPLICITY K', 46, 7, 2) 
C/                                                                      
   3     CONTINUE                                                       
      CALL ENTER(1)                                                     
C SCRATCH A AND F VALUES.                                               
      IAS = ISTKGT(4*NU*(2*NU+NRHS), 4)                                 
C SCRATCH F1 AND F2 VALUES.                                             
      IFS = IAS+8*NU**2                                                 
C SPLINE BASIS AND DERIVATIVES AT L AND R.                              
      ISBSIS = ISTKGT(2*K, 4)                                           
      FAILED = D6LSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG, GETJAC,       
     1   SEPATE, A1, A2, F1, A1T, A2T, F1T, WS(IAS), WS(IFS), WS(ISBSIS)
     2   )                                                              
      CALL LEAVE                                                        
      DGLSBI = FAILED                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE DGLSBP(NU, ORDER, BC, E)                               
      INTEGER NU                                                        
      INTEGER ORDER(NU, NU, 2), BC(NU, 2, 2), E(NU, 2, 2)               
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ICE, ISTKGT, MAX0, I, J, L                                
      INTEGER IPPS, INOW, IMAORD, INOOLD, IS(1000)                      
      REAL RS(1000)                                                     
      LOGICAL ALLERO, LS(1000)                                          
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP1, TEMP2                                              
      LOGICAL TEMP, TEMP3                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO DETERMINE WHICH ODE SHOULD USE WHICH BOUNDARY CONDITION.           
C MNEMONIC - DOUBLE PRECISION GALERKIN'S METHOD FOR LINEAR SYSTEMS,     
C            BOUNDARY CONDITION PLACEMENT.                              
C SCRATCH SPACE ALLOCATED -                                             
C       S(DGLSBP) <= NU*(4*NU+15)                                       
C INTEGER WORDS.                                                        
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (NU .LT. 1) CALL SETERR(16HDGLSBP - NU.LT.1, 16, 1, 2)         
C/7S                                                                    
      IF (NU .LT. 1) CALL SETERR('DGLSBP - NU.LT.1', 16, 1, 2)          
C/                                                                      
      DO  4 L = 1, 2                                                    
         DO  3 I = 1, NU                                                
C IS ORDER(I,.,L) = (-1, ... , -1)?                                     
            ALLERO = .TRUE.                                             
            DO  1 J = 1, NU                                             
               ALLERO = ALLERO .AND. ORDER(I, J, L) .EQ. (-1)           
               TEMP = ORDER(I, J, L) .LT. (-1)                          
               IF (.NOT. TEMP) TEMP = ORDER(I, J, L) .GT. 2             
C/6S                                                                    
C              IF (TEMP) CALL SETERR(                                   
C    1            41HDGLSBP - ORDER(I,J,L) NOT ONE OF -1,0,1,2, 41, 2, 2
C    2            )                                                     
C/7S                                                                    
               IF (TEMP) CALL SETERR(                                   
     1            'DGLSBP - ORDER(I,J,L) NOT ONE OF -1,0,1,2', 41, 2, 2 
     2            )                                                     
C/                                                                      
   1           CONTINUE                                                 
            TEMP = BC(I, 1, L) .NE. (-2)                                
            IF (TEMP) TEMP = BC(I, 1, L) .NE. 0                         
            IF (TEMP) GOTO 2                                            
               TEMP3 = BC(I, 2, L) .NE. (-2)                            
               IF (TEMP3) TEMP3 = BC(I, 2, L) .NE. 1                    
               TEMP = TEMP3                                             
C/6S                                                                    
C  2        IF (TEMP) CALL SETERR(                                      
C    1         36HDGLSBP - BC(I,.,L) NOT ONE OF -2,0,1, 36, 3, 2)       
C           IF (ALLERO) CALL SETERR(                                    
C    1         33HDGLSBP - ORDER(I,.,L)=(-1,...,-1), 33, 4, 2)          
C/7S                                                                    
   2        IF (TEMP) CALL SETERR(                                      
     1         'DGLSBP - BC(I,.,L) NOT ONE OF -2,0,1', 36, 3, 2)        
            IF (ALLERO) CALL SETERR(                                    
     1         'DGLSBP - ORDER(I,.,L)=(-1,...,-1)', 33, 4, 2)           
C/                                                                      
   3        CONTINUE                                                    
   4     CONTINUE                                                       
      CALL ENTER(1)                                                     
C COMPLEMENT OF E.                                                      
      ICE = ISTKGT(NU, 2)                                               
C MAXORD(I,L) = MAX OVER J = 1, ... , NU ORDER(I,J,L).                  
      IMAORD = ISTKGT(2*NU, 2)                                          
      CALL SETI(2*NU, -1, IS(IMAORD))                                   
      DO  7 L = 1, 2                                                    
         DO  6 I = 1, NU                                                
            DO  5 J = 1, NU                                             
               TEMP2 = IMAORD+I-1+(L-1)*NU                              
               TEMP1 = IMAORD+I-1+(L-1)*NU                              
               IS(TEMP2) = MAX0(IS(TEMP1), ORDER(I, J, L))              
   5           CONTINUE                                                 
   6        CONTINUE                                                    
   7     CONTINUE                                                       
      I = 0                                                             
      IPPS = 1                                                          
      INOW = 0                                                          
   8  TEMP = I .GE. 4*NU                                                
      IF (TEMP) TEMP = IPPS .EQ. 1                                      
      IF (TEMP) GOTO  16                                                
         GOTO  13                                                       
C MAKE A NODE.                                                          
   9        INOOLD = INOW                                               
            I = I+1                                                     
            INOW = ISTKGT(NU+3, 2)                                      
            IS(INOW) = INOOLD                                           
C       GET THE CANDIDATES FOR E(I).                                    
            CALL D6LSBP(I, NU, ORDER, BC, E, IS(IMAORD), IS(ICE), IS(   
     1         INOW+3), IS(INOW+1))                                     
            IS(INOW+2) = 0                                              
            IPPS = 0                                                    
            GOTO  14                                                    
C SEARCHING A NODE.                                                     
  10        IS(INOW+2) = IS(INOW+2)+1                                   
            IF (IS(INOW+2) .LE. IS(INOW+1)) GOTO 11                     
               IPPS = -1                                                
C BACK-UP.                                                              
               GOTO  8                                                  
  11        TEMP1 = INOW+2+IS(INOW+2)-1                                 
            E(I, 1, 1) = IS(TEMP1+1)                                    
            IPPS = 1                                                    
            GOTO  14                                                    
C BACKING UP A NODE.                                                    
  12        INOW = IS(INOW)                                             
            CALL ISTKRL(1)                                              
            I = I-1                                                     
            IPPS = 0                                                    
            GOTO  14                                                    
  13        TEMP1 = IPPS+2                                              
            IF (TEMP1 .GT. 0 .AND. TEMP1 .LE. 3) GOTO ( 12,  10,  9),   
     1         TEMP1                                                    
C END SWITCH.                                                           
  14     IF (I .NE. 0) GOTO 15                                          
C/6S                                                                    
C           CALL SETERR(37HDGLSBP - IMPROPER BOUNDARY CONDITIONS, 37, 5,
C    1         1)                                                       
C/7S                                                                    
            CALL SETERR('DGLSBP - IMPROPER BOUNDARY CONDITIONS', 37, 5, 
     1         1)                                                       
C/                                                                      
            GOTO  16                                                    
  15     CONTINUE                                                       
         GOTO  8                                                        
C END WHILE.                                                            
  16  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DGLSBT(K, X, NX, NU, NRHS, NRHSG, GETJAC, A1, A2,      
     1   F1, AA, BB, GAM, BC, G, B)                                     
      INTEGER NRHS, NU, NX                                              
      INTEGER K, NRHSG, BC(NU, 2, 2)                                    
      LOGICAL GETJAC                                                    
      DOUBLE PRECISION X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS,
     1   2), AA(NU, NU, 2), BB(NU, NU, 2)                               
      DOUBLE PRECISION GAM(NU, NRHS, 2), G(1, 1), B(1)                  
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IDF, IDG, ISTKGT, I, IDGI, ISBSIS                         
      INTEGER IS(1000)                                                  
      REAL RS(1000)                                                     
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C MNEMONIC - DOUBLE PRECISION GALERKIN'S METHOD FOR LINEAR SYSTEMS,     
C            BOUNDARY TERMS.                                            
C SCRATCH SPACE ALLOCATED -                                             
C       S(DGLSBT) = NU*(4*NU+2*NRHS) + 2*K                              
C   LONG REAL WORDS +                                                   
C       3*K                                                             
C   INTEGER WORDS.                                                      
C G(NU*(NX-K),2*K*NU-1),B(NU*(NX-K),NRHS).                              
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (K .LT. 2) CALL SETERR(15HDGLSBT - K.LT.2, 15, 1, 2)           
C     IF (NX .LT. 2*K) CALL SETERR(18HDGLSBT - NX.LT.2*K, 18, 2, 2)     
C     IF (NU .LT. 1) CALL SETERR(16HDGLSBT - NU.LT.1, 16, 3, 2)         
C     IF (NRHS .LT. 1) CALL SETERR(18HDGLSBT - NRHS.LT.1, 18, 4, 2)     
C     IF (NRHSG .LT. 0) CALL SETERR(19HDGLSBT - NRHSG.LT.0, 19, 5, 2)   
C/7S                                                                    
      IF (K .LT. 2) CALL SETERR('DGLSBT - K.LT.2', 15, 1, 2)            
      IF (NX .LT. 2*K) CALL SETERR('DGLSBT - NX.LT.2*K', 18, 2, 2)      
      IF (NU .LT. 1) CALL SETERR('DGLSBT - NU.LT.1', 16, 3, 2)          
      IF (NRHS .LT. 1) CALL SETERR('DGLSBT - NRHS.LT.1', 18, 4, 2)      
      IF (NRHSG .LT. 0) CALL SETERR('DGLSBT - NRHSG.LT.0', 19, 5, 2)    
C/                                                                      
C CHECK THAT X IS MONOTONE INCREASING.                                  
      DO  1 I = 2, NX                                                   
C/6S                                                                    
C        IF (X(I-1) .GT. X(I)) CALL SETERR(                             
C    1      37HDGLSBT - X IS NOT MONOTONE INCREASING, 37, 6, 2)         
C/7S                                                                    
         IF (X(I-1) .GT. X(I)) CALL SETERR(                             
     1      'DGLSBT - X IS NOT MONOTONE INCREASING', 37, 6, 2)          
C/                                                                      
         IF (I+K .GT. NX) GOTO  1                                       
         TEMP1 = I+K                                                    
C/6S                                                                    
C        IF (X(TEMP1) .LE. X(I)) CALL SETERR(                           
C    1      37HDGLSBT - X IS NOT MONOTONE INCREASING, 37, 6, 2)         
C/7S                                                                    
         IF (X(TEMP1) .LE. X(I)) CALL SETERR(                           
     1      'DGLSBT - X IS NOT MONOTONE INCREASING', 37, 6, 2)          
C/                                                                      
   1     CONTINUE                                                       
C CHECK THE MULTIPLICITY OF THE END POINTS.                             
      DO  3 I = 1, K                                                    
         TEMP = X(I) .NE. X(1)                                          
         IF (TEMP) GOTO 2                                               
            TEMP1 = NX-K+I                                              
            TEMP = X(TEMP1) .NE. X(NX)                                  
C/6S                                                                    
C  2     IF (TEMP) CALL SETERR(                                         
C    1      46HDGLSBT - END POINTS OF X NOT OF MULTIPLICITY K, 46, 7, 2)
C/7S                                                                    
   2     IF (TEMP) CALL SETERR(                                         
     1      'DGLSBT - END POINTS OF X NOT OF MULTIPLICITY K', 46, 7, 2) 
C/                                                                      
   3     CONTINUE                                                       
      CALL ENTER(1)                                                     
      IDG = ISTKGT(NU, 4)                                               
      IDGI = ISTKGT(NU, 4)                                              
      IDF = ISTKGT(NRHS, 4)                                             
C SPLINE BASIS AND DERIVATIVES AT L AND R.                              
      ISBSIS = ISTKGT(2*K, 4)                                           
      CALL D6LSBT(K, X, NX, NU, NRHS, NRHSG, GETJAC, A1, A2, F1, AA, BB,
     1   GAM, BC, G, B, NU*(NX-K), WS(IDG), WS(IDGI), WS(IDF), WS(      
     2   ISBSIS))                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION DGLSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG        
     1   , MQ, XQ, WQ, GETJAC, SEPATE, G, GT, B, BT, ORDER, IGSSIS, SET)
      INTEGER MQ, NU, NX                                                
      EXTERNAL AF, AF1                                                  
      INTEGER K, NRHS, NRHSG, ORDER(NU, NU, 2), IGSSIS                  
      LOGICAL GETJAC, SEPATE, SET                                       
      DOUBLE PRECISION X(NX), XQ(MQ), WQ(MQ), G(1, 1), GT(1, 1), B(1, 1)
      DOUBLE PRECISION BT(1, 1)                                         
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ITQ, ISTKGT, IA1T, IA2T, IA3T, IA4T                       
      INTEGER IF1T, IF2T, I, ISBSIS, IQ, IS(1000)                       
      INTEGER IA1, IA2, IA3, IA4, IF1, IF2                              
      INTEGER IQ0, IZERO                                                
      REAL RS(1000)                                                     
      LOGICAL FAILED, LS(1000), D6LSIN                                  
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C MNEMONIC - DOUBLE PRECISION GALERKIN'S METHOD FOR LINEAR SYSTEMS,     
C            INTEGRALS.                                                 
C SCRATCH SPACE ALLOCATED -                                             
C       S(DGLSIN) = MQ*( 2*K+1 + 8*NU**2 + 2*NU*NRHS ) +                
C                   MAX( IF ( SET ) { 0 } ELSE { 3*K }, S(AF) )         
C   LONG REAL WORDS +                                                   
C                   NU                                                  
C   LOGICAL WORDS.                                                      
C LONG REAL G((NX-K)*NU,2*K*NU-1),B((NX-K)*NU,NRHS),BT((NX-K)*NU,NRHS)  
C CHECK THE DATA FOR ERRORS.                                            
C/6S                                                                    
C     IF (K .LT. 2) CALL SETERR(15HDGLSIN - K.LT.2, 15, 1, 2)           
C     IF (NX .LT. 2*K) CALL SETERR(18HDGLSIN - NX.LT.2*K, 18, 2, 2)     
C     IF (NU .LT. 1) CALL SETERR(16HDGLSIN - NU.LT.1, 16, 3, 2)         
C     IF (NRHS .LT. 1) CALL SETERR(18HDGLSIN - NRHS.LT.1, 18, 4, 2)     
C     IF (NRHSG .LT. 0) CALL SETERR(19HDGLSIN - NRHSG.LT.0, 19, 5, 2)   
C     IF (MQ .LT. K-1) CALL SETERR(18HDGLSIN - MQ.LT.K-1, 18, 6, 2)     
C     IF (XQ(1) .LT. (-1D0)) CALL SETERR(20HDGLSIN - XQ(1).LT.-1, 20, 7,
C    1   2)                                                             
C/7S                                                                    
      IF (K .LT. 2) CALL SETERR('DGLSIN - K.LT.2', 15, 1, 2)            
      IF (NX .LT. 2*K) CALL SETERR('DGLSIN - NX.LT.2*K', 18, 2, 2)      
      IF (NU .LT. 1) CALL SETERR('DGLSIN - NU.LT.1', 16, 3, 2)          
      IF (NRHS .LT. 1) CALL SETERR('DGLSIN - NRHS.LT.1', 18, 4, 2)      
      IF (NRHSG .LT. 0) CALL SETERR('DGLSIN - NRHSG.LT.0', 19, 5, 2)    
      IF (MQ .LT. K-1) CALL SETERR('DGLSIN - MQ.LT.K-1', 18, 6, 2)      
      IF (XQ(1) .LT. (-1D0)) CALL SETERR('DGLSIN - XQ(1).LT.-1', 20, 7, 
     1   2)                                                             
C/                                                                      
      IF (WQ(1) .EQ. 0D0) GOTO 1                                        
         IQ0 = 1                                                        
         GOTO  2                                                        
   1     IQ0 = MQ+1                                                     
   2  IQ = 2                                                            
         GOTO  4                                                        
   3     IQ = IQ+1                                                      
   4     IF (IQ .GT. MQ) GOTO  5                                        
C CHECK XQ FOR MONOTONICITY                                             
C AND WQ BEING 0.                                                       
C/6S                                                                    
C        IF (XQ(IQ-1) .GE. XQ(IQ)) CALL SETERR(                         
C    1      44HDGLSIN - XQ NOT STRICTLY MONOTONE INCREASING, 44, 8, 2)  
C/7S                                                                    
         IF (XQ(IQ-1) .GE. XQ(IQ)) CALL SETERR(                         
     1      'DGLSIN - XQ NOT STRICTLY MONOTONE INCREASING', 44, 8, 2)   
C/                                                                      
         IF (WQ(IQ) .NE. 0D0) IQ0 = IQ                                  
         GOTO  3                                                        
C/6S                                                                    
C  5  IF (IQ0 .GT. MQ) CALL SETERR(31HDGLSIN - WQ IS IDENTICALLY ZERO,  
C    1   31, 9, 2)                                                      
C     IF (XQ(MQ) .GT. 1D0) CALL SETERR(20HDGLSIN - XQ(MQ).GT.1, 20, 10  
C    1   , 2)                                                           
C/7S                                                                    
   5  IF (IQ0 .GT. MQ) CALL SETERR('DGLSIN - WQ IS IDENTICALLY ZERO',   
     1   31, 9, 2)                                                      
      IF (XQ(MQ) .GT. 1D0) CALL SETERR('DGLSIN - XQ(MQ).GT.1', 20, 10   
     1   , 2)                                                           
C/                                                                      
      DO  7 I = 1, K                                                    
         TEMP = X(I) .NE. X(1)                                          
         IF (TEMP) GOTO 6                                               
            TEMP1 = NX-K+I                                              
            TEMP = X(TEMP1) .NE. X(NX)                                  
C/6S                                                                    
C  6     IF (TEMP) CALL SETERR(                                         
C    1      46HDGLSIN - END POINTS OF X NOT OF MULTIPLICITY K, 46, 11, 2
C    2      )                                                           
C/7S                                                                    
   6     IF (TEMP) CALL SETERR(                                         
     1      'DGLSIN - END POINTS OF X NOT OF MULTIPLICITY K', 46, 11, 2 
     2      )                                                           
C/                                                                      
   7     CONTINUE                                                       
C/6S                                                                    
C     IF (X(1) .GE. X(NX)) CALL SETERR(                                 
C    1   34HDGLSIN - X NOT MONOTONE INCREASING, 34, 12, 2)              
C/7S                                                                    
      IF (X(1) .GE. X(NX)) CALL SETERR(                                 
     1   'DGLSIN - X NOT MONOTONE INCREASING', 34, 12, 2)               
C/                                                                      
      TEMP = SET                                                        
      IF (TEMP) TEMP = IGSSIS .LE. 1                                    
C/6S                                                                    
C     IF (TEMP) CALL SETERR(34HDGLSIN - SET=.T. AND IGSBASIS.LE.1, 34,  
C    1   13, 2)                                                         
C/7S                                                                    
      IF (TEMP) CALL SETERR('DGLSIN - SET=.T. AND IGSBASIS.LE.1', 34,   
     1   13, 2)                                                         
C/                                                                      
      CALL ENTER(1)                                                     
      ITQ = ISTKGT(MQ, 4)                                               
      ISBSIS = ISTKGT(2*K*MQ, 4)                                        
      IA1 = ISTKGT(4*MQ*NU*(2*NU+NRHS), 4)                              
      IA1T = IA1+MQ*NU**2                                               
      IA2 = IA1T+MQ*NU**2                                               
      IA2T = IA2+MQ*NU**2                                               
      IA3 = IA2T+MQ*NU**2                                               
      IA3T = IA3+MQ*NU**2                                               
      IA4 = IA3T+MQ*NU**2                                               
      IA4T = IA4+MQ*NU**2                                               
      IF1 = IA4T+MQ*NU**2                                               
      IF1T = IF1+MQ*NU*NRHS                                             
      IF2 = IF1T+MQ*NU*NRHS                                             
      IF2T = IF2+MQ*NU*NRHS                                             
      IZERO = ISTKGT(NU, 1)                                             
      FAILED = D6LSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG, MQ, XQ, WQ,   
     1   GETJAC, SEPATE, G, GT, B, BT, ORDER, IGSSIS, SET, WS(ITQ), WS( 
     2   ISBSIS), WS(IA1), WS(IA1T), WS(IA2), WS(IA2T), WS(IA3), WS(    
     3   IA3T), WS(IA4), WS(IA4T), WS(IF1), WS(IF1T), WS(IF2), WS(IF2T),
     4   LS(IZERO), WS(IGSSIS), (NX-K)*NU, 2*K*NU-1, 2*MQ*K)            
      CALL LEAVE                                                        
      DGLSIN = FAILED                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE DGLSSB(ALFA, BETA, GAMMA, NU, NRHS, AA, BB, CC,        
     1   SGAMAD, SGAMAM, BC)                                            
      INTEGER NRHS, NU                                                  
      INTEGER BC(NU, 2)                                                 
      DOUBLE PRECISION ALFA(NU, NU), BETA(NU, NU), GAMMA(NU, NRHS), AA( 
     1   NU, NU), BB(NU, NU), CC(NU, NU)                                
      DOUBLE PRECISION SGAMAD(NU, NRHS), SGAMAM(NU, NRHS)               
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER IED, IEM, ISTKGT, NERROR, IPIVOT, I                       
      INTEGER J, NERR, IA, IB, IC, IG                                   
      INTEGER IDIAG, IS(1000), NDEQS, NMEQS                             
      REAL RS(1000)                                                     
      LOGICAL ALLERO, ALFERO, LS(1000)                                  
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO CONVERT GENERAL LINEAR BC'S INTO STANDARD BC'S.                    
C MNEMONIC - DOUBLE PRECISION GALERKIN'S METHOD FOR LINEAR SYSTEMS,     
C            STANDARD BOUNDARY CONDITIONS.                              
C SCRATCH SPACE ALLOCATED -                                             
C       S(DGLSSB) <= 4*NU                                               
C   INTEGER WORDS +                                                     
C       NU*(2*NU+NRHS+2)                                                
C   LONG REAL WORDS.                                                    
C/6S                                                                    
C     IF (NU .LT. 1) CALL SETERR(16HDGLSSB - NU.LT.1, 16, 1, 2)         
C     IF (NRHS .LT. 1) CALL SETERR(18HDGLSSB - NRHS.LT.1, 18, 2, 2)     
C/7S                                                                    
      IF (NU .LT. 1) CALL SETERR('DGLSSB - NU.LT.1', 16, 1, 2)          
      IF (NRHS .LT. 1) CALL SETERR('DGLSSB - NRHS.LT.1', 18, 2, 2)      
C/                                                                      
      CALL ENTER(1)                                                     
C THE DEFAULT IS NO BOUNDARY CONDITIONS AT ALL.                         
      CALL SETD(NU**2, 0D0, AA)                                         
      CALL SETD(NU**2, 0D0, BB)                                         
      CALL SETD(NU**2, 0D0, CC)                                         
      CALL SETD(NU*NRHS, 0D0, SGAMAD)                                   
      CALL SETD(NU*NRHS, 0D0, SGAMAM)                                   
      CALL SETI(2*NU, -2, BC)                                           
C SPACE FOR THE DIRICHLET EQUATION NUMBERS.                             
      IED = ISTKGT(NU, 2)                                               
C SPACE FOR THE MIXED EQUATION NUMBERS.                                 
      IEM = ISTKGT(NU, 2)                                               
      NDEQS = 0                                                         
      NMEQS = 0                                                         
C FIND THE EQUATIONS FOR EACH.                                          
      DO  5 I = 1, NU                                                   
         ALLERO = .TRUE.                                                
         DO  3 J = 1, NU                                                
            TEMP = ALFA(I, J) .EQ. 0D0                                  
            IF (TEMP) TEMP = BETA(I, J) .EQ. 0D0                        
            ALFERO = TEMP                                               
            ALLERO = ALLERO .AND. ALFERO                                
            IF (BETA(I, J) .EQ. 0D0) GOTO 1                             
               NMEQS = NMEQS+1                                          
C THEN MIXED.                                                           
               TEMP1 = IEM-1+NMEQS                                      
               IS(TEMP1) = I                                            
               GOTO  4                                                  
   1        TEMP = J .EQ. NU                                            
            IF (TEMP) TEMP = .NOT. ALLERO                               
            IF (.NOT. TEMP) GOTO 2                                      
               NDEQS = NDEQS+1                                          
C THEN DIRICHLET.                                                       
               TEMP1 = IED-1+NDEQS                                      
               IS(TEMP1) = I                                            
   2        CONTINUE                                                    
   3        CONTINUE                                                    
C END J.                                                                
   4     CONTINUE                                                       
   5     CONTINUE                                                       
C END I.                                                                
      IF (NMEQS .LE. 0) GOTO 7                                          
         IA = ISTKGT(NMEQS*NU, 4)                                       
C CONVERT THE MIXED BC'S.                                               
         IB = ISTKGT(NMEQS*NU, 4)                                       
         IG = ISTKGT(NMEQS*NRHS, 4)                                     
         IPIVOT = ISTKGT(NU, 2)                                         
         IDIAG = ISTKGT(NU, 4)                                          
         CALL D6LSSM(ALFA, BETA, GAMMA, NU, NRHS, AA, BB, SGAMAM, BC(1  
     1      , 2), WS(IA), WS(IB), WS(IG), IS(IEM), NMEQS, IS(IPIVOT),   
     2      WS(IDIAG))                                                  
         CALL ISTKRL(5)                                                 
         IF (NERROR(NERR) .EQ. 0) GOTO 6                                
            CALL LEAVE                                                  
            RETURN                                                      
   6     CONTINUE                                                       
   7  IF (NDEQS .LE. 0) GOTO 8                                          
         IC = ISTKGT(NDEQS*NU, 4)                                       
C CONVERT THE DIRICHLET EQUATIONS.                                      
         IG = ISTKGT(NDEQS*NRHS, 4)                                     
         IPIVOT = ISTKGT(NU, 2)                                         
         IDIAG = ISTKGT(NU, 4)                                          
         CALL D6LSSD(ALFA, GAMMA, NU, NRHS, CC, SGAMAD, BC(1, 1), WS(IC)
     1      , WS(IG), IS(IED), NDEQS, IS(IPIVOT), WS(IDIAG))            
   8  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DMMPY(A, MA, NA, B, NB, C)                             
      INTEGER MA, NA, NB                                                
      DOUBLE PRECISION A(MA, NA), B(NA, NB), C(MA, NB)                  
      INTEGER I, J, K                                                   
      DOUBLE PRECISION T                                                
      LOGICAL TEMP                                                      
C  TO MULTIPLY THE MATRICES A*B AND PUT THE RESULT IN C.                
C  INPUT -                                                              
C    A  - THE MATRIX A                                                  
C    MA - IS MA BY                                                      
C    NA - NA.                                                           
C    B  - THE MATRIX B                                                  
C    NB - IS NA BY NB.                                                  
C  OUTPUT -                                                             
C    C - C=A*B IS MA BY NB.                                             
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C  ERROR STATES -                                                       
C    1 - BAD DIMENSIONS FOR A AND/OR B.                                 
      TEMP = MA .LT. 1                                                  
      IF (.NOT. TEMP) TEMP = NA .LT. 1                                  
      IF (.NOT. TEMP) TEMP = NB .LT. 1                                  
C/6S                                                                    
C     IF (TEMP) CALL SETERR(37HDMMPY - BAD DIMENSIONS FOR A AND/OR B,   
C    1   37, 1, 2)                                                      
C/7S                                                                    
      IF (TEMP) CALL SETERR('DMMPY - BAD DIMENSIONS FOR A AND/OR B',    
     1   37, 1, 2)                                                      
C/                                                                      
      DO  3 I = 1, MA                                                   
         DO  2 J = 1, NB                                                
            T = 0                                                       
            DO  1 K = 1, NA                                             
               T = T+A(I, K)*B(K, J)                                    
   1           CONTINUE                                                 
            C(I, J) = T                                                 
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D6LSBC(NU, ORDER, BC, E, MAXORD, COUNT)          
      INTEGER NU                                                        
      INTEGER ORDER(NU, NU, 2), BC(NU, 2, 2), E(NU, 2, 2), MAXORD(NU, 2)
     1   , COUNT(NU, 2)                                                 
      INTEGER MAX0, I, J, L                                             
      INTEGER TEMP1                                                     
      LOGICAL TEMP                                                      
      CALL SETI(2*NU, -1, MAXORD)                                       
      CALL SETI(2*NU, 0, COUNT)                                         
      DO  3 L = 1, 2                                                    
         DO  2 I = 1, NU                                                
            DO  1 J = 1, NU                                             
               MAXORD(I, L) = MAX0(MAXORD(I, L), ORDER(I, J, L))        
   1           CONTINUE                                                 
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      DO  9 L = 1, 2                                                    
         DO  8 I = 1, NU                                                
            IF (BC(I, 1, L) .NE. 0) GOTO 5                              
               TEMP1 = E(I, 1, L)                                       
               COUNT(TEMP1, L) = COUNT(TEMP1, L)+1                      
               TEMP1 = E(I, 1, L)                                       
               IF (MAXORD(TEMP1, L) .GT. BC(I, 1, L)) GOTO 4            
                  D6LSBC = .FALSE.                                      
                  RETURN                                                
   4           CONTINUE                                                 
   5        IF (BC(I, 2, L) .NE. 1) GOTO 7                              
               TEMP1 = E(I, 2, L)                                       
               COUNT(TEMP1, L) = COUNT(TEMP1, L)+1                      
               TEMP1 = E(I, 2, L)                                       
               IF (ORDER(TEMP1, I, L) .GT. BC(I, 2, L)) GOTO 6          
                  D6LSBC = .FALSE.                                      
                  RETURN                                                
   6           CONTINUE                                                 
   7        CONTINUE                                                    
   8        CONTINUE                                                    
   9     CONTINUE                                                       
      DO  12 I = 1, NU                                                  
         TEMP = COUNT(I, 1) .GT. 1                                      
         IF (.NOT. TEMP) TEMP = COUNT(I, 2) .GT. 1                      
         IF (.NOT. TEMP) GOTO 10                                        
            D6LSBC = .FALSE.                                            
            RETURN                                                      
  10     IF (COUNT(I, 1)+COUNT(I, 2) .LE. MAX0(MAXORD(I, 1), MAXORD(I, 2
     1      ))) GOTO 11                                                 
            D6LSBC = .FALSE.                                            
            RETURN                                                      
  11     CONTINUE                                                       
  12     CONTINUE                                                       
      D6LSBC = .TRUE.                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE D6LSBD(K, NX, NU, NRHS, NRHSG, BC, E, CC, GAM,         
     1   GETJAC, G, B, GDIM1, GDIM2)                                    
      INTEGER NRHS, GDIM1, GDIM2, NU                                    
      INTEGER K, NX, NRHSG, BC(NU, 2, 2), E(NU, 2, 2)                   
      LOGICAL GETJAC                                                    
      DOUBLE PRECISION CC(NU, NU, 2), GAM(NU, NRHS, 2), G(GDIM1, GDIM2),
     1   B(GDIM1, NRHS)                                                 
      INTEGER JLR, I, J, L, GIDX1                                       
      INTEGER TEMP                                                      
      LOGICAL TEMP1                                                     
      DO  7 JLR = 1, 2                                                  
         DO  6 I = 1, NU                                                
            TEMP1 = BC(I, 1, JLR) .NE. (-2)                             
            IF (TEMP1) TEMP1 = BC(I, 1, JLR) .NE. 0                     
C/6S                                                                    
C           IF (TEMP1) CALL SETERR(                                     
C    1         36HDGLSBD - BC(I,1,JLR) NOT ONE OF -2,0, 36, 6, 2)       
C/7S                                                                    
            IF (TEMP1) CALL SETERR(                                     
     1         'DGLSBD - BC(I,1,JLR) NOT ONE OF -2,0', 36, 6, 2)        
C/                                                                      
            TEMP1 = BC(I, 2, JLR) .NE. (-2)                             
            IF (TEMP1) TEMP1 = BC(I, 2, JLR) .NE. 1                     
C/6S                                                                    
C           IF (TEMP1) CALL SETERR(                                     
C    1         36HDGLSBD - BC(I,2,JLR) NOT ONE OF -2,1, 36, 7, 2)       
C/7S                                                                    
            IF (TEMP1) CALL SETERR(                                     
     1         'DGLSBD - BC(I,2,JLR) NOT ONE OF -2,1', 36, 7, 2)        
C/                                                                      
            IF (BC(I, 1, JLR) .NE. 0) GOTO  6                           
C ONLY DO DIRICHLET B.C.'S.                                             
            TEMP1 = E(I, 1, JLR) .LT. 1                                 
            IF (.NOT. TEMP1) TEMP1 = E(I, 1, JLR) .GT. NU               
C/6S                                                                    
C           IF (TEMP1) CALL SETERR(                                     
C    1         39HDGLSBD - E(I,1,JLR) NOT ONE OF 1,...,NU, 39, 8, 2)    
C/7S                                                                    
            IF (TEMP1) CALL SETERR(                                     
     1         'DGLSBD - E(I,1,JLR) NOT ONE OF 1,...,NU', 39, 8, 2)     
C/                                                                      
            GIDX1 = E(I, 1, JLR)+(JLR-1)*(NX-K-1)*NU                    
            L = 1                                                       
               GOTO  2                                                  
   1           L = L+1                                                  
   2           IF (L .GT. NRHSG) GOTO  3                                
               B(GIDX1, L) = GAM(I, L, JLR)                             
               GOTO  1                                                  
   3        IF (.NOT. GETJAC) GOTO  6                                   
            DO  4 J = 1, GDIM2                                          
               G(GIDX1, J) = 0                                          
   4           CONTINUE                                                 
            DO  5 J = 1, NU                                             
               TEMP1 = CC(I, J, JLR) .NE. 0D0                           
               IF (TEMP1) TEMP1 = BC(J, 1, JLR) .EQ. 0                  
C/6S                                                                    
C              IF (TEMP1) CALL SETERR(                                  
C    1        55HDGLSBD - CC(I,J,JLR).NE.0 AND BC(I,1,JLR)=0=BC(J,1,JLR)
C    2            , 55, 9, 2)                                           
C/7S                                                                    
               IF (TEMP1) CALL SETERR(                                  
     1        'DGLSBD - CC(I,J,JLR).NE.0 AND BC(I,1,JLR)=0=BC(J,1,JLR)' 
     2            , 55, 9, 2)                                           
C/                                                                      
               TEMP = J-E(I, 1, JLR)+K*NU                               
               G(GIDX1, TEMP) = -CC(I, J, JLR)                          
   5           CONTINUE                                                 
            TEMP = I-E(I, 1, JLR)+K*NU                                  
            G(GIDX1, TEMP) = 1                                          
   6        CONTINUE                                                    
   7     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D6LSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG        
     1   , GETJAC, SEPATE, A1, A2, F1, A1T, A2T, F1T, AS, FS, SBASIS)   
      INTEGER K, NRHS, NU, NX                                           
      EXTERNAL AF, AF1                                                  
      INTEGER NRHSG                                                     
      LOGICAL AF, GETJAC, SEPATE                                        
      DOUBLE PRECISION X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS,
     1   2), A1T(NU, NU, 2), A2T(NU, NU, 2)                             
      DOUBLE PRECISION F1T(NU, NRHS, 2), AS(NU, NU, 8), FS(NU, NRHS, 4),
     1   SBASIS(K, 2)                                                   
      INTEGER JLR, ID(2)                                                
      DOUBLE PRECISION LR(2)                                            
      LOGICAL TEMP                                                      
      DATA ID(1)/0/                                                     
      DATA ID(2)/1/                                                     
      LR(1) = X(1)                                                      
      LR(2) = X(NX)                                                     
C DO L AND R TERMS.                                                     
      DO  4 JLR = 1, 2                                                  
C   GET THE BASIS SPLINES AT L OR R.                                    
         CALL DBSPL1(K, X, NX, LR(JLR), 1, K+(JLR-1)*(NX-2*K), ID, 2,   
     1      SBASIS)                                                     
C DEFAULT A AND F VALUES.                                               
         CALL SETD(4*NU*(2*NU+NRHS), 0D0, AS)                           
         IF (.NOT. AF(LR(JLR), 1, NU, NRHS, NRHSG, GETJAC, SEPATE, AS(1,
     1      1, 1), AS(1, 1, 2), AS(1, 1, 3), AS(1, 1, 4), AS(1, 1, 5),  
     2      AS(1, 1, 6), AS(1, 1, 7), AS(1, 1, 8), FS(1, 1, 1), FS(1, 1,
     3      2), FS(1, 1, 3), FS(1, 1, 4), K+(JLR-1)*(NX-2*K), SBASIS, K,
     4      X, NX, AF1)) GOTO 1                                         
            D6LSBI = .TRUE.                                             
            RETURN                                                      
   1     IF (.NOT. GETJAC) GOTO 3                                       
            CALL MOVEFD(NU**2, AS(1, 1, 1), A1(1, 1, JLR))              
            CALL MOVEFD(NU**2, AS(1, 1, 3), A2(1, 1, JLR))              
            IF (.NOT. SEPATE) GOTO 2                                    
               CALL MOVEFD(NU**2, AS(1, 1, 2), A1T(1, 1, JLR))          
               CALL MOVEFD(NU**2, AS(1, 1, 4), A2T(1, 1, JLR))          
   2        CONTINUE                                                    
   3     IF (NRHSG .GT. 0) CALL MOVEFD(NU*NRHSG, FS(1, 1, 1), F1(1, 1,  
     1      JLR))                                                       
         TEMP = SEPATE                                                  
         IF (TEMP) TEMP = NRHSG .GT. 0                                  
         IF (TEMP) CALL MOVEFD(NU*NRHSG, FS(1, 1, 2), F1T(1, 1, JLR))   
   4     CONTINUE                                                       
C END JLR.                                                              
      D6LSBI = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE D6LSBP(I, NU, ORDER, BC, E, MAXORD, CE, R, N)          
      INTEGER NU                                                        
      INTEGER I, ORDER(NU, NU, 2), BC(1), E(1), MAXORD(NU, 2), CE(NU)   
      INTEGER R(NU), N                                                  
      INTEGER MOD, MAX0, J, L, NBCS, DM                                 
      INTEGER II, LR                                                    
      INTEGER TEMP2                                                     
      LOGICAL TEMP, TEMP1                                               
C BC(NU,2,2),E(NU,2,2),                                                 
C E(I-1),R(N).                                                          
      IF (BC(I) .GE. 0) GOTO 1                                          
         N = 1                                                          
         R(N) = 0                                                       
         RETURN                                                         
C LR = 1 FOR LEFT, LR = 2 FOR RIGHT.                                    
   1  LR = (I-1)/(2*NU)+1                                               
C DM = 1 FOR DIRICHLET, DM = 2 FOR MIXED BOUNDARY CONDITIONS.           
      DM = MOD((I-1)/NU, 2)+1                                           
      II = MOD(I, NU)                                                   
      IF (II .EQ. 0) II = NU                                            
C B(I) = B(II,DM,LR).                                                   
      N = 0                                                             
      DO  2 J = 1, NU                                                   
         CE(J) = J                                                      
   2     CONTINUE                                                       
C CE = COMPLEMENT OF E.                                                 
      IF (I .GT. 2*NU) GOTO 7                                           
         J = 1                                                          
            GOTO  4                                                     
   3        J = J+1                                                     
   4        IF (J .GE. I) GOTO  6                                       
            IF (BC(J) .LT. 0) GOTO 5                                    
               TEMP2 = E(J)                                             
               CE(TEMP2) = 0                                            
   5        CONTINUE                                                    
            GOTO  3                                                     
   6     CONTINUE                                                       
         GOTO  12                                                       
   7     J = 2*NU+1                                                     
            GOTO  9                                                     
   8        J = J+1                                                     
   9        IF (J .GE. I) GOTO  11                                      
            IF (BC(J) .LT. 0) GOTO 10                                   
               TEMP2 = E(J)                                             
               CE(TEMP2) = 0                                            
  10        CONTINUE                                                    
            GOTO  8                                                     
  11     CONTINUE                                                       
  12  DO  19 J = 1, NU                                                  
         IF (CE(J) .EQ. 0) GOTO  19                                     
         NBCS = 0                                                       
         L = 1                                                          
            GOTO  14                                                    
  13        L = L+1                                                     
  14        IF (L .GE. I) GOTO  15                                      
            TEMP = E(L) .EQ. J                                          
            IF (TEMP) TEMP = BC(L) .GE. 0                               
            IF (TEMP) NBCS = NBCS+1                                     
            GOTO  13                                                    
  15     TEMP = DM .EQ. 1                                               
         IF (TEMP) TEMP = MAXORD(J, LR) .GT. BC(I)                      
         IF (TEMP) GOTO 16                                              
            TEMP1 = DM .EQ. 2                                           
            IF (TEMP1) TEMP1 = ORDER(J, II, LR) .GT. BC(I)              
            TEMP = TEMP1                                                
  16     IF (.NOT. TEMP) GOTO 18                                        
            IF (NBCS .GE. MAX0(MAXORD(J, 1), MAXORD(J, 2))) GOTO 17     
               N = N+1                                                  
               R(N) = J                                                 
  17        CONTINUE                                                    
  18     CONTINUE                                                       
  19     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE D6LSBT(K, X, NX, NU, NRHS, NRHSG, GETJAC, A1, A2,      
     1   F1, AA, BB, GAM, BC, G, B, GDIM, DG, DGI, DF, SBASIS)          
      INTEGER K, GDIM, NRHS, NU, NX                                     
      INTEGER NRHSG, BC(NU, 2, 2)                                       
      LOGICAL GETJAC                                                    
      DOUBLE PRECISION X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS,
     1   2), AA(NU, NU, 2), BB(NU, NU, 2)                               
      DOUBLE PRECISION GAM(NU, NRHS, 2), G(GDIM, 1), B(GDIM, NRHS), DG( 
     1   NU), DGI(NU), DF(NRHS)                                         
      DOUBLE PRECISION SBASIS(K, 2)                                     
      INTEGER JLR, I, J, L, GIDX, ISINLR                                
      INTEGER ID(2)                                                     
      DOUBLE PRECISION BBP, BIP, SIGNLR                                 
      INTEGER TEMP                                                      
      LOGICAL TEMP1, TEMP2                                              
      DATA ID(1)/0/                                                     
      DATA ID(2)/1/                                                     
C DO L AND R TERMS.                                                     
      DO  18 JLR = 1, 2                                                 
         ISINLR = (-1)**(JLR+1)                                         
         SIGNLR = ISINLR                                                
C   GET THE BASIS SPLINES AT L OR R.                                    
         TEMP = (JLR-1)*(NX-1)                                          
         CALL DBSPL1(K, X, NX, X(TEMP+1), 1, K+(JLR-1)*(NX-2*K), ID, 2  
     1      , SBASIS)                                                   
         TEMP = (JLR-1)*(K-1)                                           
         BBP = SBASIS(TEMP+1, 2)                                        
         TEMP = (JLR-1)*(K-3)                                           
         BIP = SBASIS(TEMP+2, 2)                                        
         DO  17 I = 1, NU                                               
            GIDX = I+(JLR-1)*(NX-K-1)*NU                                
            CALL SETD(NU, 0D0, DG)                                      
            CALL SETD(NU, 0D0, DGI)                                     
            IF (NRHSG .GT. 0) CALL SETD(NRHSG, 0D0, DF)                 
            DO  11 J = 1, NU                                            
C NOT MIXED.                                                            
               TEMP1 = BC(J, 1, JLR) .EQ. 0                             
               IF (TEMP1) GOTO 1                                        
                  TEMP2 = BC(J, 1, JLR) .EQ. (-2)                       
                  IF (TEMP2) TEMP2 = BC(J, 2, JLR) .EQ. (-2)            
                  TEMP1 = TEMP2                                         
   1           IF (.NOT. TEMP1) GOTO 3                                  
                  IF (.NOT. GETJAC) GOTO 2                              
                     DG(J) = DG(J)+SIGNLR*(A1(I, J, JLR)*BBP+A2(I, J,   
     1                  JLR))                                           
                     DGI(J) = DGI(J)+SIGNLR*A1(I, J, JLR)*BIP           
   2              CONTINUE                                              
   3           IF (BC(J, 2, JLR) .NE. 1) GOTO 9                         
                  IF (.NOT. GETJAC) GOTO 5                              
                     DO  4 L = 1, NU                                    
C MIXED.                                                                
                        TEMP1 = BC(L, 2, JLR) .EQ. 1                    
                        IF (TEMP1) TEMP1 = BB(J, L, JLR) .NE. 0D0       
C/6S                                                                    
C                       IF (TEMP1) CALL SETERR(                         
C    1       56HDGLSBT - BC(L,2,JLR)=1=BC(J,2,JLR) WHEN BB(J,L,JLR).NE.0
C    2                     , 56, 8, 2)                                  
C/7S                                                                    
                        IF (TEMP1) CALL SETERR(                         
     1       'DGLSBT - BC(L,2,JLR)=1=BC(J,2,JLR) WHEN BB(J,L,JLR).NE.0' 
     2                     , 56, 8, 2)                                  
C/                                                                      
                        DG(L) = DG(L)+SIGNLR*A1(I, J, JLR)*(AA(J, L,    
     1                     JLR)+BB(J, L, JLR)*BBP)                      
                        DGI(L) = DGI(L)+SIGNLR*A1(I, J, JLR)*BB(J, L,   
     1                     JLR)*BIP                                     
   4                    CONTINUE                                        
                     DG(J) = DG(J)+SIGNLR*A2(I, J, JLR)                 
   5              L = 1                                                 
                     GOTO  7                                            
   6                 L = L+1                                            
   7                 IF (L .GT. NRHSG) GOTO  8                          
                     DF(L) = DF(L)-SIGNLR*A1(I, J, JLR)*GAM(J, L, JLR)  
                     GOTO  6                                            
   8              CONTINUE                                              
C BAD BC.                                                               
   9           TEMP1 = BC(J, 1, JLR) .NE. (-2)                          
               IF (TEMP1) TEMP1 = BC(J, 1, JLR) .NE. 0                  
               IF (TEMP1) GOTO 10                                       
                  TEMP2 = BC(J, 2, JLR) .NE. (-2)                       
                  IF (TEMP2) TEMP2 = BC(J, 2, JLR) .NE. 1               
                  TEMP1 = TEMP2                                         
C/6S                                                                    
C 10           IF (TEMP1) CALL SETERR(                                  
C    1            38HDGLSBT - BC(J,.,JLR) NOT ONE OF -2,0,1, 38, 9, 2)  
C/7S                                                                    
  10           IF (TEMP1) CALL SETERR(                                  
     1            'DGLSBT - BC(J,.,JLR) NOT ONE OF -2,0,1', 38, 9, 2)   
C/                                                                      
  11           CONTINUE                                                 
C END J.                                                                
            IF (.NOT. GETJAC) GOTO 13                                   
               DO  12 J = 1, NU                                         
                  TEMP = J-I+K*NU                                       
                  G(GIDX, TEMP) = G(GIDX, TEMP)+DG(J)                   
                  TEMP = J-I+(K+ISINLR)*NU                              
                  G(GIDX, TEMP) = G(GIDX, TEMP)+DGI(J)                  
  12              CONTINUE                                              
  13        L = 1                                                       
               GOTO  15                                                 
  14           L = L+1                                                  
  15           IF (L .GT. NRHSG) GOTO  16                               
               B(GIDX, L) = B(GIDX, L)+DF(L)+SIGNLR*F1(I, L, JLR)       
               GOTO  14                                                 
  16        CONTINUE                                                    
  17        CONTINUE                                                    
C END I.                                                                
  18     CONTINUE                                                       
C END JLR.                                                              
      RETURN                                                            
      END                                                               
      LOGICAL FUNCTION D6LSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG        
     1   , MQ, XQ, WQ, GETJAC, SEPATE, G, GT, B, BT, ORDER, IGSSIS, SET,
     2   TQ, SBASIS, A1, A1T, A2, A2T, A3, A3T, A4, A4T, F1, F1T, F2,   
     3   F2T, ZERO, GSBSIS, GDIM1, GDIM2, GSDIM1)                       
      INTEGER K, NRHS, GDIM1, GDIM2, MQ, NU                             
      INTEGER NX, GSDIM1                                                
      EXTERNAL AF, AF1                                                  
      INTEGER NRHSG, ORDER(NU, NU, 2), IGSSIS                           
      LOGICAL AF, GETJAC, SEPATE, SET, ZERO(NU)                         
      DOUBLE PRECISION X(NX), XQ(MQ), WQ(MQ), G(GDIM1, GDIM2), GT(GDIM1,
     1   GDIM2), B(GDIM1, NRHS)                                         
      DOUBLE PRECISION BT(GDIM1, NRHS), TQ(MQ), SBASIS(MQ, K, 2), A1(MQ,
     1   NU, NU), A1T(MQ, NU, NU), A2(MQ, NU, NU)                       
      DOUBLE PRECISION A2T(MQ, NU, NU), A3(MQ, NU, NU), A3T(MQ, NU, NU),
     1   A4(MQ, NU, NU), A4T(MQ, NU, NU), F1(MQ, NU, NRHS)              
      DOUBLE PRECISION F1T(MQ, NU, NRHS), F2(MQ, NU, NRHS), F2T(MQ, NU  
     1   , NRHS), GSBSIS(GSDIM1, 1)                                     
      INTEGER ILR, INT, MAX0, I, J, P                                   
      INTEGER Q, JRHS, GIDX1, GIDX2, ERRATE, ID(2)                      
      INTEGER IQ, NSPLN                                                 
      LOGICAL A12ERO, A40, A1230, A4ZERO                                
      DOUBLE PRECISION T, SCALE, TT                                     
      INTEGER TEMP4                                                     
      LOGICAL TEMP, TEMP1, TEMP2, TEMP3                                 
      DATA ID(1)/0/                                                     
      DATA ID(2)/1/                                                     
      ERRATE = 0                                                        
      NSPLN = NX-K                                                      
      IF (GETJAC) CALL SETD(GDIM1*GDIM2, 0D0, G)                        
C INITIALIZE G.                                                         
      TEMP = GETJAC                                                     
      IF (TEMP) TEMP = SEPATE                                           
      IF (TEMP) CALL SETD(GDIM1*GDIM2, 0D0, GT)                         
C GT TOO.                                                               
      IF (NRHSG .GT. 0) CALL SETD(GDIM1*NRHSG, 0D0, B)                  
C INITIALIZE B.                                                         
      TEMP = SEPATE                                                     
      IF (TEMP) TEMP = NRHSG .GT. 0                                     
      IF (TEMP) CALL SETD(GDIM1*NRHSG, 0D0, BT)                         
C INITIALIZE BT.                                                        
      IF (GETJAC) CALL SETI(2*NU**2, -1, ORDER)                         
C INITIALIZE ORDER.                                                     
C DO INTEGRALS OVER EACH MESH INTERVAL.                                 
      DO  33 INT = K, NSPLN                                             
C/6S                                                                    
C        IF (X(INT) .GT. X(INT+1)) CALL SETERR(                         
C    1      37HDGLSIN - X IS NOT MONOTONE INCREASING, 37, 12, 2)        
C/7S                                                                    
         IF (X(INT) .GT. X(INT+1)) CALL SETERR(                         
     1      'DGLSIN - X IS NOT MONOTONE INCREASING', 37, 12, 2)         
C/                                                                      
         IF (INT+K .GT. NX) GOTO 1                                      
            TEMP4 = INT+K                                               
C/6S                                                                    
C           IF (X(TEMP4) .LE. X(INT)) CALL SETERR(                      
C    1         37HDGLSIN - X IS NOT MONOTONE INCREASING, 37, 12, 2)     
C/7S                                                                    
            IF (X(TEMP4) .LE. X(INT)) CALL SETERR(                      
     1         'DGLSIN - X IS NOT MONOTONE INCREASING', 37, 12, 2)      
C/                                                                      
   1     IF (X(INT) .EQ. X(INT+1)) GOTO  33                             
C GET THE QUADRATURE POINTS ON (X(INT),X(INT+1)).                       
         DO  2 IQ = 1, MQ                                               
            TQ(IQ) = 0.5D0*((X(INT+1)+X(INT))+(X(INT+1)-X(INT))*XQ(IQ)) 
   2        CONTINUE                                                    
         SCALE = 0.5D0*(X(INT+1)-X(INT))                                
         IF (.NOT. SET) GOTO 3                                          
            TEMP4 = INT-K                                               
            CALL MOVEFD(2*MQ*K, GSBSIS(1, TEMP4+1), SBASIS)             
            GOTO  5                                                     
   3        CALL DBSPL1(K, X, NX, TQ, MQ, INT, ID, 2, SBASIS)           
C GET BASIS SPLINES.                                                    
            IF (IGSSIS .LE. 1) GOTO 4                                   
               TEMP4 = INT-K                                            
               CALL MOVEFD(2*MQ*K, SBASIS, GSBSIS(1, TEMP4+1))          
   4        CONTINUE                                                    
C DEFAULT A AND F VALUES.                                               
   5     CALL SETD(4*MQ*NU*(2*NU+NRHS), 0D0, A1)                        
C GET A AND F.                                                          
         IF (.NOT. AF(TQ, MQ, NU, NRHS, NRHSG, GETJAC, SEPATE, A1, A1T  
     1      , A2, A2T, A3, A3T, A4, A4T, F1, F1T, F2, F2T, INT, SBASIS  
     2      , K, X, NX, AF1)) GOTO 6                                    
            D6LSIN = .TRUE.                                             
            RETURN                                                      
C DO THE I-TH ODE.                                                      
   6     DO  32 I = 1, NU                                               
            IF (.NOT. GETJAC) GOTO 18                                   
               A12ERO = .TRUE.                                          
               A4ZERO = .TRUE.                                          
               DO  17 J = 1, NU                                         
                  DO  16 IQ = 1, MQ                                     
                     TEMP = A1(IQ, I, J) .EQ. 0D0                       
                     IF (TEMP) TEMP = A1T(IQ, I, J) .EQ. 0D0            
                     IF (TEMP) TEMP = A2(IQ, I, J) .EQ. 0D0             
                     IF (TEMP) TEMP = A2T(IQ, I, J) .EQ. 0D0            
                     IF (TEMP) TEMP = A3(IQ, I, J) .EQ. 0D0             
                     IF (TEMP) TEMP = A3T(IQ, I, J) .EQ. 0D0            
                     A1230 = TEMP                                       
                     TEMP = A4(IQ, I, J) .EQ. 0D0                       
                     IF (TEMP) TEMP = A4T(IQ, I, J) .EQ. 0D0            
                     A40 = TEMP                                         
                     A12ERO = A12ERO .AND. A1230                        
                     A4ZERO = A4ZERO .AND. A40                          
                     TEMP = INT .EQ. K                                  
                     IF (TEMP) TEMP = IQ .EQ. 1                         
                     IF (TEMP) GOTO 7                                   
                        TEMP1 = INT .EQ. NSPLN                          
                        IF (TEMP1) TEMP1 = IQ .EQ. MQ                   
                        TEMP = TEMP1                                    
   7                 IF (.NOT. TEMP) GOTO 14                            
                        IF (INT .NE. K) GOTO 8                          
                           ILR = 1                                      
C SET ORDER.                                                            
                           GOTO  9                                      
   8                       ILR = 2                                      
   9                    TEMP1 = A1(IQ, I, J) .NE. 0D0                   
                        IF (.NOT. TEMP1) TEMP1 = A1T(IQ, I, J) .NE. 0D0 
                        IF (.NOT. TEMP1) GOTO 10                        
                           ORDER(I, J, ILR) = MAX0(ORDER(I, J, ILR), 2) 
                           GOTO  13                                     
  10                       TEMP2 = A2(IQ, I, J) .NE. 0D0                
                           IF (.NOT. TEMP2) TEMP2 = A2T(IQ, I, J) .NE.  
     1                        0D0                                       
                           IF (.NOT. TEMP2) TEMP2 = A3(IQ, I, J) .NE.   
     1                        0D0                                       
                           IF (.NOT. TEMP2) TEMP2 = A3T(IQ, I, J) .NE.  
     1                        0D0                                       
                           IF (.NOT. TEMP2) GOTO 11                     
                              ORDER(I, J, ILR) = MAX0(ORDER(I, J, ILR)  
     1                           , 1)                                   
                              GOTO  12                                  
  11                          TEMP3 = A4(IQ, I, J) .NE. 0D0             
                              IF (.NOT. TEMP3) TEMP3 = A4T(IQ, I, J)    
     1                            .NE. 0D0                              
                              IF (TEMP3) ORDER(I, J, ILR) = MAX0(ORDER(I
     1                           , J, ILR), 0)                          
  12                    CONTINUE                                        
  13                    TEMP1 = K .EQ. NSPLN                            
                        IF (TEMP1) TEMP1 = I .EQ. NU                    
                        IF (TEMP1) TEMP1 = J .EQ. NU                    
                        IF (TEMP1) TEMP1 = IQ .EQ. MQ                   
                        IF (TEMP1) CALL MOVEFI(NU**2, ORDER, ORDER(1, 1,
     1                     2))                                          
  14                 A1(IQ, I, J) = A1(IQ, I, J)*WQ(IQ)                 
                     A2(IQ, I, J) = A2(IQ, I, J)*WQ(IQ)                 
                     A3(IQ, I, J) = A3(IQ, I, J)*WQ(IQ)                 
                     A4(IQ, I, J) = A4(IQ, I, J)*WQ(IQ)                 
                     IF (.NOT. SEPATE) GOTO 15                          
                        A1T(IQ, I, J) = A1T(IQ, I, J)*WQ(IQ)            
                        A2T(IQ, I, J) = A2T(IQ, I, J)*WQ(IQ)            
                        A3T(IQ, I, J) = A3T(IQ, I, J)*WQ(IQ)            
                        A4T(IQ, I, J) = A4T(IQ, I, J)*WQ(IQ)            
  15                 CONTINUE                                           
  16                 CONTINUE                                           
C END IQ.                                                               
C DOES U(J) NOT APPEAR IN ODE(I)?                                       
                  TEMP = A12ERO                                         
                  IF (TEMP) TEMP = A4ZERO                               
                  ZERO(J) = TEMP                                        
  17              CONTINUE                                              
C END J.                                                                
               TEMP = .NOT. A4ZERO                                      
               IF (TEMP) TEMP = A12ERO                                  
               IF (TEMP) TEMP = MQ .EQ. K-1                             
               IF (TEMP) ERRATE = 1                                     
               TEMP = A12ERO                                            
               IF (TEMP) TEMP = A4ZERO                                  
               IF (TEMP) ERRATE = 2                                     
C MAKE THE I-TH EQUATION'S RESIDUAL                                     
  18        DO  25 P = 1, K                                             
C ORTHOGONAL TO B-SUB-P.                                                
               GIDX1 = I+(INT-K-1+P)*NU                                 
C       GET THE RIGHT-HAND-SIDE B.                                      
               JRHS = 1                                                 
                  GOTO  20                                              
  19              JRHS = JRHS+1                                         
  20              IF (JRHS .GT. NRHSG) GOTO  24                         
                  T = 0                                                 
                  DO  21 IQ = 1, MQ                                     
                     T = T+(F1(IQ, I, JRHS)*SBASIS(IQ, P, 2)-F2(IQ, I,  
     1                  JRHS)*SBASIS(IQ, P, 1))*WQ(IQ)                  
  21                 CONTINUE                                           
                  B(GIDX1, JRHS) = B(GIDX1, JRHS)+SCALE*T               
                  IF (.NOT. SEPATE) GOTO 23                             
                     TT = 0                                             
                     DO  22 IQ = 1, MQ                                  
                        TT = TT+(F1T(IQ, I, JRHS)*SBASIS(IQ, P, 2)-F2T( 
     1                     IQ, I, JRHS)*SBASIS(IQ, P, 1))*WQ(IQ)        
  22                    CONTINUE                                        
                     BT(GIDX1, JRHS) = BT(GIDX1, JRHS)+SCALE*TT         
  23              CONTINUE                                              
                  GOTO  19                                              
  24           CONTINUE                                                 
  25           CONTINUE                                                 
            IF (.NOT. GETJAC) GOTO  32                                  
C GET THE J-TH COMPONENT OF (I,P) RELATION.                             
            DO  31 J = 1, NU                                            
               IF (ZERO(J)) GOTO  31                                    
C MAKE THE I-TH EQUATION'S RESIDUAL                                     
               DO  30 P = 1, K                                          
C ORTHOGONAL TO B-SUB-P.                                                
                  GIDX1 = I+(INT-K-1+P)*NU                              
                  GIDX2 = J-I+(K+1-P)*NU                                
C Q-TH COEFFICIENT OF THE (I,P,J) RELATION.                             
                  DO  29 Q = 1, K                                       
                     T = 0                                              
                     DO  26 IQ = 1, MQ                                  
                        T = T+(A1(IQ, I, J)*SBASIS(IQ, Q, 2)+A2(IQ, I, J
     1                     )*SBASIS(IQ, Q, 1))*SBASIS(IQ, P, 2)+(A3(IQ  
     2                     , I, J)*SBASIS(IQ, Q, 2)+A4(IQ, I, J)*SBASIS(
     3                     IQ, Q, 1))*SBASIS(IQ, P, 1)                  
  26                    CONTINUE                                        
                     G(GIDX1, GIDX2) = G(GIDX1, GIDX2)+T*SCALE          
                     IF (.NOT. SEPATE) GOTO 28                          
                        TT = 0                                          
                        DO  27 IQ = 1, MQ                               
                           TT = TT+(A1T(IQ, I, J)*SBASIS(IQ, Q, 2)+A2T( 
     1                        IQ, I, J)*SBASIS(IQ, Q, 1))*SBASIS(IQ, P  
     2                        , 2)+(A3T(IQ, I, J)*SBASIS(IQ, Q, 2)+A4T( 
     3                        IQ, I, J)*SBASIS(IQ, Q, 1))*SBASIS(IQ, P  
     4                        , 1)                                      
  27                       CONTINUE                                     
                        GT(GIDX1, GIDX2) = GT(GIDX1, GIDX2)+TT*SCALE    
  28                 GIDX2 = GIDX2+NU                                   
  29                 CONTINUE                                           
C END Q.                                                                
  30              CONTINUE                                              
C END P.                                                                
  31           CONTINUE                                                 
C END J.                                                                
  32        CONTINUE                                                    
C END I.                                                                
  33     CONTINUE                                                       
C END INT.                                                              
      TEMP = .NOT. SET                                                  
      IF (TEMP) TEMP = IGSSIS .GT. 1                                    
      IF (TEMP) SET = .TRUE.                                            
C/6S                                                                    
C     IF (ERRATE .EQ. 1) CALL SETERR(                                   
C    1   33HDGLSIN - MQ=K-1 WHEN ORDER(I,.)=0, 33, 14, 1)               
C     IF (ERRATE .EQ. 2) CALL SETERR(26HDGLSIN - ODE(I) IS VACUOUS, 26  
C    1   , 15, 1)                                                       
C/7S                                                                    
      IF (ERRATE .EQ. 1) CALL SETERR(                                   
     1   'DGLSIN - MQ=K-1 WHEN ORDER(I,.)=0', 33, 14, 1)                
      IF (ERRATE .EQ. 2) CALL SETERR('DGLSIN - ODE(I) IS VACUOUS', 26   
     1   , 15, 1)                                                       
C/                                                                      
      D6LSIN = .FALSE.                                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE D6LSSD(ALFA, GAMMA, NU, NRHS, CC, SGAMMA, BC, C        
     1   , G, ED, NDEQS, PIVOT, DIAG)                                   
      INTEGER NRHS, NU, NDEQS                                           
      INTEGER BC(NU), ED(NU), PIVOT(NU)                                 
      DOUBLE PRECISION ALFA(NU, NU), GAMMA(NU, NRHS), CC(NU, NU),       
     1   SGAMMA(NU, NRHS), C(NDEQS, NU), G(NDEQS, NRHS)                 
      DOUBLE PRECISION DIAG(NU)                                         
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, NERROR, IPIVOT, I, J, NERR                        
      INTEGER IS(1000), NDVAR                                           
      REAL RS(1000)                                                     
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C SCRATCH SPACE ALLOCATED - S(D6LSSD) <= NU LONG REAL WORDS +           
C                                        NU INTEGER WORDS.              
      CALL ENTER(1)                                                     
C COPY ALFA AND GAMMA INTO                                              
      DO  3 I = 1, NDEQS                                                
C PROPERLY DIMENSIONED ARRAYS.                                          
         DO  1 J = 1, NU                                                
            TEMP1 = ED(I)                                               
            C(I, J) = ALFA(TEMP1, J)                                    
   1        CONTINUE                                                    
         DO  2 J = 1, NRHS                                              
            TEMP1 = ED(I)                                               
            G(I, J) = GAMMA(TEMP1, J)                                   
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      NDVAR = 0                                                         
C COUNT THE VARIABLES.                                                  
      DO  7 J = 1, NU                                                   
         DO  5 I = 1, NDEQS                                             
            IF (C(I, J) .EQ. 0D0) GOTO 4                                
               NDVAR = NDVAR+1                                          
               GOTO  6                                                  
   4        CONTINUE                                                    
   5        CONTINUE                                                    
   6     CONTINUE                                                       
   7     CONTINUE                                                       
      IF (NDVAR .GE. NDEQS) GOTO 8                                      
C/6S                                                                    
C        CALL SETERR(                                                   
C    1      57HDGLSSB - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED
C    2      , 57, 4, 1)                                                 
C/7S                                                                    
         CALL SETERR(                                                   
     1      'DGLSSB - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED' 
     2      , 57, 4, 1)                                                 
C/                                                                      
         CALL LEAVE                                                     
         RETURN                                                         
C GET THE QR DECOMPOSITION OF C.                                        
   8  CALL DQRD(NDEQS, NU, C, DIAG, PIVOT)                              
      IF (NERROR(NERR) .EQ. 0) GOTO 9                                   
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(37HDGLSSB - SINGULAR DIRICHLET SUBSYSTEM, 37, 6, 1)
C/7S                                                                    
         CALL SETERR('DGLSSB - SINGULAR DIRICHLET SUBSYSTEM', 37, 6, 1) 
C/                                                                      
         CALL LEAVE                                                     
         RETURN                                                         
C FORM G = Q*G.                                                         
   9  CALL DQRQTV(NDEQS, NU, C, DIAG, NRHS, G)                          
C GET A PHONY PIVOT(I) = I ARRAY.                                       
      IPIVOT = ISTKGT(NU, 2)                                            
      DO  10 I = 1, NU                                                  
         TEMP1 = IPIVOT-1+I                                             
         IS(TEMP1) = I                                                  
  10     CONTINUE                                                       
      CALL DQRBS(NDEQS, NDEQS, C, DIAG, IS(IPIVOT), NRHS, G, G)         
      IF (NDEQS .LT. NU) CALL DQRBS(NDEQS, NDEQS, C, DIAG, IS(IPIVOT),  
     1   NU-NDEQS, C(1, NDEQS+1), C(1, NDEQS+1))                        
C PUT THE STANDARD BC'S INTO CC AND SGAMMA.                             
      DO  15 I = 1, NDEQS                                               
         J = NDEQS+1                                                    
            GOTO  12                                                    
  11        J = J+1                                                     
  12        IF (J .GT. NU) GOTO  13                                     
            TEMP1 = PIVOT(I)                                            
            TEMP = PIVOT(J)                                             
            CC(TEMP1, TEMP) = -C(I, J)                                  
            GOTO  11                                                    
  13     DO  14 J = 1, NRHS                                             
            TEMP = PIVOT(I)                                             
            SGAMMA(TEMP, J) = G(I, J)                                   
  14        CONTINUE                                                    
         TEMP = PIVOT(I)                                                
         BC(TEMP) = 0                                                   
  15     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE D6LSSM(ALFA, BETA, GAMMA, NU, NRHS, AA, BB,            
     1   SGAMMA, BC, A, B, G, EM, NMEQS, PIVOT, DIAG)                   
      INTEGER NRHS, NU, NMEQS                                           
      INTEGER BC(NU), EM(NU), PIVOT(NU)                                 
      DOUBLE PRECISION ALFA(NU, NU), BETA(NU, NU), GAMMA(NU, NRHS), AA( 
     1   NU, NU), BB(NU, NU), SGAMMA(NU, NRHS)                          
      DOUBLE PRECISION A(NMEQS, NU), B(NMEQS, NU), G(NMEQS, NRHS), DIAG(
     1   NU)                                                            
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, NERROR, IPIVOT, I, J, NERR                        
      INTEGER IS(1000), NMVAR                                           
      REAL RS(1000)                                                     
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION WS(500)                                          
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C SCRATCH SPACE ALLOCATED - S(D6LSSM) <= NU LONG REAL WORDS +           
C                                        NU INTEGER WORDS.              
      CALL ENTER(1)                                                     
C COPY ALFA, BETA AND GAMMA INTO                                        
      DO  3 I = 1, NMEQS                                                
C PROPERLY DIMENSIONED ARRAYS.                                          
         DO  1 J = 1, NU                                                
            TEMP1 = EM(I)                                               
            A(I, J) = ALFA(TEMP1, J)                                    
            TEMP1 = EM(I)                                               
            B(I, J) = BETA(TEMP1, J)                                    
   1        CONTINUE                                                    
         DO  2 J = 1, NRHS                                              
            TEMP1 = EM(I)                                               
            G(I, J) = GAMMA(TEMP1, J)                                   
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      NMVAR = 0                                                         
C COUNT THE VARIABLES.                                                  
      DO  7 J = 1, NU                                                   
         DO  5 I = 1, NMEQS                                             
            IF (B(I, J) .EQ. 0D0) GOTO 4                                
               NMVAR = NMVAR+1                                          
               GOTO  6                                                  
   4        CONTINUE                                                    
   5        CONTINUE                                                    
   6     CONTINUE                                                       
   7     CONTINUE                                                       
      IF (NMVAR .GE. NMEQS) GOTO 8                                      
C/6S                                                                    
C        CALL SETERR(                                                   
C    1      53HDGLSSB - MIXED BOUNDARY CONDITIONS ARE OVERDETERMINED,   
C    2      53, 3, 1)                                                   
C/7S                                                                    
         CALL SETERR(                                                   
     1      'DGLSSB - MIXED BOUNDARY CONDITIONS ARE OVERDETERMINED',    
     2      53, 3, 1)                                                   
C/                                                                      
         CALL LEAVE                                                     
         RETURN                                                         
C GET THE QR DECOMPOSITION OF B.                                        
   8  CALL DQRD(NMEQS, NU, B, DIAG, PIVOT)                              
      IF (NERROR(NERR) .EQ. 0) GOTO 9                                   
         CALL ERROFF                                                    
C/6S                                                                    
C        CALL SETERR(33HDGLSSB - SINGULAR MIXED SUBSYSTEM, 33, 5, 1)    
C/7S                                                                    
         CALL SETERR('DGLSSB - SINGULAR MIXED SUBSYSTEM', 33, 5, 1)     
C/                                                                      
         CALL LEAVE                                                     
         RETURN                                                         
C FORM G = Q*G.                                                         
   9  CALL DQRQTV(NMEQS, NU, B, DIAG, NRHS, G)                          
C FORM Q*A.                                                             
      CALL DQRQTV(NMEQS, NU, B, DIAG, NU, A)                            
C FORM PHONY PIVOT(I) = I.                                              
      IPIVOT = ISTKGT(NMEQS, 2)                                         
      DO  10 I = 1, NMEQS                                               
         TEMP1 = IPIVOT-1+I                                             
         IS(TEMP1) = I                                                  
  10     CONTINUE                                                       
      CALL DQRBS(NMEQS, NMEQS, B, DIAG, IS(IPIVOT), NRHS, G, G)         
      CALL DQRBS(NMEQS, NMEQS, B, DIAG, IS(IPIVOT), NU, A, A)           
      IF (NMEQS .LT. NU) CALL DQRBS(NMEQS, NMEQS, B, DIAG, IS(IPIVOT),  
     1   NU-NMEQS, B(1, NMEQS+1), B(1, NMEQS+1))                        
C PUT THE STANDARD FORM INTO AA,BB,SGAMMA AND BC.                       
      DO  14 I = 1, NMEQS                                               
         DO  12 J = 1, NU                                               
            TEMP1 = PIVOT(I)                                            
            AA(TEMP1, J) = -A(I, J)                                     
            IF (J .LE. NMEQS) GOTO 11                                   
               TEMP1 = PIVOT(I)                                         
               TEMP = PIVOT(J)                                          
               BB(TEMP1, TEMP) = -B(I, J)                               
  11        CONTINUE                                                    
  12        CONTINUE                                                    
         DO  13 J = 1, NRHS                                             
            TEMP = PIVOT(I)                                             
            SGAMMA(TEMP, J) = G(I, J)                                   
  13        CONTINUE                                                    
         TEMP = PIVOT(I)                                                
         BC(TEMP) = 1                                                   
  14     CONTINUE                                                       
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DQRQTV(M, N, QR, ALFA, NB, B)                          
      INTEGER M, N, NB                                                  
      DOUBLE PRECISION QR(M, N), ALFA(N), B(M, NB)                      
      INTEGER MIN0, I, J, JB                                            
      DOUBLE PRECISION DDOT, GAMMA                                      
      INTEGER TEMP                                                      
C TO FORM Q*B.                                                          
C MNEMONIC - DOUBLE PRECISION QR FACTOR Q TIMES A VECTOR.               
C INPUT -                                                               
C   M    - THE NUMBER OF ROWS IN THE MATRIX.                            
C   N    - THE NUMBER OF COLUMNS IN THE MATRIX.                         
C   QR   - THE QR FACTORIZATION, AS DESCRIBED IN DQRD.                  
C   ALFA - THE DIAGONAL OF R, AS DESCRIBED IN DQRD.                     
C   NB   - THE NUMBER OF RIGHT-HAND-SIDES.                              
C   B    - THE RIGHT-HAND-SIDES.                                        
C OUTPUT -                                                              
C   B - B = Q*B.                                                        
C SCRATCH SPACE ALLOCATED - NONE.                                       
C ERROR STATES -                                                        
C   1 - M.LT.1.                                                         
C   2 - N.LT.1.                                                         
C   3 - NB.LT.1.                                                        
C   4 - ALFA(J)=0.                                                      
C   5 - QR(J,J)=0.                                                      
C ALFA(MIN(M,N)).                                                       
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(15HDQRQTV - M.LT.1, 15, 1, 2)           
C     IF (N .LT. 1) CALL SETERR(15HDQRQTV - N.LT.1, 15, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(16HDQRQTV - NB.LT.1, 16, 3, 2)         
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DQRQTV - M.LT.1', 15, 1, 2)            
      IF (N .LT. 1) CALL SETERR('DQRQTV - N.LT.1', 15, 2, 2)            
      IF (NB .LT. 1) CALL SETERR('DQRQTV - NB.LT.1', 16, 3, 2)          
C/                                                                      
C MULTIPLY ALL THE VECTORS.                                             
      DO  3 JB = 1, NB                                                  
C APPLY THE J-TH TRANSFORMATION.                                        
         TEMP = MIN0(M, N)                                              
         DO  2 J = 1, TEMP                                              
C/6S                                                                    
C           IF (ALFA(J) .EQ. 0D0) CALL SETERR(18HDQRQTV - ALFA(J)=0, 18,
C    1         4, 2)                                                    
C           IF (QR(J, J) .EQ. 0D0) CALL SETERR(18HDQRQTV - QR(J,J)=0,   
C    1         18, 5, 2)                                                
C/7S                                                                    
            IF (ALFA(J) .EQ. 0D0) CALL SETERR('DQRQTV - ALFA(J)=0', 18, 
     1         4, 2)                                                    
            IF (QR(J, J) .EQ. 0D0) CALL SETERR('DQRQTV - QR(J,J)=0',    
     1         18, 5, 2)                                                
C/                                                                      
            GAMMA = DDOT(M-J+1, QR(J, J), 1, B(J, JB), 1)/(ALFA(J)*QR(J,
     1         J))                                                      
            DO  1 I = J, M                                              
               B(I, JB) = B(I, JB)+GAMMA*QR(I, J)                       
   1           CONTINUE                                                 
   2        CONTINUE                                                    
   3     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DBNDBS(N, M, U, NB, B)                                 
      INTEGER M, N, NB                                                  
      DOUBLE PRECISION U(N, M), B(N, NB)                                
      INTEGER I, J, K, L, MIN0                                          
      DOUBLE PRECISION X                                                
      INTEGER TEMP                                                      
C TO SOLVE U*X = B, WHERE U IS AN UPPER TRIANGULAR BANDED MATRIX.       
C MNEMONIC - DOUBLE PRECISION BANDED BACK-SOLVE.                        
C INPUT -                                                               
C   N  - THE ORDER OF THE SYSTEM.                                       
C   M  - THE NUMBER OF NONZERO ENTRIES ON AND ABOVE                     
C        THE DIAGONAL OF U.                                             
C   U  - THE UPPER TRIANGULAR BAND MATRIX.                              
C   NB - THE NUMBER OF RIGHT-HAND-SIDES.                                
C   B  - THE RIGHT-HAND-SIDES.                                          
C OUTPUT -                                                              
C   B - THE SOLUTION VECTORS, X.                                        
C SCRATCH SPACE ALLOCATED - NONE.                                       
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - M.LT.1.                                                         
C   3 - NB.LT.1.                                                        
C   4 - U(I,1)=0.                                                       
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(15HDBNDBS - N.LT.1, 15, 1, 2)           
C     IF (M .LT. 1) CALL SETERR(15HDBNDBS - M.LT.1, 15, 2, 2)           
C     IF (NB .LT. 1) CALL SETERR(16HDBNDBS - NB.LT.1, 16, 3, 2)         
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DBNDBS - N.LT.1', 15, 1, 2)            
      IF (M .LT. 1) CALL SETERR('DBNDBS - M.LT.1', 15, 2, 2)            
      IF (NB .LT. 1) CALL SETERR('DBNDBS - NB.LT.1', 16, 3, 2)          
C/                                                                      
C PROTECT AGAINST AN EXISTING ERROR STATE.                              
      CALL ENTSRC(I, 0)                                                 
      DO  6 J = 1, NB                                                   
         L = 1                                                          
         I = N                                                          
            GOTO  2                                                     
   1        I = I-1                                                     
   2        IF (I .LT. 1) GOTO  5                                       
            X = B(I, J)                                                 
            IF (L .LE. 1) GOTO 4                                        
               DO  3 K = 2, L                                           
                  TEMP = I-1+K                                          
                  X = X-U(I, K)*B(TEMP, J)                              
   3              CONTINUE                                              
C/6S                                                                    
C  4        IF (U(I, 1) .EQ. 0.0D0) CALL SETERR(17HDBNDBS - U(I,1)=0,   
C    1         17, 4, 2)                                                
C/7S                                                                    
   4        IF (U(I, 1) .EQ. 0.0D0) CALL SETERR('DBNDBS - U(I,1)=0',    
     1         17, 4, 2)                                                
C/                                                                      
            B(I, J) = X/U(I, 1)                                         
            L = MIN0(L+1, M)                                            
            GOTO  1                                                     
   5     CONTINUE                                                       
   6     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DQRBS(M, N, QR, ALFA, PIVOT, NB, B, X)                 
      INTEGER M, N, NB                                                  
      INTEGER PIVOT(N)                                                  
      DOUBLE PRECISION QR(M, N), ALFA(N), B(M, NB), X(N, NB)            
      COMMON /CSTAK/ DS                                                 
      DOUBLE PRECISION DS(500)                                          
      INTEGER ISTKGT, I, JB, IS(1000), IZ                               
      REAL RS(1000)                                                     
      LOGICAL LS(1000)                                                  
      DOUBLE PRECISION DDOT, WS(500)                                    
      INTEGER TEMP, TEMP1                                               
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))                   
C TO SOLVE R*X = B.                                                     
C MNEMONIC - DOUBLE PRECISION QR BACKSOLVE.                             
C INPUT -                                                               
C   M     - THE NUMBER OF ROWS IN THE MATRIX.                           
C   N     - THE NUMBER OF COLUMNS IN THE MATRIX.                        
C   QR    - THE QR FACTORIZATION, AS DESCRIBED IN DQRD.                 
C   ALFA  - THE DIAGONAL OF R, AS DESCRIBED IN DQRD.                    
C   PIVOT - THE PIVOTING VECTOR, AS DESCRIBED IN DQRD.                  
C   NB    - THE NUMBER OF RIGHT-HAND-SIDES.                             
C   B     - THE RIGHT-HAND-SIDES.                                       
C OUTPUT -                                                              
C   X - THE SOLUTION VECTORS.                                           
C SCRATCH STORAGE ALLOCATED - N*MU WORDS.                               
C ERROR STATES -                                                        
C   1 - N.LT.1.                                                         
C   2 - M.LT.N.                                                         
C   3 - NB.LT.1.                                                        
C   4 - ALFA(I)=0.                                                      
C   5 - PIVOT(I) NOT ONE OF 1,...,N.                                    
C THE PORT LIBRARY STACK AND ITS ALIASES.                               
C DEFINE Z(J) WS(IZ-1+J)                                                
C CHECK THE INPUT FOR ERRORS.                                           
C/6S                                                                    
C     IF (N .LT. 1) CALL SETERR(14HDQRBS - N.LT.1, 14, 1, 2)            
C     IF (M .LT. N) CALL SETERR(14HDQRBS - M.LT.N, 14, 2, 2)            
C     IF (NB .LT. 1) CALL SETERR(15HDQRBS - NB.LT.1, 15, 3, 2)          
C/7S                                                                    
      IF (N .LT. 1) CALL SETERR('DQRBS - N.LT.1', 14, 1, 2)             
      IF (M .LT. N) CALL SETERR('DQRBS - M.LT.N', 14, 2, 2)             
      IF (NB .LT. 1) CALL SETERR('DQRBS - NB.LT.1', 15, 3, 2)           
C/                                                                      
      DO  1 I = 1, N                                                    
C/6S                                                                    
C        IF (ALFA(I) .EQ. 0D0) CALL SETERR(17HDQRBS - ALFA(I)=0, 17, 4  
C    1      , 2)                                                        
C        IF (PIVOT(I) .LT. 1 .OR. PIVOT(I) .GT. N) CALL SETERR(         
C    1      35HDQRBS - PIVOT(I) NOT ONE OF 1,...,N, 35, 5, 2)           
C/7S                                                                    
         IF (ALFA(I) .EQ. 0D0) CALL SETERR('DQRBS - ALFA(I)=0', 17, 4   
     1      , 2)                                                        
         IF (PIVOT(I) .LT. 1 .OR. PIVOT(I) .GT. N) CALL SETERR(         
     1      'DQRBS - PIVOT(I) NOT ONE OF 1,...,N', 35, 5, 2)            
C/                                                                      
   1     CONTINUE                                                       
      IZ = ISTKGT(N, 4)                                                 
C DO ALL THE RIGHT-HAND-SIDES.                                          
      DO  6 JB = 1, NB                                                  
         TEMP = IZ+N                                                    
         WS(TEMP-1) = B(N, JB)/ALFA(N)                                  
         I = N-1                                                        
            GOTO  3                                                     
   2        I = I-1                                                     
   3        IF (I .LT. 1) GOTO  4                                       
            TEMP = IZ-1+I                                               
            TEMP1 = IZ+I                                                
            WS(TEMP) = (-(DDOT(N-I, QR(I, I+1), M, WS(TEMP1), 1)-B(I,   
     1         JB)))/ALFA(I)                                            
            GOTO  2                                                     
   4     DO  5 I = 1, N                                                 
            TEMP1 = PIVOT(I)                                            
            TEMP = IZ+I                                                 
            X(TEMP1, JB) = WS(TEMP-1)                                   
   5        CONTINUE                                                    
   6     CONTINUE                                                       
      CALL ISTKRL(1)                                                    
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DEXPL(X)                                
      DOUBLE PRECISION X                                                
      DOUBLE PRECISION SMALL, D1MACH, LOGBIG, BIG, LOGALL, DLOG         
      DOUBLE PRECISION DEXP                                             
      DATA SMALL/0D0/                                                   
      DATA BIG/0D0/                                                     
      DATA LOGALL/0D0/                                                  
      DATA LOGBIG/0D0/                                                  
C DEXPL(X) = EXP(X) EXCEPT WHEN IT WILL -                               
C   UNDERFLOW - 0 IS RETURNED,                                          
C     OR                                                                
C   OVERFLOW - +BIG IS RETURNED, WITH AN ERROR STATE.                   
      IF (SMALL .NE. 0D0) GOTO 1                                        
         SMALL = D1MACH(1)                                              
         BIG = D1MACH(2)                                                
         LOGALL = DLOG(SMALL*(50D0*D1MACH(4)+1D0))                      
         LOGBIG = DLOG(BIG*(1D0-50D0*D1MACH(4)))                        
   1  IF (X .GT. LOGALL) GOTO 2                                         
         DEXPL = 0                                                      
         RETURN                                                         
   2     IF (X .LT. LOGBIG) GOTO 3                                      
C/6S                                                                    
C           CALL SETERR(25HDEXPL - EXPONENT OVERFLOW, 25, 1, 2)         
C/7S                                                                    
            CALL SETERR('DEXPL - EXPONENT OVERFLOW', 25, 1, 2)          
C/                                                                      
            DEXPL = BIG                                                 
            RETURN                                                      
   3  CONTINUE                                                          
   4  DEXPL = DEXP(X)                                                   
      RETURN                                                            
      END                                                               
C****END OF ROUTINES NEEDED FOR PORT 3 PDE CHAPTER**********************
