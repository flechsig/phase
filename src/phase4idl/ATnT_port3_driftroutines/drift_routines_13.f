      LOGICAL FUNCTION A3PLNI(K,T,N,                                    
     1                        X,NX,ILEFT,                               
     2                        BIX,                                      
     3                        COL,DM,DP)                                
C                                                                       
C  TO OBTAIN THE INTEGRAL OF ALL NON-ZERO BASIS SPLINES,                
C                                                                       
C           BIX(IX,I) =                                                 
C           INTEGRAL ( T(1) TO X(IX) ) B(ILEFT+I-K) (ZETA) DZETA        
C                                                                       
C  FOR IX=1,...,NX, AND I=1,...,K.                                      
C                                                                       
C  INPUT -                                                              
C                                                                       
C    K      - THE ORDER OF THE B-SPLINES TO BE USED.                    
C             2.LE.K IS ASSUMED.                                        
C    T      - THE B-SPLINE MESH.                                        
C    N      - THE NUMBER OF POINTS IN THE MESH T.                       
C    X      - POINTS OF EVALUATION FOR THE INTEGRALS OF THE B-SPLINES.  
C    NX     - THE NUMBER OF POINTS IN X.                                
C    ILEFT  - MUST HAVE T(ILEFT).LT.T(ILEFT+1) AND                      
C             T(ILEFT) .LE. X(IX) .LE. T(ILEFT+1), IX=1,...,NX.         
C    COL    - COL(L)=INTEGRAL B(ILEFT+L-K) (ZETA) DZETA.                
C    DM     - A SCRATCH ARRAY OF LENGTH K.                              
C    DP     - A SCRATCH ARRAY OF LENGTH K.                              
C                                                                       
C  OUTPUT -                                                             
C                                                                       
C    BIX    - THE INTEGRALS OF THE BASIS SPLINES.                       
C    A3PLNI - A3PLNI = .TRUE. IF SUCCESSFUL.                            
C             A3PLNI = .FALSE. IF T IS NOT MONOTONE INCREASING.         
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES - NONE.                                                 
C                                                                       
      REAL T(N),X(NX),BIX(NX,K),COL(K),DM(K),DP(K)                      
C                                                                       
      REAL FACTOR                                                       
      LOGICAL A3PLNN                                                    
C                                                                       
      A3PLNI=.TRUE.                                                     
C                                                                       
      CALL SETR(NX*K,0.0E0,BIX)                                         
C                                                                       
      DO 30 IX=1,NX                                                     
C                                                                       
         JJ=0                                                           
         DO 20 J=1,K                                                    
            IF (.NOT.A3PLNN(T,N,X(IX),ILEFT,J,                          
     1                      COL,DM,DP,JJ)) GO TO 70                     
C                                                                       
            DO 10 L=1,J                                                 
C                                                                       
               IDX1=MAX0(1,ILEFT+L-J)                                   
               IDX2=MIN0(N,ILEFT+L)                                     
C                                                                       
               IF (T(IDX1).GE.T(IDX2)) GO TO 70                         
C                                                                       
 10            BIX(IX,L)=BIX(IX,L)+((X(IX)-T(IDX1))/(T(IDX2)-T(IDX1)))* 
     1                             COL(L)                               
 20         CONTINUE                                                    
 30      CONTINUE                                                       
C                                                                       
      DO 60 L=1,K                                                       
         IDX1=MIN0(N,ILEFT+L)                                           
         IDX2=MAX0(1,ILEFT+L-K)                                         
         FACTOR=(T(IDX1)-T(IDX2))/FLOAT(K)                              
         DO 50 IX=1,NX                                                  
 50         BIX(IX,L)=BIX(IX,L)*FACTOR                                  
 60      CONTINUE                                                       
C                                                                       
      GO TO 80                                                          
C                                                                       
 70   A3PLNI=.FALSE.                                                    
C                                                                       
 80   RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION A3PLNN(T,N,X,ILEFT,J,COL,DM,DP,JJ)               
C                                                                       
C  TO SET                                                               
C                                                                       
C  COL(I) = N(ILEFT+I-J,J)(X)  I=1,...,J                                
C                                                                       
C  FOR X IN (T(ILEFT),T(ILEFT+1)).                                      
C                                                                       
C  THIS ROUTINE IS MEANT TO BE CALLED SEQUENTIALLY (J=1,...,K)          
C  AND A START FROM SCRATCH IS SIGNALLED BY JJ=0, OTHERWISE             
C  EVERYTHING BUT J IS ASSUMED TO BE AS THE LAST CALL LEFT THEM.        
C                                                                       
C  A3PLNN=.TRUE. IS RETURNED IF SUCCESSFUL.                             
C  A3PLNN=.FALSE. IS RETURNED IF THE MESH T IS NOT MONOTONE INCREASING. 
C                                                                       
      REAL T(N),X,COL(J),DM(J),DP(J)                                    
C                                                                       
      REAL PREV,TEMP                                                    
C                                                                       
      A3PLNN=.TRUE.                                                     
C                                                                       
      IF (JJ.NE.0) GO TO 10                                             
      JJ=1                                                              
      COL(1)=1.0E0                                                      
 10      IF (JJ.GE.J) GO TO 40                                          
         IPJJ=MIN0(ILEFT+JJ,N)                                          
         DP(JJ)=T(IPJJ)-X                                               
C                                                                       
         IF (DP(JJ).LT.0.0E0) GO TO 30                                  
C                                                                       
         IMJJP1=MAX0(ILEFT+1-JJ,1)                                      
         DM(JJ)=X-T(IMJJP1)                                             
C                                                                       
         IF (DM(JJ).LT.0.0E0) GO TO 30                                  
C                                                                       
         PREV=0.0E0                                                     
         JJP1=JJ+1                                                      
         DO 20 I=1,JJ                                                   
            JJP1MI=JJP1-I                                               
            TEMP=DP(I)+DM(JJP1MI)                                       
C                                                                       
            IF (TEMP.EQ.0.0E0) GO TO 30                                 
C                                                                       
            TEMP=COL(I)/TEMP                                            
            COL(I)=PREV+TEMP*DP(I)                                      
 20         PREV=DM(JJP1MI)*TEMP                                        
         COL(JJP1)=PREV                                                 
         JJ=JJP1                                                        
         GO TO 10                                                       
C                                                                       
 30   A3PLNN=.FALSE.                                                    
C                                                                       
 40   RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION D3PLNI(K,T,N,                                    
     1                        X,NX,ILEFT,                               
     2                        BIX,                                      
     3                        COL,DM,DP)                                
C                                                                       
C  TO OBTAIN THE INTEGRAL OF ALL NON-ZERO BASIS SPLINES,                
C                                                                       
C           BIX(IX,I) =                                                 
C           INTEGRAL ( T(1) TO X(IX) ) B(ILEFT+I-K) (ZETA) DZETA        
C                                                                       
C  FOR IX=1,...,NX, AND I=1,...,K.                                      
C                                                                       
C  INPUT -                                                              
C                                                                       
C    K      - THE ORDER OF THE B-SPLINES TO BE USED.                    
C             2.LE.K IS ASSUMED.                                        
C    T      - THE B-SPLINE MESH.                                        
C    N      - THE NUMBER OF POINTS IN THE MESH T.                       
C    X      - POINTS OF EVALUATION FOR THE INTEGRALS OF THE B-SPLINES.  
C    NX     - THE NUMBER OF POINTS IN X.                                
C    ILEFT  - MUST HAVE T(ILEFT).LT.T(ILEFT+1) AND                      
C             T(ILEFT) .LE. X(IX) .LE. T(ILEFT+1), IX=1,...,NX.         
C    COL    - COL(L)=INTEGRAL B(ILEFT+L-K) (ZETA) DZETA.                
C    DM     - A SCRATCH ARRAY OF LENGTH K.                              
C    DP     - A SCRATCH ARRAY OF LENGTH K.                              
C                                                                       
C  OUTPUT -                                                             
C                                                                       
C    BIX    - THE INTEGRALS OF THE BASIS SPLINES.                       
C    D3PLNI - D3PLNI = .TRUE. IF SUCCESSFUL.                            
C             D3PLNI = .FALSE. IF T IS NOT MONOTONE INCREASING.         
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES - NONE.                                                 
C                                                                       
      DOUBLE PRECISION T(N),X(NX),BIX(NX,K),COL(K),DM(K),DP(K)          
C                                                                       
      DOUBLE PRECISION FACTOR                                           
      LOGICAL D3PLNN                                                    
C                                                                       
      D3PLNI=.TRUE.                                                     
C                                                                       
      CALL SETD(NX*K,0.0D0,BIX)                                         
C                                                                       
      DO 30 IX=1,NX                                                     
C                                                                       
         JJ=0                                                           
         DO 20 J=1,K                                                    
            IF (.NOT.D3PLNN(T,N,X(IX),ILEFT,J,                          
     1                      COL,DM,DP,JJ)) GO TO 70                     
C                                                                       
            DO 10 L=1,J                                                 
C                                                                       
               IDX1=MAX0(1,ILEFT+L-J)                                   
               IDX2=MIN0(N,ILEFT+L)                                     
C                                                                       
               IF (T(IDX1).GE.T(IDX2)) GO TO 70                         
C                                                                       
 10            BIX(IX,L)=BIX(IX,L)+((X(IX)-T(IDX1))/(T(IDX2)-T(IDX1)))* 
     1                             COL(L)                               
 20         CONTINUE                                                    
 30      CONTINUE                                                       
C                                                                       
      DO 60 L=1,K                                                       
         IDX1=MIN0(N,ILEFT+L)                                           
         IDX2=MAX0(1,ILEFT+L-K)                                         
         FACTOR=(T(IDX1)-T(IDX2))/FLOAT(K)                              
         DO 50 IX=1,NX                                                  
 50         BIX(IX,L)=BIX(IX,L)*FACTOR                                  
 60      CONTINUE                                                       
C                                                                       
      GO TO 80                                                          
C                                                                       
 70   D3PLNI=.FALSE.                                                    
C                                                                       
 80   RETURN                                                            
C                                                                       
      END                                                               
      LOGICAL FUNCTION D3PLNN(T,N,X,ILEFT,J,COL,DM,DP,JJ)               
C                                                                       
C  TO SET                                                               
C                                                                       
C  COL(I) = N(ILEFT+I-J,J)(X)  I=1,...,J                                
C                                                                       
C  FOR X IN (T(ILEFT),T(ILEFT+1)).                                      
C                                                                       
C  THIS ROUTINE IS MEANT TO BE CALLED SEQUENTIALLY (J=1,...,K)          
C  AND A START FROM SCRATCH IS SIGNALLED BY JJ=0, OTHERWISE             
C  EVERYTHING BUT J IS ASSUMED TO BE AS THE LAST CALL LEFT THEM.        
C                                                                       
C  D3PLNN=.TRUE. IS RETURNED IF SUCCESSFUL.                             
C  D3PLNN=.FALSE. IS RETURNED IF THE MESH T IS NOT MONOTONE INCREASING. 
C                                                                       
      DOUBLE PRECISION T(N),X,COL(J),DM(J),DP(J)                        
C                                                                       
      DOUBLE PRECISION PREV,TEMP                                        
C                                                                       
      D3PLNN=.TRUE.                                                     
C                                                                       
      IF (JJ.NE.0) GO TO 10                                             
      JJ=1                                                              
      COL(1)=1.0D0                                                      
 10      IF (JJ.GE.J) GO TO 40                                          
         IPJJ=MIN0(ILEFT+JJ,N)                                          
         DP(JJ)=T(IPJJ)-X                                               
C                                                                       
         IF (DP(JJ).LT.0.0D0) GO TO 30                                  
C                                                                       
         IMJJP1=MAX0(ILEFT+1-JJ,1)                                      
         DM(JJ)=X-T(IMJJP1)                                             
C                                                                       
         IF (DM(JJ).LT.0.0D0) GO TO 30                                  
C                                                                       
         PREV=0.0D0                                                     
         JJP1=JJ+1                                                      
         DO 20 I=1,JJ                                                   
            JJP1MI=JJP1-I                                               
            TEMP=DP(I)+DM(JJP1MI)                                       
C                                                                       
            IF (TEMP.EQ.0.0D0) GO TO 30                                 
C                                                                       
            TEMP=COL(I)/TEMP                                            
            COL(I)=PREV+TEMP*DP(I)                                      
 20         PREV=DM(JJP1MI)*TEMP                                        
         COL(JJP1)=PREV                                                 
         JJ=JJP1                                                        
         GO TO 10                                                       
C                                                                       
 30   D3PLNN=.FALSE.                                                    
C                                                                       
 40   RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE CSPFE(X,Y,YP,YPP,N,XX,YY,NN)                           
      REAL X(N),Y(N),YP(N),YPP(N),XX(NN),YY(NN)                         
C                                                                       
C INPUTS                                                                
C N DATA PAIRS (X,Y)                                                    
C SPLINE DATA YP,YPP AS DONE BY CSPFIT, FOR EXAMPLE                     
C NN DATA POINTS XX AT WHICH TO INTERPOLATE                             
C                                                                       
C OUTPUT                                                                
C INTERPOLATED VALUE OF SPLINE AT XX POINTS                             
C                                                                       
C/6S                                                                    
C     IF (N.LT.2) CALL SETERR(                                          
C    *      34H CSPFE - MUST HAVE MORE THAN ONE X,34,1,2)               
C/7S                                                                    
      IF (N.LT.2) CALL SETERR(                                          
     *      ' CSPFE - MUST HAVE MORE THAN ONE X',34,1,2)                
C/                                                                      
C                                                                       
      N1=N-1                                                            
      DO 10 J=1,N1                                                      
C/6S                                                                    
C  10    IF (X(J).GE.X(J+1)) CALL SETERR(                               
C    *      44H CSPFE - X ARRAY MUST BE IN INCREASING ORDER,44,2,2)     
C/7S                                                                    
   10    IF (X(J).GE.X(J+1)) CALL SETERR(                               
     *      ' CSPFE - X ARRAY MUST BE IN INCREASING ORDER',44,2,2)      
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NN.LT.1) CALL SETERR(16H CSPFE - NN.LT.1,16,3,2)              
C/7S                                                                    
      IF (NN.LT.1) CALL SETERR(' CSPFE - NN.LT.1',16,3,2)               
C/                                                                      
      DO 20 J=1,NN                                                      
   20    CALL C2SPFT(X,Y,YP,YPP,N,XX(J),YY(J),YYP,YYPP)                 
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CSPDI(X,Y,N,XX,YY,YYP,NN)                              
       REAL X(N),Y(N),B(6),XX(NN),YY(NN),YYP(NN),YYPP                   
       REAL C3SPFT                                                      
C                                                                       
C INPUTS                                                                
C N DATA PAIRS (X,Y)                                                    
C XX IS ARRAY OF VALUES AT WHICH TO INTERPOLATE                         
C NN IS NUMBER OF XX-VALUES                                             
C                                                                       
C OUTPUT                                                                
C CUBIC SPLINE FIT TO Y(X).                                             
C YY(J) IS INTERPOLATED VALUE AT XX(J),J=1,...,NN.                      
C YYP(J) IS FIT VALUE OF DERIVATIVE AT XX(J)                            
C                                                                       
       COMMON/CSTAK/D                                                   
       DOUBLE PRECISION D(500)                                          
       REAL R(1000)                                                     
       EQUIVALENCE (D(1),R(1))                                          
C                                                                       
C CHECK FOR ERRORS IN INPUT PARAMETERS                                  
C                                                                       
C/6S                                                                    
C      IF (NN .LT. 1) CALL SETERR(                                      
C    1    48HCSPDI - MUST DIFFERENTIATE AT ONE POINT AT LEAST,48,1,2)   
C/7S                                                                    
       IF (NN .LT. 1) CALL SETERR(                                      
     1    'CSPDI - MUST DIFFERENTIATE AT ONE POINT AT LEAST',48,1,2)    
C/                                                                      
C                                                                       
C/6S                                                                    
C      IF (N.LT.4)   CALL SETERR(                                       
C    1   35HCSPDI - MUST HAVE AT LEAST FOUR X S,35,2,2)                 
C/7S                                                                    
       IF (N.LT.4)   CALL SETERR(                                       
     1   'CSPDI - MUST HAVE AT LEAST FOUR X S',35,2,2)                  
C/                                                                      
C                                                                       
       N1 = N-1                                                         
       DO 10 J=1,N1                                                     
C/6S                                                                    
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
C    1    43HCSPDI - X ARRAY MUST BE IN INCREASING ORDER,43,3,2)        
C/7S                                                                    
   10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
     1    'CSPDI - X ARRAY MUST BE IN INCREASING ORDER',43,3,2)         
C/                                                                      
C                                                                       
       DO 20 K=1,NN                                                     
C/6S                                                                    
C  20  IF (XX(K) .LT.  X(1) .OR. XX(K) .GT. X(N)) CALL SETERR(          
C    1    34HCSPDI -  POINT NOT INSIDE INTERVAL,34,4,2)                 
C/7S                                                                    
   20  IF (XX(K) .LT.  X(1) .OR. XX(K) .GT. X(N)) CALL SETERR(          
     1    'CSPDI -  POINT NOT INSIDE INTERVAL',34,4,2)                  
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
C    1   46HCSPDI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,46,5,2)      
C/7S                                                                    
       IF (ISTKQU(3) .LT. 2*N) CALL SETERR(                             
     1   'CSPDI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',46,5,2)       
C/                                                                      
       IYP=ISTKGT(2*N,3)                                                
       IYPP=IYP+N                                                       
C                                                                       
C CHECK IF HAVE ENOUGH SPACE FOR CSPFI                                  
C                                                                       
C/6S                                                                    
C      IF (ISTKQU(3) .LT. 4*N) CALL SETERR(                             
C    1   46HCSPDI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,46,5,2)      
C/7S                                                                    
       IF (ISTKQU(3) .LT. 4*N) CALL SETERR(                             
     1   'CSPDI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',46,5,2)       
C/                                                                      
C                                                                       
C FIT THE SPLINE                                                        
C                                                                       
       CALL CSPFI(X,Y,N,B,R(IYP),R(IYPP))                               
C                                                                       
C DO THE DIFFERENTIATION                                                
C                                                                       
       DO 50 J=1,NN                                                     
C                                                                       
   50  CALL C2SPFT(X,Y,R(IYP),R(IYPP),N,XX(J),YY(J),YYP(J),YYPP)        
C                                                                       
       CALL ISTKRL(1)                                                   
       RETURN                                                           
       END                                                              
      SUBROUTINE CSPIN(X,Y,N,XX,YY,NN)                                  
       REAL X(N),Y(N),B(6),XX(NN),YY(NN),YYP,YYPP                       
       REAL C3SPFT                                                      
C                                                                       
C INPUTS                                                                
C N DATA PAIRS (X,Y)                                                    
C XX IS ARRAY OF VALUES AT WHICH TO INTERPOLATE                         
C NN IS NUMBER OF XX-VALUES                                             
C                                                                       
C OUTPUT                                                                
C CUBIC SPLINE FIT TO Y(X).                                             
C YY(J) IS INTERPOLATED VALUE AT XX(J),J=1,...,NN.                      
C                                                                       
       COMMON/CSTAK/D                                                   
       DOUBLE PRECISION D(500)                                          
       REAL R(1000)                                                     
       EQUIVALENCE (D(1),R(1))                                          
C                                                                       
C CHECK FOR ERRORS IN INPUT PARAMETERS                                  
C                                                                       
C/6S                                                                    
C      IF (NN .LT. 1) CALL SETERR(                                      
C    1    46HCSPIN - MUST INTERPOLATE AT ONE POINT AT LEAST,46,1,2)     
C/7S                                                                    
       IF (NN .LT. 1) CALL SETERR(                                      
     1    'CSPIN - MUST INTERPOLATE AT ONE POINT AT LEAST',46,1,2)      
C/                                                                      
C                                                                       
C/6S                                                                    
C      IF (N.LT.4)   CALL SETERR(                                       
C    1   35HCSPIN - MUST HAVE AT LEAST FOUR X S,35,2,2)                 
C/7S                                                                    
       IF (N.LT.4)   CALL SETERR(                                       
     1   'CSPIN - MUST HAVE AT LEAST FOUR X S',35,2,2)                  
C/                                                                      
C                                                                       
       N1 = N-1                                                         
       DO 10 J=1,N1                                                     
C/6S                                                                    
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
C    1    43HCSPIN - X ARRAY MUST BE IN INCREASING ORDER,43,3,2)        
C/7S                                                                    
   10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
     1    'CSPIN - X ARRAY MUST BE IN INCREASING ORDER',43,3,2)         
C/                                                                      
C                                                                       
       DO 20 K=1,NN                                                     
C/6S                                                                    
C  20  IF (XX(K) .LT.  X(1) .OR. XX(K) .GT. X(N)) CALL SETERR(          
C    1    34HCSPIN -  POINT NOT INSIDE INTERVAL,34,4,2)                 
C/7S                                                                    
   20  IF (XX(K) .LT.  X(1) .OR. XX(K) .GT. X(N)) CALL SETERR(          
     1    'CSPIN -  POINT NOT INSIDE INTERVAL',34,4,2)                  
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
C    1   46HCSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,46,5,2)      
C/7S                                                                    
       IF (ISTKQU(3) .LT. 2*N) CALL SETERR(                             
     1   'CSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',46,5,2)       
C/                                                                      
       IYP=ISTKGT(2*N,3)                                                
       IYPP=IYP+N                                                       
C                                                                       
C CHECK IF HAVE ENOUGH SPACE FOR CSPFI                                  
C                                                                       
C/6S                                                                    
C      IF (ISTKQU(3) .LT. 4*N) CALL SETERR(                             
C    1   46HCSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,46,5,2)      
C/7S                                                                    
       IF (ISTKQU(3) .LT. 4*N) CALL SETERR(                             
     1   'CSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',46,5,2)       
C/                                                                      
C                                                                       
C FIT THE SPLINE                                                        
C                                                                       
       CALL CSPFI(X,Y,N,B,R(IYP),R(IYPP))                               
C                                                                       
C DO THE INTERPOLATION                                                  
C                                                                       
       DO 50 J=1,NN                                                     
C                                                                       
   50  CALL C2SPFT(X,Y,R(IYP),R(IYPP),N,XX(J),YY(J),YYP,YYPP)           
C                                                                       
       CALL ISTKRL(1)                                                   
       RETURN                                                           
       END                                                              
      SUBROUTINE CSPFI(X,Y,N,B,YP,YPP)                                  
       DIMENSION X(N),Y(N),B(6),YP(N),YPP(N)                            
C                                                                       
C INPUTS                                                                
C N DATA PAIRS (X,Y)                                                    
C B IS BOUNDARY CONDITION MATRIX                                        
C                                                                       
C      B(1)*YP(1) + B(2)*YPP(1) = B(3) AT X(1)                          
C      B(4)*YP(N) + B(5)*YPP(N) = B(6) AT X(N)                          
C                                                                       
C WHERE                                                                 
C YP AND YPP ARE THE DERIVATIVES OF THE FITTED CUBIC SPLINE             
C XX IS ARRAY OF VALUES AT WHICH TO INTERPOLATE                         
C NN IS NUMBER OF XX-VALUES                                             
C                                                                       
C OUTPUT                                                                
C CUBIC SPLINE FIT TO Y(X).                                             
C PARAMETER VECTORS, YP AND YPP AT DATA POINTS, ARE RETURNED.           
C                                                                       
       COMMON/CSTAK/D                                                   
       DOUBLE PRECISION D(500)                                          
       REAL R(1000)                                                     
       EQUIVALENCE (D(1),R(1))                                          
C                                                                       
C CHECK FOR ERRORS IN INPUT                                             
C                                                                       
C/6S                                                                    
C      IF (N.LT.2)   CALL SETERR(                                       
C    1   33HCSPFI - MUST HAVE MORE THAN ONE X,33,1,2)                   
C/7S                                                                    
       IF (N.LT.2)   CALL SETERR(                                       
     1   'CSPFI - MUST HAVE MORE THAN ONE X',33,1,2)                    
C/                                                                      
C                                                                       
       N1=N-1                                                           
       DO 10 J=1,N1                                                     
C/6S                                                                    
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
C    1    43HCSPFI - X ARRAY MUST BE IN INCREASING ORDER,43,2,2)        
C/7S                                                                    
   10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
     1    'CSPFI - X ARRAY MUST BE IN INCREASING ORDER',43,2,2)         
C/                                                                      
C                                                                       
C/6S                                                                    
C      IF (AMAX1(ABS(B(1)),ABS(B(2))).LE.0.0)  CALL SETERR(             
C    1    41HCSPFI - B(1) AND B(2) CANNOT BOTH BE ZERO,41,3,2)          
C/7S                                                                    
       IF (AMAX1(ABS(B(1)),ABS(B(2))).LE.0.0)  CALL SETERR(             
     1    'CSPFI - B(1) AND B(2) CANNOT BOTH BE ZERO',41,3,2)           
C/                                                                      
C                                                                       
C/6S                                                                    
C      IF (AMAX1(ABS(B(4)),ABS(B(5))).LE.0.0)  CALL SETERR(             
C    1   41HCSPFI - B(4) AND B(5) CANNOT BOTH BE ZERO,41,4,2)           
C/7S                                                                    
       IF (AMAX1(ABS(B(4)),ABS(B(5))).LE.0.0)  CALL SETERR(             
     1   'CSPFI - B(4) AND B(5) CANNOT BOTH BE ZERO',41,4,2)            
C/                                                                      
C                                                                       
C/6S                                                                    
C      IF (ISTKQU(3) .LT. 4*N) CALL SETERR(                             
C    1   46HCSPFI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,46,5,2)      
C/7S                                                                    
       IF (ISTKQU(3) .LT. 4*N) CALL SETERR(                             
     1   'CSPFI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',46,5,2)       
C/                                                                      
C                                                                       
C SET UP THE STACK AND THEN CALL THE ACTUAL FITTING PROCESS             
C                                                                       
       IH=ISTKGT(4*N,3)                                                 
       IU=IH+N                                                          
       IV=IU+N                                                          
       ID=IV+N                                                          
C                                                                       
C TURN RECOVERY ON AND SAVE OLD RECOVERY SWITCH                         
C                                                                       
      CALL ENTSRC (IRSAVE, 1)                                           
C                                                                       
       CALL C1SPFT(X,Y,N,B,YP,YPP,R(IH),R(IU),R(IV),R(ID))              
C                                                                       
      IF (NERROR(NERR) .EQ. 0)  GOTO 30                                 
C                                                                       
C ERROR 1 IN C1SPFT.                                                    
C TURN OFF ERROR STATE AND RESET IT TO                                  
C GIVE A LOCAL ERROR STATE FOR CSPFI.                                   
C                                                                       
      CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR (37HCSPFI - SINGULAR MATRIX. CHECK INPUTS,37,6,2)     
C/7S                                                                    
      CALL SETERR ('CSPFI - SINGULAR MATRIX. CHECK INPUTS',37,6,2)      
C/                                                                      
C                                                                       
C                                                                       
   30  CALL RETSRC (IRSAVE)                                             
      CALL ISTKRL(1)                                                    
C                                                                       
       RETURN                                                           
       END                                                              
      SUBROUTINE C1SPFT(X,Y,N,B,YP,YPP,H,U,V,D)                         
       DIMENSION X(N),Y(N),YP(N),YPP(N),H(N),U(N),V(N),D(N),B(6)        
C                                                                       
C FIT CUBIC SPLINE TO N DATA PAIRS (X,Y).                               
C OUTPUTS ARE ARRAYS YP AND YPP, FITTED VALUES OF FIRST AND             
C SECOND DERIVATIVES AT THE DATA POINTS.                                
C ARRAYS H,U,V,D ARE USED FOR SCRATCH WORK.                             
C                                                                       
C                                                                       
C GENERATE MESH SPACINGS                                                
C                                                                       
       N1=N-1                                                           
       DO 10 J=1,N1                                                     
   10  H(J)=X(J+1)-X(J)                                                 
       DY=(Y(2)-Y(1))/H(1)                                              
C                                                                       
C FILL TRIDIAGONAL MATRIX                                               
C                                                                       
       IF (N1.LT.2)   GOTO 30                                           
       DO 20 J=2,N1                                                     
       U(J)=H(J-1)                                                      
       V(J)=H(J)                                                        
       D(J)=2.0*(H(J-1)+H(J))                                           
       DYP=(Y(J+1)-Y(J))/H(J)                                           
       YPP(J)=6.0*(DYP-DY)                                              
       DY=DYP                                                           
   20  CONTINUE                                                         
C                                                                       
C BOUNDARY CONDITIONS AT X(1)                                           
C                                                                       
   30  U(1)=0.                                                          
       V(1)=-H(1)*B(1)                                                  
       D(1)=6.0*B(2)+2.0*V(1)                                           
       YPP(1)=6.0*(B(3)-B(1)*(Y(2)-Y(1))/H(1))                          
C                                                                       
C BOUNDARY CONDITIONS AT X(N)                                           
C                                                                       
       V(N)=0.                                                          
       U(N)=H(N1)*B(4)                                                  
       D(N)=6.0*B(5)+2.0*U(N)                                           
       YPP(N)=6.0*(B(6)-B(4)*(Y(N)-Y(N1))/H(N1))                        
C                                                                       
C USE GIVENS ROTATIONS TO ELIMINATE U S                                 
C BELOW THE DIAGONAL.                                                   
C THIS PUTS ANOTHER NONZERO                                             
C ROW ABOVE DIAGONAL.                                                   
C                                                                       
       DO 50 J=1,N1                                                     
       ROOT=SQRT(D(J)**2+U(J+1)**2)                                     
       IF (ROOT.GT.0.0)   GOTO 40                                       
C/6S                                                                    
C      CALL SETERR(                                                     
C    1     38HC1SPFT - SINGULAR MATRIX. CHECK INPUTS,38,1,1)            
C/7S                                                                    
       CALL SETERR(                                                     
     1     'C1SPFT - SINGULAR MATRIX. CHECK INPUTS',38,1,1)             
C/                                                                      
       RETURN                                                           
C                                                                       
   40  C=D(J)/ROOT                                                      
       S=-U(J+1)/ROOT                                                   
       D(J)=ROOT                                                        
       TEMP1=C*V(J)-S*D(J+1)                                            
C                                                                       
       TEMP2=S*V(J)+C*D(J+1)                                            
       V(J)=TEMP1                                                       
       D(J+1)=TEMP2                                                     
C                                                                       
C USE U FOR THE NEW NONZERO ROW                                         
C                                                                       
       U(J)=-S*V(J+1)                                                   
       V(J+1)=C*V(J+1)                                                  
       TEMP1=C*YPP(J)-S*YPP(J+1)                                        
       TEMP2=S*YPP(J)+C*YPP(J+1)                                        
       YPP(J)=TEMP1                                                     
       YPP(J+1)=TEMP2                                                   
   50  CONTINUE                                                         
C                                                                       
C BACK SUBSTITUTE                                                       
C                                                                       
       YPP(N)=YPP(N)/D(N)                                               
       YPP(N1)=(YPP(N1)-V(N1)*YPP(N))/D(N1)                             
C                                                                       
       IF (N.LE.2) GOTO 70                                              
C                                                                       
       DO 60 JJ=3,N                                                     
       J=1+N-JJ                                                         
   60  YPP(J)=(YPP(J)-V(J)*YPP(J+1)-U(J)*YPP(J+2))/D(J)                 
C                                                                       
C EVALUATE FIRST DERIVATIVES                                            
C                                                                       
   70  DO 80 J=1,N1                                                     
   80  YP(J)=(Y(J+1)-Y(J))/H(J)-H(J)*(2.0*YPP(J)+YPP(J+1))/6.0          
       YP(N)=YP(N1)+0.5*H(N1)*(YPP(N1)+YPP(N))                          
C                                                                       
       RETURN                                                           
       END                                                              
      SUBROUTINE C2SPFT(X,Y,YP,YPP,N,XX,YY,YYP,YYPP)                    
      DIMENSION X(N),Y(N),YP(N),YPP(N)                                  
C                                                                       
C USING ALREADY-FIT CUBIC SPLINE TO N DATA PAIRS (X,Y)                  
C INTERPOLATE AT XX TO GET VALUE AND FIRST TWO DERIVATIVES.             
C YP AND YPP, FITTED SPLINE PARAMETERS, ARE FIRST TWO DERIVATIVES       
C AT THE DATA POINTS.                                                   
C                                                                       
       N1=N-1                                                           
C                                                                       
C FIND THE CORRECT INTERVAL.  ASSUME X(1) .LE. XX .LE. X(N).            
C                                                                       
       J=INTRVR(N,X,XX)                                                 
C                                                                       
       H=X(J+1)-X(J)                                                    
       XP=(X(J+1)-XX)/H                                                 
       XM=(XX-X(J))/H                                                   
       YY=Y(J)*XP+Y(J+1)*XM -H**2*(YPP(J)*XP*(1.-XP**2)+                
     1             YPP(J+1)*XM*(1.-XM**2))/6.0                          
       YYP=YP(J)+.5*H*(YPP(J)*(1.-XP**2)+YPP(J+1)*XM**2)                
       YYPP=YPP(J)*XP+YPP(J+1)*XM                                       
C                                                                       
       RETURN                                                           
       END                                                              
      SUBROUTINE C2SPQU(X, Y, YP, YPP, N, X1, X2, ANS)                  
      INTEGER N                                                         
      REAL X(N), Y(N), YP(N), YPP(N), X1, X2                            
      REAL ANS                                                          
      INTEGER J, INTRVR, K, JP, M                                       
      REAL H, Y3P, Y3M, XP, XM, S                                       
      REAL SP, SPP, S3                                                  
C                                                                       
C GIVEN CUBIC SPLINE, INTEGRATE FROM X1 TO X2                           
C                                                                       
C/6S                                                                    
C     IF (N .LT. 2) CALL SETERR(17HC2SPQU - N .LT. 2, 17, 1, 2)         
C     IF (X1 .LT. X(1)) CALL SETERR(21HC2SPQU - X1 .LT. X(1), 21, 2, 2) 
C     IF (X2 .GT. X(N)) CALL SETERR(21HC2SPQU - X2 .GT. X(N), 21, 3, 2) 
C     IF (X2 .LT. X1) CALL SETERR(19HC2SPQU - X2 .LT. X1, 19, 4, 2)     
C/7S                                                                    
      IF (N .LT. 2) CALL SETERR('C2SPQU - N .LT. 2', 17, 1, 2)          
      IF (X1 .LT. X(1)) CALL SETERR('C2SPQU - X1 .LT. X(1)', 21, 2, 2)  
      IF (X2 .GT. X(N)) CALL SETERR('C2SPQU - X2 .GT. X(N)', 21, 3, 2)  
      IF (X2 .LT. X1) CALL SETERR('C2SPQU - X2 .LT. X1', 19, 4, 2)      
C/                                                                      
      J = INTRVR(N, X, X1)                                              
      K = INTRVR(N, X, X2)                                              
      IF (X2 .EQ. X(K)) K = K-1                                         
      ANS = 0.0E0                                                       
      IF (J .GE. K) GOTO 2                                              
         Y3M = (YPP(J+1)-YPP(J))/(X(J+1)-X(J))                          
         JP = J+1                                                       
         DO  1 M = JP, K                                                
            Y3P = (YPP(M+1)-YPP(M))/(X(M+1)-X(M))                       
            ANS = ANS+(Y3P-Y3M)*X(M)**4                                 
            Y3M = Y3P                                                   
   1        CONTINUE                                                    
C                                                                       
C LOWER LIMIT                                                           
C                                                                       
   2  M = J+1                                                           
      H = X(M)-X(J)                                                     
      XP = (X(M)-X1)/H                                                  
      XM = (X1-X(J))/H                                                  
      S = Y(J)*XP+Y(M)*XM-H*H*(YPP(J)*XP*(1.0-XP*XP)+YPP(M)*XM*(1.0-XM* 
     1   XM))/6.0                                                       
      SP = (Y(M)-Y(J))/H-H*(YPP(J)*(3.0*XP*XP-1.0)+YPP(M)*(1.0-3.0*XM*  
     1   XM))/6.0                                                       
      SPP = YPP(J)*XP+YPP(M)*XM                                         
      S3 = (YPP(M)-YPP(J))/H                                            
      ANS = ANS-X1*(24.*S-X1*(12.*SP-X1*(4.*SPP-X1*S3)))                
C                                                                       
C UPPER LIMIT                                                           
C                                                                       
      M = K+1                                                           
      H = X(M)-X(K)                                                     
      XP = (X(M)-X2)/H                                                  
      XM = (X2-X(K))/H                                                  
      S = Y(K)*XP+Y(M)*XM-H*H*(YPP(K)*XP*(1.0-XP*XP)+YPP(M)*XM*(1.0-XM* 
     1   XM))/6.0                                                       
      SP = (Y(M)-Y(K))/H-H*(YPP(K)*(3.0*XP*XP-1.0)+YPP(M)*(1.0-3.0*XM*  
     1   XM))/6.0                                                       
      SPP = YPP(K)*XP+YPP(M)*XM                                         
      S3 = (YPP(M)-YPP(K))/H                                            
      ANS = ANS+X2*(24.*S-X2*(12.*SP-X2*(4.*SPP-X2*S3)))                
      ANS = ANS/24.E0                                                   
      RETURN                                                            
      END                                                               
      REAL FUNCTION C3SPFT(X,Y,XX)                                      
C                                                                       
C USE THE LAGRANGE INTERPOLATION FORMULA TO FIT A CUBIC POLY-           
C NOMIAL TO DATA PAIRS (X(I),Y(I),I=1,2,3,4).  EVALUATE AND RE-         
C TURN THE SECOND DERIVATIVE OF THE CUBIC AT POINT XX.                  
C                                                                       
      REAL X(4),Y(4),XX,XSUM,PROD                                       
C                                                                       
      XSUM=0.                                                           
      DO 10 I=1,4                                                       
   10    XSUM=XSUM+X(I)                                                 
      C3SPFT=0.                                                         
      DO 30 I=1,4                                                       
         PROD=1.                                                        
         DO 20 J=1,4                                                    
   20       IF (I .NE. J) PROD=PROD*(X(I)-X(J))                         
   30    C3SPFT=C3SPFT+2.*Y(I)*(3.*XX+X(I)-XSUM)/PROD                   
      RETURN                                                            
      END                                                               
      SUBROUTINE DCSPFE(X,Y,YP,YPP,N,XX,YY,NN)                          
      DOUBLE PRECISION X(N),Y(N),YP(N),YPP(N),XX(NN),YY(NN),YYP,YYPP    
C                                                                       
C INPUTS                                                                
C N DATA PAIRS (X,Y)                                                    
C SPLINE DATA YP,YPP AS DONE BY DCSPFI, FOR EXAMPLE                     
C NN DATA POINTS XX AT WHICH TO INTERPOLATE                             
C                                                                       
C OUTPUT                                                                
C INTERPOLATED VALUE OF SPLINE AT XX POINTS                             
C                                                                       
C/6S                                                                    
C     IF (N.LT.2) CALL SETERR(                                          
C    *      34HDCSPFE - MUST HAVE MORE THAN ONE X,34,1,2)               
C/7S                                                                    
      IF (N.LT.2) CALL SETERR(                                          
     *      'DCSPFE - MUST HAVE MORE THAN ONE X',34,1,2)                
C/                                                                      
C                                                                       
      N1=N-1                                                            
      DO 10 J=1,N1                                                      
C/6S                                                                    
C  10    IF (X(J).GE.X(J+1)) CALL SETERR(                               
C    *      44HDCSPFE - X ARRAY MUST BE IN INCREASING ORDER,44,2,2)     
C/7S                                                                    
   10    IF (X(J).GE.X(J+1)) CALL SETERR(                               
     *      'DCSPFE - X ARRAY MUST BE IN INCREASING ORDER',44,2,2)      
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NN.LT.1) CALL SETERR(16HDCSPFE - NN.LT.1,16,3,2)              
C/7S                                                                    
      IF (NN.LT.1) CALL SETERR('DCSPFE - NN.LT.1',16,3,2)               
C/                                                                      
      DO 20 J=1,NN                                                      
   20    CALL DC2SPF(X,Y,YP,YPP,N,XX(J),YY(J),YYP,YYPP)                 
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DCSPDI(X,Y,N,XX,YY,YYP,NN)                             
       DOUBLE PRECISION X(N),Y(N),B(6),XX(NN),YY(NN),YYP(NN),YYPP       
       DOUBLE PRECISION DC3SPF                                          
C                                                                       
C INPUTS                                                                
C N DATA PAIRS (X,Y)                                                    
C XX IS ARRAY OF VALUES AT WHICH TO INTERPOLATE                         
C NN IS NUMBER OF XX-VALUES                                             
C                                                                       
C OUTPUT                                                                
C CUBIC SPLINE FIT TO Y(X).                                             
C YY(J) IS INTERPOLATED VALUE AT XX(J),J=1,...,NN.                      
C YYP(J) IS FIT VALUE OF DERIVATIVE AT XX(J)                            
C                                                                       
       COMMON/CSTAK/D                                                   
       DOUBLE PRECISION D(500)                                          
C                                                                       
C CHECK FOR ERRORS IN INPUT PARAMETERS                                  
C                                                                       
C/6S                                                                    
C      IF (NN .LT. 1) CALL SETERR(                                      
C    1    49HDCSPDI - MUST DIFFERENTIATE AT ONE POINT AT LEAST,49,1,2)  
C/7S                                                                    
       IF (NN .LT. 1) CALL SETERR(                                      
     1    'DCSPDI - MUST DIFFERENTIATE AT ONE POINT AT LEAST',49,1,2)   
C/                                                                      
C                                                                       
C/6S                                                                    
C      IF (N.LT.4)   CALL SETERR(                                       
C    1   36HDCSPDI - MUST HAVE AT LEAST FOUR X S,36,2,2)                
C/7S                                                                    
       IF (N.LT.4)   CALL SETERR(                                       
     1   'DCSPDI - MUST HAVE AT LEAST FOUR X S',36,2,2)                 
C/                                                                      
C                                                                       
       N1 = N-1                                                         
       DO 10 J=1,N1                                                     
C/6S                                                                    
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
C    1    44HDCSPDI - X ARRAY MUST BE IN INCREASING ORDER,44,3,2)       
C/7S                                                                    
   10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
     1    'DCSPDI - X ARRAY MUST BE IN INCREASING ORDER',44,3,2)        
C/                                                                      
C                                                                       
       DO 20 K=1,NN                                                     
C/6S                                                                    
C  20  IF (XX(K) .LT.  X(1) .OR. XX(K) .GT. X(N)) CALL SETERR(          
C    1    35HDCSPDI -  POINT NOT INSIDE INTERVAL,35,4,2)                
C/7S                                                                    
   20  IF (XX(K) .LT.  X(1) .OR. XX(K) .GT. X(N)) CALL SETERR(          
     1    'DCSPDI -  POINT NOT INSIDE INTERVAL',35,4,2)                 
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
C    1   47HDCSPDI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,47,5,2)     
C/7S                                                                    
       IF (ISTKQU(4) .LT. 2*N) CALL SETERR(                             
     1   'DCSPDI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',47,5,2)      
C/                                                                      
       IYP=ISTKGT(2*N,4)                                                
       IYPP=IYP+N                                                       
C                                                                       
C CHECK IF HAVE ENOUGH SPACE FOR CSPFIT                                 
C                                                                       
C/6S                                                                    
C      IF (ISTKQU(4) .LT. 4*N) CALL SETERR(                             
C    1   47HDCSPDI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,47,5,2)     
C/7S                                                                    
       IF (ISTKQU(4) .LT. 4*N) CALL SETERR(                             
     1   'DCSPDI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',47,5,2)      
C/                                                                      
C                                                                       
C FIT THE SPLINE                                                        
C                                                                       
       CALL DCSPFI(X,Y,N,B,D(IYP),D(IYPP))                              
C                                                                       
C DO THE DIFFERENTIATION                                                
C                                                                       
       DO 50 J=1,NN                                                     
C                                                                       
   50  CALL DC2SPF(X,Y,D(IYP),D(IYPP),N,XX(J),YY(J),YYP(J),YYPP)        
C                                                                       
       CALL ISTKRL(1)                                                   
       RETURN                                                           
       END                                                              
      SUBROUTINE DCSPIN(X,Y,N,XX,YY,NN)                                 
       DOUBLE PRECISION X(N),Y(N),B(6),XX(NN),YY(NN),YYP,YYPP           
       DOUBLE PRECISION DC3SPF                                          
C                                                                       
C INPUTS                                                                
C N DATA PAIRS (X,Y)                                                    
C XX IS ARRAY OF VALUES AT WHICH TO INTERPOLATE                         
C NN IS NUMBER OF XX-VALUES                                             
C                                                                       
C OUTPUT                                                                
C CUBIC SPLINE FIT TO Y(X).                                             
C YY(J) IS INTERPOLATED VALUE AT XX(J),J=1,...,NN.                      
C                                                                       
       COMMON/CSTAK/D                                                   
       DOUBLE PRECISION D(500)                                          
C                                                                       
C CHECK FOR ERRORS IN INPUT PARAMETERS                                  
C                                                                       
C/6S                                                                    
C      IF (NN .LT. 1) CALL SETERR(                                      
C    1    47HDCSPIN - MUST INTERPOLATE AT ONE POINT AT LEAST,47,1,2)    
C/7S                                                                    
       IF (NN .LT. 1) CALL SETERR(                                      
     1    'DCSPIN - MUST INTERPOLATE AT ONE POINT AT LEAST',47,1,2)     
C/                                                                      
C                                                                       
C/6S                                                                    
C      IF (N.LT.4)   CALL SETERR(                                       
C    1   36HDCSPIN - MUST HAVE AT LEAST FOUR X S,36,2,2)                
C/7S                                                                    
       IF (N.LT.4)   CALL SETERR(                                       
     1   'DCSPIN - MUST HAVE AT LEAST FOUR X S',36,2,2)                 
C/                                                                      
C                                                                       
       N1 = N-1                                                         
       DO 10 J=1,N1                                                     
C/6S                                                                    
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
C    1    44HDCSPIN - X ARRAY MUST BE IN INCREASING ORDER,44,3,2)       
C/7S                                                                    
   10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
     1    'DCSPIN - X ARRAY MUST BE IN INCREASING ORDER',44,3,2)        
C/                                                                      
C                                                                       
       DO 20 K=1,NN                                                     
C/6S                                                                    
C  20  IF (XX(K) .LT.  X(1) .OR. XX(K) .GT. X(N)) CALL SETERR(          
C    1    35HDCSPIN -  POINT NOT INSIDE INTERVAL,35,4,2)                
C/7S                                                                    
   20  IF (XX(K) .LT.  X(1) .OR. XX(K) .GT. X(N)) CALL SETERR(          
     1    'DCSPIN -  POINT NOT INSIDE INTERVAL',35,4,2)                 
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
C    1   47HDCSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,47,5,2)     
C/7S                                                                    
       IF (ISTKQU(4) .LT. 2*N) CALL SETERR(                             
     1   'DCSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',47,5,2)      
C/                                                                      
       IYP=ISTKGT(2*N,4)                                                
       IYPP=IYP+N                                                       
C                                                                       
C CHECK IF HAVE ENOUGH SPACE FOR CSPFIT                                 
C                                                                       
C/6S                                                                    
C      IF (ISTKQU(4) .LT. 4*N) CALL SETERR(                             
C    1   47HDCSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,47,5,2)     
C/7S                                                                    
       IF (ISTKQU(4) .LT. 4*N) CALL SETERR(                             
     1   'DCSPIN - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',47,5,2)      
C/                                                                      
C                                                                       
C FIT THE SPLINE                                                        
C                                                                       
       CALL DCSPFI(X,Y,N,B,D(IYP),D(IYPP))                              
C                                                                       
C DO THE INTERPOLATION                                                  
C                                                                       
       DO 50 J=1,NN                                                     
C                                                                       
   50  CALL DC2SPF(X,Y,D(IYP),D(IYPP),N,XX(J),YY(J),YYP,YYPP)           
C                                                                       
       CALL ISTKRL(1)                                                   
       RETURN                                                           
       END                                                              
      SUBROUTINE DCSPFI(X,Y,N,B,YP,YPP)                                 
       DOUBLE PRECISION X(N),Y(N),B(6),YP(N),YPP(N)                     
C                                                                       
C INPUTS                                                                
C N DATA PAIRS (X,Y)                                                    
C B IS BOUNDARY CONDITION MATRIX                                        
C                                                                       
C      B(1)*YP(1) + B(2)*YPP(1) = B(3) AT X(1)                          
C      B(4)*YP(N) + B(5)*YPP(N) = B(6) AT X(N)                          
C                                                                       
C WHERE                                                                 
C YP AND YPP ARE THE DERIVATIVES OF THE FITTED CUBIC SPLINE             
C XX IS ARRAY OF VALUES AT WHICH TO INTERPOLATE                         
C NN IS NUMBER OF XX-VALUES                                             
C                                                                       
C OUTPUT                                                                
C CUBIC SPLINE FIT TO Y(X).                                             
C PARAMETER VECTORS, YP AND YPP AT DATA POINTS, ARE RETURNED.           
C                                                                       
       COMMON/CSTAK/D                                                   
       DOUBLE PRECISION D(500)                                          
C                                                                       
C CHECK FOR ERRORS IN INPUT                                             
C                                                                       
C/6S                                                                    
C      IF (N.LT.2)   CALL SETERR(                                       
C    1   34HDCSPFI - MUST HAVE MORE THAN ONE X,34,1,2)                  
C/7S                                                                    
       IF (N.LT.2)   CALL SETERR(                                       
     1   'DCSPFI - MUST HAVE MORE THAN ONE X',34,1,2)                   
C/                                                                      
C                                                                       
       N1=N-1                                                           
       DO 10 J=1,N1                                                     
C/6S                                                                    
C  10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
C    1    44HDCSPFI - X ARRAY MUST BE IN INCREASING ORDER,44,2,2)       
C/7S                                                                    
   10  IF (X(J).GE.X(J+1))   CALL SETERR(                               
     1    'DCSPFI - X ARRAY MUST BE IN INCREASING ORDER',44,2,2)        
C/                                                                      
C                                                                       
C/6S                                                                    
C      IF (DMAX1(DABS(B(1)),DABS(B(2))).LE.0.0)  CALL SETERR(           
C    1    42HDCSPFI - B(1) AND B(2) CANNOT BOTH BE ZERO,42,3,2)         
C/7S                                                                    
       IF (DMAX1(DABS(B(1)),DABS(B(2))).LE.0.0)  CALL SETERR(           
     1    'DCSPFI - B(1) AND B(2) CANNOT BOTH BE ZERO',42,3,2)          
C/                                                                      
C                                                                       
C/6S                                                                    
C      IF (DMAX1(DABS(B(4)),DABS(B(5))).LE.0.0)  CALL SETERR(           
C    1   42HDCSPFI - B(4) AND B(5) CANNOT BOTH BE ZERO,42,4,2)          
C/7S                                                                    
       IF (DMAX1(DABS(B(4)),DABS(B(5))).LE.0.0)  CALL SETERR(           
     1   'DCSPFI - B(4) AND B(5) CANNOT BOTH BE ZERO',42,4,2)           
C/                                                                      
C                                                                       
C/6S                                                                    
C      IF (ISTKQU(4) .LT. 4*N) CALL SETERR(                             
C    1   47HDCSPFI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN,47,5,2)     
C/7S                                                                    
       IF (ISTKQU(4) .LT. 4*N) CALL SETERR(                             
     1   'DCSPFI - NOT ENOUGH STACK ROOM, USE PORT ISTKIN',47,5,2)      
C/                                                                      
C                                                                       
C SET UP THE STACK AND THEN CALL THE ACTUAL FITTING PROCESS             
C                                                                       
       IH=ISTKGT(4*N,4)                                                 
       IU=IH+N                                                          
       IV=IU+N                                                          
       ID=IV+N                                                          
C                                                                       
C TURN RECOVERY ON AND SAVE OLD RECOVERY SWITCH                         
C                                                                       
       CALL ENTSRC (IRSAVE, 1)                                          
C                                                                       
       CALL DC1SPF(X,Y,N,B,YP,YPP,D(IH),D(IU),D(IV),D(ID))              
C                                                                       
       IF (NERROR(NERR) .EQ. 0)  GOTO 30                                
C                                                                       
C ERROR 1 IN DC1SPF.                                                    
C TURN OFF ERROR STATE AND RESET IT TO                                  
C GIVE A LOCAL ERROR STATE FOR DCSPFI.                                  
C                                                                       
       CALL ERROFF                                                      
C/6S                                                                    
C      CALL SETERR (38HDCSPFI - SINGULAR MATRIX. CHECK INPUTS,38,6,2)   
C/7S                                                                    
       CALL SETERR ('DCSPFI - SINGULAR MATRIX. CHECK INPUTS',38,6,2)    
C/                                                                      
C                                                                       
C                                                                       
   30  CALL RETSRC (IRSAVE)                                             
       CALL ISTKRL(1)                                                   
C                                                                       
       RETURN                                                           
       END                                                              
      SUBROUTINE DC1SPF(X,Y,N,B,YP,YPP,H,U,V,D)                         
       DOUBLE PRECISION X(N),Y(N),YP(N),YPP(N),H(N),U(N),V(N),D(N),B(6) 
       DOUBLE PRECISION ROOT,C,S,TEMP1,TEMP2,DY,DYP,DSQRT               
C                                                                       
C FIT CUBIC SPLINE TO N DATA PAIRS (X,Y).                               
C OUTPUTS ARE ARRAYS YP AND YPP, FITTED VALUES OF FIRST AND             
C SECOND DERIVATIVES AT THE DATA POINTS.                                
C ARRAYS H,U,V,D ARE USED FOR SCRATCH WORK.                             
C                                                                       
C                                                                       
C GENERATE MESH SPACINGS                                                
C                                                                       
       N1=N-1                                                           
       DO 10 J=1,N1                                                     
   10  H(J)=X(J+1)-X(J)                                                 
       DY=(Y(2)-Y(1))/H(1)                                              
C                                                                       
C FILL TRIDIAGONAL MATRIX                                               
C                                                                       
       IF (N1.LT.2)   GOTO 30                                           
       DO 20 J=2,N1                                                     
       U(J)=H(J-1)                                                      
       V(J)=H(J)                                                        
       D(J)=2.0D0*(H(J-1)+H(J))                                         
       DYP=(Y(J+1)-Y(J))/H(J)                                           
       YPP(J)=6.0D0*(DYP-DY)                                            
       DY=DYP                                                           
   20  CONTINUE                                                         
C                                                                       
C BOUNDARY CONDITIONS AT X(1)                                           
C                                                                       
   30  U(1)=0.D0                                                        
       V(1)=-H(1)*B(1)                                                  
       D(1)=6.0D0*B(2)+2.0D0*V(1)                                       
       YPP(1)=6.0D0*(B(3)-B(1)*(Y(2)-Y(1))/H(1))                        
C                                                                       
C BOUNDARY CONDITIONS AT X(N)                                           
C                                                                       
       V(N)=0.D0                                                        
       U(N)=H(N1)*B(4)                                                  
       D(N)=6.0D0*B(5)+2.0D0*U(N)                                       
       YPP(N)=6.D0*(B(6)-B(4)*(Y(N)-Y(N1))/H(N1))                       
C                                                                       
C USE GIVENS ROTATIONS TO ELIMINATE U S                                 
C BELOW THE DIAGONAL.                                                   
C THIS PUTS ANOTHER NONZERO                                             
C ROW ABOVE DIAGONAL.                                                   
C                                                                       
       DO 50 J=1,N1                                                     
       ROOT=DSQRT(D(J)**2+U(J+1)**2)                                    
       IF (ROOT.GT.0.0D0)   GOTO 40                                     
C/6S                                                                    
C      CALL SETERR(                                                     
C    1     38HDC1SPF - SINGULAR MATRIX. CHECK INPUTS,38,1,1)            
C/7S                                                                    
       CALL SETERR(                                                     
     1     'DC1SPF - SINGULAR MATRIX. CHECK INPUTS',38,1,1)             
C/                                                                      
       RETURN                                                           
C                                                                       
   40  C=D(J)/ROOT                                                      
       S=-U(J+1)/ROOT                                                   
       D(J)=ROOT                                                        
       TEMP1=C*V(J)-S*D(J+1)                                            
C                                                                       
       TEMP2=S*V(J)+C*D(J+1)                                            
       V(J)=TEMP1                                                       
       D(J+1)=TEMP2                                                     
C                                                                       
C USE U FOR THE NEW NONZERO ROW                                         
C                                                                       
       U(J)=-S*V(J+1)                                                   
       V(J+1)=C*V(J+1)                                                  
       TEMP1=C*YPP(J)-S*YPP(J+1)                                        
       TEMP2=S*YPP(J)+C*YPP(J+1)                                        
       YPP(J)=TEMP1                                                     
       YPP(J+1)=TEMP2                                                   
   50  CONTINUE                                                         
C                                                                       
C BACK SUBSTITUTE                                                       
C                                                                       
       YPP(N)=YPP(N)/D(N)                                               
       YPP(N1)=(YPP(N1)-V(N1)*YPP(N))/D(N1)                             
C                                                                       
       IF (N.LE.2) GOTO 70                                              
C                                                                       
       DO 60 JJ=3,N                                                     
       J=1+N-JJ                                                         
   60  YPP(J)=(YPP(J)-V(J)*YPP(J+1)-U(J)*YPP(J+2))/D(J)                 
C                                                                       
C EVALUATE FIRST DERIVATIVES                                            
C                                                                       
   70  DO 80 J=1,N1                                                     
   80  YP(J)=(Y(J+1)-Y(J))/H(J)-H(J)*(2.0D0*YPP(J)+YPP(J+1))/6.0D0      
       YP(N)=YP(N1)+0.5D0*H(N1)*(YPP(N1)+YPP(N))                        
C                                                                       
       RETURN                                                           
       END                                                              
      SUBROUTINE DC2SPF(X,Y,YP,YPP,N,XX,YY,YYP,YYPP)                    
      DOUBLE PRECISION X(N),Y(N),YP(N),YPP(N)                           
      DOUBLE PRECISION H,XM,XP,XX,YY,YYP,YYPP                           
C                                                                       
C USING ALREADY-FIT CUBIC SPLINE TO N DATA PAIRS (X,Y)                  
C INTERPOLATE AT XX TO GET VALUE AND FIRST TWO DERIVATIVES.             
C YP AND YPP, FITTED SPLINE PARAMETERS, ARE FIRST TWO DERIVATIVES       
C AT THE DATA POINTS.                                                   
C                                                                       
       N1=N-1                                                           
C                                                                       
C FIND THE CORRECT INTERVAL.  ASSUME X(1) .LE. XX .LE. X(N).            
C                                                                       
       J=INTRVD(N,X,XX)                                                 
C                                                                       
       H=X(J+1)-X(J)                                                    
       XP=(X(J+1)-XX)/H                                                 
       XM=(XX-X(J))/H                                                   
       YY=Y(J)*XP+Y(J+1)*XM -H**2*(YPP(J)*XP*(1.0D0-XP**2)+             
     1            YPP(J+1)*XM*(1.0D0-XM**2))/6.0D0                      
       YYP=YP(J)+.5D0*H*(YPP(J)*(1.D0-XP**2)+YPP(J+1)*XM**2)            
       YYPP=YPP(J)*XP+YPP(J+1)*XM                                       
C                                                                       
       RETURN                                                           
       END                                                              
      SUBROUTINE DC2SPQ(X, Y, YP, YPP, N, X1, X2, ANS)                  
      INTEGER N                                                         
      DOUBLE PRECISION X(N), Y(N), YP(N), YPP(N), X1, X2                
      DOUBLE PRECISION ANS                                              
      INTEGER J, INTRVD, K, JP, M                                       
      DOUBLE PRECISION H, Y3P, Y3M, XP, XM, S                           
      DOUBLE PRECISION SP, SPP, S3                                      
C                                                                       
C GIVEN CUBIC SPLINE, INTEGRATE FROM X1 TO X2                           
C                                                                       
C/6S                                                                    
C     IF (N .LT. 2) CALL SETERR(17HDC2SPQ - N .LT. 2, 17, 1, 2)         
C     IF (X1 .LT. X(1)) CALL SETERR(21HDC2SPQ - X1 .LT. X(1), 21, 2, 2) 
C     IF (X2 .GT. X(N)) CALL SETERR(21HDC2SPQ - X2 .GT. X(N), 21, 3, 2) 
C     IF (X2 .LT. X1) CALL SETERR(19HDC2SPQ - X2 .LT. X1, 19, 4, 2)     
C/7S                                                                    
      IF (N .LT. 2) CALL SETERR('DC2SPQ - N .LT. 2', 17, 1, 2)          
      IF (X1 .LT. X(1)) CALL SETERR('DC2SPQ - X1 .LT. X(1)', 21, 2, 2)  
      IF (X2 .GT. X(N)) CALL SETERR('DC2SPQ - X2 .GT. X(N)', 21, 3, 2)  
      IF (X2 .LT. X1) CALL SETERR('DC2SPQ - X2 .LT. X1', 19, 4, 2)      
C/                                                                      
      J = INTRVD(N, X, X1)                                              
      K = INTRVD(N, X, X2)                                              
      IF (X2 .EQ. X(K)) K = K-1                                         
      ANS = 0.0E0                                                       
      IF (J .GE. K) GOTO 2                                              
         Y3M = (YPP(J+1)-YPP(J))/(X(J+1)-X(J))                          
         JP = J+1                                                       
         DO  1 M = JP, K                                                
            Y3P = (YPP(M+1)-YPP(M))/(X(M+1)-X(M))                       
            ANS = ANS+(Y3P-Y3M)*X(M)**4                                 
            Y3M = Y3P                                                   
   1        CONTINUE                                                    
C                                                                       
C LOWER LIMIT                                                           
C                                                                       
   2  M = J+1                                                           
      H = X(M)-X(J)                                                     
      XP = (X(M)-X1)/H                                                  
      XM = (X1-X(J))/H                                                  
      S = Y(J)*XP+Y(M)*XM-H*H*(YPP(J)*XP*(1.0-XP*XP)+YPP(M)*XM*(1.0-XM* 
     1   XM))/6.0D0                                                     
      SP = (Y(M)-Y(J))/H-H*(YPP(J)*(3.0*XP*XP-1.0)+YPP(M)*(1.0-3.0*XM*  
     1   XM))/6.0D0                                                     
      SPP = YPP(J)*XP+YPP(M)*XM                                         
      S3 = (YPP(M)-YPP(J))/H                                            
      ANS = ANS-X1*(24.*S-X1*(12.*SP-X1*(4.*SPP-X1*S3)))                
C                                                                       
C UPPER LIMIT                                                           
C                                                                       
      M = K+1                                                           
      H = X(M)-X(K)                                                     
      XP = (X(M)-X2)/H                                                  
      XM = (X2-X(K))/H                                                  
      S = Y(K)*XP+Y(M)*XM-H*H*(YPP(K)*XP*(1.0-XP*XP)+YPP(M)*XM*(1.0-XM* 
     1   XM))/6.0D0                                                     
      SP = (Y(M)-Y(K))/H-H*(YPP(K)*(3.0*XP*XP-1.0)+YPP(M)*(1.0-3.0*XM*  
     1   XM))/6.0D0                                                     
      SPP = YPP(K)*XP+YPP(M)*XM                                         
      S3 = (YPP(M)-YPP(K))/H                                            
      ANS = ANS+X2*(24.*S-X2*(12.*SP-X2*(4.*SPP-X2*S3)))                
      ANS = ANS/24.D0                                                   
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DC3SPF(X,Y,XX)                          
C                                                                       
C USE THE LAGRANGE INTERPOLATION FORMULA TO FIT A CUBIC POLY-           
C NOMIAL TO DATA PAIRS (X(I),Y(I),I=1,2,3,4).  EVALUATE AND RE-         
C TURN THE SECOND DERIVATIVE OF THE CUBIC AT POINT XX.                  
C                                                                       
      DOUBLE PRECISION X(4),Y(4),XX,XSUM,PROD                           
C                                                                       
      XSUM=0.D0                                                         
      DO 10 I=1,4                                                       
   10    XSUM=XSUM+X(I)                                                 
      DC3SPF=0.D0                                                       
      DO 30 I=1,4                                                       
         PROD=1.D0                                                      
         DO 20 J=1,4                                                    
   20       IF (I .NE. J) PROD=PROD*(X(I)-X(J))                         
   30    DC3SPF=DC3SPF+2.*Y(I)*(3.*XX+X(I)-XSUM)/PROD                   
      RETURN                                                            
      END                                                               
      SUBROUTINE CLINQ (N,AR,AI,BR,BI,NB,XR,XI)                         
C                                                                       
C SOLUTION OF A SET OF COMPLEX LINEAR EQUATIONS                         
C                                                                       
C  METHOD - QR DECOMPOSITION OF A BY HOUSEHOLDER TRANSFORMATIONS        
C           FOLLOWED BY BACK SUBSTITUTION                               
C                                                                       
C  INPUT                                                                
C                                                                       
C    N    - THE NUMBER OF EQUATIONS                                     
C    AR   - THE REAL PART OF THE MATRIX                                 
C    AI   - THE IMAGINARY PART OF THE MATRIX                            
C    BR   - THE REAL PART OF THE RIGHT-HAND SIDES                       
C    BI   - THE IMAGINARY PART OF THE RIGHT-HAND SIDES                  
C    NB   - THE NUMBER OF RIGHT-HAND SIDES                              
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    AR   - BOTH THE REAL AND IMAGINARY PARTS OF THE                    
C    AI   - MATRIX HAVE BEEN CLOBBERED.                                 
C    BR   - BOTH THE REAL AND IMAGINARY PARTS OF THE                    
C    BI   - MATRIX HAVE BEEN CLOBBERED.                                 
C    XR   - THE REAL PART OF THE SOLUTION VECTORS                       
C    XI   - THE IMAGINARY PART OF THE SOLUTION VECTORS                  
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - N.LT.1                                                         
C    2 - NB.LT.1                                                        
C    3 - A IS SINGULAR (RECOVERABLE)                                    
C                                                                       
      REAL AR(N,N),BR(N,NB),XR(N,NB)                                    
      REAL AI(N,N),BI(N,NB),XI(N,NB)                                    
C                                                                       
C   SAVE AND TURN ON RECOVERY SWITCH                                    
C                                                                       
      CALL ENTSRC(IRSAVE, 1)                                            
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (N.LT.1) CALL SETERR(15H CLINQ - N.LT.1,15,1,2)                
C/7S                                                                    
      IF (N.LT.1) CALL SETERR(' CLINQ - N.LT.1',15,1,2)                 
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NB.LT.1) CALL SETERR(16H CLINQ - NB.LT.1,16,2,2)              
C/7S                                                                    
      IF (NB.LT.1) CALL SETERR(' CLINQ - NB.LT.1',16,2,2)               
C/                                                                      
C                                                                       
C ... CALL CLST2 TO GET THE SOLUTION                                    
C                                                                       
      CALL CLST2(N,N,N,N,AR,AI,BR,BI,NB,XR,XI)                          
C                                                                       
      IF (NERROR(NERR).EQ.0) GO TO 10                                   
C                                                                       
C ... SINGULAR MATRIX                                                   
C                                                                       
      CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(22H CLINQ - A IS SINGULAR,22,3,1)                     
C/7S                                                                    
      CALL SETERR(' CLINQ - A IS SINGULAR',22,3,1)                      
C/                                                                      
C                                                                       
 10   CALL RETSRC(IRSAVE)                                               
      RETURN                                                            
      END                                                               
      SUBROUTINE CLST2(MDIM,NDIM,M,N,AR,AI,BR,BI,NB,XR,XI)              
C                                                                       
C  LEAST SQUARES SOLUTION OF THE COMPLEX LINEAR ALGEBRAIC SYSTEM        
C  OF EQUATIONS A*X=B.                                                  
C                                                                       
C  METHOD - QR DECOMPOSITION OF A BY HOUSEHOLDER TRANSFORMATIONS.       
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN SIZE OF AR, AI, BR AND BI.           
C    NDIM - THE DIMENSIONED COLUMN SIZE OF XR AND XI.                   
C    M    - THE NUMBER OF EQUATIONS.                                    
C    N    - THE NUMBER OF UNKNOWNS. N.LE.M IS ASSUMED.                  
C    AR   - THE REAL PART OF THE MATRIX A.                              
C    AI   - THE IMAGINARY PART OF THE MATRIX A.                         
C    BR   - THE REAL PART OF THE RIGHT HAND SIDES B.                    
C    BI   - THE IMAGINARY PART OF THE RIGHT HAND SIDES B.               
C    NB   - THE NUMBER OF RIGHT HAND SIDES B.                           
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    AR   - BOTH THE REAL AND IMAGINARY PARTS OF THE                    
C    AI   - MATRIX A HAVE BEEN CLOBBERED.                               
C    BR   - BOTH BR AND BI HAVE BEEN CLOBBERED.                         
C           SQRT(SUM(I=N+1,M)(BR(I,J)**2+BI(I,J)**2)) IS THE            
C    BI   - L2 NORM OF THE RESIDUAL IN THE SOLUTION FOR THE             
C           J-TH RIGHT HAND-SIDE, J=1,...,NB.                           
C    XR   - THE REAL PART OF THE SOLUTION VECTOR X.                     
C    XI   - THE IMAGINARY PART OF THE SOLUTION VECTOR X.                
C           X=B IS OK IF NDIM=MDIM.                                     
C                                                                       
C  SCRATCH SPACE ALLOCATED - N REAL WORDS.                              
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - NDIM.LT.N.                                                     
C    3 - N.LT.1.                                                        
C    4 - M.LT.N.                                                        
C    5 - NB.LT.1.                                                       
C    6 - WHEN XR=BR OR XI=BI MUST HAVE NDIM=MDIM.                       
C    7 - A IS RANK-DEFICIENT. (RECOVERABLE)                             
C                                                                       
      REAL AR(MDIM,N),AI(MDIM,N),BR(MDIM,NB),BI(MDIM,NB),               
     1                 XR(NDIM,NB),XI(NDIM,NB)                          
C                                                                       
      COMMON /CSTAK/DS                                                  
      REAL QR,QI                                                        
      REAL WS(1)                                                        
      DOUBLE PRECISION DS(500)                                          
      EQUIVALENCE (DS(1),WS(1))                                         
C                                                                       
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18H CLST2 - MDIM.LT.M,18,1,2)          
C     IF (NDIM.LT.N) CALL SETERR(18H CLST2 - NDIM.LT.N,18,2,2)          
C     IF (N.LT.1) CALL SETERR(15H CLST2 - N.LT.1,15,3,2)                
C     IF (M.LT.N) CALL SETERR(15H CLST2 - M.LT.N,15,4,2)                
C     IF (NB.LT.1) CALL SETERR(16H CLST2 - NB.LT.1,16,5,2)              
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR(' CLST2 - MDIM.LT.M',18,1,2)           
      IF (NDIM.LT.N) CALL SETERR(' CLST2 - NDIM.LT.N',18,2,2)           
      IF (N.LT.1) CALL SETERR(' CLST2 - N.LT.1',15,3,2)                 
      IF (M.LT.N) CALL SETERR(' CLST2 - M.LT.N',15,4,2)                 
      IF (NB.LT.1) CALL SETERR(' CLST2 - NB.LT.1',16,5,2)               
C/                                                                      
C                                                                       
C ... MAKE SURE THAT NDIM=MDIM IF BR=XR OR BI=XI AS ARRAYS.             
C                                                                       
      QR=BR(1,1)                                                        
      QI=BI(1,1)                                                        
      XR(1,1)=+1.0E0                                                    
      XI(1,1)=+1.0E0                                                    
      BR(1,1)=-1.0E0                                                    
      BI(1,1)=-1.0E0                                                    
C/6S                                                                    
C     IF ((XR(1,1).EQ.BR(1,1).OR.XI(1,1).EQ.BI(1,1)) .AND.              
C    1     NDIM.NE.MDIM ) CALL SETERR                                   
C    2   (48H CLST2 - WHEN XR=BR OR XI=BI MUST HAVE NDIM=MDIM,48,6,2)   
C/7S                                                                    
      IF ((XR(1,1).EQ.BR(1,1).OR.XI(1,1).EQ.BI(1,1)) .AND.              
     1     NDIM.NE.MDIM ) CALL SETERR                                   
     2   (' CLST2 - WHEN XR=BR OR XI=BI MUST HAVE NDIM=MDIM',48,6,2)    
C/                                                                      
      BR(1,1)=QR                                                        
      BI(1,1)=QI                                                        
C                                                                       
      ID=ISTKGT(N,3)                                                    
C                                                                       
C ... GET THE QR DECOMPOSITION OF A.                                    
C                                                                       
      CALL CL2SD(MDIM,M,N,AR,AI,WS(ID))                                 
C                                                                       
C ... IF A HAD RANK = N, SOLVE THE EQUATIONS ONE AT A TIME.             
C                                                                       
      IF (NERROR(NERR).NE.0) GO TO 20                                   
C                                                                       
      DO 10 J=1,NB                                                      
 10   CALL CL2SS(MDIM,M,N,AR,AI,BR(1,J),BI(1,J),WS(ID),XR(1,J),XI(1,J)) 
C                                                                       
      GO TO 30                                                          
C                                                                       
C ... HERE FOR A RANK-DEFICIENT MATRIX A.                               
C                                                                       
 20   CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(28H CLST2 - A IS RANK-DEFICIENT,28,7,1)               
C/7S                                                                    
      CALL SETERR(' CLST2 - A IS RANK-DEFICIENT',28,7,1)                
C/                                                                      
C                                                                       
 30   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE CL2SD(MDIM,M,N,AR,AI,D)                                
C                                                                       
C  TO OBTAIN THE Q*U DECOMPOSITION OF A COMPLEX MATRIX A.               
C                                                                       
C  METHOD - HOUSEHOLDER TRANSFORMATIONS.                                
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN SIZE OF AR AND AI.                   
C    M    - THE NUMBER OF ROWS IN A.                                    
C    N    - THE NUMBER OF COLUMNS IN A. N.LE.M IS ASSUMED.              
C    AR   - THE REAL PART OF THE MATRIX A.                              
C    AI   - THE IMAGINARY PART OF THE MATRIX A.                         
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    AR   - THE REAL PART OF THE DECOMPOSED MATRIX A.                   
C    AI   - THE IMAGINARY PART OF THE DECOMPOSED MATRIX A.              
C           LET V(J)(I)=0      FOR I=1,...,J-1                          
C           AND V(J)(I)=A(I,J) FOR I=J,...,M, THEN                      
C                                                                       
C           Q = PRODUCT(J=1,...,N)(I-BETA(J)*V(J)*V(J)-TRANSPOSE)       
C                                                                       
C           WHERE BETA(J)=1/(D(J)*ABS(A(J,J)))                          
C           A(I,J) FOR I.LT.J GIVES THE OFF-DIAGONAL ELEMENTS OF U.     
C    D    - THE DIAGONAL ENTRIES OF U ARE GIVEN BY                      
C           U(I,I)=-D(I)*A(I,I)/CABS(A(I,I)), FOR I=1,...,N.            
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - N.LT.1.                                                        
C    3 - M.LT.N.                                                        
C    4 - A IS RANK-DEFICIENT. (RECOVERABLE)                             
C                                                                       
C  P.A. BUSINGER, NUM. MATH. 7, 269-276(1965).                          
C                                                                       
      REAL AR(MDIM,N),AI(MDIM,N),D(N)                                   
C                                                                       
      REAL QR,QI,Z,W                                                    
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18H CL2SD - MDIM.LT.M,18,1,2)          
C     IF (N.LT.1) CALL SETERR(15H CL2SD - N.LT.1,15,2,2)                
C     IF (M.LT.N) CALL SETERR(15H CL2SD - M.LT.N,15,3,2)                
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR(' CL2SD - MDIM.LT.M',18,1,2)           
      IF (N.LT.1) CALL SETERR(' CL2SD - N.LT.1',15,2,2)                 
      IF (M.LT.N) CALL SETERR(' CL2SD - M.LT.N',15,3,2)                 
C/                                                                      
C                                                                       
      DO 60 L=1,N                                                       
         Z=0.E0                                                         
         DO 10 I=L,M                                                    
   10       Z=Z+AR(I,L)**2+AI(I,L)**2                                   
         Z=SQRT(Z)                                                      
         IF (Z.EQ.0.0E0) GO TO 70                                       
         D(L)=Z                                                         
         W=SQRT(AR(L,L)**2+AI(L,L)**2)                                  
         QR=1.E0                                                        
         QI=0.E0                                                        
         IF(W.EQ.0.E0)GOTO 20                                           
         QR=AR(L,L)/W                                                   
         QI=AI(L,L)/W                                                   
   20    AR(L,L)=QR*(Z+W)                                               
         AI(L,L)=QI*(Z+W)                                               
         IF (L.EQ.N) GO TO 60                                           
         LP1=L+1                                                        
         DO 50 J=LP1,N                                                  
            QR=0.E0                                                     
            QI=0.E0                                                     
            DO 30 I=L,M                                                 
               QR=QR+AR(I,L)*AR(I,J)+AI(I,L)*AI(I,J)                    
   30          QI=QI+AR(I,L)*AI(I,J)-AI(I,L)*AR(I,J)                    
            QR=QR/(Z*(Z+W))                                             
            QI=QI/(Z*(Z+W))                                             
            DO 40 I=L,M                                                 
               AR(I,J)=AR(I,J)-QR*AR(I,L)+QI*AI(I,L)                    
   40          AI(I,J)=AI(I,J)-QR*AI(I,L)-QI*AR(I,L)                    
   50       CONTINUE                                                    
   60    CONTINUE                                                       
      GO TO 80                                                          
C                                                                       
C ... HERE FOR A RANK-DEFICIENT MATRIX A.                               
C                                                                       
C/6S                                                                    
C  70 CALL SETERR(28H CL2SD - A IS RANK-DEFICIENT,28,4,1)               
C/7S                                                                    
   70 CALL SETERR(' CL2SD - A IS RANK-DEFICIENT',28,4,1)                
C/                                                                      
C                                                                       
   80 RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE CL2SS(MDIM,M,N,AR,AI,BR,BI,D,XR,XI)                    
C                                                                       
C  LEAST SQUARES SOLUTION OF THE COMPLEX LINEAR SYSTEM OF ALGEBRAIC     
C  EQUATIONS A*X=B, WHERE A HAS BEEN FACTORED BY A CALL TO CL2SD.       
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN SIZE OF AR, AI, BR AND BI.           
C    M    - THE NUMBER OF EQUATIONS.                                    
C    N    - THE NUMBER OF UNKNOWNS.                                     
C    AR   - THE REAL PART OF THE DECOMPOSED MATRIX A.                   
C    AI   - THE IMAGINARY PART OF THE DECOMPOSED MATRIX A.              
C    BR   - THE REAL PART OF THE RIGHT HAND SIDE B.                     
C    BI   - THE IMAGINARY PART OF THE RIGHT HAND SIDE B.                
C    D    - AS GIVEN BY CL2SD.                                          
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    BR   - BOTH BR AND BI HAVE BEEN CLOBBERED.                         
C           SQRT(SUM(I=N+1,M)(BR(I)**2+BI(I)**2)) IS THE                
C    BI   - L2 NORM OF THE RESIDUAL IN THE SOLUTION OF THE EQUATIONS.   
C    XR   - THE REAL PART OF THE SOLUTION VECTOR X.                     
C    XI   - THE IMAGINARY PART OF THE SOLUTION VECTOR X.                
C           X=B IS OK.                                                  
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - N.LT.1.                                                        
C    3 - M.LT.N.                                                        
C    4 - D(L).LE.0.                                                     
C    5 - A(L,L)=0.                                                      
C                                                                       
      REAL AR(MDIM,N),AI(MDIM,N),BR(M),BI(M),D(N),                      
     1                 XR(N),XI(N)                                      
C                                                                       
      REAL Z,ZPW,QR,QI                                                  
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18H CL2SS - MDIM.LT.M,18,1,2)          
C     IF (N.LT.1) CALL SETERR(15H CL2SS - N.LT.1,15,2,2)                
C     IF (M.LT.N) CALL SETERR(15H CL2SS - M.LT.N,15,3,2)                
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR(' CL2SS - MDIM.LT.M',18,1,2)           
      IF (N.LT.1) CALL SETERR(' CL2SS - N.LT.1',15,2,2)                 
      IF (M.LT.N) CALL SETERR(' CL2SS - M.LT.N',15,3,2)                 
C/                                                                      
C                                                                       
C ... APPLY Q-STAR TO THE RIGHT-HAND-SIDE B.                            
C                                                                       
      DO 30 L=1,N                                                       
         Z=D(L)                                                         
C/6S                                                                    
C        IF (Z.LE.0.0E0) CALL SETERR(18H CL2SS - D(L).LE.0,18,4,2)      
C/7S                                                                    
         IF (Z.LE.0.0E0) CALL SETERR(' CL2SS - D(L).LE.0',18,4,2)       
C/                                                                      
         ZPW=SQRT(AR(L,L)**2+AI(L,L)**2)                                
C/6S                                                                    
C        IF (ZPW.EQ.0.0E0) CALL SETERR(17H CL2SS - A(L,L)=0,17,5,2)     
C/7S                                                                    
         IF (ZPW.EQ.0.0E0) CALL SETERR(' CL2SS - A(L,L)=0',17,5,2)      
C/                                                                      
         QR=0.E0                                                        
         QI=0.E0                                                        
         DO 10 I=L,M                                                    
            QR=QR+AR(I,L)*BR(I)+AI(I,L)*BI(I)                           
   10       QI=QI+AR(I,L)*BI(I)-AI(I,L)*BR(I)                           
         QR=QR/(Z*ZPW)                                                  
         QI=QI/(Z*ZPW)                                                  
         DO 20 I=L,M                                                    
            BR(I)=BR(I)-QR*AR(I,L)+QI*AI(I,L)                           
   20       BI(I)=BI(I)-QR*AI(I,L)-QI*AR(I,L)                           
         XR(L)=BR(L)                                                    
   30    XI(L)=BI(L)                                                    
C                                                                       
C ... BACK-SOLVE THE UPPER-TRIANGULAR SYSTEM U*X=(Q-STAR)*B.            
C                                                                       
      DO 60 II=1,N                                                      
         I=N+1-II                                                       
         ZPW=SQRT(AR(I,I)**2+AI(I,I)**2)                                
         QR=XR(I)                                                       
         QI=XI(I)                                                       
         IP1=I+1                                                        
         IF(IP1.GT.N)GOTO 50                                            
         DO 40 J=IP1,N                                                  
            QR=QR-AR(I,J)*XR(J)+AI(I,J)*XI(J)                           
   40       QI=QI-AR(I,J)*XI(J)-AI(I,J)*XR(J)                           
   50    XR(I)=-( QR*AR(I,I)+QI*AI(I,I))/(ZPW*D(I))                     
   60    XI(I)=-(-QR*AI(I,I)+QI*AR(I,I))/(ZPW*D(I))                     
      RETURN                                                            
      END                                                               
      SUBROUTINE DCLINQ (N,AR,AI,BR,BI,NB,XR,XI)                        
C                                                                       
C SOLUTION OF A SET OF COMPLEX LINEAR EQUATIONS                         
C                                                                       
C  METHOD - QR DECOMPOSITION OF A BY HOUSEHOLDER TRANSFORMATIONS        
C           FOLLOWED BY BACK SUBSTITUTION                               
C                                                                       
C  INPUT                                                                
C                                                                       
C    N    - THE NUMBER OF EQUATIONS                                     
C    AR   - THE REAL PART OF THE MATRIX                                 
C    AI   - THE IMAGINARY PART OF THE MATRIX                            
C    BR   - THE REAL PART OF THE RIGHT-HAND SIDES                       
C    BI   - THE IMAGINARY PART OF THE RIGHT-HAND SIDES                  
C    NB   - THE NUMBER OF RIGHT-HAND SIDES                              
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    AR   - BOTH THE REAL AND IMAGINARY PARTS OF THE                    
C    AI   - MATRIX HAVE BEEN CLOBBERED.                                 
C    BR   - BOTH THE REAL AND IMAGINARY PARTS OF THE                    
C    BI   - MATRIX HAVE BEEN CLOBBERED.                                 
C    XR   - THE REAL PART OF THE SOLUTION VECTORS                       
C    XI   - THE IMAGINARY PART OF THE SOLUTION VECTORS                  
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - N.LT.1                                                         
C    2 - NB.LT.1                                                        
C    3 - A IS SINGULAR (RECOVERABLE)                                    
C                                                                       
      DOUBLE PRECISION AR(N,N),BR(N,NB),XR(N,NB)                        
      DOUBLE PRECISION AI(N,N),BI(N,NB),XI(N,NB)                        
C                                                                       
C   SAVE AND TURN ON RECOVERY SWITCH                                    
C                                                                       
      CALL ENTSRC(IRSAVE, 1)                                            
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (N.LT.1) CALL SETERR(15HDCLINQ - N.LT.1,15,1,2)                
C/7S                                                                    
      IF (N.LT.1) CALL SETERR('DCLINQ - N.LT.1',15,1,2)                 
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NB.LT.1) CALL SETERR(16HDCLINQ - NB.LT.1,16,2,2)              
C/7S                                                                    
      IF (NB.LT.1) CALL SETERR('DCLINQ - NB.LT.1',16,2,2)               
C/                                                                      
C                                                                       
C ... CALL DCLST2 TO GET THE SOLUTION                                   
C                                                                       
      CALL DCLST2(N,N,N,N,AR,AI,BR,BI,NB,XR,XI)                         
C                                                                       
      IF (NERROR(NERR).EQ.0) GO TO 10                                   
C                                                                       
C ... SINGULAR MATRIX                                                   
C                                                                       
      CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(22HDCLINQ - A IS SINGULAR,22,3,1)                     
C/7S                                                                    
      CALL SETERR('DCLINQ - A IS SINGULAR',22,3,1)                      
C/                                                                      
C                                                                       
 10   CALL RETSRC(IRSAVE)                                               
      RETURN                                                            
      END                                                               
      SUBROUTINE DCLST2(MDIM,NDIM,M,N,AR,AI,BR,BI,NB,XR,XI)             
C                                                                       
C  LEAST SQUARES SOLUTION OF THE COMPLEX LINEAR ALGEBRAIC SYSTEM        
C  OF EQUATIONS A*X=B.                                                  
C                                                                       
C  METHOD - QR DECOMPOSITION OF A BY HOUSEHOLDER TRANSFORMATIONS.       
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN SIZE OF AR, AI, BR AND BI.           
C    NDIM - THE DIMENSIONED COLUMN SIZE OF XR AND XI.                   
C    M    - THE NUMBER OF EQUATIONS.                                    
C    N    - THE NUMBER OF UNKNOWNS. N.LE.M IS ASSUMED.                  
C    AR   - THE REAL PART OF THE MATRIX A.                              
C    AI   - THE IMAGINARY PART OF THE MATRIX A.                         
C    BR   - THE REAL PART OF THE RIGHT HAND SIDES B.                    
C    BI   - THE IMAGINARY PART OF THE RIGHT HAND SIDES B.               
C    NB   - THE NUMBER OF RIGHT HAND SIDES B.                           
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    AR   - BOTH THE REAL AND IMAGINARY PARTS OF THE                    
C    AI   - MATRIX A HAVE BEEN CLOBBERED.                               
C    BR   - BOTH BR AND BI HAVE BEEN CLOBBERED.                         
C           SQRT(SUM(I=N+1,M)(BR(I,J)**2+BI(I,J)**2)) IS THE            
C    BI   - L2 NORM OF THE RESIDUAL IN THE SOLUTION FOR THE             
C           J-TH RIGHT HAND-SIDE, J=1,...,NB.                           
C    XR   - THE REAL PART OF THE SOLUTION VECTOR X.                     
C    XI   - THE IMAGINARY PART OF THE SOLUTION VECTOR X.                
C           X=B IS OK IF NDIM=MDIM.                                     
C                                                                       
C  SCRATCH SPACE ALLOCATED - N DOUBLE PRECISION WORDS.                  
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - NDIM.LT.N.                                                     
C    3 - N.LT.1.                                                        
C    4 - M.LT.N.                                                        
C    5 - NB.LT.1.                                                       
C    6 - WHEN XR=BR OR XI=BI MUST HAVE NDIM=MDIM.                       
C    7 - A IS RANK-DEFICIENT. (RECOVERABLE)                             
C                                                                       
      DOUBLE PRECISION AR(MDIM,N),AI(MDIM,N),BR(MDIM,NB),BI(MDIM,NB),   
     1                 XR(NDIM,NB),XI(NDIM,NB)                          
C                                                                       
      COMMON /CSTAK/S                                                   
      DOUBLE PRECISION S(500)                                           
      DOUBLE PRECISION QR,QI                                            
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18HDCLST2 - MDIM.LT.M,18,1,2)          
C     IF (NDIM.LT.N) CALL SETERR(18HDCLST2 - NDIM.LT.N,18,2,2)          
C     IF (N.LT.1) CALL SETERR(15HDCLST2 - N.LT.1,15,3,2)                
C     IF (M.LT.N) CALL SETERR(15HDCLST2 - M.LT.N,15,4,2)                
C     IF (NB.LT.1) CALL SETERR(16HDCLST2 - NB.LT.1,16,5,2)              
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR('DCLST2 - MDIM.LT.M',18,1,2)           
      IF (NDIM.LT.N) CALL SETERR('DCLST2 - NDIM.LT.N',18,2,2)           
      IF (N.LT.1) CALL SETERR('DCLST2 - N.LT.1',15,3,2)                 
      IF (M.LT.N) CALL SETERR('DCLST2 - M.LT.N',15,4,2)                 
      IF (NB.LT.1) CALL SETERR('DCLST2 - NB.LT.1',16,5,2)               
C/                                                                      
C                                                                       
C ... MAKE SURE THAT NDIM=MDIM IF BR=XR OR BI=XI AS ARRAYS.             
C                                                                       
      QR=BR(1,1)                                                        
      QI=BI(1,1)                                                        
      XR(1,1)=+1.0D0                                                    
      XI(1,1)=+1.0D0                                                    
      BR(1,1)=-1.0D0                                                    
      BI(1,1)=-1.0D0                                                    
C/6S                                                                    
C     IF ((XR(1,1).EQ.BR(1,1).OR.XI(1,1).EQ.BI(1,1)) .AND.              
C    1     NDIM.NE.MDIM ) CALL SETERR                                   
C    2   (48HDCLST2 - WHEN XR=BR OR XI=BI MUST HAVE NDIM=MDIM,48,6,2)   
C/7S                                                                    
      IF ((XR(1,1).EQ.BR(1,1).OR.XI(1,1).EQ.BI(1,1)) .AND.              
     1     NDIM.NE.MDIM ) CALL SETERR                                   
     2   ('DCLST2 - WHEN XR=BR OR XI=BI MUST HAVE NDIM=MDIM',48,6,2)    
C/                                                                      
      BR(1,1)=QR                                                        
      BI(1,1)=QI                                                        
C                                                                       
      ID=ISTKGT(N,4)                                                    
C                                                                       
C ... GET THE QR DECOMPOSITION OF A.                                    
C                                                                       
      CALL DCL2SD(MDIM,M,N,AR,AI,S(ID))                                 
C                                                                       
C ... IF A HAD RANK = N, SOLVE THE EQUATIONS ONE AT A TIME.             
C                                                                       
      IF (NERROR(NERR).NE.0) GO TO 20                                   
C                                                                       
      DO 10 J=1,NB                                                      
 10   CALL DCL2SS(MDIM,M,N,AR,AI,BR(1,J),BI(1,J),S(ID),XR(1,J),XI(1,J)) 
C                                                                       
      GO TO 30                                                          
C                                                                       
C ... HERE FOR A RANK-DEFICIENT MATRIX A.                               
C                                                                       
 20   CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(28HDCLST2 - A IS RANK-DEFICIENT,28,7,1)               
C/7S                                                                    
      CALL SETERR('DCLST2 - A IS RANK-DEFICIENT',28,7,1)                
C/                                                                      
C                                                                       
 30   CALL LEAVE                                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DCL2SD(MDIM,M,N,AR,AI,D)                               
C                                                                       
C  TO OBTAIN THE Q*U DECOMPOSITION OF A COMPLEX MATRIX A.               
C                                                                       
C  METHOD - HOUSEHOLDER TRANSFORMATIONS.                                
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN SIZE OF AR AND AI.                   
C    M    - THE NUMBER OF ROWS IN A.                                    
C    N    - THE NUMBER OF COLUMNS IN A. N.LE.M IS ASSUMED.              
C    AR   - THE REAL PART OF THE MATRIX A.                              
C    AI   - THE IMAGINARY PART OF THE MATRIX A.                         
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    AR   - THE REAL PART OF THE DECOMPOSED MATRIX A.                   
C    AI   - THE IMAGINARY PART OF THE DECOMPOSED MATRIX A.              
C           LET V(J)(I)=0      FOR I=1,...,J-1                          
C           AND V(J)(I)=A(I,J) FOR I=J,...,M, THEN                      
C                                                                       
C           Q = PRODUCT(J=1,...,N)(I-BETA(J)*V(J)*V(J)-TRANSPOSE)       
C                                                                       
C           WHERE BETA(J)=1/(D(J)*ABS(A(J,J)))                          
C           A(I,J) FOR I.LT.J GIVES THE OFF-DIAGONAL ELEMENTS OF U.     
C    D    - THE DIAGONAL ENTRIES OF U ARE GIVEN BY                      
C           U(I,I)=-D(I)*A(I,I)/CABS(A(I,I)), FOR I=1,...,N.            
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - N.LT.1.                                                        
C    3 - M.LT.N.                                                        
C    4 - A IS RANK-DEFICIENT. (RECOVERABLE)                             
C                                                                       
C  P.A. BUSINGER, NUM. MATH. 7, 269-276(1965).                          
C                                                                       
      DOUBLE PRECISION AR(MDIM,N),AI(MDIM,N),D(N)                       
C                                                                       
      DOUBLE PRECISION QR,QI,Z,W,DSQRT                                  
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18HDCL2SD - MDIM.LT.M,18,1,2)          
C     IF (N.LT.1) CALL SETERR(15HDCL2SD - N.LT.1,15,2,2)                
C     IF (M.LT.N) CALL SETERR(15HDCL2SD - M.LT.N,15,3,2)                
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR('DCL2SD - MDIM.LT.M',18,1,2)           
      IF (N.LT.1) CALL SETERR('DCL2SD - N.LT.1',15,2,2)                 
      IF (M.LT.N) CALL SETERR('DCL2SD - M.LT.N',15,3,2)                 
C/                                                                      
C                                                                       
      DO 60 L=1,N                                                       
         Z=0.D0                                                         
         DO 10 I=L,M                                                    
   10       Z=Z+AR(I,L)**2+AI(I,L)**2                                   
         Z=DSQRT(Z)                                                     
         IF (Z.EQ.0.0D0) GO TO 70                                       
         D(L)=Z                                                         
         W=DSQRT(AR(L,L)**2+AI(L,L)**2)                                 
         QR=1.D0                                                        
         QI=0.D0                                                        
         IF(W.EQ.0.D0)GOTO 20                                           
         QR=AR(L,L)/W                                                   
         QI=AI(L,L)/W                                                   
   20    AR(L,L)=QR*(Z+W)                                               
         AI(L,L)=QI*(Z+W)                                               
         IF (L.EQ.N) GO TO 60                                           
         LP1=L+1                                                        
         DO 50 J=LP1,N                                                  
            QR=0.D0                                                     
            QI=0.D0                                                     
            DO 30 I=L,M                                                 
               QR=QR+AR(I,L)*AR(I,J)+AI(I,L)*AI(I,J)                    
   30          QI=QI+AR(I,L)*AI(I,J)-AI(I,L)*AR(I,J)                    
            QR=QR/(Z*(Z+W))                                             
            QI=QI/(Z*(Z+W))                                             
            DO 40 I=L,M                                                 
               AR(I,J)=AR(I,J)-QR*AR(I,L)+QI*AI(I,L)                    
   40          AI(I,J)=AI(I,J)-QR*AI(I,L)-QI*AR(I,L)                    
   50       CONTINUE                                                    
   60    CONTINUE                                                       
      GO TO 80                                                          
C                                                                       
C ... HERE FOR A RANK-DEFICIENT MATRIX A.                               
C                                                                       
C/6S                                                                    
C  70 CALL SETERR(28HDCL2SD - A IS RANK-DEFICIENT,28,4,1)               
C/7S                                                                    
   70 CALL SETERR('DCL2SD - A IS RANK-DEFICIENT',28,4,1)                
C/                                                                      
C                                                                       
   80 RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE DCL2SS(MDIM,M,N,AR,AI,BR,BI,D,XR,XI)                   
C                                                                       
C  LEAST SQUARES SOLUTION OF THE COMPLEX LINEAR SYSTEM OF ALGEBRAIC     
C  EQUATIONS A*X=B, WHERE A HAS BEEN FACTORED BY A CALL TO DCL2SD.      
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN SIZE OF AR, AI, BR AND BI.           
C    M    - THE NUMBER OF EQUATIONS.                                    
C    N    - THE NUMBER OF UNKNOWNS.                                     
C    AR   - THE REAL PART OF THE DECOMPOSED MATRIX A.                   
C    AI   - THE IMAGINARY PART OF THE DECOMPOSED MATRIX A.              
C    BR   - THE REAL PART OF THE RIGHT HAND SIDE B.                     
C    BI   - THE IMAGINARY PART OF THE RIGHT HAND SIDE B.                
C    D    - AS GIVEN BY DCL2SD.                                         
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    BR   - BOTH BR AND BI HAVE BEEN CLOBBERED.                         
C           SQRT(SUM(I=N+1,M)(BR(I)**2+BI(I)**2)) IS THE                
C    BI   - L2 NORM OF THE RESIDUAL IN THE SOLUTION OF THE EQUATIONS.   
C    XR   - THE REAL PART OF THE SOLUTION VECTOR X.                     
C    XI   - THE IMAGINARY PART OF THE SOLUTION VECTOR X.                
C           X=B IS OK.                                                  
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - N.LT.1.                                                        
C    3 - M.LT.N.                                                        
C    4 - D(L).LE.0.                                                     
C    5 - A(L,L)=0.                                                      
C                                                                       
      DOUBLE PRECISION AR(MDIM,N),AI(MDIM,N),BR(M),BI(M),D(N),          
     1                 XR(N),XI(N)                                      
C                                                                       
      DOUBLE PRECISION Z,ZPW,QR,QI,DSQRT                                
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18HDCL2SS - MDIM.LT.M,18,1,2)          
C     IF (N.LT.1) CALL SETERR(15HDCL2SS - N.LT.1,15,2,2)                
C     IF (M.LT.N) CALL SETERR(15HDCL2SS - M.LT.N,15,3,2)                
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR('DCL2SS - MDIM.LT.M',18,1,2)           
      IF (N.LT.1) CALL SETERR('DCL2SS - N.LT.1',15,2,2)                 
      IF (M.LT.N) CALL SETERR('DCL2SS - M.LT.N',15,3,2)                 
C/                                                                      
C                                                                       
C ... APPLY Q-STAR TO THE RIGHT-HAND-SIDE B.                            
C                                                                       
      DO 30 L=1,N                                                       
         Z=D(L)                                                         
C/6S                                                                    
C        IF (Z.LE.0.0D0) CALL SETERR(18HDCL2SS - D(L).LE.0,18,4,2)      
C/7S                                                                    
         IF (Z.LE.0.0D0) CALL SETERR('DCL2SS - D(L).LE.0',18,4,2)       
C/                                                                      
         ZPW=DSQRT(AR(L,L)**2+AI(L,L)**2)                               
C/6S                                                                    
C        IF (ZPW.EQ.0.0D0) CALL SETERR(17HDCL2SS - A(L,L)=0,17,5,2)     
C/7S                                                                    
         IF (ZPW.EQ.0.0D0) CALL SETERR('DCL2SS - A(L,L)=0',17,5,2)      
C/                                                                      
         QR=0.D0                                                        
         QI=0.D0                                                        
         DO 10 I=L,M                                                    
            QR=QR+AR(I,L)*BR(I)+AI(I,L)*BI(I)                           
   10       QI=QI+AR(I,L)*BI(I)-AI(I,L)*BR(I)                           
         QR=QR/(Z*ZPW)                                                  
         QI=QI/(Z*ZPW)                                                  
         DO 20 I=L,M                                                    
            BR(I)=BR(I)-QR*AR(I,L)+QI*AI(I,L)                           
   20       BI(I)=BI(I)-QR*AI(I,L)-QI*AR(I,L)                           
         XR(L)=BR(L)                                                    
   30    XI(L)=BI(L)                                                    
C                                                                       
C ... BACK-SOLVE THE UPPER-TRIANGULAR SYSTEM U*X=(Q-STAR)*B.            
C                                                                       
      DO 60 II=1,N                                                      
         I=N+1-II                                                       
         ZPW=DSQRT(AR(I,I)**2+AI(I,I)**2)                               
         QR=XR(I)                                                       
         QI=XI(I)                                                       
         IP1=I+1                                                        
         IF(IP1.GT.N)GOTO 50                                            
         DO 40 J=IP1,N                                                  
            QR=QR-AR(I,J)*XR(J)+AI(I,J)*XI(J)                           
   40       QI=QI-AR(I,J)*XI(J)-AI(I,J)*XR(J)                           
   50    XR(I)=-( QR*AR(I,I)+QI*AI(I,I))/(ZPW*D(I))                     
   60    XI(I)=-(-QR*AI(I,I)+QI*AR(I,I))/(ZPW*D(I))                     
      RETURN                                                            
      END                                                               
      SUBROUTINE LINEQ (N, A, B, NB, X)                                 
C                                                                       
C SOLUTION OF A SET OF LINEAR EQUATIONS                                 
C                                                                       
C  METHOD - QR DECOMPOSITION OF A BY HOUSEHOLDER TRANSFORMATIONS        
C           FOLLOWED BY BACK SUBSTITUTION                               
C                                                                       
C  INPUT                                                                
C                                                                       
C    N    - THE NUMBER OF EQUATIONS                                     
C    A    - THE MATRIX                                                  
C    B    - THE RIGHT-HAND SIDES                                        
C    NB   - THE NUMBER OF RIGHT-HAND SIDES                              
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    A - CLOBBERED                                                      
C    B - CLOBBERED                                                      
C    X - THE SOLUTION VECTORS                                           
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - N.LT.1                                                         
C    2 - NB.LT.1                                                        
C    3 - A IS SINGULAR (RECOVERABLE)                                    
C                                                                       
      DIMENSION A(N,N),B(N,NB),X(N,NB)                                  
C                                                                       
C   SAVE AND TURN ON RECOVERY SWITCH                                    
C                                                                       
       CALL ENTSRC(IRSAVE, 1)                                           
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (N.LT.1) CALL SETERR(14HLINEQ - N.LT.1,14,1,2)                 
C/7S                                                                    
      IF (N.LT.1) CALL SETERR('LINEQ - N.LT.1',14,1,2)                  
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NB.LT.1) CALL SETERR(15HLINEQ - NB.LT.1,15,2,2)               
C/7S                                                                    
      IF (NB.LT.1) CALL SETERR('LINEQ - NB.LT.1',15,2,2)                
C/                                                                      
C                                                                       
C ... CALL LSTSQ TO GET THE SOLUTION                                    
C                                                                       
      CALL LSTSQ(N,N,N,N,A,B,NB,X)                                      
C                                                                       
      IF (NERROR(NERR).EQ.0) GO TO 10                                   
C                                                                       
C ... SINGULAR MATRIX                                                   
C                                                                       
      CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(21HLINEQ - A IS SINGULAR,21,3,1)                      
C/7S                                                                    
      CALL SETERR('LINEQ - A IS SINGULAR',21,3,1)                       
C/                                                                      
C                                                                       
 10   CALL RETSRC(IRSAVE)                                               
      RETURN                                                            
      END                                                               
      SUBROUTINE LSTSQ(MDIM,NDIM,M,N,A,B,NB,X)                          
C                                                                       
C  LEAST SQUARES SOLUTION TO A SYSTEM OF LINEAR ALGEBRAIC               
C  EQUATIONS WITH M BY N COEFFICIENT MATRIX A AND RIGHT                 
C  HAND SIDES B(*,J), J=1,...,NB. 1.LE.N.LE.M IS ASSUMED.               
C                                                                       
C  METHOD - QR DECOMPOSITION OF A BY HOUSEHOLDER TRANSFORMATIONS.       
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN LENGTH OF A AND B.                   
C    NDIM - THE DIMENSIONED COLUMN LENGTH OF X.                         
C    M    - THE NUMBER OF EQUATIONS                                     
C    N    - THE NUMBER OF UNKNOWNS. N.LE.M IS ASSUMED.                  
C    A    - THE MATRIX.                                                 
C    B    - THE RIGHT-HAND SIDES.                                       
C    NB   - THE NUMBER OF RIGHT-HAND SIDES B.                           
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    A - CLOBBERED.                                                     
C    B - B HAS BEEN CLOBBERED.                                          
C        SQRT(SUM(I=N+1,M)(B(I,J)**2)) IS THE L2 NORM OF THE RESIDUAL   
C        IN THE SOLUTION FOR THE J-TH RIGHT-HAND SIDE, J=1,...,NB.      
C    X - THE SOLUTION VECTORS. X=B IS OK IF NDIM=MDIM.                  
C                                                                       
C  SCRATCH SPACE ALLOCATED - N REAL WORDS.                              
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - NDIM.LT.N.                                                     
C    3 - N.LT.1.                                                        
C    4 - N.GT.M.                                                        
C    5 - NB.LT.1.                                                       
C    6 - WHEN X=B MUST HAVE NDIM=MDIM.                                  
C    7 - A IS RANK DEFICIENT. (RECOVERABLE)                             
C                                                                       
      REAL A(MDIM,N),B(MDIM,NB),X(NDIM,NB)                              
C                                                                       
      COMMON /CSTAK/DS                                                  
      DOUBLE PRECISION DS(500)                                          
      REAL S(1000),SAVE                                                 
      EQUIVALENCE (DS(1),S(1))                                          
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18H LSTSQ - MDIM.LT.M,18,1,2)          
C     IF (NDIM.LT.N) CALL SETERR(18H LSTSQ - NDIM.LT.N,18,2,2)          
C     IF (N.LT.1) CALL SETERR(15H LSTSQ - N.LT.1,15,3,2)                
C     IF (N.GT.M) CALL SETERR(15H LSTSQ - N.GT.M,15,4,2)                
C     IF (NB.LT.1) CALL SETERR(16H LSTSQ - NB.LT.1,16,5,2)              
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR(' LSTSQ - MDIM.LT.M',18,1,2)           
      IF (NDIM.LT.N) CALL SETERR(' LSTSQ - NDIM.LT.N',18,2,2)           
      IF (N.LT.1) CALL SETERR(' LSTSQ - N.LT.1',15,3,2)                 
      IF (N.GT.M) CALL SETERR(' LSTSQ - N.GT.M',15,4,2)                 
      IF (NB.LT.1) CALL SETERR(' LSTSQ - NB.LT.1',16,5,2)               
C/                                                                      
C                                                                       
C ... MAKE SURE THAT IF X=B THEN NDIM=MDIM.                             
C                                                                       
      SAVE=B(1,1)                                                       
      X(1,1)=+1.0E0                                                     
      B(1,1)=-1.0E0                                                     
C/6S                                                                    
C     IF (X(1,1).EQ.B(1,1) .AND. NDIM.NE.MDIM) CALL SETERR              
C    1   (37H LSTSQ - WHEN X=B MUST HAVE NDIM=MDIM,37,6,2)              
C/7S                                                                    
      IF (X(1,1).EQ.B(1,1) .AND. NDIM.NE.MDIM) CALL SETERR              
     1   (' LSTSQ - WHEN X=B MUST HAVE NDIM=MDIM',37,6,2)               
C/                                                                      
      B(1,1)=SAVE                                                       
C                                                                       
C ... GET THE QR DECOMPOSITION OF A.                                    
C                                                                       
      IUD=ISTKGT(N,3)                                                   
      CALL LST2D(MDIM,M,N,A,S(IUD))                                     
C                                                                       
C ... IF RANK(A)=N, SOLVE THE SYSTEMS ONE AT A TIME.                    
C                                                                       
      IF (NERROR(NERR).NE.0) GO TO 20                                   
      DO 10 J=1,NB                                                      
 10      CALL LST2S(MDIM,M,N,A,S(IUD),B(1,J),X(1,J))                    
      GO TO 30                                                          
C                                                                       
C ... RANK DEFICIENT MATRIX.                                            
C                                                                       
 20   CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(28H LSTSQ - A IS RANK DEFICIENT,28,7,1)               
C/7S                                                                    
      CALL SETERR(' LSTSQ - A IS RANK DEFICIENT',28,7,1)                
C/                                                                      
C                                                                       
 30   CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE LST2D(MDIM,M,N,A,UD)                                   
C                                                                       
C  TO OBTAIN THE Q*U DECOMPOSITION OF THE MATRIX A USING HOUSEHOLDER    
C  TRANSFORMATIONS.                                                     
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN LENGTH OF A                          
C    M    - THE NUMBER OF ROWS IN A.                                    
C    N    - THE NUMBER OF COLUMNS IN A. N.LE.M IS ASSUMED.              
C    A    - THE MATRIX A.                                               
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    A  - THE DECOMPOSED MATRIX.                                        
C         LET V(J)(I)=0      FOR I=1,...,J-1                            
C         AND V(J)(I)=A(I,J) FOR I=J,...,M, THEN                        
C                                                                       
C             Q = PRODUCT(J=1,...,N)(I-BETA(J)*V(J)*V(J)-TRANSPOSE)     
C                                                                       
C         WHERE BETA(J)=1/(UD(J)*ABS(A(J,J)))                           
C         A(I,J) FOR I.LT.J GIVES THE OFF-DIAGONAL ELEMENTS OF U.       
C    UD - THE DIAGONAL OF U.                                            
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - N.LT.1.                                                        
C    3 - N.GT.M.                                                        
C    4 - A IS RANK-DEFICIENT. (RECOVERABLE)                             
C                                                                       
C  P.A. BUSINGER, NUM. MATH. 7, 269-276(1965).                          
C                                                                       
      REAL A(MDIM,N),UD(N)                                              
C                                                                       
      DOUBLE PRECISION S,BE,DSQRT                                       
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18H LST2D - MDIM.LT.M,18,1,2)          
C     IF (N.LT.1) CALL SETERR(15H LST2D - N.LT.1,15,2,2)                
C     IF (N.GT.M) CALL SETERR(15H LST2D - N.GT.M,15,3,2)                
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR(' LST2D - MDIM.LT.M',18,1,2)           
      IF (N.LT.1) CALL SETERR(' LST2D - N.LT.1',15,2,2)                 
      IF (N.GT.M) CALL SETERR(' LST2D - N.GT.M',15,3,2)                 
C/                                                                      
C                                                                       
      DO 30 L=1,N                                                       
         S=0.D0                                                         
         DO 10 I=L,M                                                    
 10         S=S+DBLE(A(I,L))**2                                         
         IF (S.EQ.0.0E0) GO TO 40                                       
         UD(L)=DSQRT(S)                                                 
         IF (A(L,L).GE.0.0E0) UD(L)=-UD(L)                              
         BE=S-A(L,L)*DBLE(UD(L))                                        
         A(L,L)=A(L,L)-UD(L)                                            
         IF (L.EQ.N) GO TO 50                                           
         LP1=L+1                                                        
         DO 30 J=LP1,N                                                  
            S=0.D0                                                      
            DO 20 I=L,M                                                 
 20            S=S+A(I,L)*DBLE(A(I,J))                                  
            S=S/BE                                                      
            DO 30 I=L,M                                                 
 30            A(I,J)=A(I,J)-S*A(I,L)                                   
      GO TO 50                                                          
C                                                                       
C ... RANK-DEFICIENT MATRIX.                                            
C                                                                       
C/6S                                                                    
C40   CALL SETERR(28H LST2D - A IS RANK-DEFICIENT,28,4,1)               
C/7S                                                                    
 40   CALL SETERR(' LST2D - A IS RANK-DEFICIENT',28,4,1)                
C/                                                                      
C                                                                       
 50   RETURN                                                            
      END                                                               
      SUBROUTINE LST2S(MDIM,M,N,A,UD,B,X)                               
C                                                                       
C  TO SOLVE THE LEAST SQUARES PROBLEM A*X=B WHEN THE MATRIX             
C  A HAS ALREADY BEEN DECOMPOSED BY LST2D.                              
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN LENGTH OF A.                         
C    M    - THE NUMBER OF ROWS OF A                                     
C    N    - THE NUMBER OF COLUMNS OF A. N.LE.M IS ASSUMED.              
C    A    - THE DECOMPOSED MATRIX.                                      
C    UD   - THE DIAGONAL OF U.                                          
C    B    - THE RIGHT-HAND SIDE.                                        
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    B - B HAS BEEN CLOBBERED.                                          
C        SQRT(SUM(I=N+1,M)(B(I)**2)) IS THE L2 NORM OF THE RESIDUAL     
C        IN THE SOLUTION OF THE EQUATIONS.                              
C    X - THE SOLUTION VECTORS. X=B IS OK.                               
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - N.LT.1.                                                        
C    3 - N.GT.M.                                                        
C    4 - UD(J)=0 OR A(J,J)=0.                                           
C                                                                       
      REAL A(MDIM,N),UD(N),B(M),X(N)                                    
C                                                                       
      DOUBLE PRECISION S                                                
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18H LST2S - MDIM.LT.M,18,1,2)          
C     IF (N.LT.1) CALL SETERR(15H LST2S - N.LT.1,15,2,2)                
C     IF (N.GT.M) CALL SETERR(15H LST2S - N.GT.M,15,3,2)                
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR(' LST2S - MDIM.LT.M',18,1,2)           
      IF (N.LT.1) CALL SETERR(' LST2S - N.LT.1',15,2,2)                 
      IF (N.GT.M) CALL SETERR(' LST2S - N.GT.M',15,3,2)                 
C/                                                                      
C                                                                       
C ... APPLY Q-TRANSPOSE TO B.                                           
C                                                                       
      DO 20 J=1,N                                                       
C/6S                                                                    
C        IF (UD(J).EQ.0.0E0.OR.A(J,J).EQ.0.0E0) CALL SETERR             
C    1      (28H LST2S - UD(J)=0 OR A(J,J)=0,28,4,2)                    
C/7S                                                                    
         IF (UD(J).EQ.0.0E0.OR.A(J,J).EQ.0.0E0) CALL SETERR             
     1      (' LST2S - UD(J)=0 OR A(J,J)=0',28,4,2)                     
C/                                                                      
         S=0.D0                                                         
         DO 10 I=J,M                                                    
 10         S=S+A(I,J)*DBLE(B(I))                                       
         S=S/(UD(J)*DBLE(A(J,J)))                                       
         DO 20 I=J,M                                                    
 20         B(I)=B(I)+S*A(I,J)                                          
C                                                                       
C ... BACK-SOLVE THE TRIANGULAR SYSTEM U*X=(Q-TRANSPOSE)*B.             
C                                                                       
      X(N)=B(N)/UD(N)                                                   
      IF (N.EQ.1) GO TO 50                                              
      DO 40 II=2,N                                                      
         I=N+1-II                                                       
         S=B(I)                                                         
         IP1=I+1                                                        
         DO 30 J=IP1,N                                                  
 30         S=S-A(I,J)*DBLE(X(J))                                       
 40      X(I)=S/UD(I)                                                   
C                                                                       
 50   RETURN                                                            
      END                                                               
      SUBROUTINE DLINEQ (N, A, B, NB, X)                                
C                                                                       
C SOLUTION OF A SET OF LINEAR EQUATIONS                                 
C                                                                       
C  METHOD - QR DECOMPOSITION OF A BY HOUSEHOLDER TRANSFORMATIONS        
C           FOLLOWED BY BACK SUBSTITUTION                               
C                                                                       
C  INPUT                                                                
C                                                                       
C    N    - THE NUMBER OF EQUATIONS                                     
C    A    - THE MATRIX                                                  
C    B    - THE RIGHT-HAND SIDES                                        
C    NB   - THE NUMBER OF RIGHT-HAND SIDES                              
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    A - CLOBBERED                                                      
C    B - CLOBBERED                                                      
C    X - THE SOLUTION VECTORS                                           
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - N.LT.1                                                         
C    2 - NB.LT.1                                                        
C    3 - A IS SINGULAR (RECOVERABLE)                                    
C                                                                       
      DOUBLE PRECISION A(N,N),B(N,NB),X(N,NB)                           
C                                                                       
C   SAVE AND TURN ON RECOVERY SWITCH                                    
C                                                                       
       CALL ENTSRC(IRSAVE, 1)                                           
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (N.LT.1) CALL SETERR(15HDLINEQ - N.LT.1,15,1,2)                
C/7S                                                                    
      IF (N.LT.1) CALL SETERR('DLINEQ - N.LT.1',15,1,2)                 
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (NB.LT.1) CALL SETERR(16HDLINEQ - NB.LT.1,16,2,2)              
C/7S                                                                    
      IF (NB.LT.1) CALL SETERR('DLINEQ - NB.LT.1',16,2,2)               
C/                                                                      
C                                                                       
C ... CALL DLSTSQ TO GET THE SOLUTION                                   
C                                                                       
      CALL DLSTSQ(N,N,N,N,A,B,NB,X)                                     
C                                                                       
      IF (NERROR(NERR).EQ.0) GO TO 10                                   
C                                                                       
C ... SINGULAR MATRIX                                                   
C                                                                       
      CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(22HDLINEQ - A IS SINGULAR,22,3,1)                     
C/7S                                                                    
      CALL SETERR('DLINEQ - A IS SINGULAR',22,3,1)                      
C/                                                                      
C                                                                       
 10   CALL RETSRC(IRSAVE)                                               
      RETURN                                                            
      END                                                               
      SUBROUTINE DLSTSQ(MDIM,NDIM,M,N,A,B,NB,X)                         
C                                                                       
C  LEAST SQUARES SOLUTION TO A SYSTEM OF LINEAR ALGEBRAIC               
C  EQUATIONS WITH M BY N COEFFICIENT MATRIX A AND RIGHT                 
C  HAND SIDES B(*,J), J=1,...,NB. 1.LE.N.LE.M IS ASSUMED.               
C                                                                       
C  METHOD - QR DECOMPOSITION OF A BY HOUSEHOLDER TRANSFORMATIONS.       
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN LENGTH OF A AND B.                   
C    NDIM - THE DIMENSIONED COLUMN LENGTH OF X.                         
C    M    - THE NUMBER OF EQUATIONS                                     
C    N    - THE NUMBER OF UNKNOWNS. N.LE.M IS ASSUMED.                  
C    A    - THE MATRIX.                                                 
C    B    - THE RIGHT-HAND SIDES.                                       
C    NB   - THE NUMBER OF RIGHT-HAND SIDES B.                           
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    A - CLOBBERED.                                                     
C    B - CLOBBERED.                                                     
C        SQRT(SUM(I=N+1,M)(B(I,J)**2)) IS THE L2 NORM OF THE RESIDUAL   
C        IN THE SOLUTION FOR THE J-TH RIGHT-HAND SIDE, J=1,...,NB.      
C    X - THE SOLUTION VECTORS. X=B IS OK IF NDIM=MDIM.                  
C                                                                       
C  SCRATCH SPACE ALLOCATED - N DOUBLE PRECISION WORDS.                  
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - NDIM.LT.N.                                                     
C    3 - N.LT.1.                                                        
C    4 - N.GT.M.                                                        
C    5 - NB.LT.1.                                                       
C    6 - WHEN X=B MUST HAVE NDIM=MDIM.                                  
C    7 - A IS RANK DEFICIENT. (RECOVERABLE)                             
C                                                                       
      DOUBLE PRECISION A(MDIM,N),B(MDIM,NB),X(NDIM,NB)                  
C                                                                       
      COMMON /CSTAK/S                                                   
      DOUBLE PRECISION S(500)                                           
      DOUBLE PRECISION SAVE                                             
C                                                                       
      CALL ENTER(1)                                                     
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18HDLSTSQ - MDIM.LT.M,18,1,2)          
C     IF (NDIM.LT.N) CALL SETERR(18HDLSTSQ - NDIM.LT.N,18,2,2)          
C     IF (N.LT.1) CALL SETERR(15HDLSTSQ - N.LT.1,15,3,2)                
C     IF (N.GT.M) CALL SETERR(15HDLSTSQ - N.GT.M,15,4,2)                
C     IF (NB.LT.1) CALL SETERR(16HDLSTSQ - NB.LT.1,16,5,2)              
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR('DLSTSQ - MDIM.LT.M',18,1,2)           
      IF (NDIM.LT.N) CALL SETERR('DLSTSQ - NDIM.LT.N',18,2,2)           
      IF (N.LT.1) CALL SETERR('DLSTSQ - N.LT.1',15,3,2)                 
      IF (N.GT.M) CALL SETERR('DLSTSQ - N.GT.M',15,4,2)                 
      IF (NB.LT.1) CALL SETERR('DLSTSQ - NB.LT.1',16,5,2)               
C/                                                                      
C                                                                       
C ... MAKE SURE THAT IF X=B THEN NDIM=MDIM.                             
C                                                                       
      SAVE=B(1,1)                                                       
      X(1,1)=+1.0D0                                                     
      B(1,1)=-1.0D0                                                     
C/6S                                                                    
C     IF (X(1,1).EQ.B(1,1) .AND. NDIM.NE.MDIM) CALL SETERR              
C    1   (37HDLSTSQ - WHEN X=B MUST HAVE NDIM=MDIM,37,6,2)              
C/7S                                                                    
      IF (X(1,1).EQ.B(1,1) .AND. NDIM.NE.MDIM) CALL SETERR              
     1   ('DLSTSQ - WHEN X=B MUST HAVE NDIM=MDIM',37,6,2)               
C/                                                                      
      B(1,1)=SAVE                                                       
C                                                                       
C ... GET THE QR DECOMPOSITION OF A.                                    
C                                                                       
      IUD=ISTKGT(N,4)                                                   
      CALL DLST2D(MDIM,M,N,A,S(IUD))                                    
C                                                                       
C ... IF RANK(A)=N, SOLVE THE SYSTEMS ONE AT A TIME.                    
C                                                                       
      IF (NERROR(NERR).NE.0) GO TO 20                                   
      DO 10 J=1,NB                                                      
 10      CALL DLST2S(MDIM,M,N,A,S(IUD),B(1,J),X(1,J))                   
      GO TO 30                                                          
C                                                                       
C ... RANK DEFICIENT MATRIX.                                            
C                                                                       
 20   CALL ERROFF                                                       
C/6S                                                                    
C     CALL SETERR(28HDLSTSQ - A IS RANK DEFICIENT,28,7,1)               
C/7S                                                                    
      CALL SETERR('DLSTSQ - A IS RANK DEFICIENT',28,7,1)                
C/                                                                      
C                                                                       
 30   CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DLST2D(MDIM,M,N,A,UD)                                  
C                                                                       
C  TO OBTAIN THE Q*U DECOMPOSITION OF THE MATRIX A USING HOUSEHOLDER    
C  TRANSFORMATIONS.                                                     
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN LENGTH OF A                          
C    M    - THE NUMBER OF ROWS IN A.                                    
C    N    - THE NUMBER OF COLUMNS IN A. N.LE.M IS ASSUMED.              
C    A    - THE MATRIX A.                                               
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    A  - THE DECOMPOSED MATRIX.                                        
C         LET V(J)(I)=0      FOR I=1,...,J-1                            
C         AND V(J)(I)=A(I,J) FOR I=J,...,M, THEN                        
C                                                                       
C             Q = PRODUCT(J=1,...,N)(I-BETA(J)*V(J)*V(J)-TRANSPOSE)     
C                                                                       
C         WHERE BETA(J)=1/(UD(J)*ABS(A(J,J)))                           
C         A(I,J) FOR I.LT.J GIVES THE OFF-DIAGONAL ELEMENTS OF U.       
C    UD - THE DIAGONAL OF U.                                            
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - N.LT.1.                                                        
C    3 - N.GT.M.                                                        
C    4 - A IS RANK-DEFICIENT. (RECOVERABLE)                             
C                                                                       
C  P.A. BUSINGER, NUM. MATH. 7, 269-276(1965).                          
C                                                                       
      DOUBLE PRECISION A(MDIM,N),UD(N)                                  
C                                                                       
      DOUBLE PRECISION S,BE,DSQRT                                       
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18HDLST2D - MDIM.LT.M,18,1,2)          
C     IF (N.LT.1) CALL SETERR(15HDLST2D - N.LT.1,15,2,2)                
C     IF (N.GT.M) CALL SETERR(15HDLST2D - N.GT.M,15,3,2)                
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR('DLST2D - MDIM.LT.M',18,1,2)           
      IF (N.LT.1) CALL SETERR('DLST2D - N.LT.1',15,2,2)                 
      IF (N.GT.M) CALL SETERR('DLST2D - N.GT.M',15,3,2)                 
C/                                                                      
C                                                                       
      DO 30 L=1,N                                                       
         S=0.D0                                                         
         DO 10 I=L,M                                                    
 10         S=S+A(I,L)**2                                               
         IF (S.EQ.0.0D0) GO TO 40                                       
         UD(L)=DSQRT(S)                                                 
         IF (A(L,L).GE.0.0D0) UD(L)=-UD(L)                              
         BE=S-A(L,L)*UD(L)                                              
         A(L,L)=A(L,L)-UD(L)                                            
         IF (L.EQ.N) GO TO 50                                           
         LP1=L+1                                                        
         DO 30 J=LP1,N                                                  
            S=0.D0                                                      
            DO 20 I=L,M                                                 
 20            S=S+A(I,L)*A(I,J)                                        
            S=S/BE                                                      
            DO 30 I=L,M                                                 
 30            A(I,J)=A(I,J)-S*A(I,L)                                   
      GO TO 50                                                          
C                                                                       
C ... RANK-DEFICIENT MATRIX.                                            
C                                                                       
C/6S                                                                    
C40   CALL SETERR(28HDLST2D - A IS RANK-DEFICIENT,28,4,1)               
C/7S                                                                    
 40   CALL SETERR('DLST2D - A IS RANK-DEFICIENT',28,4,1)                
C/                                                                      
C                                                                       
 50   RETURN                                                            
      END                                                               
      SUBROUTINE DLST2S(MDIM,M,N,A,UD,B,X)                              
C                                                                       
C  TO SOLVE THE LEAST SQUARES PROBLEM A*X=B WHEN THE MATRIX             
C  A HAS ALREADY BEEN DECOMPOSED BY DLST2D.                             
C                                                                       
C  INPUT                                                                
C                                                                       
C    MDIM - THE DIMENSIONED COLUMN LENGTH OF A.                         
C    M    - THE NUMBER OF ROWS OF A                                     
C    N    - THE NUMBER OF COLUMNS OF A. N.LE.M IS ASSUMED.              
C    A    - THE DECOMPOSED MATRIX.                                      
C    UD   - THE DIAGONAL OF U.                                          
C    B    - THE RIGHT-HAND SIDE.                                        
C                                                                       
C  OUTPUT                                                               
C                                                                       
C    B - B HAS BEEN CLOBBERED.                                          
C        SQRT(SUM(I=N+1,M)(B(I)**2)) IS THE L2 NORM OF THE RESIDUAL     
C        IN THE SOLUTION OF THE EQUATIONS.                              
C    X - THE SOLUTION VECTORS. X=B IS OK.                               
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE.                                      
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MDIM.LT.M.                                                     
C    2 - N.LT.1.                                                        
C    3 - N.GT.M.                                                        
C    4 - UD(J)=0 OR A(J,J)=0.                                           
C                                                                       
      DOUBLE PRECISION A(MDIM,N),UD(N),B(M),X(N)                        
C                                                                       
      DOUBLE PRECISION S                                                
C                                                                       
C ... CHECK THE INPUT.                                                  
C                                                                       
C/6S                                                                    
C     IF (MDIM.LT.M) CALL SETERR(18HDLST2S - MDIM.LT.M,18,1,2)          
C     IF (N.LT.1) CALL SETERR(15HDLST2S - N.LT.1,15,2,2)                
C     IF (N.GT.M) CALL SETERR(15HDLST2S - N.GT.M,15,3,2)                
C/7S                                                                    
      IF (MDIM.LT.M) CALL SETERR('DLST2S - MDIM.LT.M',18,1,2)           
      IF (N.LT.1) CALL SETERR('DLST2S - N.LT.1',15,2,2)                 
      IF (N.GT.M) CALL SETERR('DLST2S - N.GT.M',15,3,2)                 
C/                                                                      
C                                                                       
C ... APPLY Q-TRANSPOSE TO B.                                           
C                                                                       
      DO 20 J=1,N                                                       
C/6S                                                                    
C        IF (UD(J).EQ.0.0D0.OR.A(J,J).EQ.0.0D0) CALL SETERR             
C    1      (28HDLST2S - UD(J)=0 OR A(J,J)=0,28,4,2)                    
C/7S                                                                    
         IF (UD(J).EQ.0.0D0.OR.A(J,J).EQ.0.0D0) CALL SETERR             
     1      ('DLST2S - UD(J)=0 OR A(J,J)=0',28,4,2)                     
C/                                                                      
         S=0.D0                                                         
         DO 10 I=J,M                                                    
 10         S=S+A(I,J)*B(I)                                             
         S=S/(UD(J)*A(J,J))                                             
         DO 20 I=J,M                                                    
 20         B(I)=B(I)+S*A(I,J)                                          
C                                                                       
C ... BACK-SOLVE THE TRIANGULAR SYSTEM U*X=(Q-TRANSPOSE)*B.             
C                                                                       
      X(N)=B(N)/UD(N)                                                   
      IF (N.EQ.1) GO TO 50                                              
      DO 40 II=2,N                                                      
         I=N+1-II                                                       
         S=B(I)                                                         
         IP1=I+1                                                        
         DO 30 J=IP1,N                                                  
 30         S=S-A(I,J)*X(J)                                             
 40      X(I)=S/UD(I)                                                   
C                                                                       
 50   RETURN                                                            
      END                                                               
      SUBROUTINE BURAM(NPTS, MESH, FN, M, N, P, Q, DELK)                
      INTEGER NPTS                                                      
      INTEGER M, N                                                      
      REAL MESH(NPTS), FN(NPTS), P(1), Q(1), DELK                       
      INTEGER MAXITR, ITOL, NERROR, IER, I                              
      REAL FNMAX, FNMIN                                                 
      LOGICAL SMONOR                                                    
C   BURAM  IS A REAL PROCEDURE WHICH FINDS A                            
C   A RATIONAL FUNCTION WHICH IS THE BEST APPROXIMATION,                
C   IN THE UNIFORM OR MINIMAX SENSE, TO A GIVEN DISCRETE                
C   FUNCTION.  THE RATIONAL FUNCTION IS REPRESENTED AS                  
C   THE QUOTIENT OF TWO POLYNOMIALS EACH EXPANDED IN TERMS              
C   OF TCHEBYCHEV POLYNOMIALS.  THIS ROUTINE IS A SHELL                 
C   WHICH IN TURN CALLS THE ROUTINE BURM1  WITH CERTAIN                 
C   DEFAULT VALUES FOR THE INITIAL APPROXIMATION AND  FOR               
C   THE STOPPING CRITERIA.                                              
C   INPUT:                                                              
C   NPTS   - THE NUMBER OF MESH POINTS.                                 
C   MESH   - THE ARRAY OF MESH POINTS.                                  
C   FN     - THE ARRAY OF FUNCTION VALUES.                              
C   M      - THE DEGREE OF THE DESIRED NUMERATOR POLYNOMIAL.            
C   N      - THE DEGREE OF THE DESIRED DENOMINATOR POLYNOMIAL.          
C   OUTPUT:                                                             
C   P      - THE ARRAY OF COEFFICIENTS FOR THE NUMERATOR POLYNOMIAL.    
C   Q      - THE ARRAY OF COEFFICIENTS FOR THE DENOMINATOR POLYNOMIAL.  
C   DELK   - THE MAXIMUM ERROR IN THE APPROXIMATION.                    
C   ERROR STATES (ASTERISK INDICATES RECOVERABLE):                      
C   1  - INVALID DEGREE                                                 
C   2  - TOO FEW MESH POINTS                                            
C   3  - MESH IS NOT STRICTLY MONOTONE                                  
C   4* - APPROXIMATION EQUALS FUNCTION                                  
C   5* - NO IMPROVEMENT IN APPROXIMATION                                
C   6* - REACHED 50 ITERATIONS                                          
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
C    1   23HBURAM  - INVALID DEGREE, 23, 1, 2)                          
C     IF (NPTS .LT. M+N+2) CALL SETERR(28HBURAM  - TOO FEW MESH POINTS  
C    1   , 28, 2, 2)                                                    
C     IF (.NOT. SMONOR(MESH, NPTS, 1)) CALL SETERR(                     
C    1   38HBURAM  - MESH IS NOT STRICTLY MONOTONE, 38, 3, 2)           
C/7S                                                                    
      IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
     1   'BURAM  - INVALID DEGREE', 23, 1, 2)                           
      IF (NPTS .LT. M+N+2) CALL SETERR('BURAM  - TOO FEW MESH POINTS'   
     1   , 28, 2, 2)                                                    
      IF (.NOT. SMONOR(MESH, NPTS, 1)) CALL SETERR(                     
     1   'BURAM  - MESH IS NOT STRICTLY MONOTONE', 38, 3, 2)            
C/                                                                      
C   INITIALIZE THE NUMERATOR AND DEMONINATOR POLYNOMIALS.               
      FNMAX = FN(1)                                                     
      FNMIN = FN(1)                                                     
      DO  3 I = 2, NPTS                                                 
         IF (FNMAX .GE. FN(I)) GOTO 1                                   
            FNMAX = FN(I)                                               
            GOTO  2                                                     
   1        IF (FN(I) .LT. FNMIN) FNMIN = FN(I)                         
   2     CONTINUE                                                       
   3     CONTINUE                                                       
      CALL SETR(M+1, 0.0E0, P)                                          
      P(1) = 0.5E0*(FNMAX+FNMIN)                                        
      CALL SETR(N+1, 0.0E0, Q)                                          
      Q(1) = 1.0E0                                                      
      DELK = FNMAX-P(1)                                                 
      IF (0 .GE. M .AND. 0 .GE. N) GOTO 11                              
         MAXITR = 50                                                    
         ITOL = 2                                                       
         CALL BURM1(NPTS, MESH, FN, MAXITR, ITOL, M, N, P, Q, DELK)     
         IF (NERROR(IER) .EQ. 0) GOTO 10                                
            IF (IER .NE. 7) GOTO 4                                      
C/6S                                                                    
C              CALL N5ERR(38HBURAM  - APPROXIMATION EQUALS FUNCTION, 38,
C    1            4, 1)                                                 
C/7S                                                                    
               CALL N5ERR('BURAM  - APPROXIMATION EQUALS FUNCTION', 38, 
     1            4, 1)                                                 
C/                                                                      
               GOTO  9                                                  
   4           IF (IER .NE. 8) GOTO 5                                   
C/6S                                                                    
C                 CALL N5ERR(                                           
C    1               40HBURAM  - NO IMPROVEMENT IN APPROXIMATION, 40, 5,
C    2               1)                                                 
C/7S                                                                    
                  CALL N5ERR(                                           
     1               'BURAM  - NO IMPROVEMENT IN APPROXIMATION', 40, 5, 
     2               1)                                                 
C/                                                                      
                  GOTO  8                                               
   5              IF (IER .NE. 9) GOTO 6                                
C/6S                                                                    
C                    CALL N5ERR(30HBURAM  - REACHED 50 ITERATIONS, 30, 6
C    1                  , 1)                                            
C/7S                                                                    
                     CALL N5ERR('BURAM  - REACHED 50 ITERATIONS', 30, 6 
     1                  , 1)                                            
C/                                                                      
                     GOTO  7                                            
   6                 CALL EPRINT                                        
   7           CONTINUE                                                 
   8        CONTINUE                                                    
   9        CONTINUE                                                    
  10     CONTINUE                                                       
  11  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE BURM1(NPTS, MESH, FN, MAXITR, ITOL, M, N, P, Q,        
     1   DELK)                                                          
      INTEGER NPTS                                                      
      INTEGER MAXITR, ITOL, M, N                                        
      REAL MESH(NPTS), FN(NPTS), P(1), Q(1), DELK                       
      COMMON /CSTAK/ DSTAK                                              
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER IDIG, IFLR, I1MACH, ISTKGT, NPPTR, NQPTR                  
      INTEGER ENPTR, QKPTR, IEXPTR, J, ISTAK(1000)                      
      REAL R1MACH, FLOAT, QLRG, WS(500), ABS                            
      LOGICAL SMONOR                                                    
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), WS(1))                                     
C   BURM1  IS A REAL PROCEDURE WHICH FINDS A                            
C   A RATIONAL FUNCTION WHICH IS THE BEST APPROXIMATION,                
C   IN THE UNIFORM OR MINIMAX SENSE, TO A GIVEN DISCRETE                
C   FUNCTION.  THE RATIONAL FUNCTION IS REPRESENTED AS                  
C   THE QUOTIENT OF TWO POLYNOMIALS EACH EXPANDED IN TERMS              
C   OF TCHEBYCHEV POLYNOMIALS.  THIS ROUTINE STARTS FROM AN             
C   INITIAL APPROXIMATION AND TERMINATES FOR ONE OF FOUR                
C   REASONS: (1) THE ERROR CURVE EQUIOSCILLATES AND THE                 
C   ALTERNATING EXTREMA MATCH TO ITOL DIGITS, (2) THE NUMBER            
C   OF ITERATIONS EXCEEDS MAXITR, (3) THE APPROXIMATION                 
C   CANNOT BE IMPROVED, OR (4) THE APPROXIMATION IS ESSENTIALLY         
C   EQUAL TO THE GIVEN DISCRETE FUNCTION.                               
C   INPUT:                                                              
C   NPTS   - THE NUMBER OF MESH POINTS.                                 
C   MESH   - THE ARRAY OF MESH POINTS.                                  
C   FN     - THE ARRAY OF FUNCTION VALUES.                              
C   MAXITR - THE MAXIMUM NUMBER OF ITERATIONS.                          
C   ITOL   - THE NUMBER OF DIGITS TO WHICH THE EXTREMA SHOULD MATCH.    
C   M      - THE DEGREE OF THE DESIRED NUMERATOR POLYNOMIAL.            
C   N      - THE DEGREE OF THE DESIRED DENOMINATOR POLYNOMIAL.          
C   P      - THE ARRAY OF COEFFICIENTS FOR THE INITIAL NUMERATOR.       
C   Q      - THE ARRAY OF COEFFICIENTS FOR THE INITIAL DENOMINATOR.     
C   OUTPUT:                                                             
C   P      - THE ARRAY OF COEFFICIENTS FOR THE NUMERATOR POLYNOMIAL.    
C   Q      - THE ARRAY OF COEFFICIENTS FOR THE DENOMINATOR POLYNOMIAL.  
C   DELK   - THE MAXIMUM ERROR IN THE APPROXIMATION.                    
C   ERROR STATES (ASTERISK INDICATES RECOVERABLE):                      
C   1  - INVALID DEGREE                                                 
C   2  - TOO FEW MESH POINTS                                            
C   3  - MESH IS NOT STRICTLY MONOTONE                                  
C   4  - MAXITR .LT. 0                                                  
C   5  - INVALID ACCURACY REQUEST                                       
C   6  - DENOMINATOR IS NONPOSITIVE                                     
C   7* - APPROXIMATION EQUALS FUNCTION                                  
C   8* - NO IMPROVEMENT IN APPROXIMATION                                
C   9* - REACHED MAXIMUM NO. OF ITERATIONS                              
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
C    1   23HBURM1  - INVALID DEGREE, 23, 1, 2)                          
C     IF (NPTS .LT. M+N+2) CALL SETERR(28HBURM1  - TOO FEW MESH POINTS  
C    1   , 28, 2, 2)                                                    
C     IF (.NOT. SMONOR(MESH, NPTS, 1)) CALL SETERR(                     
C    1   38HBURM1  - MESH IS NOT STRICTLY MONOTONE, 38, 3, 2)           
C     IF (MAXITR .LT. 0) CALL SETERR(22HBURM1  - MAXITR .LT. 0, 22, 4, 2
C    1   )                                                              
C/7S                                                                    
      IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
     1   'BURM1  - INVALID DEGREE', 23, 1, 2)                           
      IF (NPTS .LT. M+N+2) CALL SETERR('BURM1  - TOO FEW MESH POINTS'   
     1   , 28, 2, 2)                                                    
      IF (.NOT. SMONOR(MESH, NPTS, 1)) CALL SETERR(                     
     1   'BURM1  - MESH IS NOT STRICTLY MONOTONE', 38, 3, 2)            
      IF (MAXITR .LT. 0) CALL SETERR('BURM1  - MAXITR .LT. 0', 22, 4, 2 
     1   )                                                              
C/                                                                      
      IDIG = IFLR(R1MACH(5)*FLOAT(I1MACH(14)))                          
C/6S                                                                    
C     IF (ITOL .LT. 1 .OR. IDIG .LT. ITOL) CALL SETERR(                 
C    1   33HBURM1  - INVALID ACCURACY REQUEST, 33, 5, 2)                
C/7S                                                                    
      IF (ITOL .LT. 1 .OR. IDIG .LT. ITOL) CALL SETERR(                 
     1   'BURM1  - INVALID ACCURACY REQUEST', 33, 5, 2)                 
C/                                                                      
      QLRG = ABS(Q(1))                                                  
      J = 2                                                             
         GOTO  2                                                        
   1     J = J+1                                                        
   2     IF (J .GT. N+1) GOTO  3                                        
         IF (QLRG .LT. ABS(Q(J))) QLRG = ABS(Q(J))                      
         GOTO  1                                                        
   3  IF (QLRG .NE. 0.0E0) GOTO 4                                       
C/6S                                                                    
C        CALL SETERR(35HBURM1  - DENOMINATOR IS NONPOSITIVE, 35, 6, 2)  
C/7S                                                                    
         CALL SETERR('BURM1  - DENOMINATOR IS NONPOSITIVE', 35, 6, 2)   
C/                                                                      
         GOTO  11                                                       
   4     J = 1                                                          
            GOTO  6                                                     
   5        J = J+1                                                     
   6        IF (J .GT. N+1) GOTO  7                                     
            Q(J) = Q(J)/QLRG                                            
            GOTO  5                                                     
   7     J = 1                                                          
            GOTO  9                                                     
   8        J = J+1                                                     
   9        IF (J .GT. M+1) GOTO  10                                    
            P(J) = P(J)/QLRG                                            
            GOTO  8                                                     
  10     CONTINUE                                                       
  11  NPPTR = ISTKGT(M+1, 3)                                            
      NQPTR = ISTKGT(N+1, 3)                                            
      ENPTR = ISTKGT(NPTS, 3)                                           
      QKPTR = ISTKGT(NPTS, 3)                                           
      IEXPTR = ISTKGT(NPTS, 2)                                          
      CALL B1RM1(NPTS, MESH, FN, MAXITR, ITOL, M, N, P, Q, DELK, WS(    
     1   NPPTR), WS(NQPTR), WS(ENPTR), WS(QKPTR), ISTAK(IEXPTR))        
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE B1RM1(NPTS, X, FN, MAXITR, ITOL, M, N, P, Q,           
     1   DELK, NEWP, NEWQ, EN, QK, IEXT)                                
      INTEGER NPTS                                                      
      INTEGER MAXITR, ITOL, M, N, IEXT(NPTS)                            
      REAL X(NPTS), FN(NPTS), P(1), Q(1), DELK, NEWP(1)                 
      REAL NEWQ(1), EN(NPTS), QK(NPTS)                                  
      INTEGER NITR, NEX, IMAX, IMIN, ILRG, L5RGXR                       
      INTEGER NERROR, IER, I                                            
      REAL EPS, BND, R1MACH, DELNEW, ABS                                
      EPS = R1MACH(4)*10.0E0**ITOL                                      
      CALL EXTRMR(NPTS, FN, NEX, IEXT, IMAX, IMIN, ILRG)                
      BND = ABS(FN(ILRG))*EPS                                           
      CALL C5EQK(NPTS, X, FN, M, N, P, Q, QK, EN)                       
      DO  1 I = 1, NPTS                                                 
C/6S                                                                    
C        IF (QK(I) .LE. 0.0E0) CALL SETERR(                             
C    1      35HBURM1  - DENOMINATOR IS NONPOSITIVE, 35, 6, 2)           
C/7S                                                                    
         IF (QK(I) .LE. 0.0E0) CALL SETERR(                             
     1      'BURM1  - DENOMINATOR IS NONPOSITIVE', 35, 6, 2)            
C/                                                                      
   1     CONTINUE                                                       
      CALL EXTRMR(NPTS, EN, NEX, IEXT, IMAX, IMIN, ILRG)                
      DELK = ABS(EN(ILRG))                                              
      DELNEW = DELK                                                     
      CALL MOVEFR(M+1, P, NEWP)                                         
      CALL MOVEFR(N+1, Q, NEWQ)                                         
      NITR = 0                                                          
         GOTO  3                                                        
   2     NITR = NITR+1                                                  
   3     IF (NITR .GE. MAXITR) GOTO  6                                  
C   OUTPT3 (X,NPTS,P,Q,DELK,M,N,EN,IEXT,NEX)                            
         IF (DELK .GT. BND) GOTO 4                                      
C/6S                                                                    
C           CALL SETERR(38HBURM1  - APPROXIMATION EQUALS FUNCTION, 39, 7
C    1         , 1)                                                     
C/7S                                                                    
            CALL SETERR('BURM1  - APPROXIMATION EQUALS FUNCTION', 39, 7 
     1         , 1)                                                     
C/                                                                      
            RETURN                                                      
C   TEST FOR OPTIMAL SOLUTION.                                          
   4     IF (L5RGXR(NPTS, EN, NEX, IEXT, ILRG, ITOL) .GE. M+N+2) RETURN 
         CALL L5STP(NPTS, X, FN, QK, DELNEW, M, N, NEWP, NEWQ)          
         IF (NERROR(IER) .NE. 0) CALL ERROFF                            
         CALL C5EQK(NPTS, X, FN, M, N, NEWP, NEWQ, QK, EN)              
         CALL EXTRMR(NPTS, EN, NEX, IEXT, IMAX, IMIN, ILRG)             
         DELNEW = ABS(EN(ILRG))                                         
         IF (DELK .GT. DELNEW) GOTO 5                                   
C/6S                                                                    
C           CALL SETERR(40HBURM1  - NO IMPROVEMENT IN APPROXIMATION, 40,
C    1         8, 1)                                                    
C/7S                                                                    
            CALL SETERR('BURM1  - NO IMPROVEMENT IN APPROXIMATION', 40, 
     1         8, 1)                                                    
C/                                                                      
            RETURN                                                      
   5     CALL MOVEFR(M+1, NEWP, P)                                      
         CALL MOVEFR(N+1, NEWQ, Q)                                      
         DELK = DELNEW                                                  
         GOTO  2                                                        
C/6S                                                                    
C  6  CALL SETERR(42HBURM1  - REACHED MAXIMUM NO. OF ITERATIONS, 42, 9  
C    1   , 1)                                                           
C/7S                                                                    
   6  CALL SETERR('BURM1  - REACHED MAXIMUM NO. OF ITERATIONS', 42, 9   
     1   , 1)                                                           
C/                                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE  L5STP(NPTS, MESH, FN, QK, DELK, M, N, P, Q)           
      INTEGER NPTS                                                      
      INTEGER M, N                                                      
      REAL MESH(NPTS), FN(NPTS), QK(NPTS), DELK, P(1), Q(1)             
      COMMON /CSTAK/ DSTAK                                              
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER APTR, XPTR, ISTKGT, ISTAK(1000)                           
      INTEGER BC, BX, C, G, IW, LIW, LW, MM, NN, W                      
      REAL WS(500)                                                      
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), WS(1))                                     
C   THIS ROUTINE ALLOCATES STORAGE SO THAT                              
C    L9STP CAN DEFINE THE LINEAR PROGRAMMING SUBPROBLEM OF              
C   THE DIFFERENTIAL CORRECTION ALGORITHM AND CALL A GENERAL            
C   PURPOSE LINEAR PROGRAMMING PACKAGE.                                 
C   INPUT...                                                            
C   NPTS   - THE NUMBER OF MESH POINTS.                                 
C   MESH   - THE ARRAY OF MESH POINTS.                                  
C   FN     - THE ARRAY OF FUNCTION VALUES.                              
C   QK     - THE ARRAY OF CURRENT DENOMINATOR VALUES.                   
C   DELK   - THE CURRENT MINIMAX ERROR.                                 
C   M      - THE DEGREE OF THE NUMERATOR POLYNOMIAL.                    
C   N      - THE DEGREE OF THE DENOMINATOR POLYNOMIAL.                  
C   P      - THE CURRENT NUMERATOR POLYNOMIAL.                          
C   Q      - THE CURRENT DENOMINATOR POLYNOMIAL.                        
C   OUTPUT...                                                           
C   P      - THE ARRAY OF COEFFICIENTS FOR THE NUMERATOR POLYNOMIAL.    
C   Q      - THE ARRAY OF COEFFICIENTS FOR THE DENOMINATOR POLYNOMIAL.  
C   ERROR STATES (ASTERISK INDICATES FATAL)...                          
C   1* - INVALID DEGREE                                                 
C   2* - TOO FEW MESH POINTS                                            
C   3* - NONPOSITIVE DELK                                               
C   4  - NO IMPROVEMENT IN THE LP SUBPROBLEM                            
C                                                                       
C *** BODY ***                                                          
C                                                                       
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
C    1   23H L5STP - INVALID DEGREE, 23, 1, 2)                          
C     IF (NPTS .LT. M+N+2) CALL SETERR(28H L5STP - TOO FEW MESH POINTS, 
C    1    28, 2, 2)                                                     
C/7S                                                                    
      IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
     1   ' L5STP - INVALID DEGREE', 23, 1, 2)                           
      IF (NPTS .LT. M+N+2) CALL SETERR(' L5STP - TOO FEW MESH POINTS',  
     1    28, 2, 2)                                                     
C/                                                                      
      MM = 2 * NPTS                                                     
      NN = M + N + 3                                                    
      LIW = MM + NN + 7                                                 
      LW = NN*(3*NN+17)/2 + MM + 2                                      
      G = ISTKGT(NN, 3)                                                 
      C = ISTKGT(NN*MM, 3)                                              
      BC = ISTKGT(2*MM, 3)                                              
      BX = ISTKGT(2*NN, 3)                                              
      W = ISTKGT(LW, 3)                                                 
      IW = ISTKGT(LIW, 2)                                               
      APTR = ISTKGT(3*NPTS+1, 3)                                        
      XPTR = ISTKGT(NN, 3)                                              
      CALL  L9STP(NPTS, MESH, FN, QK, DELK, M, N, P, Q, WS(APTR),       
     1            WS(BC), WS(BX), WS(C), WS(G), ISTAK(IW), LIW, LW,     
     2            MM, NN, WS(W), WS(XPTR))                              
      CALL LEAVE                                                        
      RETURN                                                            
C *** LAST LINE OF  L5STP FOLLOWS ***                                   
      END                                                               
      SUBROUTINE  L9STP(NPTS, MESH, FN, QK, DELK, M, N, P, Q, A, BC, BX,
     1                  C, G, IW, LIW, LW, MM, NN, W, X)                
      INTEGER NPTS, M, N, MM, NN, LIW, LW                               
      INTEGER IW(LIW)                                                   
      REAL MESH(NPTS), FN(NPTS), QK(NPTS), DELK, P(1), Q(1)             
      REAL A(1), BC(2,MM), BX(2,NN), C(NN,MM), G(NN), W(LW),            
     1                 X(NN)                                            
C                                                                       
      COMMON / D5FCM/ NPTSC, MC, NC, I1, I2, I3, I4                     
      INTEGER NPTSC, MC, NC, I1, I2, I3, I4                             
C/+                                                                     
      REAL  ABS                                                         
C/                                                                      
      EXTERNAL  R7MDC                                                   
      REAL  R7MDC                                                       
C                                                                       
      INTEGER I, J, JP, K, M1, MAXMN, MAXMN1, N1, N2, NERROR, IER       
      REAL CTX, CTXNEW, FDELK, FNI, QLRG, QZ, Z                         
      REAL BIG, ONE, ZERO                                               
      DATA BIG/0.E+0/, ONE/1.E+0/, ZERO/0.E+0/                          
C                                                                       
C *** BODY ***                                                          
C                                                                       
C/6S                                                                    
C     IF (M .LT. 0 .OR. N .LT. 0)                                       
C    1   CALL SETERR(26H L9STP - INVALID DIMENSION, 26, 1, 2)           
C/7S                                                                    
      IF (M .LT. 0 .OR. N .LT. 0)                                       
     1   CALL SETERR(' L9STP - INVALID DIMENSION', 26, 1, 2)            
C/                                                                      
      IF (BIG .LE. ZERO) BIG =  R7MDC(6)                                
      M1 = M + 1                                                        
      N1 = N + 1                                                        
      N2 = N + 2                                                        
      NPTSC = NPTS                                                      
      MC = M                                                            
      NC = N                                                            
      I1 = NPTS                                                         
      I2 = I1 + NPTS                                                    
      I3 = I2 + N1                                                      
      I4 = I3 + N1                                                      
      CALL MOVEFR(N1, Q, X)                                             
      CALL MOVEFR(M1, P, X(N2))                                         
      X(NN) = ZERO                                                      
      CALL SETR(4*NPTS, ZERO, BC)                                       
      DO 10 I = 1, I2                                                   
 10      BC(2,I) = BIG                                                  
      DO 20 I = 1, N1                                                   
         BX(2,I) = ONE                                                  
         BX(1,I) = -ONE                                                 
 20      CONTINUE                                                       
      DO 30 I = N2, NN                                                  
         BX(2,I) = BIG                                                  
         BX(1,I) = -BIG                                                 
 30      CONTINUE                                                       
      CALL SETR(NN, ZERO, G)                                            
      G(NN) = ONE                                                       
      CALL MOVEFR(NPTS, MESH, A)                                        
      CALL MOVEFR(NPTS, FN, A(NPTS+1))                                  
      CALL MOVEFR(NPTS, QK, A(2*NPTS+1))                                
C/6S                                                                    
C     IF (DELK .LE. ZERO)                                               
C    1   CALL SETERR(25H L5STP - NONPOSITIVE DELK, 25, 3, 2)            
C/7S                                                                    
      IF (DELK .LE. ZERO)                                               
     1   CALL SETERR(' L5STP - NONPOSITIVE DELK', 25, 3, 2)             
C/                                                                      
      A(3*NPTS+1) = DELK                                                
      MAXMN = MAX0(M, N)                                                
      MAXMN1 = MAXMN + 1                                                
      DO 50 I = 1, NPTS                                                 
         K = I + NPTS                                                   
         Z = MESH(I)                                                    
         FNI = FN(I)                                                    
         QZ = QK(I)                                                     
         FDELK = DELK + FNI                                             
         CALL  T5COF(Z, A(1), A(NPTS), MAXMN, C(1,I))                   
         CALL V7CPY(MAXMN1, C(1,K), C(1,I))                             
         J = M1                                                         
 40         JP = J + N1                                                 
            C(JP,I) = -C(J,I)                                           
            C(JP,K) =  C(J,K)                                           
            J = J - 1                                                   
            IF (J .GE. 1) GO TO 40                                      
         CALL  V7SCL(N1, C(1,I), DELK+FNI, C(1,I))                      
         CALL  V7SCL(N1, C(1,K), DELK-FNI, C(1,K))                      
         C(NN,I) = QZ                                                   
         C(NN,K) = QZ                                                   
 50      CONTINUE                                                       
      CTX = ZERO                                                        
C                                                                       
C   SOLVE THE LP PROBLEM   MIN G(T)X SUBJECT TO C*X .GE. 0              
C   AND -1 .LE. X(I) .LE. 1 FOR I = 1(1)M+1 (THE Q COEFFICIENTS).       
C                                                                       
      CALL L7PF(BC, BX, C, G, IW, LIW, LW, MM, NN, NN, W, X)            
C/6S                                                                    
C       IF (IW(1) .NE. 0)                                               
C    1      CALL SETERR(27HFUNNY RETURN CODE FROM L7PF, 27, IW(0), 2)   
C/7S                                                                    
        IF (IW(1) .NE. 0)                                               
     1      CALL SETERR('FUNNY RETURN CODE FROM L7PF', 27, IW(0), 2)    
C/                                                                      
      CTXNEW = -X(NN)                                                   
      IF (NERROR(IER) .NE. 0) CALL ERROFF                               
      IF (CTX .GE. CTXNEW) GO TO 150                                    
         QLRG = ZERO                                                    
         J = 1                                                          
            GO TO 70                                                    
 60         J = J+1                                                     
 70         IF (J .GT. N+1) GO TO 80                                    
            IF (QLRG .LT.  ABS(X(J))) QLRG =  ABS(X(J))                 
            GO TO 60                                                    
 80      J = 1                                                          
            GO TO 100                                                   
 90         J = J+1                                                     
 100        IF (J .GT. N+1) GO TO 110                                   
            Q(J) = X(J)/QLRG                                            
            GO TO 90                                                    
 110     I = 0                                                          
         J = N+2                                                        
            GO TO 130                                                   
 120        J = J+1                                                     
 130        IF (J .GT. M+N+2) GO TO 140                                 
            I = I+1                                                     
            P(I) = X(J)/QLRG                                            
            GO TO 120                                                   
 140     CONTINUE                                                       
         GO TO 999                                                      
C/6S                                                                    
C150     CALL SETERR(44H L5STP - NO IMPROVEMENT IN THE LP SUBPROBLEM,   
C    1               44, 4, 1)                                          
C/7S                                                                    
 150     CALL SETERR(' L5STP - NO IMPROVEMENT IN THE LP SUBPROBLEM',    
     1               44, 4, 1)                                          
C/                                                                      
 999  RETURN                                                            
C *** LAST LINE OF  L9STP FOLLOWS ***                                   
      END                                                               
      SUBROUTINE LPPH2(A, M, N, AMAN, B, C, X, MAXITR, CTX)             
      INTEGER M, N                                                      
      EXTERNAL AMAN                                                     
      INTEGER MAXITR                                                    
      REAL A(1), B(M), C(N), X(N), CTX                                  
      COMMON /CSTAK/ DSTAK                                              
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER WPTR, QPTR, LTPTR, PPTR, VPTR, SCLPTR                     
      INTEGER LSTPTR, ISTAK(1000), ISTKGT                               
      REAL WS(500)                                                      
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), WS(1))                                     
C   LPPH2 IS A REAL PROCEDURE FOR SOLVING                               
C   PHASE 2 OF A LINEAR PROGRAMMING PROBLEM.                            
C   SPECIFICALLY, THIS ROUTINE FINDS A VECTOR X WHICH                   
C   SOLVES THE CANONICAL PROBLEM                                        
C                   MAXIMIZE     C(T)X                                  
C                   SUBJECT TO   AX .GE. B                              
C                                                                       
C   THIS ROUTINE STARTS WITH A VECTOR X WHICH IS ALREADY                
C   FEASIBLE, THAT IS WHICH SATISFIES THE CONSTRAINTS.                  
C   THE ALGORITHM IS A GENERALIZATION OF THE STEEPEST EDGE SIMPLEX      
C   ALGORITHM.  AN IMPORTANT FEATURE OF THIS PACKAGE WITH               
C   RESPECT TO NUMERICAL STABILITY IS THAT THE BASIS MATRIX IS          
C   FACTORED INTO THE PRODUCT OF A LEFT TRIANGULAR MATRIX TIMES         
C   AN ORTHOGONAL MATRIX.  THIS FACTORIZATION IS UPDATED AT EACH        
C   PIVOT BY AN O(N**2) ALGORITHM USING GIVENS ROTATIONS.               
C   INPUT-                                                              
C   A      - THE CONSTRAINT MATRIX.                                     
C   M      - THE ROW DIMENSION OF A.                                    
C   N      - THE COLUMN DIMENSION OF A.  NOTE THAT N MUST BE            
C            LESS THAN OR EQUAL TO M.                                   
C   AMAN   - A PROCEDURE THAT MANIPULATES THE MATRIX A.                 
C              SINCE ALL REFERENCES TO THE MATRIX A ARE VIA AMAN,       
C              THE USER MAY USE ANY DATA STRUCTURE FOR REPRESENTING     
C              A THAT EXPLOITS ANY SPARSITY.  IN THE EVENT THAT A       
C              IS STORED AS AN M BY N MATRIX, A DEFAULT PROCEDURE       
C              LPMAN  MAY BE USED.                                      
C   B      - THE RIGHT HAND SIDE.                                       
C   C      - THE COST FUNCTIONAL.                                       
C   X      - AN INITIAL APPROXIMATION TO THE SOLUTION.                  
C            THIS INITIAL VECTOR MUST BE FEASIBLE, THAT IS              
C            AX .GE. B  MUST BE TRUE.                                   
C   MAXITR - THE MAXIMUM NUMBER OF ALLOWABLE ITERATIONS.                
C            EXPERIANCE INDICATES THAT WELL BEHAVED PROBLEMS            
C            ARE SOLVED WITH FEWER THAN 3*M ITERATIONS.                 
C   OUTPUT-                                                             
C   X      - THE SOLUTION.                                              
C   CTX    - THE VALUE OF THE COST FUNCTIONAL EVALUATED AT X.           
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
C    1   25HLPPH2 - INVALID DIMENSION, 26, 1, 2)                        
C     IF (M .LT. N) CALL SETERR(25HLPPH2 - INVALID DIMENSION, 26, 2, 2) 
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
     1   'LPPH2 - INVALID DIMENSION', 26, 1, 2)                         
      IF (M .LT. N) CALL SETERR('LPPH2 - INVALID DIMENSION', 26, 2, 2)  
C/                                                                      
C   ALLOCATE THE WORKING STORAGE.                                       
      WPTR = ISTKGT(M, 3)                                               
      QPTR = ISTKGT(N**2, 3)                                            
      LTPTR = ISTKGT(N**2, 3)                                           
      PPTR = ISTKGT(N, 3)                                               
      VPTR = ISTKGT(M, 3)                                               
      SCLPTR = ISTKGT(M, 3)                                             
      LSTPTR = ISTKGT(2*M+2, 2)                                         
      CALL L9PH2(A, M, N, AMAN, B, C, X, MAXITR, CTX, WS(WPTR), WS(QPTR)
     1   , WS(LTPTR), WS(PPTR), WS(VPTR), WS(SCLPTR), ISTAK(LSTPTR))    
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE L9PH2(A, M, N, AMAN, B, C, X, MAXITR, CTX, W, Q        
     1   , LT, P, V, SCALE, LLIST)                                      
      INTEGER M, N                                                      
      EXTERNAL AMAN                                                     
      INTEGER MAXITR, LLIST(2, 1)                                       
      REAL A(1), B(M), C(N), X(N), CTX, W(M)                            
      REAL Q(N, N), LT(N, N), P(N), V(M), SCALE(M)                      
      COMMON /L5COM/ COND, BOUND, ITRPH1, ITRPH2                        
      INTEGER ITRPH1, ITRPH2                                            
      REAL COND, BOUND                                                  
      INTEGER TOP, BOTTOM, KK, INDX1, INDX2, L5NKF                      
      INTEGER I                                                         
      REAL FLOAT, R1MACH, SNRM2, EPS, THETA, UMAX                       
      REAL V5MAX, XNRM, CNRM, PNRM, TEMP1                               
      LOGICAL UNBNDD                                                    
C   INITIALIZATION.                                                     
      EPS = FLOAT(N)*R1MACH(4)                                          
      TOP = M+1                                                         
      XNRM = SNRM2(N, X, 1)                                             
      CNRM = SNRM2(N, C, 1)                                             
      DO  1 I = 1, M                                                    
         CALL AMAN(.TRUE., A, M, N, I, X, TEMP1)                        
         W(I) = B(I)-TEMP1                                              
         CALL AMAN(.FALSE., A, M, N, I, P, TEMP1)                       
         SCALE(I) = SNRM2(N, P, 1)                                      
C/6S                                                                    
C        IF (EPS*SCALE(I)*XNRM .LT. W(I)) CALL SETERR(                  
C    1      28HLPPH2 - INITIAL X INFEASIBLE, 29, 3, 2)                  
C/7S                                                                    
         IF (EPS*SCALE(I)*XNRM .LT. W(I)) CALL SETERR(                  
     1      'LPPH2 - INITIAL X INFEASIBLE', 29, 3, 2)                   
C/                                                                      
   1     CONTINUE                                                       
      KK = 0                                                            
      CALL SETR(N**2, 0.0E0, Q)                                         
      TEMP1 = 0.0E0                                                     
      DO  2 I = 1, N                                                    
         Q(I, I) = 1.0E0                                                
         TEMP1 = TEMP1+C(I)*X(I)                                        
   2     CONTINUE                                                       
      CTX = TEMP1                                                       
      LLIST(1, M+1) = 1                                                 
      LLIST(2, 1) = M+1                                                 
      DO  3 I = 1, M                                                    
         LLIST(1, I) = I+1                                              
         LLIST(2, I+1) = I                                              
   3     CONTINUE                                                       
      ITRPH2 = 1                                                        
         GOTO  5                                                        
   4     ITRPH2 = ITRPH2+1                                              
   5     IF (ITRPH2 .GT. MAXITR) GOTO  29                               
         XNRM = SNRM2(N, X, 1)                                          
C   STEP 1-    APPEND THE NEW ACTIVE CONSTRAINTS.                       
         INDX1 = L5NKF(TOP, LLIST, KK)                                  
         I = LLIST(1, INDX1)                                            
            GOTO  7                                                     
   6        I = LLIST(1, I)                                             
   7        IF (I .GE. TOP) GOTO  8                                     
            IF (W(I) .LT. (-EPS)*SCALE(I)*XNRM) GOTO  6                 
            CALL AMAN(.FALSE., A, M, N, I, P, TEMP1)                    
            CALL M5TOP(N, N, Q, 1, N, 1, N, P, 1, P)                    
            PNRM = SNRM2(N-KK, P(KK+1), 1)                              
            IF (PNRM .LE. EPS*SCALE(I)) GOTO  6                         
            CALL L5NKD(LLIST, I)                                        
            CALL L5NKI(LLIST, I, INDX1)                                 
            INDX1 = I                                                   
            CALL C5APP(N, KK, Q, LT, KK+1, P)                           
            IF (KK .EQ. N) GOTO  8                                      
            GOTO  6                                                     
C   STEP 2-    DELETE AN OLD CONSTRAINT IF POSSIBLE.                    
   8     IF (0 .GE. KK) GOTO 10                                         
            CALL M5TOP(N, N, Q, 1, KK, 1, N, C, 1, P)                   
            CALL M5TOP(N, N, LT, 1, KK, 1, KK, P, 4, V)                 
            UMAX = V5MAX(KK, V, INDX2)                                  
            IF (EPS*CNRM .GE. UMAX) GOTO 9                              
               CALL C5DRP(N, KK, Q, LT, INDX2)                          
               I = L5NKF(TOP, LLIST, INDX2)                             
               CALL L5NKD(LLIST, I)                                     
               BOTTOM = LLIST(2, TOP)                                   
               CALL L5NKI(LLIST, I, BOTTOM)                             
   9        CONTINUE                                                    
C   STEP 3-    COMPUTE THE GRADIENT PROJECTION.                         
  10     IF (KK .NE. 0) GOTO 12                                         
            DO  11 I = 1, N                                             
               P(I) = C(I)                                              
  11           CONTINUE                                                 
            GOTO  14                                                    
  12        IF (EPS*CNRM .LT. UMAX) CALL M5TOP(N, N, Q, 1, KK, 1, N, C  
     1         , 1, P)                                                  
            CALL M5TOP(N, N, Q, 1, KK, 1, N, P, 2, P)                   
            DO  13 I = 1, N                                             
               P(I) = C(I)-P(I)                                         
  13           CONTINUE                                                 
  14     PNRM = SNRM2(N, P, 1)                                          
         IF (KK .EQ. N .OR. PNRM .LT. EPS*CNRM) RETURN                  
C   STEP 4-    FIND THE NEXT CONSTRAINT ALONG THE                       
C              GRADIENT PROJECTION.                                     
         UNBNDD = .TRUE.                                                
         INDX1 = L5NKF(TOP, LLIST, KK)                                  
         I = LLIST(1, INDX1)                                            
            GOTO  16                                                    
  15        I = LLIST(1, I)                                             
  16        IF (I .GE. TOP) GOTO  20                                    
            CALL AMAN(.TRUE., A, M, N, I, P, V(I))                      
            IF (V(I) .GE. (-EPS)*SCALE(I)*PNRM) GOTO 19                 
               IF (.NOT. UNBNDD) GOTO 17                                
                  UNBNDD = .FALSE.                                      
                  THETA = W(I)/V(I)                                     
                  GOTO  18                                              
  17              IF (V(I)*THETA .LT. W(I)) THETA = W(I)/V(I)           
  18        CONTINUE                                                    
  19        CONTINUE                                                    
            GOTO  15                                                    
  20     IF (.NOT. UNBNDD) GOTO 21                                      
C/6S                                                                    
C           CALL SETERR(26HLPPH2 - UNBOUNDED SOLUTION, 27, 4, 1)        
C/7S                                                                    
            CALL SETERR('LPPH2 - UNBOUNDED SOLUTION', 27, 4, 1)         
C/                                                                      
            RETURN                                                      
C   STEP 5-    UPDATE THE CURRENT SOLUTION.                             
  21     TEMP1 = 0.0E0                                                  
         DO  22 I = 1, N                                                
            TEMP1 = TEMP1+C(I)*P(I)                                     
  22        CONTINUE                                                    
         IF (0.0E0 .GT. TEMP1) GOTO 23                                  
            CTX = CTX+THETA*TEMP1                                       
            GOTO  24                                                    
C/6S                                                                    
C 23        CALL SETERR(35HLPPH2 - TERMINATED FOR CONDITIONING, 36, 7, 1
C    1         )                                                        
C/7S                                                                    
  23        CALL SETERR('LPPH2 - TERMINATED FOR CONDITIONING', 36, 7, 1 
     1         )                                                        
C/                                                                      
            RETURN                                                      
  24     DO  25 I = 1, N                                                
            X(I) = X(I)+THETA*P(I)                                      
  25        CONTINUE                                                    
         I = LLIST(1, INDX1)                                            
            GOTO  27                                                    
  26        I = LLIST(1, I)                                             
  27        IF (I .GE. TOP) GOTO  28                                    
            W(I) = W(I)-THETA*V(I)                                      
            GOTO  26                                                    
  28     CONTINUE                                                       
         GOTO  4                                                        
C/6S                                                                    
C 29  CALL SETERR(44HLPPH2 - NUMBER OF ITERATIONS EXCEEDED MAXITR, 45, 6
C    1   , 1)                                                           
C/7S                                                                    
  29  CALL SETERR('LPPH2 - NUMBER OF ITERATIONS EXCEEDED MAXITR', 45, 6 
     1   , 1)                                                           
C/                                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE C5APP(M, N, Q, R, INDEX, B)                            
      INTEGER M                                                         
      INTEGER N, INDEX                                                  
      REAL Q(M, M), R(M, 1), B(M)                                       
      INTEGER J, I                                                      
      REAL ALFA, BETA                                                   
C   IF A IS AN MXN MATRIX DEFINED BY QA = R, WHERE Q IS                 
C   AN ORTHOGONAL MATRIX AND R IS A RIGHT TRIANGULAR MATRIX,            
C   THEN THIS PROCEDURE OBTAINS THE Q(T)R FACTORIZATION                 
C   OF THE MATRIX DETERMINED BY APPENDING A COLUMN TO A.                
C   THE COLUMN OF A WHICH IS BEING APPENDED IS DEFINED BY               
C   Q(T)B.  THIS NEW FACTORIZATION IS OBTAINED BY AN                    
C   EFFICIENT UPDATE PROCESS.                                           
C   INPUT-                                                              
C   M      - THE COLUMN DIMENSION OF Q AND R.                           
C   N      - THE ROW DIMENSION OF R (N .LT. M).                         
C   Q      - AN MXM ORTHOGONAL MATRIX.                                  
C   R      - AN MXN RIGHT TRIANGULAR MATRIX.                            
C   INDEX  - THE INDEX OF THE COLUMN TO BE ENTERED.                     
C   B      - THE NEW COLUMN.  IF A IS THE NEW COLUMN  OF                
C          - THE MATRIX A, THEN  B = QA.                                
C   OUTPUT-                                                             
C   N      - THE NEW ROW DIMENSION OF R.                                
C   Q      - THE NEW ORTHOGONAL MATRIX.                                 
C   R      - THE NEW RIGHT TRIANGULAR MATRIX.                           
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LT. 0 .OR. M .LE. N) CALL SETERR(            
C    1   26HC5APP  - INVALID DIMENSION, 26, 1, 2)                       
C     IF (INDEX .LE. 0 .OR. N+1 .LT. INDEX) CALL SETERR(                
C    1   22HC5APP  - INVALID INDEX, 22, 2, 2)                           
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LT. 0 .OR. M .LE. N) CALL SETERR(            
     1   'C5APP  - INVALID DIMENSION', 26, 1, 2)                        
      IF (INDEX .LE. 0 .OR. N+1 .LT. INDEX) CALL SETERR(                
     1   'C5APP  - INVALID INDEX', 22, 2, 2)                            
C/                                                                      
      J = N                                                             
         GOTO  2                                                        
   1     J = J-1                                                        
   2     IF (INDEX .GT. J) GOTO  3                                      
         CALL MOVEBR(M, R(1, J), R(1, J+1))                             
         GOTO  1                                                        
   3  N = N+1                                                           
      CALL MOVEFR(M, B, R(1, INDEX))                                    
      I = M-1                                                           
         GOTO  5                                                        
   4     I = I-1                                                        
   5     IF (INDEX .GT. I) GOTO  6                                      
         CALL SROTG(R(I, INDEX), R(I+1, INDEX), ALFA, BETA)             
         CALL SROT(N-INDEX, R(I, INDEX+1), M, R(I+1, INDEX+1), M, ALFA  
     1      , BETA)                                                     
         CALL SROT(M, Q(I, 1), M, Q(I+1, 1), M, ALFA, BETA)             
         GOTO  4                                                        
   6  RETURN                                                            
      END                                                               
      SUBROUTINE C5DRP(M, N, Q, R, INDEX)                               
      INTEGER M                                                         
      INTEGER N, INDEX                                                  
      REAL Q(M, M), R(M, 1)                                             
      INTEGER J, I                                                      
      REAL ALFA, BETA                                                   
C   IF A IS AN MXN MATRIX DEFINED BY QA = R, WHERE Q IS                 
C   AN ORTHOGONAL MATRIX AND R IS A RIGHT TRIANGULAR MATRIX,            
C   THEN THIS PROCEDURE OBTAINS THE Q(T)R FACTORIZATION                 
C   OF THE MATRIX DETERMINED BY DELETING A COLUMN OF A.                 
C   THIS NEW FACTORIZATION IS OBTAINED BY AN EFFICIENT UPDATE           
C   PROCESS.                                                            
C   INPUT-                                                              
C   M      - THE COLUMN DIMENSION OF Q AND R.                           
C   N      - THE ROW DIMENSION OF R (N .LE. M).                         
C   Q      - AN MXM ORTHOGONAL MATRIX.                                  
C   R      - AN MXN RIGHT TRIANGULAR MATRIX.                            
C   INDEX  - THE INDEX OF THE COLUMN TO BE DELETED.                     
C   OUTPUT-                                                             
C   N      - THE NEW ROW DIMENSION OF R.                                
C   Q      - THE NEW ORTHOGONAL MATRIX.                                 
C   R      - THE NEW RIGHT TRIANGULAR MATRIX.                           
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LE. 0 .OR. M .LT. N) CALL SETERR(            
C    1   26HC5DRP  - INVALID DIMENSION, 26, 1, 2)                       
C     IF (INDEX .LE. 0 .OR. N .LT. INDEX) CALL SETERR(                  
C    1   22HC5DRP  - INVALID INDEX, 22, 2, 2)                           
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LE. 0 .OR. M .LT. N) CALL SETERR(            
     1   'C5DRP  - INVALID DIMENSION', 26, 1, 2)                        
      IF (INDEX .LE. 0 .OR. N .LT. INDEX) CALL SETERR(                  
     1   'C5DRP  - INVALID INDEX', 22, 2, 2)                            
C/                                                                      
      J = INDEX+1                                                       
         GOTO  2                                                        
   1     J = J+1                                                        
   2     IF (J .GT. N) GOTO  3                                          
         CALL MOVEFR(J, R(1, J), R(1, J-1))                             
         GOTO  1                                                        
   3  N = N-1                                                           
      I = INDEX                                                         
         GOTO  5                                                        
   4     I = I+1                                                        
   5     IF (I .GT. N) GOTO  6                                          
         CALL SROTG(R(I, I), R(I+1, I), ALFA, BETA)                     
         CALL SROT(N-I, R(I, I+1), M, R(I+1, I+1), M, ALFA, BETA)       
         CALL SROT(M, Q(I, 1), M, Q(I+1, 1), M, ALFA, BETA)             
         GOTO  4                                                        
   6  RETURN                                                            
      END                                                               
      SUBROUTINE M5TOP(M, N, B, ILO, IHI, JLO, JHI, X, IOP, Y)          
      INTEGER M, N                                                      
      INTEGER ILO, IHI, JLO, JHI, IOP                                   
      REAL B(M, N), X(1), Y(1)                                          
      COMMON /CSTAK/ DSTAK                                              
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER MM, HPTR, ISTAK(1000), ISTKGT                             
      REAL WS(500)                                                      
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), WS(1))                                     
C   THIS PROCEDURE PERFORMS THE FOLLOWING MATRIX VECTOR COMPUTATIONS    
C   CASE 1-  Y = CX,                                                    
C   CASE 2-  Y = C(T)X,                                                 
C   CASE 3-  Y = RX,                                                    
C   CASE 4-  Y = R(-1)X.                                                
C   THE MATRIX C IS THE RECTANGULAR SUBMATRIX OF B SPECIFIED            
C   BY ILO,IHI,JLO, AND JHI.  THE MATRIX R IS THE RIGHT TRIANGULAR      
C   SUBMATRIX OF B SPECIFIED BY THE SAME INDICES.  THE RIGHT TRIANGULAR 
C   SUBMATRIX MUST BE SQUARE AND CENTERED.                              
C   INPUT-                                                              
C   M      - THE COLUMN DIMENSION OF THE MATRIX.                        
C   N      - THE ROW DIMENSION OF THE MATRIX.                           
C   B      - AN MXN MATRIX                                              
C   ILO    - THE SMALLER RWO INDEX.                                     
C   IHI    - THE LARGER ROW INDEX.                                      
C   JLO    - THE SMALLER COLUMN INDEX.                                  
C   JHI    - THE LARGER COLUMN INDEX.                                   
C   X      - A VECTOR OF LENGTH N FOR CASES 1, 3 AND 4, AND             
C            A VECTOR OF LENGTH M FOR CASE 2.                           
C   IOP    - AN INDEX FOR THE MATRIX VECTOR OPERATION TO BE             
C            PERFORMED.                                                 
C   OUTPUT-                                                             
C   Y      - A VECTOR OF LENGTH M FOR CASES 1, 3 AND 4, AND             
C            A VECTOR OF LENGTH N FOR CASE 2.  THE PROGRAM              
C            IS ORGANIZED SO THAT X AND Y MAY SHARE THE SAME            
C            STORAGE.                                                   
      CALL ENTER(0)                                                     
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
C    1   26HM5TOP  - INVALID DIMENSION, 26, 2, 2)                       
C     IF (IHI .LT. ILO .OR. JHI .LT. JLO) CALL SETERR(                  
C    1   24HM5TOP  - INVALID INDICES, 24, 3, 2)                         
C     IF (ILO .LT. 1 .OR. M .LT. IHI .OR. JLO .LT. 1 .OR. N .LT. JHI)   
C    1   CALL SETERR(26HM5TOP  - INVALID SUBMATRIX, 26, 4, 2)           
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
     1   'M5TOP  - INVALID DIMENSION', 26, 2, 2)                        
      IF (IHI .LT. ILO .OR. JHI .LT. JLO) CALL SETERR(                  
     1   'M5TOP  - INVALID INDICES', 24, 3, 2)                          
      IF (ILO .LT. 1 .OR. M .LT. IHI .OR. JLO .LT. 1 .OR. N .LT. JHI)   
     1   CALL SETERR('M5TOP  - INVALID SUBMATRIX', 26, 4, 2)            
C/                                                                      
      GOTO  5                                                           
   1     MM = M                                                         
         GOTO  6                                                        
   2     MM = N                                                         
         GOTO  6                                                        
C/6S                                                                    
C  3     IF (ILO .NE. JLO .OR. IHI .NE. JHI) CALL SETERR(               
C    1      25HM5TOP  - INVALID TRIANGLE, 25, 5, 2)                     
C/7S                                                                    
   3     IF (ILO .NE. JLO .OR. IHI .NE. JHI) CALL SETERR(               
     1      'M5TOP  - INVALID TRIANGLE', 25, 5, 2)                      
C/                                                                      
         MM = M                                                         
         GOTO  6                                                        
C/6S                                                                    
C  4     CALL SETERR(20HM5TOP  - INVALID IOP, 21, 1, 2)                 
C/7S                                                                    
   4     CALL SETERR('M5TOP  - INVALID IOP', 21, 1, 2)                  
C/                                                                      
         GOTO  6                                                        
   5     IF (IOP .EQ. 4) GOTO  3                                        
         IF (IOP .EQ. 3) GOTO  3                                        
         IF (IOP .EQ. 2) GOTO  2                                        
         IF (IOP .EQ. 1) GOTO  1                                        
         GOTO  4                                                        
   6  HPTR = ISTKGT(MM, 3)                                              
      CALL M6TOP(M, N, B, ILO, IHI, JLO, JHI, X, IOP, Y, WS(HPTR))      
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE M6TOP(M, N, B, ILO, IHI, JLO, JHI, X, IOP, Y, H)       
      INTEGER M, N                                                      
      INTEGER ILO, IHI, JLO, JHI, IOP                                   
      REAL B(M, N), X(1), Y(1), H(1)                                    
      INTEGER I, J                                                      
      REAL TOL, R1MACH, ABS                                             
      IF (IOP .NE. 1) GOTO 6                                            
         DO  1 I = ILO, IHI                                             
C  COMPUTE  Y = CX.                                                     
            H(I) = 0.0E0                                                
   1        CONTINUE                                                    
         DO  4 J = JLO, JHI                                             
            IF (X(J) .EQ. 0.0E0) GOTO 3                                 
               DO  2 I = ILO, IHI                                       
                  H(I) = H(I)+B(I, J)*X(J)                              
   2              CONTINUE                                              
   3        CONTINUE                                                    
   4        CONTINUE                                                    
         DO  5 I = ILO, IHI                                             
            Y(I) = H(I)                                                 
   5        CONTINUE                                                    
         GOTO  29                                                       
   6     IF (IOP .NE. 2) GOTO 12                                        
            DO  7 J = JLO, JHI                                          
C  COMPUTE  Y = C(T)X.                                                  
               H(J) = 0.0E0                                             
   7           CONTINUE                                                 
            DO  10 I = ILO, IHI                                         
               IF (X(I) .EQ. 0.0E0) GOTO 9                              
                  DO  8 J = JLO, JHI                                    
                     H(J) = H(J)+B(I, J)*X(I)                           
   8                 CONTINUE                                           
   9           CONTINUE                                                 
  10           CONTINUE                                                 
            DO  11 J = JLO, JHI                                         
               Y(J) = H(J)                                              
  11           CONTINUE                                                 
            GOTO  28                                                    
  12        IF (IOP .NE. 3) GOTO 18                                     
               DO  13 I = ILO, IHI                                      
C  COMPUTE  Y = RX.                                                     
                  H(I) = 0.0E0                                          
  13              CONTINUE                                              
               DO  16 J = ILO, IHI                                      
                  IF (X(J) .EQ. 0.0E0) GOTO 15                          
                     DO  14 I = J, IHI                                  
                        H(I) = H(I)+B(I, J)*X(J)                        
  14                    CONTINUE                                        
  15              CONTINUE                                              
  16              CONTINUE                                              
               DO  17 I = ILO, IHI                                      
                  Y(I) = H(I)                                           
  17              CONTINUE                                              
               GOTO  27                                                 
  18           TOL = 1.0E2*R1MACH(1)                                    
C  SOLVE RY = X.                                                        
               DO  19 I = ILO, IHI                                      
                  Y(I) = X(I)                                           
  19              CONTINUE                                              
               I = IHI                                                  
                  GOTO  21                                              
  20              I = I-1                                               
  21              IF (I .LT. ILO) GOTO  26                              
                  J = I+1                                               
                     GOTO  23                                           
  22                 J = J+1                                            
  23                 IF (J .GT. IHI) GOTO  24                           
                     Y(I) = Y(I)-B(I, J)*Y(J)                           
                     GOTO  22                                           
  24              IF (ABS(B(I, I)) .GE. TOL) GOTO 25                    
C/6S                                                                    
C                    CALL SETERR(24HM5TOP  - SINGULAR SYSTEM, 24, 6, 1) 
C/7S                                                                    
                     CALL SETERR('M5TOP  - SINGULAR SYSTEM', 24, 6, 1)  
C/                                                                      
                     RETURN                                             
  25              Y(I) = Y(I)/B(I, I)                                   
                  GOTO  20                                              
  26           CONTINUE                                                 
  27     CONTINUE                                                       
  28  CONTINUE                                                          
  29  RETURN                                                            
      END                                                               
      REAL FUNCTION V5MAX(M, A, INDEX)                                  
      INTEGER M                                                         
      INTEGER INDEX                                                     
      REAL A(M)                                                         
      INTEGER I                                                         
      REAL B                                                            
C  THIS REAL FUNCTION RETURNS THE LARGEST COMPONENT                     
C  OF THE REAL VECTOR A.                                                
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(26HV5MAX  - INVALID DIMENSION, 26, 1, 2)
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('V5MAX  - INVALID DIMENSION', 26, 1, 2) 
C/                                                                      
      B = A(1)                                                          
      INDEX = 1                                                         
      DO  2 I = 1, M                                                    
         IF (A(I) .LE. B) GOTO 1                                        
            B = A(I)                                                    
            INDEX = I                                                   
   1     CONTINUE                                                       
   2     CONTINUE                                                       
      V5MAX = B                                                         
      RETURN                                                            
      END                                                               
      INTEGER FUNCTION L5RGXR(NPTS, EN, NEX, IEXT, ILRG, TOL)           
      INTEGER NPTS, NEX                                                 
      INTEGER IEXT(NEX), ILRG, TOL                                      
      REAL EN(NPTS)                                                     
      INTEGER J, K, L                                                   
      REAL HOLD, ABS                                                    
C    FUNCTION L5RGXR FINDS THE NO. OF ERROR EXTREMA WITH MAGNITUDES     
C    WITHIN TOLERANCE OF MAGNITUDE OF LARGEST ERROR.                    
C/6S                                                                    
C     IF (NPTS .LE. 0) CALL SETERR(24HL5RGXR-INVALID DIMENSION, 24, 1, 2
C    1   )                                                              
C     IF (NEX .LE. 0 .OR. ILRG .LE. 0) CALL SETERR(                     
C    1   20HL5RGXR-INVALID INDEX, 20, 2, 2)                             
C/7S                                                                    
      IF (NPTS .LE. 0) CALL SETERR('L5RGXR-INVALID DIMENSION', 24, 1, 2 
     1   )                                                              
      IF (NEX .LE. 0 .OR. ILRG .LE. 0) CALL SETERR(                     
     1   'L5RGXR-INVALID INDEX', 20, 2, 2)                              
C/                                                                      
      K = 0                                                             
      DO  1 J = 1, NEX                                                  
         L = IEXT(J)                                                    
         HOLD = ABS(EN(ILRG))-ABS(EN(L))                                
         IF (HOLD .LE. 10.0E0**(-TOL)*ABS(EN(ILRG))) K = K+1            
   1     CONTINUE                                                       
      L5RGXR = K                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE C5EQK(NPTS, X, FN, M, N, P, Q, QK, EN)                 
      INTEGER NPTS                                                      
      INTEGER M, N                                                      
      REAL X(NPTS), FN(NPTS), P(1), Q(1), QK(NPTS), EN(NPTS)            
      INTEGER I                                                         
      REAL TCHBP, PK                                                    
C   PROCEDURE C5EQK  COMPUTES EN AND QK.                                
C   EN=ERROR VALUES AT MESH POINTS.                                     
C   QK=VALUE OF DENOMINATOR POLYNOMIAL AT MESH POINTS.                  
C/6S                                                                    
C     IF (NPTS .LE. 0 .OR. M .LT. 0 .OR. N .LT. 0) CALL SETERR(         
C    1   23HC5EQK-INVALID DIMENSION, 23, 1, 2)                          
C/7S                                                                    
      IF (NPTS .LE. 0 .OR. M .LT. 0 .OR. N .LT. 0) CALL SETERR(         
     1   'C5EQK-INVALID DIMENSION', 23, 1, 2)                           
C/                                                                      
      DO  1 I = 1, NPTS                                                 
         QK(I) = TCHBP(N, Q, X(I), X(1), X(NPTS))                       
C/6S                                                                    
C        IF (QK(I) .EQ. 0.0E0) CALL SETERR(21HC5EQK-DIVISOR .EQ. 0., 21,
C    1      2, 2)                                                       
C/7S                                                                    
         IF (QK(I) .EQ. 0.0E0) CALL SETERR('C5EQK-DIVISOR .EQ. 0.', 21, 
     1      2, 2)                                                       
C/                                                                      
         PK = TCHBP(M, P, X(I), X(1), X(NPTS))                          
         EN(I) = (FN(I)*QK(I)-PK)/QK(I)                                 
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE D5FMT(INPROD, A, MM, NN, IROW, X, DINPRD)              
      INTEGER NN                                                        
      INTEGER MM, IROW                                                  
      REAL A(1), X(NN), DINPRD                                          
      LOGICAL INPROD                                                    
      COMMON /D5FCM/ NPTS, M, N, I1, I2, I3, I4                         
      INTEGER NPTS, M, N, I1, I2, I3                                    
      INTEGER I4                                                        
      INTEGER IRM1, IRM2, IRM3, ZPTR, FNPTR, QZPTR                      
      INTEGER JP, MAXMN, J, MAX0                                        
      REAL FCT, FDELK, DELK, Z, FN, QZ                                  
      REAL TCHBP                                                        
C   D5FMT  HANDLES REFERENCES BY THE LP ROUTINE TO                      
C   THE MATRIX FOR THE LINEAR PROGRAMMING SUBPROBLEM.                   
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (MM .NE. I4 .OR. NN .NE. M+N+3) CALL SETERR(                   
C    1   26HD5FMT  - INVALID DIMENSION, 26, 1, 2)                       
C     IF (IROW .LT. 0 .OR. MM .LT. IROW) CALL SETERR(                   
C    1   22HD5FMT  - INVALID INDEX, 22, 2, 2)                           
C/7S                                                                    
      IF (MM .NE. I4 .OR. NN .NE. M+N+3) CALL SETERR(                   
     1   'D5FMT  - INVALID DIMENSION', 26, 1, 2)                        
      IF (IROW .LT. 0 .OR. MM .LT. IROW) CALL SETERR(                   
     1   'D5FMT  - INVALID INDEX', 22, 2, 2)                            
C/                                                                      
      IRM1 = IROW-I1                                                    
      IRM2 = IROW-I2                                                    
      IRM3 = IROW-I3                                                    
      IF ((.NOT. INPROD) .OR. I2 .GE. IROW) GOTO 3                      
         IF (I3 .GE. IROW) GOTO 1                                       
            DINPRD = -X(IRM3)                                           
            GOTO  2                                                     
   1        DINPRD = X(IRM2)                                            
   2     CONTINUE                                                       
         GOTO  18                                                       
   3     IF (I2 .GE. IROW) GOTO 6                                       
            CALL SETR(NN, 0.0E0, X)                                     
            IF (I3 .GE. IROW) GOTO 4                                    
               X(IRM3) = -1.0E0                                         
               GOTO  5                                                  
   4           X(IRM2) = 1.0E0                                          
   5        CONTINUE                                                    
            GOTO  17                                                    
   6        IF (I1 .GE. IROW) GOTO 7                                    
               FCT = -1.0E0                                             
               ZPTR = IRM1                                              
               GOTO  8                                                  
   7           FCT = 1.0E0                                              
               ZPTR = IROW                                              
   8        Z = A(ZPTR)                                                 
            FNPTR = ZPTR+NPTS                                           
            FN = A(FNPTR)                                               
            QZPTR = FNPTR+NPTS                                          
            QZ = A(QZPTR)                                               
            DELK = A(3*NPTS+1)                                          
            FDELK = FCT*FN+DELK                                         
            IF (.NOT. INPROD) GOTO 9                                    
               DINPRD = FDELK*TCHBP(N, X, Z, A(1), A(NPTS))-FCT*TCHBP(M,
     1            X(N+2), Z, A(1), A(NPTS))+QZ*X(NN)                    
               GOTO  16                                                 
   9           MAXMN = MAX0(M, N)                                       
               CALL T5COF(Z, A(1), A(NPTS), MAXMN, X)                   
               J = M+1                                                  
                  GOTO  11                                              
  10              J = J-1                                               
  11              IF (1 .GT. J) GOTO  12                                
                  JP = J+N+1                                            
                  X(JP) = (-FCT)*X(J)                                   
                  GOTO  10                                              
  12           J = 1                                                    
                  GOTO  14                                              
  13              J = J+1                                               
  14              IF (J .GT. N+1) GOTO  15                              
                  X(J) = FDELK*X(J)                                     
                  GOTO  13                                              
  15           X(NN) = QZ                                               
  16        CONTINUE                                                    
  17  CONTINUE                                                          
  18  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE T5COF(X, A, B, DEG, XX)                                
      INTEGER DEG                                                       
      REAL X, A, B, XX(2)                                               
      INTEGER I                                                         
      REAL TWOXX                                                        
C    PROCEDURE T5COF  COMPUTES THE DEG+1 TCHEBYCHEFF                    
C    COEFFICIENTS OF THE POINT X.                                       
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (DEG .LT. 0) CALL SETERR(21HT5COF -INVALID DEGREE, 21, 1, 2)   
C/7S                                                                    
      IF (DEG .LT. 0) CALL SETERR('T5COF -INVALID DEGREE', 21, 1, 2)    
C/                                                                      
      XX(1) = 1.0E0                                                     
      IF (DEG .LE. 0) GOTO 3                                            
         IF (B .GT. A) GOTO 1                                           
C/6S                                                                    
C           CALL SETERR(23HT5COF -INVALID INTERVAL, 23, 2, 2)           
C/7S                                                                    
            CALL SETERR('T5COF -INVALID INTERVAL', 23, 2, 2)            
C/                                                                      
            GOTO  2                                                     
   1        XX(2) = 2.0E0*(X-(A+B)/2.0E0)/(B-A)                         
CSCALE X TO THE INTERVAL (-1.0E0,1.0E0)                                 
   2  CONTINUE                                                          
   3  IF (DEG .GT. 1) TWOXX = 2.0E0*XX(2)                               
      I = 3                                                             
         GOTO  5                                                        
   4     I = I+1                                                        
   5     IF (I .GT. DEG+1) GOTO  6                                      
         XX(I) = TWOXX*XX(I-1)-XX(I-2)                                  
         GOTO  4                                                        
   6  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE L7PF(BC, BX, C, G, IW, LIW, LW, M, P, PP, W, X)        
      INTEGER LIW, M, P, PP, LW                                         
      INTEGER IW(LIW)                                                   
      REAL BC(2, M), BX(2, P), C(PP, M), G(P), W(LW), X(P)              
      EXTERNAL  V2NRM,L7P2                                              
      INTEGER MPP, I, Q, R, S, I1                                       
      INTEGER W1, CN, LR                                                
      REAL  V2NRM, CNI, ONE, ZERO                                       
      DATA ONE/1.E+0/                                                   
      DATA ZERO/0.E+0/                                                  
C LP SOLVER, INEQUALITIES ONLY, STARTING FROM FEASIBLE X.               
C *** LOCAL VARIABLES ***                                               
C *** BODY ***                                                          
      IW(1) = -2                                                        
      MPP = M+P                                                         
      IF (LIW .LT. MPP+7) GOTO  60                                      
      CN = P+2                                                          
      W1 = CN+MPP                                                       
      S = W1+5*P                                                        
      R = S+P                                                           
      LR = P*(P+1)/2                                                    
      Q = R+LR                                                          
      IW(1) = -(Q+P*P)                                                  
      IF (IW(1)+LW .LT. 0) GOTO  60                                     
      IW(1) = 0                                                         
      IW(3) = 0                                                         
      IW(4) = 0                                                         
      IW(5) = 0                                                         
      IW(6) = 0                                                         
      IW(7) = P                                                         
      I1 = MPP+7                                                        
      DO  10 I = 8, I1                                                  
         IW(I) = I-7                                                    
  10     CONTINUE                                                       
      I1 = CN                                                           
      DO  20 I = 1, P                                                   
         W(I1) = ONE                                                    
         I1 = I1+1                                                      
  20     CONTINUE                                                       
      IF (M .LE. 0) GOTO 50                                             
         DO  40 I = 1, M                                                
            CNI =  V2NRM(P, C(1, I))                                    
            IF (CNI .GT. ZERO) GOTO 30                                  
               IW(1) = I+MPP                                            
               GOTO  60                                                 
  30        W(I1) = CNI                                                 
            I1 = I1+1                                                   
  40        CONTINUE                                                    
  50  CALL L7P2(IW(8), BC, BX, C, W(CN), G, IW(1), IW(2), LR, M, IW(3), 
     1   IW(4), IW(5), MPP, MPP, P, IW(7), PP, W(Q), W(R), W(1), W(S), W
     2   (W1), X)                                                       
  60  RETURN                                                            
      END                                                               
      SUBROUTINE L7P2(A, BC, BX, C, CN, G, INFO, ITER, LR, M, M0,       
     1   MC, ME, MPP, MPP0, P, PC, PP, Q, R, RCOND, S, W, X)            
      INTEGER MPP, MPP0, M, P, LR, PP                                   
      INTEGER A(MPP), INFO, ITER, M0, MC, ME                            
      INTEGER PC                                                        
      REAL BC(2, M), BX(2, P), C(PP, M), CN(MPP0), G(P), Q(P            
     1   , P)                                                           
      REAL R(LR), RCOND, S(P), W(P, 3), X(P)                            
      EXTERNAL  L7CHP,  B7VAL,  R7MDC, V2AXY,  D7TP1                    
      INTEGER NCA, NCD, I, J, K, L                                      
      INTEGER MC1                                                       
      LOGICAL MOVE                                                      
      REAL BIG,  B7VAL,  R7MDC, MPMEPS, T                               
      REAL  B1, ZERO, T1, SC,  D7TP1                                    
      DATA BIG/0.E+0/                                                   
      DATA MPMEPS/0.E+0/                                                
      DATA ZERO/0.E+0/                                                  
C *** LOCAL VARIABLES ***                                               
C-------------------------------  BODY  --------------------------------
      IF (BIG .GT. ZERO) GOTO 10                                        
         BIG =  R7MDC(6)                                                
         MPMEPS = (-(FLOAT(P)))* R7MDC(3)                               
  10  MC = P-PC                                                         
      M0 = MC                                                           
      ITER = 0                                                          
  20     ITER = ITER+1                                                  
         CALL  L7CHP(A, .FALSE., C, CN, G, LR, M, M0, MC, ME, MOVE, MPP,
     1      MPP0, NCA, NCD, P, PC, PP, Q, R, RCOND, S, W, W(1, 2), W(1  
     2      , 3))                                                       
         IF (.NOT. MOVE) GOTO  100                                      
         K = 0                                                          
         T = BIG                                                        
         IF (MC .GE. MPP) GOTO 60                                       
            MC1 = MC+1                                                  
            DO  50 I = MC1, MPP                                         
               J = IABS(A(I))                                           
               SC =  D7TP1(C, J, P, PP, S)                              
               IF (SC .EQ. ZERO) GOTO  50                               
               IF (SC .LE. ZERO) GOTO 30                                
                  J = -J                                                
                  SC = -SC                                              
  30           B1 =  B7VAL(BC, BX, J, M, P)                             
               IF ( ABS(B1) .GE. BIG) GOTO  50                          
               T1 = (B1- D7TP1(C, J, P, PP, X))/SC                      
               IF (T .LE. T1) GOTO 40                                   
                  K = I                                                 
                  L = J                                                 
                  T = T1                                                
                  MOVE = T*SC .LT. MPMEPS* ABS(B1)                      
  40           CONTINUE                                                 
  50           CONTINUE                                                 
  60     IF (K .NE. 0) GOTO 70                                          
            INFO = -1                                                   
            GOTO  110                                                   
  70     IF (.NOT. MOVE) GOTO 80                                        
            CALL V2AXY(P, X, T, S, X)                                   
            M0 = MC1                                                    
            A(K) = A(M0)                                                
            A(M0) = L                                                   
            GOTO  20                                                    
  80     IF (K .LE. M0) GOTO 90                                         
            M0 = M0+1                                                   
            A(K) = A(M0)                                                
            A(M0) = L                                                   
            GOTO  20                                                    
  90     A(K) = L                                                       
         GOTO  20                                                       
 100  CONTINUE                                                          
 110  RETURN                                                            
      END                                                               
      SUBROUTINE  L7CHP(A, ADD2QR, C, CN, G, LR, M, M0, MC, ME,         
     1   MOVE, MPP, MPP0, NCA, NCD, P, PC, PP, Q, R, RCOND, S, W, X, Y) 
      INTEGER MPP, MPP0, M, P, LR, PP                                   
      INTEGER A(MPP), M0, MC, ME, NCA, NCD                              
      INTEGER PC                                                        
      LOGICAL ADD2QR, MOVE                                              
      REAL C(PP, M), CN(MPP0), G(P), Q(P, P), R(LR), RCOND              
      REAL S(P), W(P), X(P), Y(P)                                       
      EXTERNAL  V2NRM,  C7COL,  R7MDC,  V7SCL, I7SHFT,  D7TPR           
      EXTERNAL  L7SVX,  L7SVN,  L7ITV,  V7SCP, Q7RGS, V7CPY             
      EXTERNAL V2AXY,  D7TP1,  Q7RS1                                    
      INTEGER MCD, MCSAVE, I, J, K, L                                   
      INTEGER IERR, JA, KA, MC0, MC1                                    
      INTEGER ME1                                                       
      LOGICAL DRDONE, ADDTRY, DEGEN, SAMEB, XZERO                       
      REAL  V2NRM, ONE, EPS, SNI, NEGONE,  R7MDC                        
      REAL TWO,  D7TPR, NPMEPS,  L7SVX,  L7SVN                          
      REAL T, ZETA, MEPS, ZERO, P1, AMIN1                               
      REAL AMAX1, CS, CX, GS, SI, GNORM                                 
      REAL PMEPS, SNORM, GX0,  D7TP1                                    
      INTEGER TEMP                                                      
      DATA NEGONE/-1.E+0/                                               
      DATA ONE/1.E+0/                                                   
      DATA P1/0.1E+0/                                                   
      DATA TWO/2.E+0/                                                   
      DATA ZERO/0.E+0/                                                  
      DATA MEPS/0.E+0/                                                  
C M0 = NO. OF CONSTRAINTS SATISFIED AS EQUALITIES BY S = 0              
C MC = NO. OF CONSTRAINTS SATISFIED AS EQUALITIES BY RETURN VALUE OF S  
C ME = NO. OF EQUALITY CONSTRAINTS                                      
C MOVE IS SET TO TRUE IFF THE S RETURNED IS NONZERO                     
C *** LOCAL VARIABLES ***                                               
C-------------------------------  BODY  --------------------------------
      CALL  V7SCP(P, X, ZERO)                                           
      XZERO = .TRUE.                                                    
      DRDONE = .FALSE.                                                  
      ADDTRY = .TRUE.                                                   
      SAMEB = .FALSE.                                                   
      GNORM =  V2NRM(P, G)                                              
      IF (GNORM .LE. ZERO) GOTO  440                                    
      IF (MEPS .LE. ZERO) MEPS =  R7MDC(3)                              
      PMEPS = (FLOAT(P))*MEPS                                           
      NPMEPS = -PMEPS                                                   
      ME1 = ME+1                                                        
      MC0 = P-PC                                                        
      MCD = MC0                                                         
      NCA = 0                                                           
      NCD = 0                                                           
      RCOND = ONE                                                       
      IF (MC0 .GT. 0) RCOND =  L7SVN(MC0, R, S, S)/ L7SVX(MC0, R, S, S) 
      DEGEN = .FALSE.                                                   
  10  MOVE = .FALSE.                                                    
      MC0 = P-PC                                                        
      CALL  V7SCL(P, S, NEGONE, G)                                      
      IF (MC0 .LE. 0) GOTO 30                                           
         DO  20 I = 1, MC0                                              
            W(I) =  D7TPR(P, Q(1, I), S)                                
            CALL V2AXY(P, S, -W(I), Q(1, I), S)                         
  20        CONTINUE                                                    
  30  SNORM =  V2NRM(P, S)                                              
      IF (PC .LE. 0) GOTO  150                                          
      IF (SNORM .LE. (GNORM/RCOND)*PMEPS) GOTO  150                     
      CALL  V7SCL(P, S, ONE/SNORM, S)                                   
      GS =  D7TPR(P, G, S)                                              
      IF (GS .GE. ZERO) GOTO  140                                       
      IF (XZERO) GOTO 40                                                
         IF ((GS-GX0)/GX0 .LE. PMEPS) GOTO  140                         
  40  MC1 = MC+1                                                        
      K = 0                                                             
      ZETA = ONE                                                        
      IF (.NOT. DRDONE) GOTO 50                                         
         CS =  D7TP1(C, A(MC1), P, PP, S)                               
         IF (CS .LT. ZERO) GOTO  140                                    
         MC1 = MC1+1                                                    
  50  SAMEB = .FALSE.                                                   
      IF (MC1 .GT. M0) GOTO 80                                          
         L = MPP0                                                       
         DO  70 I = MC1, M0                                             
            J = A(I)                                                    
            CS =  D7TP1(C, J, P, PP, S)                                 
            IF (CS .GE. ZERO) GOTO  70                                  
            CX = ZERO                                                   
            IF (.NOT. XZERO) CX = AMAX1(ZERO,  D7TP1(C, J, P, PP, X))   
            T = CX-CS                                                   
            IF (T*ZETA .LT. CX) GOTO  70                                
            IF (.NOT. DEGEN) GOTO 60                                    
               IF (L .LT. J) GOTO  70                                   
               L = J                                                    
  60        ZETA = CX/T                                                 
            K = I                                                       
  70        CONTINUE                                                    
  80  DEGEN = ZETA .LT. P1                                              
      IF (K .NE. 0) GOTO 90                                             
         MOVE = .TRUE.                                                  
         CALL V7CPY(P, X, S)                                            
         GX0 = GS                                                       
         XZERO = .FALSE.                                                
         SAMEB = .TRUE.                                                 
         GOTO  150                                                      
  90  IF (ZETA .LE. PMEPS) GOTO 110                                     
         DO  100 I = 1, P                                               
            X(I) = X(I)+ZETA*(S(I)-X(I))                                
 100        CONTINUE                                                    
         CALL  V7SCL(P, X, ONE/ V2NRM(P, X), X)                         
         GX0 =  D7TPR(P, G, X)                                          
 110  MCD = MC0                                                         
      J = A(K)                                                          
      MC1 = MC0+1                                                       
      ADDTRY = .TRUE.                                                   
      CALL  C7COL(C, J, M, P, PP, Q(1, MC1))                            
      CALL Q7RGS(IERR, A, MC1, P, P, P, MC1, Q, R, Y)                   
      MC = MC+1                                                         
      IF (.NOT. DRDONE) GOTO 120                                        
         I = A(MC)                                                      
         A(MC) = J                                                      
         A(K) = A(MC+1)                                                 
         A(MC+1) = I                                                    
         GOTO  130                                                      
 120     A(K) = A(MC)                                                   
         A(MC) = J                                                      
 130  IF (IERR .NE. 0) GOTO  40                                         
      IF ( D7TPR(P, Q(1, MC1), S) .GE. ZERO) GOTO  40                   
      T =  L7SVN(MC1, R, Y, Y)/ L7SVX(MC1, R, Y, Y)                     
      IF (T .LE. PMEPS) GOTO  40                                        
      DRDONE = .FALSE.                                                  
      RCOND = T                                                         
      PC = PC-1                                                         
      NCA = NCA+1                                                       
      MCD = MCD+1                                                       
      A(MC) = A(MC1)                                                    
      A(MC1) = J                                                        
      CALL V2AXY(P, S, - D7TPR(P, S, Q(1, MC1)), Q(1, MC1), S)          
      GOTO  10                                                          
 140  IF (ADDTRY) GOTO  150                                             
      MC = MCSAVE                                                       
      PC = PC-1                                                         
      MC0 = P-PC                                                        
 150  K = 0                                                             
      IF (ME1 .GT. MCD) GOTO 240                                        
         EPS = PMEPS/RCOND                                              
         CALL  L7ITV(MC0, Y, R, W)                                      
         SNI =  V2NRM(MC0, Y)                                           
         IF (SNI .LE. ZERO) GOTO 230                                    
            SNI = ONE/SNI                                               
            IF (.NOT. DEGEN) GOTO 200                                   
               DO  190 I = ME1, MCD                                     
                  SI = Y(I)                                             
                  IF (SNI*SI .LT. EPS) GOTO  190                        
                  JA = IABS(A(I))                                       
                  IF (K .NE. 0) GOTO 160                                
                     K = I                                              
                     KA = JA                                            
                     GOTO  180                                          
 160                 IF (KA .LE. JA) GOTO 170                           
                        K = I                                           
                        KA = JA                                         
 170              CONTINUE                                              
 180              CONTINUE                                              
 190              CONTINUE                                              
               GOTO  220                                                
 200           T = ZERO                                                 
               DO  210 I = ME1, MCD                                     
                  SI = Y(I)*SNI                                         
                  IF (SI .LT. EPS) GOTO  210                            
                  JA = IABS(A(I))                                       
                  SI = SI/CN(JA)                                        
                  IF (SI .LT. T) GOTO  210                              
                  T = SI                                                
                  K = I                                                 
 210              CONTINUE                                              
 220        CONTINUE                                                    
 230     CONTINUE                                                       
 240  IF (K .NE. 0) GOTO  250                                           
      IF (MOVE) GOTO  460                                               
      IF (XZERO) GOTO  450                                              
      IF (SAMEB) GOTO  420                                              
      GOTO  270                                                         
 250  IF (K .GE. MC0) GOTO 260                                          
         CALL  Q7RS1(K, LR, MC0, P, Q, R, Y)                            
         CALL I7SHFT(MC0, K, A)                                         
 260  MCD = MCD-1                                                       
      J = MC0*(MC0+1)/2                                                 
      K = IABS(A(MC0))                                                  
      IF (( D7TPR(P, Q(1, MC0), G)/GNORM)*(R(J)/CN(K)) .GE. NPMEPS)     
     1   GOTO  150                                                      
      RCOND = ONE                                                       
      MCSAVE = MC                                                       
      MC = MC0-1                                                        
      IF (MC .GT. 0) RCOND =  L7SVN(MC, R, Y, Y)/ L7SVX(MC, R, Y, Y)    
      PC = PC+1                                                         
      NCD = NCD+1                                                       
      ADDTRY = .FALSE.                                                  
      DRDONE = .TRUE.                                                   
      GOTO  10                                                          
C *** SPECIAL CASE OF FINISH WITH MOVE = FALSE AND XZERO = FALSE...     
C ***  MAKE SURE THAT                                                   
C *** (1) S IS ORTHOGONAL TO CONSTRAINTS THE A ARRAY CALLS ACTIVE,      
C *** (2) S DOES NOT MAKE A NEGATIVE INNER PRODUCT WITH CONSTRAINTS     
C ***  THAT THE A ARRAY CALLS INACTIVE,                                 
C *** (3) RCOND CORRESPONDS TO RETURNED R, AND                          
C *** (4) Y = VECTOR OF LAGRANGE MULTIPLIERS.                           
 270  MC0 = P-PC                                                        
      K = MC+1                                                          
      I = MC                                                            
         GOTO  290                                                      
 280     I = I-1                                                        
 290     IF (I .LE. 0) GOTO  340                                        
         J = A(I)                                                       
         IF ( D7TP1(C, J, P, PP, X) .LE. ZERO) GOTO 330                 
            IF (I .GE. MC0) GOTO 300                                    
               CALL  Q7RS1(I, LR, MC0, P, Q, R, Y)                      
               CALL I7SHFT(MC0, I, A)                                   
               GOTO  310                                                
 300           A(I) = A(MC)                                             
               A(MC) = J                                                
 310        IF (I .GT. MC0) GOTO 320                                    
               MC0 = MC0-1                                              
               PC = PC+1                                                
 320        MC = MC-1                                                   
 330     CONTINUE                                                       
         GOTO  280                                                      
 340  IF (PC .LE. 0) GOTO 390                                           
         GOTO  360                                                      
 350        K = K+1                                                     
 360        IF (K .GT. M0) GOTO  380                                    
            J = A(K)                                                    
            IF ( D7TP1(C, J, P, PP, X) .GE. ZERO) GOTO 370              
               MC1 = MC0+1                                              
               CALL  C7COL(C, J, M, P, PP, Q(1, MC1))                   
               CALL Q7RGS(IERR, A, MC1, P, P, P, MC1, Q, R, Y)          
               MC = MC+1                                                
               A(K) = A(MC)                                             
               A(MC) = J                                                
               IF (IERR .NE. 0) GOTO  350                               
               T =  L7SVN(MC1, R, Y, Y)/ L7SVX(MC1, R, Y, Y)            
               IF (T .LE. PMEPS) GOTO  350                              
               RCOND = T                                                
               MC0 = MC1                                                
               PC = PC-1                                                
               IF (PC .LE. 0) GOTO  380                                 
 370        CONTINUE                                                    
            GOTO  350                                                   
 380  CONTINUE                                                          
 390  IF (MC0 .LE. 0) GOTO 410                                          
         CALL  V7SCL(P, S, NEGONE, G)                                   
         DO  400 I = 1, MC0                                             
            W(I) =  D7TPR(P, Q(1, I), S)                                
            CALL V2AXY(P, S, -W(I), Q(1, I), S)                         
 400        CONTINUE                                                    
         CALL  L7ITV(MC0, Y, R, W)                                      
 410  CONTINUE                                                          
 420  CALL V7CPY(P, S, X)                                               
      MOVE = .TRUE.                                                     
      TEMP = MC+1                                                       
      DO  430 I = TEMP, M0                                              
         J = A(I)                                                       
         JA = IABS(J)                                                   
         T = AMIN1(T,  D7TP1(C, J, P, PP, X)/CN(JA))                    
 430     CONTINUE                                                       
      GOTO  480                                                         
 440  CALL  V7SCP(P, Y, ZERO)                                           
 450  CALL  V7SCP(P, S, ZERO)                                           
      MOVE = .FALSE.                                                    
      GOTO  480                                                         
 460  IF (.NOT. ADD2QR) GOTO 470                                        
         MC0 = P-PC                                                     
         CALL V7CPY(P, Q(1, MC0+1), S)                                  
         J = MC0*(MC0+1)/2+1                                            
         K = J+MC0                                                      
         IF (MC0 .GT. 0) CALL V7CPY(MC0, R(J), W)                       
         R(K) = SNORM                                                   
 470  CONTINUE                                                          
 480  RETURN                                                            
      END                                                               
        REAL FUNCTION  B7VAL(BC, BX, J, M, P)                           
C                                                                       
C  *** RETURN BX(1,J) FOR 1 .LE. J .LE. P, BC(1,J-P) FOR J .GT. P,      
C  *** -BX(2,-J) FOR -P .LE. J .LE. -1,  -BC(2,-J-P) FOR J .LT. -P.     
C                                                                       
        INTEGER J, M, P                                                 
        REAL BC(2,M), BX(2,P)                                           
C                                                                       
        INTEGER I                                                       
C                                                                       
C  ***  BODY                                                            
C                                                                       
        IF (J .LT. 0) GO TO 20                                          
            IF (J .GT. P) GO TO 10                                      
                 B7VAL = BX(1,J)                                        
                GO TO 999                                               
C                                                                       
 10         I = J - P                                                   
             B7VAL = BC(1,I)                                            
            GO TO 999                                                   
C                                                                       
 20     I = -J                                                          
        IF (I .GT. P) GO TO 30                                          
             B7VAL = -BX(2,I)                                           
            GO TO 999                                                   
C                                                                       
 30     I = I - P                                                       
         B7VAL = -BC(2,I)                                               
C                                                                       
 999    RETURN                                                          
        END                                                             
      REAL FUNCTION  D7TP1(C, J, P, PP, X)                              
      INTEGER P, PP                                                     
      INTEGER J                                                         
      REAL C(PP, 1), X(P)                                               
      EXTERNAL  D7TPR                                                   
      INTEGER I                                                         
      REAL  D7TPR, T                                                    
      I = IABS(J)                                                       
      IF (I .GT. P) GOTO 10                                             
         T = X(I)                                                       
         GOTO  20                                                       
  10     I = I-P                                                        
         T =  D7TPR(P, C(1, I), X)                                      
  20  IF (J .LT. 0) T = -T                                              
       D7TP1 = T                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE Q7RGS(IERR, IPIVOT, L, N, NN, NOPIVK, P, Q, R, W)      
C                                                                       
C  ***  COMPUTE QR FACTORIZATION VIA MODIFIED GRAM-SCHMIDT PROCEDURE    
C  ***  WITH COLUMN PIVOTING  ***                                       
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IERR, L, N, NN, NOPIVK, P                                 
      INTEGER IPIVOT(P)                                                 
      REAL Q(NN,P), R(1), W(P)                                          
C     DIMENSION R(P*(P+1)/2)                                            
C                                                                       
C----------------------------  DESCRIPTION  ----------------------------
C                                                                       
C        THIS ROUTINE COMPUTES COLUMNS  L  THROUGH  P  OF A QR FACTORI- 
C     ZATION OF THE MATRIX  A  THAT IS ORIGINALLY STORED IN COLUMNS  L  
C     THROUGH  P  OF  Q.  IT IS ASSUMED THAT COLUMNS 1 THROUGH  L-1  OF 
C     THE FACTORIZATION HAVE ALREADY BEEN STORED IN  Q  AND  R.  THIS   
C     CODE USES THE MODIFIED GRAM-SCHMIDT PROCEDURE WITH REORTHOGONALI- 
C     ZATION AND, IF  NOPIVK  ALLOWS IT, WITH COLUMN PIVOTING -- IF     
C     K .GT. NOPIVK,  THEN ORIGINAL COLUMN  K  IS ELIGIBLE FOR PIVOTING.
C     IF  IPIVOT(L) = 0  ON INPUT, THEN  IPIVOT  IS INITIALIZED SO THAT 
C     IPIVOT(I) = I  FOR  I = L,...,P.  WHATEVER THE ORIGINAL VALUE OF  
C     IPIVOT(L), THE CORRESPONDING ELEMENTS OF  IPIVOT  ARE INTERCHANGED
C     WHENEVER COLUMN PIVOTING OCCURS.  THUS IF  IPIVOT(L) = 0  ON IN-  
C     PUT, THEN THE  Q  AND  R  RETURNED ARE SUCH THAT COLUMN  I  OF    
C     Q*R  EQUALS COLUMN  IPIVOT(I)  OF THE ORIGINAL MATRIX  A.  THE UP-
C     PER TRIANGULAR MATRIX  R  IS STORED COMPACTLY BY COLUMNS, I.E.,   
C     THE OUTPUT VECTOR  R  CONTAINS  R(1,1), R(1,2), R(2,2), R(1,3),   
C     R(2,3), ..., R(P,P) (IN THAT ORDER).  IF ALL GOES WELL, THEN THIS 
C     ROUTINE SETS  IERR = 0.  BUT IF (PERMUTED) COLUMN  K  OF  A  IS   
C     LINEARLY DEPENDENT ON (PERMUTED) COLUMNS 1,2,...,K-1, THEN  IERR  
C     IS SET TO  K AND THE R MATRIX RETURNED HAS  R(I,J) = 0  FOR       
C     I .GE. K  AND  J .GE. K.  IN THIS CASE COLUMNS  K  THROUGH  P     
C     OF THE  Q  RETURNED ARE NOT ORTHONORMAL.  W IS A SCRATCH VECTOR.  
C        THE ORIGINAL MATRIX  A  AND THE COMPUTED ORTHOGONAL MATRIX  Q  
C     ARE  N BY P  MATRICES.  NN  IS THE LEAD DIMENSION OF THE ARRAY  Q 
C     AND MUST SATISFY  NN .GE. N.  NO PARAMETER CHECKING IS DONE.      
C                                                                       
C        CODED BY DAVID M. GAY (FALL 1979, SPRING 1984).                
C                                                                       
C--------------------------  LOCAL VARIABLES  --------------------------
C                                                                       
      INTEGER I, II, J, K, KK, KM1, KP1, LM1                            
      LOGICAL IPINIT                                                    
      REAL AK, SINGTL, T, T1, T2, WK                                    
      EXTERNAL  D7TPR,  R7MDC, V2AXY,  V7SCP, V7SWP,  V2NRM,  V7SCL     
      REAL  D7TPR,  R7MDC,  V2NRM                                       
C/+                                                                     
      REAL  SQRT                                                        
C/                                                                      
      REAL BIG, MEPS10, ONE, REOTOL, TEN, TINY, WTOL, ZERO              
C/6                                                                     
C     DATA ONE/1.0E+0/, REOTOL/0.25E+0/, TEN/1.E+1/, WTOL/0.75E+0/,     
C    1     ZERO/0.0E+0/                                                 
C/7                                                                     
      PARAMETER (ONE=1.0E+0, REOTOL=0.25E+0, TEN=1.E+1, WTOL=0.75E+0,   
     1           ZERO=0.0E+0)                                           
      SAVE MEPS10, TINY                                                 
C/                                                                      
      DATA MEPS10/0.0E+0/, TINY/0.0E+0/                                 
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IERR = 0                                                          
      IF (MEPS10 .GT. ZERO) GO TO 10                                    
         MEPS10 = TEN *  R7MDC(3)                                       
         TINY =  R7MDC(1)                                               
         BIG =  R7MDC(6)                                                
         IF (TINY*BIG .LT. ONE) TINY = ONE / BIG                        
 10   SINGTL = FLOAT(MAX0(N,P)) * MEPS10                                
      LM1 = L - 1                                                       
      J = L*LM1/2                                                       
      KK = J                                                            
      IPINIT = IPIVOT(L) .EQ. 0                                         
C                                                                       
C  ***  INITIALIZE W, IPIVOT, DIAG(R), AND R(I,J) FOR I = 1,2,...,L-1   
C  ***  AND J = L,L+1,...,P.                                            
C                                                                       
      DO 50 I = L, P                                                    
         IF (IPINIT) IPIVOT(I) = I                                      
         T =  V2NRM(N, Q(1,I))                                          
         IF (T .GT. ZERO) GO TO 20                                      
              W(I) = ONE                                                
              J = J + LM1                                               
              GO TO 40                                                  
 20      W(I) = ZERO                                                    
         IF (LM1 .EQ. 0) GO TO 40                                       
              DO 30 K = 1, LM1                                          
                   J = J + 1                                            
                   T1 =  D7TPR(N, Q(1,K), Q(1,I))                       
                   R(J) = T1                                            
                   CALL V2AXY(N, Q(1,I), -T1, Q(1,K), Q(1,I))           
                   W(I) = W(I) + (T1/T)**2                              
 30                CONTINUE                                             
 40      J = J + I - LM1                                                
         R(J) = T                                                       
 50      CONTINUE                                                       
C                                                                       
C  ***  MAIN LOOP  ***                                                  
C                                                                       
      DO 140 K = L, P                                                   
         KK = KK + K                                                    
         KP1 = K + 1                                                    
         IF (K .LE. NOPIVK) GO TO 70                                    
         IF (K .GE. P) GO TO 70                                         
C                                                                       
C        ***  FIND COLUMN WITH MINIMUM WEIGHT LOSS  ***                 
C                                                                       
              T = W(K)                                                  
              IF (T .LE. ZERO) GO TO 70                                 
              J = K                                                     
              DO 60 I = KP1, P                                          
                   IF (W(I) .GE. T) GO TO 60                            
                        T = W(I)                                        
                        J = I                                           
 60                CONTINUE                                             
              IF (J .EQ. K) GO TO 70                                    
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
                   IF (K .LE. 1) GO TO 70                               
                        I = I - J + 1                                   
                        J = KK - K + 1                                  
                        CALL V7SWP(K-1, R(I), R(J))                     
C                                                                       
C        ***  COLUMN K OF Q SHOULD BE NEARLY ORTHOGONAL TO THE PREVIOUS 
C        ***  COLUMNS.  NORMALIZE IT, TEST FOR SINGULARITY, AND DECIDE  
C        ***  WHETHER TO REORTHOGONALIZE IT.                            
C                                                                       
 70      AK = R(KK)                                                     
         IF (AK .LE. ZERO) GO TO 150                                    
         T1 = AK                                                        
         R(KK) = ONE                                                    
         T2 = ONE                                                       
         WK = W(K)                                                      
C                                                                       
C        *** SET T TO THE NORM OF (Q(K,K),...,Q(N,K))                   
C        *** AND CHECK FOR SINGULARITY.                                 
C                                                                       
 80      IF (WK .LT. WTOL) GO TO 90                                     
            T =  V2NRM(N, Q(1,K))                                       
            IF (T*T2 / AK .GT. SINGTL) GO TO 100                        
            GO TO 150                                                   
 90      T =  SQRT(ONE - WK)                                            
         IF (T*T2 .LE. SINGTL) GO TO 150                                
         T = T * AK                                                     
C                                                                       
 100     IF (T .LT. TINY) GO TO 150                                     
         R(KK) = T * R(KK)                                              
         CALL  V7SCL(N, Q(1,K), ONE/T, Q(1,K))                          
         IF (T/T1 .GE. REOTOL) GO TO 120                                
C                                                                       
C     ***  REORTHOGONALIZE COLUMN K  ***                                
C                                                                       
              AK = ONE                                                  
              T2 = T * T2                                               
              WK = ZERO                                                 
              J = KK - K                                                
              KM1 = K - 1                                               
              DO 110 I = 1, KM1                                         
                   J = J + 1                                            
                   T =  D7TPR(N, Q(1,I), Q(1,K))                        
                   WK = WK + T*T                                        
                   R(J) = R(J) + T*R(KK)                                
 110               CALL V2AXY(N, Q(1,K), -T, Q(1,I), Q(1,K))            
              T1 = ONE                                                  
              GO TO 80                                                  
C                                                                       
C        ***  COMPUTE R(K,I) FOR I = K+1,...,P AND UPDATE Q  ***        
C                                                                       
 120     IF (K .GE. P) GO TO 999                                        
         J = KK + K                                                     
         II = KK                                                        
         DO 130 I = KP1, P                                              
              II = II + I                                               
              T =  D7TPR(N, Q(1,K), Q(1,I))                             
              R(J) = T                                                  
              J = J + I                                                 
              CALL V2AXY(N, Q(1,I), -T, Q(1,K), Q(1,I))                 
              T1 = R(II)                                                
              IF (T1 .GT. ZERO)  W(I) = W(I) + (T/T1)**2                
 130          CONTINUE                                                  
 140     CONTINUE                                                       
C                                                                       
C  ***  SINGULAR Q  ***                                                 
C                                                                       
 150  IERR = K                                                          
      KM1 = K - 1                                                       
      J = KK                                                            
      DO 160 I = K, P                                                   
         CALL  V7SCP(I-KM1, R(J), ZERO)                                 
         J = J + I                                                      
 160     CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF Q7RGS FOLLOWS  ***                                 
      END                                                               
      SUBROUTINE  Q7RS1(K, LR, P, PP, Q, R, W)                          
C                                                                       
C  ***  PERMUTE COLUMN K OF R TO COLUMN P, MODIFY Q ACCORDINGLY  ***    
C                                                                       
      INTEGER K, LR, P, PP                                              
      REAL Q(PP,P), R(LR), W(P)                                         
C     DIMSNSION R(P*(P+1)/2)                                            
C                                                                       
      EXTERNAL  H2RFA,  H2RFG, V7CPY                                    
      REAL  H2RFG                                                       
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
 30      CALL  H2RFA(PP, Q(1,J), Q(1,JP1), X, Y, Z)                     
 40      T = X * WJ                                                     
         W(J) = WJ + T                                                  
         WJ = T * Z                                                     
 50      CONTINUE                                                       
      W(P) = WJ                                                         
      CALL V7CPY(P, R(K1+1), W)                                         
 999  RETURN                                                            
C  ***  LAST LINE OF  Q7RS1 FOLLOWS  ***                                
      END                                                               
      SUBROUTINE  C7COL(C, J, M, P, PP, X)                              
      INTEGER M, P, PP                                                  
      INTEGER J                                                         
      REAL C(PP, M), X(P)                                               
      EXTERNAL  V7SCL, V7CPY                                            
      INTEGER I                                                         
      REAL ONE, NEGONE, ZERO                                            
      DATA NEGONE/-1.E+0/                                               
      DATA ONE/1.E+0/                                                   
      DATA ZERO/0.E+0/                                                  
C ***  EXTRACT COLUMN J FROM C  WITH PREPENDED IDENTITY MATRIX  ***     
      I = IABS(J)                                                       
      IF (I .LE. P) GOTO 30                                             
         I = I-P                                                        
         IF (J .LE. 0) GOTO 10                                          
            CALL V7CPY(P, X, C(1, I))                                   
            GOTO  20                                                    
  10        CALL  V7SCL(P, X, NEGONE, C(1, I))                          
  20     CONTINUE                                                       
         GOTO  40                                                       
  30     CALL  V7SCP(P, X, ZERO)                                        
         X(I) = ONE                                                     
         IF (J .LT. 0) X(I) = NEGONE                                    
  40  RETURN                                                            
      END                                                               
      SUBROUTINE DBURAM(NPTS, MESH, FN, M, N, P, Q, DELK)               
      INTEGER NPTS                                                      
      INTEGER M, N                                                      
      DOUBLE PRECISION MESH(NPTS), FN(NPTS), P(1), Q(1), DELK           
      INTEGER MAXITR, ITOL, NERROR, IER, I                              
      LOGICAL SMONOD                                                    
      DOUBLE PRECISION FNMAX, FNMIN                                     
C   DBURAM IS A LONG REAL PROCEDURE WHICH FINDS A                       
C   A RATIONAL FUNCTION WHICH IS THE BEST APPROXIMATION,                
C   IN THE UNIFORM OR MINIMAX SENSE, TO A GIVEN DISCRETE                
C   FUNCTION.  THE RATIONAL FUNCTION IS REPRESENTED AS                  
C   THE QUOTIENT OF TWO POLYNOMIALS EACH EXPANDED IN TERMS              
C   OF TCHEBYCHEV POLYNOMIALS.  THIS ROUTINE IS A SHELL                 
C   WHICH IN TURN CALLS THE ROUTINE DBURM1 WITH CERTAIN                 
C   DEFAULT VALUES FOR THE INITIAL APPROXIMATION AND  FOR               
C   THE STOPPING CRITERIA.                                              
C   INPUT:                                                              
C   NPTS   - THE NUMBER OF MESH POINTS.                                 
C   MESH   - THE ARRAY OF MESH POINTS.                                  
C   FN     - THE ARRAY OF FUNCTION VALUES.                              
C   M      - THE DEGREE OF THE DESIRED NUMERATOR POLYNOMIAL.            
C   N      - THE DEGREE OF THE DESIRED DENOMINATOR POLYNOMIAL.          
C   OUTPUT:                                                             
C   P      - THE ARRAY OF COEFFICIENTS FOR THE NUMERATOR POLYNOMIAL.    
C   Q      - THE ARRAY OF COEFFICIENTS FOR THE DENOMINATOR POLYNOMIAL.  
C   DELK   - THE MAXIMUM ERROR IN THE APPROXIMATION.                    
C   ERROR STATES (ASTERISK INDICATES RECOVERABLE):                      
C   1  - INVALID DEGREE                                                 
C   2  - TOO FEW MESH POINTS                                            
C   3  - MESH IS NOT STRICTLY MONOTONE                                  
C   4* - APPROXIMATION EQUALS FUNCTION                                  
C   5* - NO IMPROVEMENT IN APPROXIMATION                                
C   6* - REACHED 50 ITERATIONS                                          
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
C    1   23HDBURAM - INVALID DEGREE, 23, 1, 2)                          
C     IF (NPTS .LT. M+N+2) CALL SETERR(28HDBURAM - TOO FEW MESH POINTS  
C    1   , 28, 2, 2)                                                    
C     IF (.NOT. SMONOD(MESH, NPTS, 1)) CALL SETERR(                     
C    1   38HDBURAM - MESH IS NOT STRICTLY MONOTONE, 38, 3, 2)           
C/7S                                                                    
      IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
     1   'DBURAM - INVALID DEGREE', 23, 1, 2)                           
      IF (NPTS .LT. M+N+2) CALL SETERR('DBURAM - TOO FEW MESH POINTS'   
     1   , 28, 2, 2)                                                    
      IF (.NOT. SMONOD(MESH, NPTS, 1)) CALL SETERR(                     
     1   'DBURAM - MESH IS NOT STRICTLY MONOTONE', 38, 3, 2)            
C/                                                                      
C   INITIALIZE THE NUMERATOR AND DEMONINATOR POLYNOMIALS.               
      FNMAX = FN(1)                                                     
      FNMIN = FN(1)                                                     
      DO  3 I = 2, NPTS                                                 
         IF (FNMAX .GE. FN(I)) GOTO 1                                   
            FNMAX = FN(I)                                               
            GOTO  2                                                     
   1        IF (FN(I) .LT. FNMIN) FNMIN = FN(I)                         
   2     CONTINUE                                                       
   3     CONTINUE                                                       
      CALL SETD(M+1, 0.0D0, P)                                          
      P(1) = 0.5D0*(FNMAX+FNMIN)                                        
      CALL SETD(N+1, 0.0D0, Q)                                          
      Q(1) = 1.0D0                                                      
      DELK = FNMAX-P(1)                                                 
      IF (0 .GE. M .AND. 0 .GE. N) GOTO 11                              
         MAXITR = 50                                                    
         ITOL = 4                                                       
         CALL DBURM1(NPTS, MESH, FN, MAXITR, ITOL, M, N, P, Q, DELK)    
         IF (NERROR(IER) .EQ. 0) GOTO 10                                
            IF (IER .NE. 7) GOTO 4                                      
C/6S                                                                    
C              CALL N5ERR(38HDBURAM - APPROXIMATION EQUALS FUNCTION, 38,
C    1            4, 1)                                                 
C/7S                                                                    
               CALL N5ERR('DBURAM - APPROXIMATION EQUALS FUNCTION', 38, 
     1            4, 1)                                                 
C/                                                                      
               GOTO  9                                                  
   4           IF (IER .NE. 8) GOTO 5                                   
C/6S                                                                    
C                 CALL N5ERR(                                           
C    1               40HDBURAM - NO IMPROVEMENT IN APPROXIMATION, 40, 5,
C    2               1)                                                 
C/7S                                                                    
                  CALL N5ERR(                                           
     1               'DBURAM - NO IMPROVEMENT IN APPROXIMATION', 40, 5, 
     2               1)                                                 
C/                                                                      
                  GOTO  8                                               
   5              IF (IER .NE. 9) GOTO 6                                
C/6S                                                                    
C                    CALL N5ERR(30HDBURAM - REACHED 50 ITERATIONS, 30, 6
C    1                  , 1)                                            
C/7S                                                                    
                     CALL N5ERR('DBURAM - REACHED 50 ITERATIONS', 30, 6 
     1                  , 1)                                            
C/                                                                      
                     GOTO  7                                            
   6                 CALL EPRINT                                        
   7           CONTINUE                                                 
   8        CONTINUE                                                    
   9        CONTINUE                                                    
  10     CONTINUE                                                       
  11  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DBURM1(NPTS, MESH, FN, MAXITR, ITOL, M, N, P, Q        
     1   , DELK)                                                        
      INTEGER NPTS                                                      
      INTEGER MAXITR, ITOL, M, N                                        
      DOUBLE PRECISION MESH(NPTS), FN(NPTS), P(1), Q(1), DELK           
      COMMON /CSTAK/ DSTAK                                              
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER IDIG, IDFLR, I1MACH, ISTKGT, NPPTR, NQPTR                 
      INTEGER ENPTR, QKPTR, IEXPTR, J, ISTAK(1000)                      
      LOGICAL SMONOD                                                    
      DOUBLE PRECISION D1MACH, DFLOAT, QLRG, WS(500), DABS              
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), WS(1))                                     
C   DBURM1 IS A LONG REAL PROCEDURE WHICH FINDS A                       
C   A RATIONAL FUNCTION WHICH IS THE BEST APPROXIMATION,                
C   IN THE UNIFORM OR MINIMAX SENSE, TO A GIVEN DISCRETE                
C   FUNCTION.  THE RATIONAL FUNCTION IS REPRESENTED AS                  
C   THE QUOTIENT OF TWO POLYNOMIALS EACH EXPANDED IN TERMS              
C   OF TCHEBYCHEV POLYNOMIALS.  THIS ROUTINE STARTS FROM AN             
C   INITIAL APPROXIMATION AND TERMINATES FOR ONE OF FOUR                
C   REASONS: (1) THE ERROR CURVE EQUIOSCILLATES AND THE                 
C   ALTERNATING EXTREMA MATCH TO ITOL DIGITS, (2) THE NUMBER            
C   OF ITERATIONS EXCEEDS MAXITR, (3) THE APPROXIMATION                 
C   CANNOT BE IMPROVED, OR (4) THE APPROXIMATION IS ESSENTIALLY         
C   EQUAL TO THE GIVEN DISCRETE FUNCTION.                               
C   INPUT:                                                              
C   NPTS   - THE NUMBER OF MESH POINTS.                                 
C   MESH   - THE ARRAY OF MESH POINTS.                                  
C   FN     - THE ARRAY OF FUNCTION VALUES.                              
C   MAXITR - THE MAXIMUM NUMBER OF ITERATIONS.                          
C   ITOL   - THE NUMBER OF DIGITS TO WHICH THE EXTREMA SHOULD MATCH.    
C   M      - THE DEGREE OF THE DESIRED NUMERATOR POLYNOMIAL.            
C   N      - THE DEGREE OF THE DESIRED DENOMINATOR POLYNOMIAL.          
C   P      - THE ARRAY OF COEFFICIENTS FOR THE INITIAL NUMERATOR.       
C   Q      - THE ARRAY OF COEFFICIENTS FOR THE INITIAL DENOMINATOR.     
C   OUTPUT:                                                             
C   P      - THE ARRAY OF COEFFICIENTS FOR THE NUMERATOR POLYNOMIAL.    
C   Q      - THE ARRAY OF COEFFICIENTS FOR THE DENOMINATOR POLYNOMIAL.  
C   DELK   - THE MAXIMUM ERROR IN THE APPROXIMATION.                    
C   ERROR STATES (ASTERISK INDICATES RECOVERABLE):                      
C   1  - INVALID DEGREE                                                 
C   2  - TOO FEW MESH POINTS                                            
C   3  - MESH IS NOT STRICTLY MONOTONE                                  
C   4  - MAXITR .LT. 0                                                  
C   5  - INVALID ACCURACY REQUEST                                       
C   6  - DENOMINATOR IS NONPOSITIVE                                     
C   7* - APPROXIMATION EQUALS FUNCTION                                  
C   8* - NO IMPROVEMENT IN APPROXIMATION                                
C   9* - REACHED MAXIMUM NO. OF ITERATIONS                              
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
C    1   23HDBURM1 - INVALID DEGREE, 23, 1, 2)                          
C     IF (NPTS .LT. M+N+2) CALL SETERR(28HDBURM1 - TOO FEW MESH POINTS  
C    1   , 28, 2, 2)                                                    
C     IF (.NOT. SMONOD(MESH, NPTS, 1)) CALL SETERR(                     
C    1   38HDBURM1 - MESH IS NOT STRICTLY MONOTONE, 38, 3, 2)           
C     IF (MAXITR .LT. 0) CALL SETERR(22HDBURM1 - MAXITR .LT. 0, 22, 4, 2
C    1   )                                                              
C/7S                                                                    
      IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
     1   'DBURM1 - INVALID DEGREE', 23, 1, 2)                           
      IF (NPTS .LT. M+N+2) CALL SETERR('DBURM1 - TOO FEW MESH POINTS'   
     1   , 28, 2, 2)                                                    
      IF (.NOT. SMONOD(MESH, NPTS, 1)) CALL SETERR(                     
     1   'DBURM1 - MESH IS NOT STRICTLY MONOTONE', 38, 3, 2)            
      IF (MAXITR .LT. 0) CALL SETERR('DBURM1 - MAXITR .LT. 0', 22, 4, 2 
     1   )                                                              
C/                                                                      
      IDIG = IDFLR(D1MACH(5)*DFLOAT(I1MACH(14)))                        
C/6S                                                                    
C     IF (ITOL .LT. 1 .OR. IDIG .LT. ITOL) CALL SETERR(                 
C    1   33HDBURM1 - INVALID ACCURACY REQUEST, 33, 5, 2)                
C/7S                                                                    
      IF (ITOL .LT. 1 .OR. IDIG .LT. ITOL) CALL SETERR(                 
     1   'DBURM1 - INVALID ACCURACY REQUEST', 33, 5, 2)                 
C/                                                                      
      QLRG = DABS(Q(1))                                                 
      J = 2                                                             
         GOTO  2                                                        
   1     J = J+1                                                        
   2     IF (J .GT. N+1) GOTO  3                                        
         IF (QLRG .LT. DABS(Q(J))) QLRG = DABS(Q(J))                    
         GOTO  1                                                        
   3  IF (QLRG .NE. 0.0D0) GOTO 4                                       
C/6S                                                                    
C        CALL SETERR(35HDBURM1 - DENOMINATOR IS NONPOSITIVE, 35, 6, 2)  
C/7S                                                                    
         CALL SETERR('DBURM1 - DENOMINATOR IS NONPOSITIVE', 35, 6, 2)   
C/                                                                      
         GOTO  11                                                       
   4     J = 1                                                          
            GOTO  6                                                     
   5        J = J+1                                                     
   6        IF (J .GT. N+1) GOTO  7                                     
            Q(J) = Q(J)/QLRG                                            
            GOTO  5                                                     
   7     J = 1                                                          
            GOTO  9                                                     
   8        J = J+1                                                     
   9        IF (J .GT. M+1) GOTO  10                                    
            P(J) = P(J)/QLRG                                            
            GOTO  8                                                     
  10     CONTINUE                                                       
  11  NPPTR = ISTKGT(M+1, 4)                                            
      NQPTR = ISTKGT(N+1, 4)                                            
      ENPTR = ISTKGT(NPTS, 4)                                           
      QKPTR = ISTKGT(NPTS, 4)                                           
      IEXPTR = ISTKGT(NPTS, 2)                                          
      CALL DB1RM1(NPTS, MESH, FN, MAXITR, ITOL, M, N, P, Q, DELK, WS(   
     1   NPPTR), WS(NQPTR), WS(ENPTR), WS(QKPTR), ISTAK(IEXPTR))        
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DB1RM1(NPTS, X, FN, MAXITR, ITOL, M, N, P, Q,          
     1   DELK, NEWP, NEWQ, EN, QK, IEXT)                                
      INTEGER NPTS                                                      
      INTEGER MAXITR, ITOL, M, N, IEXT(NPTS)                            
      DOUBLE PRECISION X(NPTS), FN(NPTS), P(1), Q(1), DELK, NEWP(1)     
      DOUBLE PRECISION NEWQ(1), EN(NPTS), QK(NPTS)                      
      INTEGER NITR, NEX, IMAX, IMIN, ILRG, L5RGXD                       
      INTEGER NERROR, IER, I                                            
      DOUBLE PRECISION EPS, BND, D1MACH, DELNEW                         
      EPS = D1MACH(4)*10.0D0**ITOL                                      
      CALL EXTRMD(NPTS, FN, NEX, IEXT, IMAX, IMIN, ILRG)                
      BND = DABS(FN(ILRG))*EPS                                          
      CALL DC5EQK(NPTS, X, FN, M, N, P, Q, QK, EN)                      
      DO  1 I = 1, NPTS                                                 
C/6S                                                                    
C        IF (QK(I) .LE. 0.0D0) CALL SETERR(                             
C    1      35HDBURM1 - DENOMINATOR IS NONPOSITIVE, 35, 6, 2)           
C/7S                                                                    
         IF (QK(I) .LE. 0.0D0) CALL SETERR(                             
     1      'DBURM1 - DENOMINATOR IS NONPOSITIVE', 35, 6, 2)            
C/                                                                      
   1     CONTINUE                                                       
      CALL EXTRMD(NPTS, EN, NEX, IEXT, IMAX, IMIN, ILRG)                
      DELK = DABS(EN(ILRG))                                             
      DELNEW = DELK                                                     
      CALL MOVEFD(M+1, P, NEWP)                                         
      CALL MOVEFD(N+1, Q, NEWQ)                                         
      NITR = 0                                                          
         GOTO  3                                                        
   2     NITR = NITR+1                                                  
   3     IF (NITR .GE. MAXITR) GOTO  6                                  
C   OUTPT3 (X,NPTS,P,Q,DELK,M,N,EN,IEXT,NEX)                            
         IF (DELK .GT. BND) GOTO 4                                      
C/6S                                                                    
C           CALL SETERR(38HDBURM1 - APPROXIMATION EQUALS FUNCTION, 39, 7
C    1         , 1)                                                     
C/7S                                                                    
            CALL SETERR('DBURM1 - APPROXIMATION EQUALS FUNCTION', 39, 7 
     1         , 1)                                                     
C/                                                                      
            RETURN                                                      
C   TEST FOR OPTIMAL SOLUTION.                                          
   4     IF (L5RGXD(NPTS, EN, NEX, IEXT, ILRG, ITOL) .GE. M+N+2) RETURN 
         CALL DL5STP(NPTS, X, FN, QK, DELNEW, M, N, NEWP, NEWQ)         
         IF (NERROR(IER) .NE. 0) CALL ERROFF                            
         CALL DC5EQK(NPTS, X, FN, M, N, NEWP, NEWQ, QK, EN)             
         CALL EXTRMD(NPTS, EN, NEX, IEXT, IMAX, IMIN, ILRG)             
         DELNEW = DABS(EN(ILRG))                                        
         IF (DELK .GT. DELNEW) GOTO 5                                   
C/6S                                                                    
C           CALL SETERR(40HDBURM1 - NO IMPROVEMENT IN APPROXIMATION, 40,
C    1         8, 1)                                                    
C/7S                                                                    
            CALL SETERR('DBURM1 - NO IMPROVEMENT IN APPROXIMATION', 40, 
     1         8, 1)                                                    
C/                                                                      
            RETURN                                                      
   5     CALL MOVEFD(M+1, NEWP, P)                                      
         CALL MOVEFD(N+1, NEWQ, Q)                                      
         DELK = DELNEW                                                  
         GOTO  2                                                        
C/6S                                                                    
C  6  CALL SETERR(42HDBURM1 - REACHED MAXIMUM NO. OF ITERATIONS, 42, 9  
C    1   , 1)                                                           
C/7S                                                                    
   6  CALL SETERR('DBURM1 - REACHED MAXIMUM NO. OF ITERATIONS', 42, 9   
     1   , 1)                                                           
C/                                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE DL5STP(NPTS, MESH, FN, QK, DELK, M, N, P, Q)           
      INTEGER NPTS                                                      
      INTEGER M, N                                                      
      DOUBLE PRECISION MESH(NPTS), FN(NPTS), QK(NPTS), DELK, P(1), Q(1) 
      COMMON /CSTAK/ DSTAK                                              
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER APTR, XPTR, ISTKGT, ISTAK(1000)                           
      INTEGER BC, BX, C, G, IW, LIW, LW, MM, NN, W                      
      DOUBLE PRECISION WS(500)                                          
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), WS(1))                                     
C   THIS ROUTINE ALLOCATES STORAGE SO THAT                              
C   DL9STP CAN DEFINE THE LINEAR PROGRAMMING SUBPROBLEM OF              
C   THE DIFFERENTIAL CORRECTION ALGORITHM AND CALL A GENERAL            
C   PURPOSE LINEAR PROGRAMMING PACKAGE.                                 
C   INPUT...                                                            
C   NPTS   - THE NUMBER OF MESH POINTS.                                 
C   MESH   - THE ARRAY OF MESH POINTS.                                  
C   FN     - THE ARRAY OF FUNCTION VALUES.                              
C   QK     - THE ARRAY OF CURRENT DENOMINATOR VALUES.                   
C   DELK   - THE CURRENT MINIMAX ERROR.                                 
C   M      - THE DEGREE OF THE NUMERATOR POLYNOMIAL.                    
C   N      - THE DEGREE OF THE DENOMINATOR POLYNOMIAL.                  
C   P      - THE CURRENT NUMERATOR POLYNOMIAL.                          
C   Q      - THE CURRENT DENOMINATOR POLYNOMIAL.                        
C   OUTPUT...                                                           
C   P      - THE ARRAY OF COEFFICIENTS FOR THE NUMERATOR POLYNOMIAL.    
C   Q      - THE ARRAY OF COEFFICIENTS FOR THE DENOMINATOR POLYNOMIAL.  
C   ERROR STATES (ASTERISK INDICATES FATAL)...                          
C   1* - INVALID DEGREE                                                 
C   2* - TOO FEW MESH POINTS                                            
C   3* - NONPOSITIVE DELK                                               
C   4  - NO IMPROVEMENT IN THE LP SUBPROBLEM                            
C                                                                       
C *** BODY ***                                                          
C                                                                       
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
C    1   23HDL5STP - INVALID DEGREE, 23, 1, 2)                          
C     IF (NPTS .LT. M+N+2) CALL SETERR(28HDL5STP - TOO FEW MESH POINTS, 
C    1    28, 2, 2)                                                     
C/7S                                                                    
      IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(                          
     1   'DL5STP - INVALID DEGREE', 23, 1, 2)                           
      IF (NPTS .LT. M+N+2) CALL SETERR('DL5STP - TOO FEW MESH POINTS',  
     1    28, 2, 2)                                                     
C/                                                                      
      MM = 2 * NPTS                                                     
      NN = M + N + 3                                                    
      LIW = MM + NN + 7                                                 
      LW = NN*(3*NN+17)/2 + MM + 2                                      
      G = ISTKGT(NN, 4)                                                 
      C = ISTKGT(NN*MM, 4)                                              
      BC = ISTKGT(2*MM, 4)                                              
      BX = ISTKGT(2*NN, 4)                                              
      W = ISTKGT(LW, 4)                                                 
      IW = ISTKGT(LIW, 2)                                               
      APTR = ISTKGT(3*NPTS+1, 4)                                        
      XPTR = ISTKGT(NN, 4)                                              
      CALL DL9STP(NPTS, MESH, FN, QK, DELK, M, N, P, Q, WS(APTR),       
     1            WS(BC), WS(BX), WS(C), WS(G), ISTAK(IW), LIW, LW,     
     2            MM, NN, WS(W), WS(XPTR))                              
      CALL LEAVE                                                        
      RETURN                                                            
C *** LAST LINE OF DL5STP FOLLOWS ***                                   
      END                                                               
      SUBROUTINE DL9STP(NPTS, MESH, FN, QK, DELK, M, N, P, Q, A, BC, BX,
     1                  C, G, IW, LIW, LW, MM, NN, W, X)                
      INTEGER NPTS, M, N, MM, NN, LIW, LW                               
      INTEGER IW(LIW)                                                   
      DOUBLE PRECISION MESH(NPTS), FN(NPTS), QK(NPTS), DELK, P(1), Q(1) 
      DOUBLE PRECISION A(1), BC(2,MM), BX(2,NN), C(NN,MM), G(NN), W(LW),
     1                 X(NN)                                            
C                                                                       
      COMMON /DD5FCM/ NPTSC, MC, NC, I1, I2, I3, I4                     
      INTEGER NPTSC, MC, NC, I1, I2, I3, I4                             
C/+                                                                     
      DOUBLE PRECISION DABS                                             
C/                                                                      
      EXTERNAL DR7MDC                                                   
      DOUBLE PRECISION DR7MDC                                           
C                                                                       
      INTEGER I, J, JP, K, M1, MAXMN, MAXMN1, N1, N2, NERROR, IER       
      DOUBLE PRECISION CTX, CTXNEW, FDELK, FNI, QLRG, QZ, Z             
      DOUBLE PRECISION BIG, ONE, ZERO                                   
      DATA BIG/0.D+0/, ONE/1.D+0/, ZERO/0.D+0/                          
C                                                                       
C *** BODY ***                                                          
C                                                                       
C/6S                                                                    
C     IF (M .LT. 0 .OR. N .LT. 0)                                       
C    1   CALL SETERR(26HDL9STP - INVALID DIMENSION, 26, 1, 2)           
C/7S                                                                    
      IF (M .LT. 0 .OR. N .LT. 0)                                       
     1   CALL SETERR('DL9STP - INVALID DIMENSION', 26, 1, 2)            
C/                                                                      
      IF (BIG .LE. ZERO) BIG = DR7MDC(6)                                
      M1 = M + 1                                                        
      N1 = N + 1                                                        
      N2 = N + 2                                                        
      NPTSC = NPTS                                                      
      MC = M                                                            
      NC = N                                                            
      I1 = NPTS                                                         
      I2 = I1 + NPTS                                                    
      I3 = I2 + N1                                                      
      I4 = I3 + N1                                                      
      CALL MOVEFD(N1, Q, X)                                             
      CALL MOVEFD(M1, P, X(N2))                                         
      X(NN) = ZERO                                                      
      CALL SETD(4*NPTS, ZERO, BC)                                       
      DO 10 I = 1, I2                                                   
 10      BC(2,I) = BIG                                                  
      DO 20 I = 1, N1                                                   
         BX(2,I) = ONE                                                  
         BX(1,I) = -ONE                                                 
 20      CONTINUE                                                       
      DO 30 I = N2, NN                                                  
         BX(2,I) = BIG                                                  
         BX(1,I) = -BIG                                                 
 30      CONTINUE                                                       
      CALL SETD(NN, ZERO, G)                                            
      G(NN) = ONE                                                       
      CALL MOVEFD(NPTS, MESH, A)                                        
      CALL MOVEFD(NPTS, FN, A(NPTS+1))                                  
      CALL MOVEFD(NPTS, QK, A(2*NPTS+1))                                
C/6S                                                                    
C     IF (DELK .LE. ZERO)                                               
C    1   CALL SETERR(25HDL5STP - NONPOSITIVE DELK, 25, 3, 2)            
C/7S                                                                    
      IF (DELK .LE. ZERO)                                               
     1   CALL SETERR('DL5STP - NONPOSITIVE DELK', 25, 3, 2)             
C/                                                                      
      A(3*NPTS+1) = DELK                                                
      MAXMN = MAX0(M, N)                                                
      MAXMN1 = MAXMN + 1                                                
      DO 50 I = 1, NPTS                                                 
         K = I + NPTS                                                   
         Z = MESH(I)                                                    
         FNI = FN(I)                                                    
         QZ = QK(I)                                                     
         FDELK = DELK + FNI                                             
         CALL DT5COF(Z, A(1), A(NPTS), MAXMN, C(1,I))                   
         CALL DV7CPY(MAXMN1, C(1,K), C(1,I))                            
         J = M1                                                         
 40         JP = J + N1                                                 
            C(JP,I) = -C(J,I)                                           
            C(JP,K) =  C(J,K)                                           
            J = J - 1                                                   
            IF (J .GE. 1) GO TO 40                                      
         CALL DV7SCL(N1, C(1,I), DELK+FNI, C(1,I))                      
         CALL DV7SCL(N1, C(1,K), DELK-FNI, C(1,K))                      
         C(NN,I) = QZ                                                   
         C(NN,K) = QZ                                                   
 50      CONTINUE                                                       
      CTX = ZERO                                                        
C                                                                       
C   SOLVE THE LP PROBLEM   MIN G(T)X SUBJECT TO C*X .GE. 0              
C   AND -1 .LE. X(I) .LE. 1 FOR I = 1(1)M+1 (THE Q COEFFICIENTS).       
C                                                                       
      CALL DL7PF(BC, BX, C, G, IW, LIW, LW, MM, NN, NN, W, X)           
C/6S                                                                    
C       IF (IW(1) .NE. 0)                                               
C    1      CALL SETERR(28HFUNNY RETURN CODE FROM DL7PF, 28, IW(0), 2)  
C/7S                                                                    
        IF (IW(1) .NE. 0)                                               
     1      CALL SETERR('FUNNY RETURN CODE FROM DL7PF', 28, IW(0), 2)   
C/                                                                      
      CTXNEW = -X(NN)                                                   
      IF (NERROR(IER) .NE. 0) CALL ERROFF                               
      IF (CTX .GE. CTXNEW) GO TO 150                                    
         QLRG = ZERO                                                    
         J = 1                                                          
            GO TO 70                                                    
 60         J = J+1                                                     
 70         IF (J .GT. N+1) GO TO 80                                    
            IF (QLRG .LT. DABS(X(J))) QLRG = DABS(X(J))                 
            GO TO 60                                                    
 80      J = 1                                                          
            GO TO 100                                                   
 90         J = J+1                                                     
 100        IF (J .GT. N+1) GO TO 110                                   
            Q(J) = X(J)/QLRG                                            
            GO TO 90                                                    
 110     I = 0                                                          
         J = N+2                                                        
            GO TO 130                                                   
 120        J = J+1                                                     
 130        IF (J .GT. M+N+2) GO TO 140                                 
            I = I+1                                                     
            P(I) = X(J)/QLRG                                            
            GO TO 120                                                   
 140     CONTINUE                                                       
         GO TO 999                                                      
C/6S                                                                    
C150     CALL SETERR(44HDL5STP - NO IMPROVEMENT IN THE LP SUBPROBLEM,   
C    1               44, 4, 1)                                          
C/7S                                                                    
 150     CALL SETERR('DL5STP - NO IMPROVEMENT IN THE LP SUBPROBLEM',    
     1               44, 4, 1)                                          
C/                                                                      
 999  RETURN                                                            
C *** LAST LINE OF DL9STP FOLLOWS ***                                   
      END                                                               
      SUBROUTINE DLPPH2(A, M, N, AMAN, B, C, X, MAXITR, CTX)            
      INTEGER M, N                                                      
      EXTERNAL AMAN                                                     
      INTEGER MAXITR                                                    
      DOUBLE PRECISION A(1), B(M), C(N), X(N), CTX                      
      COMMON /CSTAK/ DSTAK                                              
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER WPTR, QPTR, LTPTR, PPTR, VPTR, SCLPTR                     
      INTEGER LSTPTR, ISTAK(1000), ISTKGT                               
      DOUBLE PRECISION WS(500)                                          
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), WS(1))                                     
C   DLPPH2 IS A LONG REAL PROCEDURE FOR SOLVING                         
C   PHASE 2 OF A LINEAR PROGRAMMING PROBLEM.                            
C   SPECIFICALLY, THIS ROUTINE FINDS A VECTOR X WHICH                   
C   SOLVES THE CANONICAL PROBLEM                                        
C                   MAXIMIZE     C(T)X                                  
C                   SUBJECT TO   AX .GE. B                              
C                                                                       
C   THIS ROUTINE STARTS WITH A VECTOR X WHICH IS ALREADY                
C   FEASIBLE, THAT IS WHICH SATISFIES THE CONSTRAINTS.                  
C   THE ALGORITHM IS A GENERALIZATION OF THE STEEPEST EDGE SIMPLEX      
C   ALGORITHM.  AN IMPORTANT FEATURE OF THIS PACKAGE WITH               
C   RESPECT TO NUMERICAL STABILITY IS THAT THE BASIS MATRIX IS          
C   FACTORED INTO THE PRODUCT OF A LEFT TRIANGULAR MATRIX TIMES         
C   AN ORTHOGONAL MATRIX.  THIS FACTORIZATION IS UPDATED AT EACH        
C   PIVOT BY AN O(N**2) ALGORITHM USING GIVENS ROTATIONS.               
C   INPUT-                                                              
C   A      - THE CONSTRAINT MATRIX.                                     
C   M      - THE ROW DIMENSION OF A.                                    
C   N      - THE COLUMN DIMENSION OF A.  NOTE THAT N MUST BE            
C            LESS THAN OR EQUAL TO M.                                   
C   AMAN   - A PROCEDURE THAT MANIPULATES THE MATRIX A.                 
C              SINCE ALL REFERENCES TO THE MATRIX A ARE VIA AMAN,       
C              THE USER MAY USE ANY DATA STRUCTURE FOR REPRESENTING     
C              A THAT EXPLOITS ANY SPARSITY.  IN THE EVENT THAT A       
C              IS STORED AS AN M BY N MATRIX, A DEFAULT PROCEDURE       
C              DLPMAN MAY BE USED.                                      
C   B      - THE RIGHT HAND SIDE.                                       
C   C      - THE COST FUNCTIONAL.                                       
C   X      - AN INITIAL APPROXIMATION TO THE SOLUTION.                  
C            THIS INITIAL VECTOR MUST BE FEASIBLE, THAT IS              
C            AX .GE. B  MUST BE TRUE.                                   
C   MAXITR - THE MAXIMUM NUMBER OF ALLOWABLE ITERATIONS.                
C            EXPERIANCE INDICATES THAT WELL BEHAVED PROBLEMS            
C            ARE SOLVED WITH FEWER THAN 3*M ITERATIONS.                 
C   OUTPUT-                                                             
C   X      - THE SOLUTION.                                              
C   CTX    - THE VALUE OF THE COST FUNCTIONAL EVALUATED AT X.           
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
C    1   26HDLPPH2 - INVALID DIMENSION, 26, 1, 2)                       
C     IF (M .LT. N) CALL SETERR(26HDLPPH2 - INVALID DIMENSION, 26, 2, 2)
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
     1   'DLPPH2 - INVALID DIMENSION', 26, 1, 2)                        
      IF (M .LT. N) CALL SETERR('DLPPH2 - INVALID DIMENSION', 26, 2, 2) 
C/                                                                      
C   ALLOCATE THE WORKING STORAGE.                                       
      WPTR = ISTKGT(M, 4)                                               
      QPTR = ISTKGT(N**2, 4)                                            
      LTPTR = ISTKGT(N**2, 4)                                           
      PPTR = ISTKGT(N, 4)                                               
      VPTR = ISTKGT(M, 4)                                               
      SCLPTR = ISTKGT(M, 4)                                             
      LSTPTR = ISTKGT(2*M+2, 2)                                         
      CALL DL9PH2(A, M, N, AMAN, B, C, X, MAXITR, CTX, WS(WPTR), WS(    
     1   QPTR), WS(LTPTR), WS(PPTR), WS(VPTR), WS(SCLPTR), ISTAK(LSTPTR)
     2   )                                                              
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DL9PH2(A, M, N, AMAN, B, C, X, MAXITR, CTX, W, Q,      
     1   LT, P, V, SCALE, LLIST)                                        
      INTEGER M, N                                                      
      EXTERNAL AMAN                                                     
      INTEGER MAXITR, LLIST(2, 1)                                       
      DOUBLE PRECISION A(1), B(M), C(N), X(N), CTX, W(M)                
      DOUBLE PRECISION Q(N, N), LT(N, N), P(N), V(M), SCALE(M)          
      COMMON /DL5COM/ COND, BOUND, ITRPH1, ITRPH2                       
      INTEGER ITRPH1, ITRPH2                                            
      DOUBLE PRECISION COND, BOUND                                      
      INTEGER TOP, BOTTOM, KK, INDX1, INDX2, L5NKF                      
      INTEGER I                                                         
      LOGICAL UNBNDD                                                    
      DOUBLE PRECISION DFLOAT, D1MACH, DNRM2, EPS, THETA, UMAX          
      DOUBLE PRECISION DV5MAX, XNRM, CNRM, PNRM, TEMP1                  
C   INITIALIZATION.                                                     
      EPS = DFLOAT(N)*D1MACH(4)                                         
      TOP = M+1                                                         
      XNRM = DNRM2(N, X, 1)                                             
      CNRM = DNRM2(N, C, 1)                                             
      DO  1 I = 1, M                                                    
         CALL AMAN(.TRUE., A, M, N, I, X, TEMP1)                        
         W(I) = B(I)-TEMP1                                              
         CALL AMAN(.FALSE., A, M, N, I, P, TEMP1)                       
         SCALE(I) = DNRM2(N, P, 1)                                      
C/6S                                                                    
C        IF (EPS*SCALE(I)*XNRM .LT. W(I)) CALL SETERR(                  
C    1      29HDLPPH2 - INITIAL X INFEASIBLE, 29, 3, 2)                 
C/7S                                                                    
         IF (EPS*SCALE(I)*XNRM .LT. W(I)) CALL SETERR(                  
     1      'DLPPH2 - INITIAL X INFEASIBLE', 29, 3, 2)                  
C/                                                                      
   1     CONTINUE                                                       
      KK = 0                                                            
      CALL SETD(N**2, 0.0D0, Q)                                         
      TEMP1 = 0.0D0                                                     
      DO  2 I = 1, N                                                    
         Q(I, I) = 1.0D0                                                
         TEMP1 = TEMP1+C(I)*X(I)                                        
   2     CONTINUE                                                       
      CTX = TEMP1                                                       
      LLIST(1, M+1) = 1                                                 
      LLIST(2, 1) = M+1                                                 
      DO  3 I = 1, M                                                    
         LLIST(1, I) = I+1                                              
         LLIST(2, I+1) = I                                              
   3     CONTINUE                                                       
      ITRPH2 = 1                                                        
         GOTO  5                                                        
   4     ITRPH2 = ITRPH2+1                                              
   5     IF (ITRPH2 .GT. MAXITR) GOTO  29                               
         XNRM = DNRM2(N, X, 1)                                          
C   STEP 1-    APPEND THE NEW ACTIVE CONSTRAINTS.                       
         INDX1 = L5NKF(TOP, LLIST, KK)                                  
         I = LLIST(1, INDX1)                                            
            GOTO  7                                                     
   6        I = LLIST(1, I)                                             
   7        IF (I .GE. TOP) GOTO  8                                     
            IF (W(I) .LT. (-EPS)*SCALE(I)*XNRM) GOTO  6                 
            CALL AMAN(.FALSE., A, M, N, I, P, TEMP1)                    
            CALL DM5TOP(N, N, Q, 1, N, 1, N, P, 1, P)                   
            PNRM = DNRM2(N-KK, P(KK+1), 1)                              
            IF (PNRM .LE. EPS*SCALE(I)) GOTO  6                         
            CALL L5NKD(LLIST, I)                                        
            CALL L5NKI(LLIST, I, INDX1)                                 
            INDX1 = I                                                   
            CALL DC5APP(N, KK, Q, LT, KK+1, P)                          
            IF (KK .EQ. N) GOTO  8                                      
            GOTO  6                                                     
C   STEP 2-    DELETE AN OLD CONSTRAINT IF POSSIBLE.                    
   8     IF (0 .GE. KK) GOTO 10                                         
            CALL DM5TOP(N, N, Q, 1, KK, 1, N, C, 1, P)                  
            CALL DM5TOP(N, N, LT, 1, KK, 1, KK, P, 4, V)                
            UMAX = DV5MAX(KK, V, INDX2)                                 
            IF (EPS*CNRM .GE. UMAX) GOTO 9                              
               CALL DC5DRP(N, KK, Q, LT, INDX2)                         
               I = L5NKF(TOP, LLIST, INDX2)                             
               CALL L5NKD(LLIST, I)                                     
               BOTTOM = LLIST(2, TOP)                                   
               CALL L5NKI(LLIST, I, BOTTOM)                             
   9        CONTINUE                                                    
C   STEP 3-    COMPUTE THE GRADIENT PROJECTION.                         
  10     IF (KK .NE. 0) GOTO 12                                         
            DO  11 I = 1, N                                             
               P(I) = C(I)                                              
  11           CONTINUE                                                 
            GOTO  14                                                    
  12        IF (EPS*CNRM .LT. UMAX) CALL DM5TOP(N, N, Q, 1, KK, 1, N, C,
     1         1, P)                                                    
            CALL DM5TOP(N, N, Q, 1, KK, 1, N, P, 2, P)                  
            DO  13 I = 1, N                                             
               P(I) = C(I)-P(I)                                         
  13           CONTINUE                                                 
  14     PNRM = DNRM2(N, P, 1)                                          
         IF (KK .EQ. N .OR. PNRM .LT. EPS*CNRM) RETURN                  
C   STEP 4-    FIND THE NEXT CONSTRAINT ALONG THE                       
C              GRADIENT PROJECTION.                                     
         UNBNDD = .TRUE.                                                
         INDX1 = L5NKF(TOP, LLIST, KK)                                  
         I = LLIST(1, INDX1)                                            
            GOTO  16                                                    
  15        I = LLIST(1, I)                                             
  16        IF (I .GE. TOP) GOTO  20                                    
            CALL AMAN(.TRUE., A, M, N, I, P, V(I))                      
            IF (V(I) .GE. (-EPS)*SCALE(I)*PNRM) GOTO 19                 
               IF (.NOT. UNBNDD) GOTO 17                                
                  UNBNDD = .FALSE.                                      
                  THETA = W(I)/V(I)                                     
                  GOTO  18                                              
  17              IF (V(I)*THETA .LT. W(I)) THETA = W(I)/V(I)           
  18        CONTINUE                                                    
  19        CONTINUE                                                    
            GOTO  15                                                    
  20     IF (.NOT. UNBNDD) GOTO 21                                      
C/6S                                                                    
C           CALL SETERR(27HDLPPH2 - UNBOUNDED SOLUTION, 27, 4, 1)       
C/7S                                                                    
            CALL SETERR('DLPPH2 - UNBOUNDED SOLUTION', 27, 4, 1)        
C/                                                                      
            RETURN                                                      
C   STEP 5-    UPDATE THE CURRENT SOLUTION.                             
  21     TEMP1 = 0.0D0                                                  
         DO  22 I = 1, N                                                
            TEMP1 = TEMP1+C(I)*P(I)                                     
  22        CONTINUE                                                    
         IF (0.0D0 .GT. TEMP1) GOTO 23                                  
            CTX = CTX+THETA*TEMP1                                       
            GOTO  24                                                    
C/6S                                                                    
C 23        CALL SETERR(36HDLPPH2 - TERMINATED FOR CONDITIONING, 36, 7  
C    1         , 1)                                                     
C/7S                                                                    
  23        CALL SETERR('DLPPH2 - TERMINATED FOR CONDITIONING', 36, 7   
     1         , 1)                                                     
C/                                                                      
            RETURN                                                      
  24     DO  25 I = 1, N                                                
            X(I) = X(I)+THETA*P(I)                                      
  25        CONTINUE                                                    
         I = LLIST(1, INDX1)                                            
            GOTO  27                                                    
  26        I = LLIST(1, I)                                             
  27        IF (I .GE. TOP) GOTO  28                                    
            W(I) = W(I)-THETA*V(I)                                      
            GOTO  26                                                    
  28     CONTINUE                                                       
         GOTO  4                                                        
C/6S                                                                    
C 29  CALL SETERR(45HDLPPH2 - NUMBER OF ITERATIONS EXCEEDED MAXITR, 45  
C    1   , 6, 1)                                                        
C/7S                                                                    
  29  CALL SETERR('DLPPH2 - NUMBER OF ITERATIONS EXCEEDED MAXITR', 45   
     1   , 6, 1)                                                        
C/                                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE DC5APP(M, N, Q, R, INDEX, B)                           
      INTEGER M                                                         
      INTEGER N, INDEX                                                  
      DOUBLE PRECISION Q(M, M), R(M, 1), B(M)                           
      INTEGER J, I                                                      
      DOUBLE PRECISION ALFA, BETA                                       
C   IF A IS AN MXN MATRIX DEFINED BY QA = R, WHERE Q IS                 
C   AN ORTHOGONAL MATRIX AND R IS A RIGHT TRIANGULAR MATRIX,            
C   THEN THIS PROCEDURE OBTAINS THE Q(T)R FACTORIZATION                 
C   OF THE MATRIX DETERMINED BY APPENDING A COLUMN TO A.                
C   THE COLUMN OF A WHICH IS BEING APPENDED IS DEFINED BY               
C   Q(T)B.  THIS NEW FACTORIZATION IS OBTAINED BY AN                    
C   EFFICIENT UPDATE PROCESS.                                           
C   INPUT-                                                              
C   M      - THE COLUMN DIMENSION OF Q AND R.                           
C   N      - THE ROW DIMENSION OF R (N .LT. M).                         
C   Q      - AN MXM ORTHOGONAL MATRIX.                                  
C   R      - AN MXN RIGHT TRIANGULAR MATRIX.                            
C   INDEX  - THE INDEX OF THE COLUMN TO BE ENTERED.                     
C   B      - THE NEW COLUMN.  IF A IS THE NEW COLUMN  OF                
C          - THE MATRIX A, THEN  B = QA.                                
C   OUTPUT-                                                             
C   N      - THE NEW ROW DIMENSION OF R.                                
C   Q      - THE NEW ORTHOGONAL MATRIX.                                 
C   R      - THE NEW RIGHT TRIANGULAR MATRIX.                           
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LT. 0 .OR. M .LE. N) CALL SETERR(            
C    1   26HDC5APP - INVALID DIMENSION, 26, 1, 2)                       
C     IF (INDEX .LE. 0 .OR. N+1 .LT. INDEX) CALL SETERR(                
C    1   22HDC5APP - INVALID INDEX, 22, 2, 2)                           
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LT. 0 .OR. M .LE. N) CALL SETERR(            
     1   'DC5APP - INVALID DIMENSION', 26, 1, 2)                        
      IF (INDEX .LE. 0 .OR. N+1 .LT. INDEX) CALL SETERR(                
     1   'DC5APP - INVALID INDEX', 22, 2, 2)                            
C/                                                                      
      J = N                                                             
         GOTO  2                                                        
   1     J = J-1                                                        
   2     IF (INDEX .GT. J) GOTO  3                                      
         CALL MOVEBD(M, R(1, J), R(1, J+1))                             
         GOTO  1                                                        
   3  N = N+1                                                           
      CALL MOVEFD(M, B, R(1, INDEX))                                    
      I = M-1                                                           
         GOTO  5                                                        
   4     I = I-1                                                        
   5     IF (INDEX .GT. I) GOTO  6                                      
         CALL DROTG(R(I, INDEX), R(I+1, INDEX), ALFA, BETA)             
         CALL DROT(N-INDEX, R(I, INDEX+1), M, R(I+1, INDEX+1), M, ALFA  
     1      , BETA)                                                     
         CALL DROT(M, Q(I, 1), M, Q(I+1, 1), M, ALFA, BETA)             
         GOTO  4                                                        
   6  RETURN                                                            
      END                                                               
      SUBROUTINE DC5DRP(M, N, Q, R, INDEX)                              
      INTEGER M                                                         
      INTEGER N, INDEX                                                  
      DOUBLE PRECISION Q(M, M), R(M, 1)                                 
      INTEGER J, I                                                      
      DOUBLE PRECISION ALFA, BETA                                       
C   IF A IS AN MXN MATRIX DEFINED BY QA = R, WHERE Q IS                 
C   AN ORTHOGONAL MATRIX AND R IS A RIGHT TRIANGULAR MATRIX,            
C   THEN THIS PROCEDURE OBTAINS THE Q(T)R FACTORIZATION                 
C   OF THE MATRIX DETERMINED BY DELETING A COLUMN OF A.                 
C   THIS NEW FACTORIZATION IS OBTAINED BY AN EFFICIENT UPDATE           
C   PROCESS.                                                            
C   INPUT-                                                              
C   M      - THE COLUMN DIMENSION OF Q AND R.                           
C   N      - THE ROW DIMENSION OF R (N .LE. M).                         
C   Q      - AN MXM ORTHOGONAL MATRIX.                                  
C   R      - AN MXN RIGHT TRIANGULAR MATRIX.                            
C   INDEX  - THE INDEX OF THE COLUMN TO BE DELETED.                     
C   OUTPUT-                                                             
C   N      - THE NEW ROW DIMENSION OF R.                                
C   Q      - THE NEW ORTHOGONAL MATRIX.                                 
C   R      - THE NEW RIGHT TRIANGULAR MATRIX.                           
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LE. 0 .OR. M .LT. N) CALL SETERR(            
C    1   26HDC5DRP - INVALID DIMENSION, 26, 1, 2)                       
C     IF (INDEX .LE. 0 .OR. N .LT. INDEX) CALL SETERR(                  
C    1   22HDC5DRP - INVALID INDEX, 22, 2, 2)                           
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LE. 0 .OR. M .LT. N) CALL SETERR(            
     1   'DC5DRP - INVALID DIMENSION', 26, 1, 2)                        
      IF (INDEX .LE. 0 .OR. N .LT. INDEX) CALL SETERR(                  
     1   'DC5DRP - INVALID INDEX', 22, 2, 2)                            
C/                                                                      
      J = INDEX+1                                                       
         GOTO  2                                                        
   1     J = J+1                                                        
   2     IF (J .GT. N) GOTO  3                                          
         CALL MOVEFD(J, R(1, J), R(1, J-1))                             
         GOTO  1                                                        
   3  N = N-1                                                           
      I = INDEX                                                         
         GOTO  5                                                        
   4     I = I+1                                                        
   5     IF (I .GT. N) GOTO  6                                          
         CALL DROTG(R(I, I), R(I+1, I), ALFA, BETA)                     
         CALL DROT(N-I, R(I, I+1), M, R(I+1, I+1), M, ALFA, BETA)       
         CALL DROT(M, Q(I, 1), M, Q(I+1, 1), M, ALFA, BETA)             
         GOTO  4                                                        
   6  RETURN                                                            
      END                                                               
      SUBROUTINE DM5TOP(M, N, B, ILO, IHI, JLO, JHI, X, IOP, Y)         
      INTEGER M, N                                                      
      INTEGER ILO, IHI, JLO, JHI, IOP                                   
      DOUBLE PRECISION B(M, N), X(1), Y(1)                              
      COMMON /CSTAK/ DSTAK                                              
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER MM, HPTR, ISTAK(1000), ISTKGT                             
      DOUBLE PRECISION WS(500)                                          
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), WS(1))                                     
C   THIS PROCEDURE PERFORMS THE FOLLOWING MATRIX VECTOR COMPUTATIONS    
C   CASE 1-  Y = CX,                                                    
C   CASE 2-  Y = C(T)X,                                                 
C   CASE 3-  Y = RX,                                                    
C   CASE 4-  Y = R(-1)X.                                                
C   THE MATRIX C IS THE RECTANGULAR SUBMATRIX OF B SPECIFIED            
C   BY ILO,IHI,JLO, AND JHI.  THE MATRIX R IS THE RIGHT TRIANGULAR      
C   SUBMATRIX OF B SPECIFIED BY THE SAME INDICES.  THE RIGHT TRIANGULAR 
C   SUBMATRIX MUST BE SQUARE AND CENTERED.                              
C   INPUT-                                                              
C   M      - THE COLUMN DIMENSION OF THE MATRIX.                        
C   N      - THE ROW DIMENSION OF THE MATRIX.                           
C   B      - AN MXN MATRIX                                              
C   ILO    - THE SMALLER RWO INDEX.                                     
C   IHI    - THE LARGER ROW INDEX.                                      
C   JLO    - THE SMALLER COLUMN INDEX.                                  
C   JHI    - THE LARGER COLUMN INDEX.                                   
C   X      - A VECTOR OF LENGTH N FOR CASES 1, 3 AND 4, AND             
C            A VECTOR OF LENGTH M FOR CASE 2.                           
C   IOP    - AN INDEX FOR THE MATRIX VECTOR OPERATION TO BE             
C            PERFORMED.                                                 
C   OUTPUT-                                                             
C   Y      - A VECTOR OF LENGTH M FOR CASES 1, 3 AND 4, AND             
C            A VECTOR OF LENGTH N FOR CASE 2.  THE PROGRAM              
C            IS ORGANIZED SO THAT X AND Y MAY SHARE THE SAME            
C            STORAGE.                                                   
      CALL ENTER(0)                                                     
C/6S                                                                    
C     IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
C    1   26HDM5TOP - INVALID DIMENSION, 26, 2, 2)                       
C     IF (IHI .LT. ILO .OR. JHI .LT. JLO) CALL SETERR(                  
C    1   24HDM5TOP - INVALID INDICES, 24, 3, 2)                         
C     IF (ILO .LT. 1 .OR. M .LT. IHI .OR. JLO .LT. 1 .OR. N .LT. JHI)   
C    1   CALL SETERR(26HDM5TOP - INVALID SUBMATRIX, 26, 4, 2)           
C/7S                                                                    
      IF (M .LE. 0 .OR. N .LE. 0) CALL SETERR(                          
     1   'DM5TOP - INVALID DIMENSION', 26, 2, 2)                        
      IF (IHI .LT. ILO .OR. JHI .LT. JLO) CALL SETERR(                  
     1   'DM5TOP - INVALID INDICES', 24, 3, 2)                          
      IF (ILO .LT. 1 .OR. M .LT. IHI .OR. JLO .LT. 1 .OR. N .LT. JHI)   
     1   CALL SETERR('DM5TOP - INVALID SUBMATRIX', 26, 4, 2)            
C/                                                                      
      GOTO  5                                                           
   1     MM = M                                                         
         GOTO  6                                                        
   2     MM = N                                                         
         GOTO  6                                                        
C/6S                                                                    
C  3     IF (ILO .NE. JLO .OR. IHI .NE. JHI) CALL SETERR(               
C    1      25HDM5TOP - INVALID TRIANGLE, 25, 5, 2)                     
C/7S                                                                    
   3     IF (ILO .NE. JLO .OR. IHI .NE. JHI) CALL SETERR(               
     1      'DM5TOP - INVALID TRIANGLE', 25, 5, 2)                      
C/                                                                      
         MM = M                                                         
         GOTO  6                                                        
C/6S                                                                    
C  4     CALL SETERR(20HDM5TOP - INVALID IOP, 21, 1, 2)                 
C/7S                                                                    
   4     CALL SETERR('DM5TOP - INVALID IOP', 21, 1, 2)                  
C/                                                                      
         GOTO  6                                                        
   5     IF (IOP .EQ. 4) GOTO  3                                        
         IF (IOP .EQ. 3) GOTO  3                                        
         IF (IOP .EQ. 2) GOTO  2                                        
         IF (IOP .EQ. 1) GOTO  1                                        
         GOTO  4                                                        
   6  HPTR = ISTKGT(MM, 4)                                              
      CALL DM6TOP(M, N, B, ILO, IHI, JLO, JHI, X, IOP, Y, WS(HPTR))     
      CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DM6TOP(M, N, B, ILO, IHI, JLO, JHI, X, IOP, Y, H)      
      INTEGER M, N                                                      
      INTEGER ILO, IHI, JLO, JHI, IOP                                   
      DOUBLE PRECISION B(M, N), X(1), Y(1), H(1)                        
      INTEGER I, J                                                      
      DOUBLE PRECISION TOL, D1MACH                                      
      IF (IOP .NE. 1) GOTO 6                                            
         DO  1 I = ILO, IHI                                             
C  COMPUTE  Y = CX.                                                     
            H(I) = 0.0D0                                                
   1        CONTINUE                                                    
         DO  4 J = JLO, JHI                                             
            IF (X(J) .EQ. 0.0D0) GOTO 3                                 
               DO  2 I = ILO, IHI                                       
                  H(I) = H(I)+B(I, J)*X(J)                              
   2              CONTINUE                                              
   3        CONTINUE                                                    
   4        CONTINUE                                                    
         DO  5 I = ILO, IHI                                             
            Y(I) = H(I)                                                 
   5        CONTINUE                                                    
         GOTO  29                                                       
   6     IF (IOP .NE. 2) GOTO 12                                        
            DO  7 J = JLO, JHI                                          
C  COMPUTE  Y = C(T)X.                                                  
               H(J) = 0.0D0                                             
   7           CONTINUE                                                 
            DO  10 I = ILO, IHI                                         
               IF (X(I) .EQ. 0.0D0) GOTO 9                              
                  DO  8 J = JLO, JHI                                    
                     H(J) = H(J)+B(I, J)*X(I)                           
   8                 CONTINUE                                           
   9           CONTINUE                                                 
  10           CONTINUE                                                 
            DO  11 J = JLO, JHI                                         
               Y(J) = H(J)                                              
  11           CONTINUE                                                 
            GOTO  28                                                    
  12        IF (IOP .NE. 3) GOTO 18                                     
               DO  13 I = ILO, IHI                                      
C  COMPUTE  Y = RX.                                                     
                  H(I) = 0.0D0                                          
  13              CONTINUE                                              
               DO  16 J = ILO, IHI                                      
                  IF (X(J) .EQ. 0.0D0) GOTO 15                          
                     DO  14 I = J, IHI                                  
                        H(I) = H(I)+B(I, J)*X(J)                        
  14                    CONTINUE                                        
  15              CONTINUE                                              
  16              CONTINUE                                              
               DO  17 I = ILO, IHI                                      
                  Y(I) = H(I)                                           
  17              CONTINUE                                              
               GOTO  27                                                 
  18           TOL = 1.0D2*D1MACH(1)                                    
C  SOLVE RY = X.                                                        
               DO  19 I = ILO, IHI                                      
                  Y(I) = X(I)                                           
  19              CONTINUE                                              
               I = IHI                                                  
                  GOTO  21                                              
  20              I = I-1                                               
  21              IF (I .LT. ILO) GOTO  26                              
                  J = I+1                                               
                     GOTO  23                                           
  22                 J = J+1                                            
  23                 IF (J .GT. IHI) GOTO  24                           
                     Y(I) = Y(I)-B(I, J)*Y(J)                           
                     GOTO  22                                           
  24              IF (DABS(B(I, I)) .GE. TOL) GOTO 25                   
C/6S                                                                    
C                    CALL SETERR(24HDM5TOP - SINGULAR SYSTEM, 24, 6, 1) 
C/7S                                                                    
                     CALL SETERR('DM5TOP - SINGULAR SYSTEM', 24, 6, 1)  
C/                                                                      
                     RETURN                                             
  25              Y(I) = Y(I)/B(I, I)                                   
                  GOTO  20                                              
  26           CONTINUE                                                 
  27     CONTINUE                                                       
  28  CONTINUE                                                          
  29  RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DV5MAX(M, A, INDEX)                     
      INTEGER M                                                         
      INTEGER INDEX                                                     
      DOUBLE PRECISION A(M)                                             
      INTEGER I                                                         
      DOUBLE PRECISION B                                                
C  THIS LONG REAL FUNCTION RETURNS THE LARGEST COMPONENT                
C  OF THE LONG REAL VECTOR A.                                           
C/6S                                                                    
C     IF (M .LT. 1) CALL SETERR(26HDV5MAX - INVALID DIMENSION, 26, 1, 2)
C/7S                                                                    
      IF (M .LT. 1) CALL SETERR('DV5MAX - INVALID DIMENSION', 26, 1, 2) 
C/                                                                      
      B = A(1)                                                          
      INDEX = 1                                                         
      DO  2 I = 1, M                                                    
         IF (A(I) .LE. B) GOTO 1                                        
            B = A(I)                                                    
            INDEX = I                                                   
   1     CONTINUE                                                       
   2     CONTINUE                                                       
      DV5MAX = B                                                        
      RETURN                                                            
      END                                                               
      INTEGER FUNCTION L5RGXD(NPTS, EN, NEX, IEXT, ILRG, TOL)           
      INTEGER NPTS, NEX                                                 
      INTEGER IEXT(NEX), ILRG, TOL                                      
      DOUBLE PRECISION EN(NPTS)                                         
      INTEGER J, K, L                                                   
      DOUBLE PRECISION HOLD                                             
C    FUNCTION L5RGXD FINDS THE NO. OF ERROR EXTREMA WITH MAGNITUDES     
C    WITHIN TOLERANCE OF MAGNITUDE OF LARGEST ERROR.                    
C/6S                                                                    
C     IF (NPTS .LE. 0) CALL SETERR(24HL5RGXD-INVALID DIMENSION, 24, 1, 2
C    1   )                                                              
C     IF (NEX .LE. 0 .OR. ILRG .LE. 0) CALL SETERR(                     
C    1   20HL5RGXD-INVALID INDEX, 20, 2, 2)                             
C/7S                                                                    
      IF (NPTS .LE. 0) CALL SETERR('L5RGXD-INVALID DIMENSION', 24, 1, 2 
     1   )                                                              
      IF (NEX .LE. 0 .OR. ILRG .LE. 0) CALL SETERR(                     
     1   'L5RGXD-INVALID INDEX', 20, 2, 2)                              
C/                                                                      
      K = 0                                                             
      DO  1 J = 1, NEX                                                  
         L = IEXT(J)                                                    
         HOLD = DABS(EN(ILRG))-DABS(EN(L))                              
         IF (HOLD .LE. 10.0D0**(-TOL)*DABS(EN(ILRG))) K = K+1           
   1     CONTINUE                                                       
      L5RGXD = K                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DC5EQK(NPTS, X, FN, M, N, P, Q, QK, EN)                
      INTEGER NPTS                                                      
      INTEGER M, N                                                      
      DOUBLE PRECISION X(NPTS), FN(NPTS), P(1), Q(1), QK(NPTS), EN(NPTS)
      INTEGER I                                                         
      DOUBLE PRECISION DTCHBP, PK                                       
C   PROCEDURE DC5EQK COMPUTES EN AND QK.                                
C   EN=ERROR VALUES AT MESH POINTS.                                     
C   QK=VALUE OF DENOMINATOR POLYNOMIAL AT MESH POINTS.                  
C/6S                                                                    
C     IF (NPTS .LE. 0 .OR. M .LT. 0 .OR. N .LT. 0) CALL SETERR(         
C    1   23HC5EQK-INVALID DIMENSION, 23, 1, 2)                          
C/7S                                                                    
      IF (NPTS .LE. 0 .OR. M .LT. 0 .OR. N .LT. 0) CALL SETERR(         
     1   'C5EQK-INVALID DIMENSION', 23, 1, 2)                           
C/                                                                      
      DO  1 I = 1, NPTS                                                 
         QK(I) = DTCHBP(N, Q, X(I), X(1), X(NPTS))                      
C/6S                                                                    
C        IF (QK(I) .EQ. 0.0D0) CALL SETERR(21HC5EQK-DIVISOR .EQ. 0., 21,
C    1      2, 2)                                                       
C/7S                                                                    
         IF (QK(I) .EQ. 0.0D0) CALL SETERR('C5EQK-DIVISOR .EQ. 0.', 21, 
     1      2, 2)                                                       
C/                                                                      
         PK = DTCHBP(M, P, X(I), X(1), X(NPTS))                         
         EN(I) = (FN(I)*QK(I)-PK)/QK(I)                                 
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DD5FMT(INPROD, A, MM, NN, IROW, X, DINPRD)             
      INTEGER NN                                                        
      INTEGER MM, IROW                                                  
      LOGICAL INPROD                                                    
      DOUBLE PRECISION A(1), X(NN), DINPRD                              
      COMMON /DD5FCM/ NPTS, M, N, I1, I2, I3, I4                        
      INTEGER NPTS, M, N, I1, I2, I3                                    
      INTEGER I4                                                        
      INTEGER IRM1, IRM2, IRM3, ZPTR, FNPTR, QZPTR                      
      INTEGER JP, MAXMN, J, MAX0                                        
      DOUBLE PRECISION FCT, FDELK, DELK, Z, FN, QZ                      
      DOUBLE PRECISION DTCHBP                                           
C   DD5FMT HANDLES REFERENCES BY THE LP ROUTINE TO                      
C   THE MATRIX FOR THE LINEAR PROGRAMMING SUBPROBLEM.                   
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (MM .NE. I4 .OR. NN .NE. M+N+3) CALL SETERR(                   
C    1   26HDD5FMT - INVALID DIMENSION, 26, 1, 2)                       
C     IF (IROW .LT. 0 .OR. MM .LT. IROW) CALL SETERR(                   
C    1   22HDD5FMT - INVALID INDEX, 22, 2, 2)                           
C/7S                                                                    
      IF (MM .NE. I4 .OR. NN .NE. M+N+3) CALL SETERR(                   
     1   'DD5FMT - INVALID DIMENSION', 26, 1, 2)                        
      IF (IROW .LT. 0 .OR. MM .LT. IROW) CALL SETERR(                   
     1   'DD5FMT - INVALID INDEX', 22, 2, 2)                            
C/                                                                      
      IRM1 = IROW-I1                                                    
      IRM2 = IROW-I2                                                    
      IRM3 = IROW-I3                                                    
      IF ((.NOT. INPROD) .OR. I2 .GE. IROW) GOTO 3                      
         IF (I3 .GE. IROW) GOTO 1                                       
            DINPRD = -X(IRM3)                                           
            GOTO  2                                                     
   1        DINPRD = X(IRM2)                                            
   2     CONTINUE                                                       
         GOTO  18                                                       
   3     IF (I2 .GE. IROW) GOTO 6                                       
            CALL SETD(NN, 0.0D0, X)                                     
            IF (I3 .GE. IROW) GOTO 4                                    
               X(IRM3) = -1.0D0                                         
               GOTO  5                                                  
   4           X(IRM2) = 1.0D0                                          
   5        CONTINUE                                                    
            GOTO  17                                                    
   6        IF (I1 .GE. IROW) GOTO 7                                    
               FCT = -1.0D0                                             
               ZPTR = IRM1                                              
               GOTO  8                                                  
   7           FCT = 1.0D0                                              
               ZPTR = IROW                                              
   8        Z = A(ZPTR)                                                 
            FNPTR = ZPTR+NPTS                                           
            FN = A(FNPTR)                                               
            QZPTR = FNPTR+NPTS                                          
            QZ = A(QZPTR)                                               
            DELK = A(3*NPTS+1)                                          
            FDELK = FCT*FN+DELK                                         
            IF (.NOT. INPROD) GOTO 9                                    
               DINPRD = FDELK*DTCHBP(N, X, Z, A(1), A(NPTS))-FCT*DTCHBP(
     1            M, X(N+2), Z, A(1), A(NPTS))+QZ*X(NN)                 
               GOTO  16                                                 
   9           MAXMN = MAX0(M, N)                                       
               CALL DT5COF(Z, A(1), A(NPTS), MAXMN, X)                  
               J = M+1                                                  
                  GOTO  11                                              
  10              J = J-1                                               
  11              IF (1 .GT. J) GOTO  12                                
                  JP = J+N+1                                            
                  X(JP) = (-FCT)*X(J)                                   
                  GOTO  10                                              
  12           J = 1                                                    
                  GOTO  14                                              
  13              J = J+1                                               
  14              IF (J .GT. N+1) GOTO  15                              
                  X(J) = FDELK*X(J)                                     
                  GOTO  13                                              
  15           X(NN) = QZ                                               
  16        CONTINUE                                                    
  17  CONTINUE                                                          
  18  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DT5COF(X, A, B, DEG, XX)                               
      INTEGER DEG                                                       
      DOUBLE PRECISION X, A, B, XX(2)                                   
      INTEGER I                                                         
      DOUBLE PRECISION TWOXX                                            
C    PROCEDURE DT5COF COMPUTES THE DEG+1 TCHEBYCHEFF                    
C    COEFFICIENTS OF THE POINT X.                                       
      CALL ENTER(1)                                                     
C/6S                                                                    
C     IF (DEG .LT. 0) CALL SETERR(21HDT5COF-INVALID DEGREE, 21, 1, 2)   
C/7S                                                                    
      IF (DEG .LT. 0) CALL SETERR('DT5COF-INVALID DEGREE', 21, 1, 2)    
C/                                                                      
      XX(1) = 1.0D0                                                     
      IF (DEG .LE. 0) GOTO 3                                            
         IF (B .GT. A) GOTO 1                                           
C/6S                                                                    
C           CALL SETERR(23HDT5COF-INVALID INTERVAL, 23, 2, 2)           
C/7S                                                                    
            CALL SETERR('DT5COF-INVALID INTERVAL', 23, 2, 2)            
C/                                                                      
            GOTO  2                                                     
   1        XX(2) = 2.0D0*(X-(A+B)/2.0D0)/(B-A)                         
CSCALE X TO THE INTERVAL (-1.0D0,1.0D0)                                 
   2  CONTINUE                                                          
   3  IF (DEG .GT. 1) TWOXX = 2.0D0*XX(2)                               
      I = 3                                                             
         GOTO  5                                                        
   4     I = I+1                                                        
   5     IF (I .GT. DEG+1) GOTO  6                                      
         XX(I) = TWOXX*XX(I-1)-XX(I-2)                                  
         GOTO  4                                                        
   6  CALL LEAVE                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DL7PF(BC, BX, C, G, IW, LIW, LW, M, P, PP, W, X)       
      INTEGER LIW, M, P, PP, LW                                         
      INTEGER IW(LIW)                                                   
      DOUBLE PRECISION BC(2, M), BX(2, P), C(PP, M), G(P), W(LW), X(P)  
      EXTERNAL DV2NRM,DL7P2                                             
      INTEGER MPP, I, Q, R, S, I1                                       
      INTEGER W1, CN, LR                                                
      DOUBLE PRECISION DV2NRM, CNI, ONE, ZERO                           
      DATA ONE/1.D+0/                                                   
      DATA ZERO/0.D+0/                                                  
C LP SOLVER, INEQUALITIES ONLY, STARTING FROM FEASIBLE X.               
C *** LOCAL VARIABLES ***                                               
C *** BODY ***                                                          
      IW(1) = -2                                                        
      MPP = M+P                                                         
      IF (LIW .LT. MPP+7) GOTO  60                                      
      CN = P+2                                                          
      W1 = CN+MPP                                                       
      S = W1+5*P                                                        
      R = S+P                                                           
      LR = P*(P+1)/2                                                    
      Q = R+LR                                                          
      IW(1) = -(Q+P*P)                                                  
      IF (IW(1)+LW .LT. 0) GOTO  60                                     
      IW(1) = 0                                                         
      IW(3) = 0                                                         
      IW(4) = 0                                                         
      IW(5) = 0                                                         
      IW(6) = 0                                                         
      IW(7) = P                                                         
      I1 = MPP+7                                                        
      DO  10 I = 8, I1                                                  
         IW(I) = I-7                                                    
  10     CONTINUE                                                       
      I1 = CN                                                           
      DO  20 I = 1, P                                                   
         W(I1) = ONE                                                    
         I1 = I1+1                                                      
  20     CONTINUE                                                       
      IF (M .LE. 0) GOTO 50                                             
         DO  40 I = 1, M                                                
            CNI = DV2NRM(P, C(1, I))                                    
            IF (CNI .GT. ZERO) GOTO 30                                  
               IW(1) = I+MPP                                            
               GOTO  60                                                 
  30        W(I1) = CNI                                                 
            I1 = I1+1                                                   
  40        CONTINUE                                                    
  50  CALL DL7P2(IW(8), BC, BX, C, W(CN), G, IW(1), IW(2), LR, M, IW(3),
     1   IW(4), IW(5), MPP, MPP, P, IW(7), PP, W(Q), W(R), W(1), W(S), W
     2   (W1), X)                                                       
  60  RETURN                                                            
      END                                                               
      SUBROUTINE DL7P2(A, BC, BX, C, CN, G, INFO, ITER, LR, M, M0,      
     1   MC, ME, MPP, MPP0, P, PC, PP, Q, R, RCOND, S, W, X)            
      INTEGER MPP, MPP0, M, P, LR, PP                                   
      INTEGER A(MPP), INFO, ITER, M0, MC, ME                            
      INTEGER PC                                                        
      DOUBLE PRECISION BC(2, M), BX(2, P), C(PP, M), CN(MPP0), G(P), Q(P
     1   , P)                                                           
      DOUBLE PRECISION R(LR), RCOND, S(P), W(P, 3), X(P)                
      EXTERNAL DL7CHP, DB7VAL, DR7MDC,DV2AXY, DD7TP1                    
      INTEGER NCA, NCD, I, J, K, L                                      
      INTEGER MC1                                                       
      LOGICAL MOVE                                                      
      DOUBLE PRECISION BIG, DB7VAL, DR7MDC, MPMEPS, DBLE, T             
      DOUBLE PRECISION DABS, B1, ZERO, T1, SC, DD7TP1                   
      DATA BIG/0.D+0/                                                   
      DATA MPMEPS/0.D+0/                                                
      DATA ZERO/0.D+0/                                                  
C *** LOCAL VARIABLES ***                                               
C-------------------------------  BODY  --------------------------------
      IF (BIG .GT. ZERO) GOTO 10                                        
         BIG = DR7MDC(6)                                                
         MPMEPS = (-DBLE(FLOAT(P)))*DR7MDC(3)                           
  10  MC = P-PC                                                         
      M0 = MC                                                           
      ITER = 0                                                          
  20     ITER = ITER+1                                                  
         CALL DL7CHP(A, .FALSE., C, CN, G, LR, M, M0, MC, ME, MOVE, MPP,
     1      MPP0, NCA, NCD, P, PC, PP, Q, R, RCOND, S, W, W(1, 2), W(1  
     2      , 3))                                                       
         IF (.NOT. MOVE) GOTO  100                                      
         K = 0                                                          
         T = BIG                                                        
         IF (MC .GE. MPP) GOTO 60                                       
            MC1 = MC+1                                                  
            DO  50 I = MC1, MPP                                         
               J = IABS(A(I))                                           
               SC = DD7TP1(C, J, P, PP, S)                              
               IF (SC .EQ. ZERO) GOTO  50                               
               IF (SC .LE. ZERO) GOTO 30                                
                  J = -J                                                
                  SC = -SC                                              
  30           B1 = DB7VAL(BC, BX, J, M, P)                             
               IF (DABS(B1) .GE. BIG) GOTO  50                          
               T1 = (B1-DD7TP1(C, J, P, PP, X))/SC                      
               IF (T .LE. T1) GOTO 40                                   
                  K = I                                                 
                  L = J                                                 
                  T = T1                                                
                  MOVE = T*SC .LT. MPMEPS*DABS(B1)                      
  40           CONTINUE                                                 
  50           CONTINUE                                                 
  60     IF (K .NE. 0) GOTO 70                                          
            INFO = -1                                                   
            GOTO  110                                                   
  70     IF (.NOT. MOVE) GOTO 80                                        
            CALL DV2AXY(P, X, T, S, X)                                  
            M0 = MC1                                                    
            A(K) = A(M0)                                                
            A(M0) = L                                                   
            GOTO  20                                                    
  80     IF (K .LE. M0) GOTO 90                                         
            M0 = M0+1                                                   
            A(K) = A(M0)                                                
            A(M0) = L                                                   
            GOTO  20                                                    
  90     A(K) = L                                                       
         GOTO  20                                                       
 100  CONTINUE                                                          
 110  RETURN                                                            
      END                                                               
      SUBROUTINE DL7CHP(A, ADD2QR, C, CN, G, LR, M, M0, MC, ME,         
     1   MOVE, MPP, MPP0, NCA, NCD, P, PC, PP, Q, R, RCOND, S, W, X, Y) 
      INTEGER MPP, MPP0, M, P, LR, PP                                   
      INTEGER A(MPP), M0, MC, ME, NCA, NCD                              
      INTEGER PC                                                        
      LOGICAL ADD2QR, MOVE                                              
      DOUBLE PRECISION C(PP, M), CN(MPP0), G(P), Q(P, P), R(LR), RCOND  
      DOUBLE PRECISION S(P), W(P), X(P), Y(P)                           
      EXTERNAL DV2NRM, DC7COL, DR7MDC, DV7SCL, I7SHFT, DD7TPR           
      EXTERNAL DL7SVX, DL7SVN, DL7ITV, DV7SCP,DQ7RGS,DV7CPY             
      EXTERNAL DV2AXY, DD7TP1, DQ7RS1                                   
      INTEGER MCD, MCSAVE, I, J, K, L                                   
      INTEGER IERR, JA, KA, MC0, MC1                                    
      INTEGER ME1                                                       
      LOGICAL DRDONE, ADDTRY, DEGEN, SAMEB, XZERO                       
      DOUBLE PRECISION DV2NRM, ONE, EPS, SNI, NEGONE, DR7MDC            
      DOUBLE PRECISION TWO, DD7TPR, NPMEPS, DL7SVX, DL7SVN, DBLE        
      DOUBLE PRECISION T, ZETA, MEPS, ZERO, P1, DMIN1                   
      DOUBLE PRECISION DMAX1, CS, CX, GS, SI, GNORM                     
      DOUBLE PRECISION PMEPS, SNORM, GX0, DD7TP1                        
      INTEGER TEMP                                                      
      DATA NEGONE/-1.D+0/                                               
      DATA ONE/1.D+0/                                                   
      DATA P1/0.1D+0/                                                   
      DATA TWO/2.D+0/                                                   
      DATA ZERO/0.D+0/                                                  
      DATA MEPS/0.D+0/                                                  
C M0 = NO. OF CONSTRAINTS SATISFIED AS EQUALITIES BY S = 0              
C MC = NO. OF CONSTRAINTS SATISFIED AS EQUALITIES BY RETURN VALUE OF S  
C ME = NO. OF EQUALITY CONSTRAINTS                                      
C MOVE IS SET TO TRUE IFF THE S RETURNED IS NONZERO                     
C *** LOCAL VARIABLES ***                                               
C-------------------------------  BODY  --------------------------------
      CALL DV7SCP(P, X, ZERO)                                           
      XZERO = .TRUE.                                                    
      DRDONE = .FALSE.                                                  
      ADDTRY = .TRUE.                                                   
      SAMEB = .FALSE.                                                   
      GNORM = DV2NRM(P, G)                                              
      IF (GNORM .LE. ZERO) GOTO  440                                    
      IF (MEPS .LE. ZERO) MEPS = DR7MDC(3)                              
      PMEPS = DBLE(FLOAT(P))*MEPS                                       
      NPMEPS = -PMEPS                                                   
      ME1 = ME+1                                                        
      MC0 = P-PC                                                        
      MCD = MC0                                                         
      NCA = 0                                                           
      NCD = 0                                                           
      RCOND = ONE                                                       
      IF (MC0 .GT. 0) RCOND = DL7SVN(MC0, R, S, S)/DL7SVX(MC0, R, S, S) 
      DEGEN = .FALSE.                                                   
  10  MOVE = .FALSE.                                                    
      MC0 = P-PC                                                        
      CALL DV7SCL(P, S, NEGONE, G)                                      
      IF (MC0 .LE. 0) GOTO 30                                           
         DO  20 I = 1, MC0                                              
            W(I) = DD7TPR(P, Q(1, I), S)                                
            CALL DV2AXY(P, S, -W(I), Q(1, I), S)                        
  20        CONTINUE                                                    
  30  SNORM = DV2NRM(P, S)                                              
      IF (PC .LE. 0) GOTO  150                                          
      IF (SNORM .LE. (GNORM/RCOND)*PMEPS) GOTO  150                     
      CALL DV7SCL(P, S, ONE/SNORM, S)                                   
      GS = DD7TPR(P, G, S)                                              
      IF (GS .GE. ZERO) GOTO  140                                       
      IF (XZERO) GOTO 40                                                
         IF ((GS-GX0)/GX0 .LE. PMEPS) GOTO  140                         
  40  MC1 = MC+1                                                        
      K = 0                                                             
      ZETA = ONE                                                        
      IF (.NOT. DRDONE) GOTO 50                                         
         CS = DD7TP1(C, A(MC1), P, PP, S)                               
         IF (CS .LT. ZERO) GOTO  140                                    
         MC1 = MC1+1                                                    
  50  SAMEB = .FALSE.                                                   
      IF (MC1 .GT. M0) GOTO 80                                          
         L = MPP0                                                       
         DO  70 I = MC1, M0                                             
            J = A(I)                                                    
            CS = DD7TP1(C, J, P, PP, S)                                 
            IF (CS .GE. ZERO) GOTO  70                                  
            CX = ZERO                                                   
            IF (.NOT. XZERO) CX = DMAX1(ZERO, DD7TP1(C, J, P, PP, X))   
            T = CX-CS                                                   
            IF (T*ZETA .LT. CX) GOTO  70                                
            IF (.NOT. DEGEN) GOTO 60                                    
               IF (L .LT. J) GOTO  70                                   
               L = J                                                    
  60        ZETA = CX/T                                                 
            K = I                                                       
  70        CONTINUE                                                    
  80  DEGEN = ZETA .LT. P1                                              
      IF (K .NE. 0) GOTO 90                                             
         MOVE = .TRUE.                                                  
         CALL DV7CPY(P, X, S)                                           
         GX0 = GS                                                       
         XZERO = .FALSE.                                                
         SAMEB = .TRUE.                                                 
         GOTO  150                                                      
  90  IF (ZETA .LE. PMEPS) GOTO 110                                     
         DO  100 I = 1, P                                               
            X(I) = X(I)+ZETA*(S(I)-X(I))                                
 100        CONTINUE                                                    
         CALL DV7SCL(P, X, ONE/DV2NRM(P, X), X)                         
         GX0 = DD7TPR(P, G, X)                                          
 110  MCD = MC0                                                         
      J = A(K)                                                          
      MC1 = MC0+1                                                       
      ADDTRY = .TRUE.                                                   
      CALL DC7COL(C, J, M, P, PP, Q(1, MC1))                            
      CALL DQ7RGS(IERR, A, MC1, P, P, P, MC1, Q, R, Y)                  
      MC = MC+1                                                         
      IF (.NOT. DRDONE) GOTO 120                                        
         I = A(MC)                                                      
         A(MC) = J                                                      
         A(K) = A(MC+1)                                                 
         A(MC+1) = I                                                    
         GOTO  130                                                      
 120     A(K) = A(MC)                                                   
         A(MC) = J                                                      
 130  IF (IERR .NE. 0) GOTO  40                                         
      IF (DD7TPR(P, Q(1, MC1), S) .GE. ZERO) GOTO  40                   
      T = DL7SVN(MC1, R, Y, Y)/DL7SVX(MC1, R, Y, Y)                     
      IF (T .LE. PMEPS) GOTO  40                                        
      DRDONE = .FALSE.                                                  
      RCOND = T                                                         
      PC = PC-1                                                         
      NCA = NCA+1                                                       
      MCD = MCD+1                                                       
      A(MC) = A(MC1)                                                    
      A(MC1) = J                                                        
      CALL DV2AXY(P, S, -DD7TPR(P, S, Q(1, MC1)), Q(1, MC1), S)         
      GOTO  10                                                          
 140  IF (ADDTRY) GOTO  150                                             
      MC = MCSAVE                                                       
      PC = PC-1                                                         
      MC0 = P-PC                                                        
 150  K = 0                                                             
      IF (ME1 .GT. MCD) GOTO 240                                        
         EPS = PMEPS/RCOND                                              
         CALL DL7ITV(MC0, Y, R, W)                                      
         SNI = DV2NRM(MC0, Y)                                           
         IF (SNI .LE. ZERO) GOTO 230                                    
            SNI = ONE/SNI                                               
            IF (.NOT. DEGEN) GOTO 200                                   
               DO  190 I = ME1, MCD                                     
                  SI = Y(I)                                             
                  IF (SNI*SI .LT. EPS) GOTO  190                        
                  JA = IABS(A(I))                                       
                  IF (K .NE. 0) GOTO 160                                
                     K = I                                              
                     KA = JA                                            
                     GOTO  180                                          
 160                 IF (KA .LE. JA) GOTO 170                           
                        K = I                                           
                        KA = JA                                         
 170              CONTINUE                                              
 180              CONTINUE                                              
 190              CONTINUE                                              
               GOTO  220                                                
 200           T = ZERO                                                 
               DO  210 I = ME1, MCD                                     
                  SI = Y(I)*SNI                                         
                  IF (SI .LT. EPS) GOTO  210                            
                  JA = IABS(A(I))                                       
                  SI = SI/CN(JA)                                        
                  IF (SI .LT. T) GOTO  210                              
                  T = SI                                                
                  K = I                                                 
 210              CONTINUE                                              
 220        CONTINUE                                                    
 230     CONTINUE                                                       
 240  IF (K .NE. 0) GOTO  250                                           
      IF (MOVE) GOTO  460                                               
      IF (XZERO) GOTO  450                                              
      IF (SAMEB) GOTO  420                                              
      GOTO  270                                                         
 250  IF (K .GE. MC0) GOTO 260                                          
         CALL DQ7RS1(K, LR, MC0, P, Q, R, Y)                            
         CALL I7SHFT(MC0, K, A)                                         
 260  MCD = MCD-1                                                       
      J = MC0*(MC0+1)/2                                                 
      K = IABS(A(MC0))                                                  
      IF ((DD7TPR(P, Q(1, MC0), G)/GNORM)*(R(J)/CN(K)) .GE. NPMEPS)     
     1   GOTO  150                                                      
      RCOND = ONE                                                       
      MCSAVE = MC                                                       
      MC = MC0-1                                                        
      IF (MC .GT. 0) RCOND = DL7SVN(MC, R, Y, Y)/DL7SVX(MC, R, Y, Y)    
      PC = PC+1                                                         
      NCD = NCD+1                                                       
      ADDTRY = .FALSE.                                                  
      DRDONE = .TRUE.                                                   
      GOTO  10                                                          
C *** SPECIAL CASE OF FINISH WITH MOVE = FALSE AND XZERO = FALSE...     
C ***  MAKE SURE THAT                                                   
C *** (1) S IS ORTHOGONAL TO CONSTRAINTS THE A ARRAY CALLS ACTIVE,      
C *** (2) S DOES NOT MAKE A NEGATIVE INNER PRODUCT WITH CONSTRAINTS     
C ***  THAT THE A ARRAY CALLS INACTIVE,                                 
C *** (3) RCOND CORRESPONDS TO RETURNED R, AND                          
C *** (4) Y = VECTOR OF LAGRANGE MULTIPLIERS.                           
 270  MC0 = P-PC                                                        
      K = MC+1                                                          
      I = MC                                                            
         GOTO  290                                                      
 280     I = I-1                                                        
 290     IF (I .LE. 0) GOTO  340                                        
         J = A(I)                                                       
         IF (DD7TP1(C, J, P, PP, X) .LE. ZERO) GOTO 330                 
            IF (I .GE. MC0) GOTO 300                                    
               CALL DQ7RS1(I, LR, MC0, P, Q, R, Y)                      
               CALL I7SHFT(MC0, I, A)                                   
               GOTO  310                                                
 300           A(I) = A(MC)                                             
               A(MC) = J                                                
 310        IF (I .GT. MC0) GOTO 320                                    
               MC0 = MC0-1                                              
               PC = PC+1                                                
 320        MC = MC-1                                                   
 330     CONTINUE                                                       
         GOTO  280                                                      
 340  IF (PC .LE. 0) GOTO 390                                           
         GOTO  360                                                      
 350        K = K+1                                                     
 360        IF (K .GT. M0) GOTO  380                                    
            J = A(K)                                                    
            IF (DD7TP1(C, J, P, PP, X) .GE. ZERO) GOTO 370              
               MC1 = MC0+1                                              
               CALL DC7COL(C, J, M, P, PP, Q(1, MC1))                   
               CALL DQ7RGS(IERR, A, MC1, P, P, P, MC1, Q, R, Y)         
               MC = MC+1                                                
               A(K) = A(MC)                                             
               A(MC) = J                                                
               IF (IERR .NE. 0) GOTO  350                               
               T = DL7SVN(MC1, R, Y, Y)/DL7SVX(MC1, R, Y, Y)            
               IF (T .LE. PMEPS) GOTO  350                              
               RCOND = T                                                
               MC0 = MC1                                                
               PC = PC-1                                                
               IF (PC .LE. 0) GOTO  380                                 
 370        CONTINUE                                                    
            GOTO  350                                                   
 380  CONTINUE                                                          
 390  IF (MC0 .LE. 0) GOTO 410                                          
         CALL DV7SCL(P, S, NEGONE, G)                                   
         DO  400 I = 1, MC0                                             
            W(I) = DD7TPR(P, Q(1, I), S)                                
            CALL DV2AXY(P, S, -W(I), Q(1, I), S)                        
 400        CONTINUE                                                    
         CALL DL7ITV(MC0, Y, R, W)                                      
 410  CONTINUE                                                          
 420  CALL DV7CPY(P, S, X)                                              
      MOVE = .TRUE.                                                     
      TEMP = MC+1                                                       
      DO  430 I = TEMP, M0                                              
         J = A(I)                                                       
         JA = IABS(J)                                                   
         T = DMIN1(T, DD7TP1(C, J, P, PP, X)/CN(JA))                    
 430     CONTINUE                                                       
      GOTO  480                                                         
 440  CALL DV7SCP(P, Y, ZERO)                                           
 450  CALL DV7SCP(P, S, ZERO)                                           
      MOVE = .FALSE.                                                    
      GOTO  480                                                         
 460  IF (.NOT. ADD2QR) GOTO 470                                        
         MC0 = P-PC                                                     
         CALL DV7CPY(P, Q(1, MC0+1), S)                                 
         J = MC0*(MC0+1)/2+1                                            
         K = J+MC0                                                      
         IF (MC0 .GT. 0) CALL DV7CPY(MC0, R(J), W)                      
         R(K) = SNORM                                                   
 470  CONTINUE                                                          
 480  RETURN                                                            
      END                                                               
        DOUBLE PRECISION FUNCTION DB7VAL(BC, BX, J, M, P)               
C                                                                       
C  *** RETURN BX(1,J) FOR 1 .LE. J .LE. P, BC(1,J-P) FOR J .GT. P,      
C  *** -BX(2,-J) FOR -P .LE. J .LE. -1,  -BC(2,-J-P) FOR J .LT. -P.     
C                                                                       
        INTEGER J, M, P                                                 
        DOUBLE PRECISION BC(2,M), BX(2,P)                               
C                                                                       
        INTEGER I                                                       
C                                                                       
C  ***  BODY                                                            
C                                                                       
        IF (J .LT. 0) GO TO 20                                          
            IF (J .GT. P) GO TO 10                                      
                DB7VAL = BX(1,J)                                        
                GO TO 999                                               
C                                                                       
 10         I = J - P                                                   
            DB7VAL = BC(1,I)                                            
            GO TO 999                                                   
C                                                                       
 20     I = -J                                                          
        IF (I .GT. P) GO TO 30                                          
            DB7VAL = -BX(2,I)                                           
            GO TO 999                                                   
C                                                                       
 30     I = I - P                                                       
        DB7VAL = -BC(2,I)                                               
C                                                                       
 999    RETURN                                                          
        END                                                             
      DOUBLE PRECISION FUNCTION DD7TP1(C, J, P, PP, X)                  
      INTEGER P, PP                                                     
      INTEGER J                                                         
      DOUBLE PRECISION C(PP, 1), X(P)                                   
      EXTERNAL DD7TPR                                                   
      INTEGER I                                                         
      DOUBLE PRECISION DD7TPR, T                                        
      I = IABS(J)                                                       
      IF (I .GT. P) GOTO 10                                             
         T = X(I)                                                       
         GOTO  20                                                       
  10     I = I-P                                                        
         T = DD7TPR(P, C(1, I), X)                                      
  20  IF (J .LT. 0) T = -T                                              
      DD7TP1 = T                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE DQ7RGS(IERR, IPIVOT, L, N, NN, NOPIVK, P, Q, R, W)     
C                                                                       
C  ***  COMPUTE QR FACTORIZATION VIA MODIFIED GRAM-SCHMIDT PROCEDURE    
C  ***  WITH COLUMN PIVOTING  ***                                       
C                                                                       
C  ***  PARAMETER DECLARATIONS  ***                                     
C                                                                       
      INTEGER IERR, L, N, NN, NOPIVK, P                                 
      INTEGER IPIVOT(P)                                                 
      DOUBLE PRECISION Q(NN,P), R(1), W(P)                              
C     DIMENSION R(P*(P+1)/2)                                            
C                                                                       
C----------------------------  DESCRIPTION  ----------------------------
C                                                                       
C        THIS ROUTINE COMPUTES COLUMNS  L  THROUGH  P  OF A QR FACTORI- 
C     ZATION OF THE MATRIX  A  THAT IS ORIGINALLY STORED IN COLUMNS  L  
C     THROUGH  P  OF  Q.  IT IS ASSUMED THAT COLUMNS 1 THROUGH  L-1  OF 
C     THE FACTORIZATION HAVE ALREADY BEEN STORED IN  Q  AND  R.  THIS   
C     CODE USES THE MODIFIED GRAM-SCHMIDT PROCEDURE WITH REORTHOGONALI- 
C     ZATION AND, IF  NOPIVK  ALLOWS IT, WITH COLUMN PIVOTING -- IF     
C     K .GT. NOPIVK,  THEN ORIGINAL COLUMN  K  IS ELIGIBLE FOR PIVOTING.
C     IF  IPIVOT(L) = 0  ON INPUT, THEN  IPIVOT  IS INITIALIZED SO THAT 
C     IPIVOT(I) = I  FOR  I = L,...,P.  WHATEVER THE ORIGINAL VALUE OF  
C     IPIVOT(L), THE CORRESPONDING ELEMENTS OF  IPIVOT  ARE INTERCHANGED
C     WHENEVER COLUMN PIVOTING OCCURS.  THUS IF  IPIVOT(L) = 0  ON IN-  
C     PUT, THEN THE  Q  AND  R  RETURNED ARE SUCH THAT COLUMN  I  OF    
C     Q*R  EQUALS COLUMN  IPIVOT(I)  OF THE ORIGINAL MATRIX  A.  THE UP-
C     PER TRIANGULAR MATRIX  R  IS STORED COMPACTLY BY COLUMNS, I.E.,   
C     THE OUTPUT VECTOR  R  CONTAINS  R(1,1), R(1,2), R(2,2), R(1,3),   
C     R(2,3), ..., R(P,P) (IN THAT ORDER).  IF ALL GOES WELL, THEN THIS 
C     ROUTINE SETS  IERR = 0.  BUT IF (PERMUTED) COLUMN  K  OF  A  IS   
C     LINEARLY DEPENDENT ON (PERMUTED) COLUMNS 1,2,...,K-1, THEN  IERR  
C     IS SET TO  K AND THE R MATRIX RETURNED HAS  R(I,J) = 0  FOR       
C     I .GE. K  AND  J .GE. K.  IN THIS CASE COLUMNS  K  THROUGH  P     
C     OF THE  Q  RETURNED ARE NOT ORTHONORMAL.  W IS A SCRATCH VECTOR.  
C        THE ORIGINAL MATRIX  A  AND THE COMPUTED ORTHOGONAL MATRIX  Q  
C     ARE  N BY P  MATRICES.  NN  IS THE LEAD DIMENSION OF THE ARRAY  Q 
C     AND MUST SATISFY  NN .GE. N.  NO PARAMETER CHECKING IS DONE.      
C                                                                       
C        CODED BY DAVID M. GAY (FALL 1979, SPRING 1984).                
C                                                                       
C--------------------------  LOCAL VARIABLES  --------------------------
C                                                                       
      INTEGER I, II, J, K, KK, KM1, KP1, LM1                            
      LOGICAL IPINIT                                                    
      DOUBLE PRECISION AK, SINGTL, T, T1, T2, WK                        
      EXTERNAL DD7TPR, DR7MDC,DV2AXY, DV7SCP,DV7SWP, DV2NRM, DV7SCL     
      DOUBLE PRECISION DD7TPR, DR7MDC, DV2NRM                           
C/+                                                                     
      DOUBLE PRECISION DSQRT                                            
C/                                                                      
      DOUBLE PRECISION BIG, MEPS10, ONE, REOTOL, TEN, TINY, WTOL, ZERO  
C/6                                                                     
C     DATA ONE/1.0D+0/, REOTOL/0.25D+0/, TEN/1.D+1/, WTOL/0.75D+0/,     
C    1     ZERO/0.0D+0/                                                 
C/7                                                                     
      PARAMETER (ONE=1.0D+0, REOTOL=0.25D+0, TEN=1.D+1, WTOL=0.75D+0,   
     1           ZERO=0.0D+0)                                           
      SAVE MEPS10, TINY                                                 
C/                                                                      
      DATA MEPS10/0.0D+0/, TINY/0.0D+0/                                 
C                                                                       
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C                                                                       
      IERR = 0                                                          
      IF (MEPS10 .GT. ZERO) GO TO 10                                    
         MEPS10 = TEN * DR7MDC(3)                                       
         TINY = DR7MDC(1)                                               
         BIG = DR7MDC(6)                                                
         IF (TINY*BIG .LT. ONE) TINY = ONE / BIG                        
 10   SINGTL = FLOAT(MAX0(N,P)) * MEPS10                                
      LM1 = L - 1                                                       
      J = L*LM1/2                                                       
      KK = J                                                            
      IPINIT = IPIVOT(L) .EQ. 0                                         
C                                                                       
C  ***  INITIALIZE W, IPIVOT, DIAG(R), AND R(I,J) FOR I = 1,2,...,L-1   
C  ***  AND J = L,L+1,...,P.                                            
C                                                                       
      DO 50 I = L, P                                                    
         IF (IPINIT) IPIVOT(I) = I                                      
         T = DV2NRM(N, Q(1,I))                                          
         IF (T .GT. ZERO) GO TO 20                                      
              W(I) = ONE                                                
              J = J + LM1                                               
              GO TO 40                                                  
 20      W(I) = ZERO                                                    
         IF (LM1 .EQ. 0) GO TO 40                                       
              DO 30 K = 1, LM1                                          
                   J = J + 1                                            
                   T1 = DD7TPR(N, Q(1,K), Q(1,I))                       
                   R(J) = T1                                            
                   CALL DV2AXY(N, Q(1,I), -T1, Q(1,K), Q(1,I))          
                   W(I) = W(I) + (T1/T)**2                              
 30                CONTINUE                                             
 40      J = J + I - LM1                                                
         R(J) = T                                                       
 50      CONTINUE                                                       
C                                                                       
C  ***  MAIN LOOP  ***                                                  
C                                                                       
      DO 140 K = L, P                                                   
         KK = KK + K                                                    
         KP1 = K + 1                                                    
         IF (K .LE. NOPIVK) GO TO 70                                    
         IF (K .GE. P) GO TO 70                                         
C                                                                       
C        ***  FIND COLUMN WITH MINIMUM WEIGHT LOSS  ***                 
C                                                                       
              T = W(K)                                                  
              IF (T .LE. ZERO) GO TO 70                                 
              J = K                                                     
              DO 60 I = KP1, P                                          
                   IF (W(I) .GE. T) GO TO 60                            
                        T = W(I)                                        
                        J = I                                           
 60                CONTINUE                                             
              IF (J .EQ. K) GO TO 70                                    
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
                   IF (K .LE. 1) GO TO 70                               
                        I = I - J + 1                                   
                        J = KK - K + 1                                  
                        CALL DV7SWP(K-1, R(I), R(J))                    
C                                                                       
C        ***  COLUMN K OF Q SHOULD BE NEARLY ORTHOGONAL TO THE PREVIOUS 
C        ***  COLUMNS.  NORMALIZE IT, TEST FOR SINGULARITY, AND DECIDE  
C        ***  WHETHER TO REORTHOGONALIZE IT.                            
C                                                                       
 70      AK = R(KK)                                                     
         IF (AK .LE. ZERO) GO TO 150                                    
         T1 = AK                                                        
         R(KK) = ONE                                                    
         T2 = ONE                                                       
         WK = W(K)                                                      
C                                                                       
C        *** SET T TO THE NORM OF (Q(K,K),...,Q(N,K))                   
C        *** AND CHECK FOR SINGULARITY.                                 
C                                                                       
 80      IF (WK .LT. WTOL) GO TO 90                                     
            T = DV2NRM(N, Q(1,K))                                       
            IF (T*T2 / AK .GT. SINGTL) GO TO 100                        
            GO TO 150                                                   
 90      T = DSQRT(ONE - WK)                                            
         IF (T*T2 .LE. SINGTL) GO TO 150                                
         T = T * AK                                                     
C                                                                       
 100     IF (T .LT. TINY) GO TO 150                                     
         R(KK) = T * R(KK)                                              
         CALL DV7SCL(N, Q(1,K), ONE/T, Q(1,K))                          
         IF (T/T1 .GE. REOTOL) GO TO 120                                
C                                                                       
C     ***  REORTHOGONALIZE COLUMN K  ***                                
C                                                                       
              AK = ONE                                                  
              T2 = T * T2                                               
              WK = ZERO                                                 
              J = KK - K                                                
              KM1 = K - 1                                               
              DO 110 I = 1, KM1                                         
                   J = J + 1                                            
                   T = DD7TPR(N, Q(1,I), Q(1,K))                        
                   WK = WK + T*T                                        
                   R(J) = R(J) + T*R(KK)                                
 110               CALL DV2AXY(N, Q(1,K), -T, Q(1,I), Q(1,K))           
              T1 = ONE                                                  
              GO TO 80                                                  
C                                                                       
C        ***  COMPUTE R(K,I) FOR I = K+1,...,P AND UPDATE Q  ***        
C                                                                       
 120     IF (K .GE. P) GO TO 999                                        
         J = KK + K                                                     
         II = KK                                                        
         DO 130 I = KP1, P                                              
              II = II + I                                               
              T = DD7TPR(N, Q(1,K), Q(1,I))                             
              R(J) = T                                                  
              J = J + I                                                 
              CALL DV2AXY(N, Q(1,I), -T, Q(1,K), Q(1,I))                
              T1 = R(II)                                                
              IF (T1 .GT. ZERO)  W(I) = W(I) + (T/T1)**2                
 130          CONTINUE                                                  
 140     CONTINUE                                                       
C                                                                       
C  ***  SINGULAR Q  ***                                                 
C                                                                       
 150  IERR = K                                                          
      KM1 = K - 1                                                       
      J = KK                                                            
      DO 160 I = K, P                                                   
         CALL DV7SCP(I-KM1, R(J), ZERO)                                 
         J = J + I                                                      
 160     CONTINUE                                                       
C                                                                       
 999  RETURN                                                            
C  ***  LAST CARD OF DQ7RGS FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DQ7RS1(K, LR, P, PP, Q, R, W)                          
C                                                                       
C  ***  PERMUTE COLUMN K OF R TO COLUMN P, MODIFY Q ACCORDINGLY  ***    
C                                                                       
      INTEGER K, LR, P, PP                                              
      DOUBLE PRECISION Q(PP,P), R(LR), W(P)                             
C     DIMSNSION R(P*(P+1)/2)                                            
C                                                                       
      EXTERNAL DH2RFA, DH2RFG,DV7CPY                                    
      DOUBLE PRECISION DH2RFG                                           
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
 30      CALL DH2RFA(PP, Q(1,J), Q(1,JP1), X, Y, Z)                     
 40      T = X * WJ                                                     
         W(J) = WJ + T                                                  
         WJ = T * Z                                                     
 50      CONTINUE                                                       
      W(P) = WJ                                                         
      CALL DV7CPY(P, R(K1+1), W)                                        
 999  RETURN                                                            
C  ***  LAST LINE OF DQ7RS1 FOLLOWS  ***                                
      END                                                               
      SUBROUTINE DC7COL(C, J, M, P, PP, X)                              
      INTEGER M, P, PP                                                  
      INTEGER J                                                         
      DOUBLE PRECISION C(PP, M), X(P)                                   
      EXTERNAL DV7SCL,DV7CPY                                            
      INTEGER I                                                         
      DOUBLE PRECISION ONE, NEGONE, ZERO                                
      DATA NEGONE/-1.D+0/                                               
      DATA ONE/1.D+0/                                                   
      DATA ZERO/0.D+0/                                                  
C ***  EXTRACT COLUMN J FROM C  WITH PREPENDED IDENTITY MATRIX  ***     
      I = IABS(J)                                                       
      IF (I .LE. P) GOTO 30                                             
         I = I-P                                                        
         IF (J .LE. 0) GOTO 10                                          
            CALL DV7CPY(P, X, C(1, I))                                  
            GOTO  20                                                    
  10        CALL DV7SCL(P, X, NEGONE, C(1, I))                          
  20     CONTINUE                                                       
         GOTO  40                                                       
  30     CALL DV7SCP(P, X, ZERO)                                        
         X(I) = ONE                                                     
         IF (J .LT. 0) X(I) = NEGONE                                    
  40  RETURN                                                            
      END                                                               
      INTEGER FUNCTION L5NKF(TOP, LLIST, K)                             
      INTEGER TOP                                                       
      INTEGER LLIST(2, TOP), K                                          
      INTEGER I, J                                                      
      I = TOP                                                           
      J = 1                                                             
         GOTO  2                                                        
   1     J = J+1                                                        
   2     IF (J .GT. K) GOTO  3                                          
         I = LLIST(1, I)                                                
         GOTO  1                                                        
   3  L5NKF = I                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE L5NKI(LLIST, INDEX, IBACK)                             
      INTEGER LLIST(2, 1), INDEX, IBACK                                 
      INTEGER INEXT                                                     
      INEXT = LLIST(1, IBACK)                                           
      LLIST(1, IBACK) = INDEX                                           
      LLIST(2, INDEX) = IBACK                                           
      LLIST(1, INDEX) = INEXT                                           
      LLIST(2, INEXT) = INDEX                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE L5NKD(LLIST, INDEX)                                    
      INTEGER LLIST(2, 1), INDEX                                        
      INTEGER IBACK, INEXT                                              
      INEXT = LLIST(1, INDEX)                                           
      IBACK = LLIST(2, INDEX)                                           
      LLIST(1, IBACK) = INEXT                                           
      LLIST(2, INEXT) = IBACK                                           
      RETURN                                                            
      END                                                               
       SUBROUTINE VDSS1 (X,N,U,L,A,F,FPRIME)                            
C X--POINT AT WHICH SPLINE IS TO BE EVALUATED                           
C N--NUMBER OF SAMPLES                                                  
C U--UPPER BOUND OF INTERVAL                                            
C L--LOWER BOUND OF INTERVAL                                            
C A--SPLINE COEFFICIENTS                                                
C F--FUNCTION                                                           
C FPRIME--DERIVATIVE                                                    
       INTEGER N,L1,J                                                   
       REAL X,A(1),F,FPRIME,U,L,X1,DIST                                 
C ERROR CHECKING                                                        
C/6S                                                                    
C      IF (N.LE.0) CALL SETERR(13HVDSS1- N.LE.0,13,1,2)                 
C      IF (U.LE.L) CALL SETERR(13HVDSS1- U.LE.L,13,2,2)                 
C/7S                                                                    
       IF (N.LE.0) CALL SETERR('VDSS1- N.LE.0',13,1,2)                  
       IF (U.LE.L) CALL SETERR('VDSS1- U.LE.L',13,2,2)                  
C/                                                                      
C SCALE TO INTERVAL, 0,1, (INCLUDING ENDPOINTS)                         
       X1 = X - L                                                       
       DIST = U-L                                                       
       X1 = X1 / DIST                                                   
       L1 = INT(FLOAT(N-1)*X1 + 0.5)                                    
       J = MAX(0,MIN(N-3,L1-1))                                         
C CALL SPLINE ROUTINE                                                   
       CALL T1UED (X1,L1,1,N-1,A(J+1),F,FPRIME)                         
       FPRIME = FPRIME/DIST                                             
       RETURN                                                           
       END                                                              
       SUBROUTINE VDSS2 (X,N1,N2,U,L,A,NA1,F,FPRIME)                    
C X--POINT AT WHICH SPLINE IS TO BE EVALUATED                           
C N1,N2--NUMBER OF SAMPLES                                              
C U(2)--UPPER BOUND OF INTERVAL                                         
C L(2)--LOWER BOUND OF INTERVAL                                         
C A--SPLINE COEFFICIENTS                                                
C NA1--ACTUAL FIRST DIMENSION OF A                                      
C F--FUNCTION                                                           
C FPRIME(2)--PARTIALS                                                   
       INTEGER N1,N2,NA1,I                                              
       REAL X(2), A(NA1,N2), F, FPRIME(2), U(2), L(2), X1(2)            
       REAL DIST(2)                                                     
C ERROR CHECKING                                                        
C/6S                                                                    
C      IF (N1.LE.0) CALL SETERR(14HVDSS2- N1.LE.0,13,1,2)               
C      IF (N2.LE.0) CALL SETERR(14HVDSS2- N2.LE.0,13,1,2)               
C      IF (NA1.LT.N1) CALL SETERR(16HVDSS2- NA1.LT.N1,13,2,2)           
C      IF (U(1).LE.L(1)) CALL SETERR(19HVDSS2- U(1).LE.L(1),13,3,2)     
C      IF (U(2).LE.L(2)) CALL SETERR(19HVDSS2- U(2).LE.L(2),13,3,2)     
C/7S                                                                    
       IF (N1.LE.0) CALL SETERR('VDSS2- N1.LE.0',13,1,2)                
       IF (N2.LE.0) CALL SETERR('VDSS2- N2.LE.0',13,1,2)                
       IF (NA1.LT.N1) CALL SETERR('VDSS2- NA1.LT.N1',13,2,2)            
       IF (U(1).LE.L(1)) CALL SETERR('VDSS2- U(1).LE.L(1)',13,3,2)      
       IF (U(2).LE.L(2)) CALL SETERR('VDSS2- U(2).LE.L(2)',13,3,2)      
C/                                                                      
C SCALE TO UNIT SQUARE                                                  
       DO 100 I=1,2                                                     
              X1(I) = X(I) - L(I)                                       
              DIST(I) = U(I) - L(I)                                     
              X1(I) = X1(I) / DIST(I)                                   
 100   CONTINUE                                                         
C CALL SPLINE ROUTINE                                                   
       CALL T2UEV(X1,N1,N2,A,NA1,F,FPRIME(1),FPRIME(2))                 
       FPRIME(1) = FPRIME(1) /DIST(1)                                   
       FPRIME(2) = FPRIME(2) /DIST(2)                                   
       RETURN                                                           
       END                                                              
       SUBROUTINE VDSS3(X,N1,N2,N3,U,L,A,NA1,NA2,F,FPRIME)              
C DRIVER FOR 3D                                                         
C X(3)--POINT AT WHICH SPLINE IS TO BE EVALUATED                        
C N1,N2,N3--NUMBER OF SAMPLES                                           
C U(3)--UPPER BOUND OF INTERVAL                                         
C L(3)--LOWER BOUND OF INTERVAL                                         
C A(NA1,NA2,N3)--SPLINE COEFFICIENTS                                    
C NA1,NA2--ACTUAL FIRST DIMENSION OF A                                  
C F--FUNCTION                                                           
C FPRIME(3)--PARTIALS                                                   
       INTEGER N1,N2,N3,NA1,NA2,I                                       
       REAL X(3),A(NA1,NA2,N3),F,FPRIME(3),X1(3)                        
       REAL U(3),L(3),DIST(3)                                           
C ERROR CHECKING                                                        
C/6S                                                                    
C      IF (N1.LE.0) CALL SETERR(14HVDSS3- N1.LE.0,13,1,2)               
C      IF (N2.LE.0) CALL SETERR(14HVDSS3- N2.LE.0,13,1,2)               
C      IF (N3.LE.0) CALL SETERR(14HVDSS3- N3.LE.0,13,1,2)               
C      IF (NA1.LT.N1) CALL SETERR(16HVDSS3- NA1.LT.N1,13,2,2)           
C      IF (NA2.LT.N2) CALL SETERR(16HVDSS3- NA2.LT.N2,13,2,2)           
C      IF (U(1).LE.L(1)) CALL SETERR(19HVDSS3- U(1).LE.L(1),13,3,2)     
C      IF (U(2).LE.L(2)) CALL SETERR(19HVDSS3- U(2).LE.L(2),13,3,2)     
C      IF (U(3).LE.L(3)) CALL SETERR(19HVDSS3- U(3).LE.L(3),13,3,2)     
C/7S                                                                    
       IF (N1.LE.0) CALL SETERR('VDSS3- N1.LE.0',13,1,2)                
       IF (N2.LE.0) CALL SETERR('VDSS3- N2.LE.0',13,1,2)                
       IF (N3.LE.0) CALL SETERR('VDSS3- N3.LE.0',13,1,2)                
       IF (NA1.LT.N1) CALL SETERR('VDSS3- NA1.LT.N1',13,2,2)            
       IF (NA2.LT.N2) CALL SETERR('VDSS3- NA2.LT.N2',13,2,2)            
       IF (U(1).LE.L(1)) CALL SETERR('VDSS3- U(1).LE.L(1)',13,3,2)      
       IF (U(2).LE.L(2)) CALL SETERR('VDSS3- U(2).LE.L(2)',13,3,2)      
       IF (U(3).LE.L(3)) CALL SETERR('VDSS3- U(3).LE.L(3)',13,3,2)      
C/                                                                      
C SCALE TO UNIT CUBE                                                    
       DO 100 I=1,3                                                     
              X1(I) = X(I) - L(I)                                       
              DIST(I) = U(I) - L(I)                                     
              X1(I) = X1(I) / DIST(I)                                   
 100   CONTINUE                                                         
C CALL SPLINE ROUTINE                                                   
       CALL T3UEV(X1,N1,N2,N3,A,NA1,NA2,F,FPRIME(1),FPRIME(2),FPRIME(3))
       DO 110 I=1,3                                                     
              FPRIME(I) = FPRIME(I)/DIST(I)                             
 110   CONTINUE                                                         
       RETURN                                                           
       END                                                              
      SUBROUTINE T3UEV ( X, N1,N2,N3, A, NA1, NA2, F,F1,F2,F3 )         
C     EVALUATE TENSOR PRODUCT SPLINE AND FIRST PARTIALS                 
C        3-DIM, K=3, UNIF KNOT                                          
C     INPUT                                                             
C        X            POINT AT WHICH SPLINE IS TO BE EVALUATED          
C        N1,N2,N3     NUMBER OF SAMPLE POINTS                           
C        A            SPLINE COEFFICIENTS                               
C     OUTPUT                                                            
C        F,F1,F2,F3   VALUE (AND DERIVATIVES) OF SPLINE AT X            
      INTEGER N1,N2,N3,L1,L2,L3,J1,J2,J3,I1,I2,I3,N,J,NA1,NA2           
      REAL X(3), A(NA1,NA2,N3), F,F1,F2,F3, W(3,3,3)                    
      REAL FYZ(9), F1YZ(9), FZ(3), F1Z(3), F2Z(3), Z1,Z2,Z3,FD,Y        
      L1=INT(FLOAT(N1-1)*X(1)+0.5)                                      
      L2=INT(FLOAT(N2-1)*X(2)+0.5)                                      
      L3=INT(FLOAT(N3-1)*X(3)+0.5)                                      
      J1=MAX(0,MIN(N1-3,L1-1))                                          
      J2=MAX(0,MIN(N2-3,L2-1))                                          
      J3=MAX(0,MIN(N3-3,L3-1))                                          
      DO 100 I1 = 1, 3                                                  
         DO 100 I2 = 1, 3                                               
            DO 100 I3 = 1, 3                                            
  100          W(I1,I2,I3) = A(I1+J1,I2+J2,I3+J3)                       
      CALL T1UED ( X(1), L1, 9, N1-1, W, FYZ, F1YZ )                    
      CALL T1UED ( X(2), L2, 3, N2-1, FYZ, FZ, F2Z )                    
      CALL T1UEV ( X(2), L2, 3, N2-1, F1YZ, F1Z )                       
C     CALL T1UED ( X(3), L3, 1, N3-1, FZ, F, F3 )                       
C     CALL T1UEV ( X(3), L3, 1, N3-1, F1Z, F1 )                         
C     CALL T1UEV ( X(3), L3, 1, N3-1, F2Z, F2 )                         
C       (THIS INLINE CODE SAVES 1MS ON SWIFT)                           
      N=N3-1                                                            
      J = 1                                                             
      IF(L3.LT.1)GOTO 250                                               
      IF(L3.LT.N)GOTO 290                                               
        J = 2                                                           
  250 Y = X(3) - FLOAT(J-1)*(1.-1./FLOAT(N))                            
      F3 = (FZ(J+1)-FZ(J))*FLOAT(N)                                     
      F = FZ(J) + Y * F3                                                
      FD = (F1Z(J+1)-F1Z(J))*FLOAT(N)                                   
      F1 = F1Z(J) + Y * FD                                              
      FD = (F2Z(J+1)-F2Z(J))*FLOAT(N)                                   
      F2 = F2Z(J) + Y * FD                                              
      RETURN                                                            
  290 Y=FLOAT(N)*X(3)+0.5-FLOAT(L3)                                     
      Z3=0.5*Y*Y                                                        
      Z2=(-2.*Z3+Y+0.5)                                                 
      Z1=(Z3-Y+0.5)                                                     
      F  = Z1*FZ(1) + Z2*FZ(2) + Z3*FZ(3)                               
      F1 = Z1*F1Z(1) + Z2*F1Z(2) + Z3*F1Z(3)                            
      F2 = Z1*F2Z(1) + Z2*F2Z(2) + Z3*F2Z(3)                            
      F3 = FLOAT(N)*(Y*FZ(3) + (1.-2.*Y)*FZ(2) + (Y-1.)*FZ(1))          
      RETURN                                                            
      END                                                               
      SUBROUTINE T2UEV ( X, N1,N2, A, NA1, F,F1,F2 )                    
C     EVALUATE TENSOR PRODUCT SPLINE AND FIRST PARTIALS                 
C        2-DIM, K=3, UNIF KNOT                                          
C     INPUT                                                             
C        X            POINT AT WHICH SPLINE IS TO BE EVALUATED          
C        N1,N2        NUMBER OF SAMPLE POINTS                           
C        A            SPLINE COEFFICIENTS                               
C     OUTPUT                                                            
C        F,F1,F2      VALUE (AND DERIVATIVES) OF SPLINE AT X            
      INTEGER N1,N2,L1,L2,J1,J2,I1,I2,N,J,NA1                           
      REAL X(2), A(NA1,N2), F,F1,F2, W(3,3)                             
      REAL FY(3), F1Y(3), Z1,Z2,Z3,FD,Y                                 
      L1=INT(FLOAT(N1-1)*X(1)+0.5)                                      
      L2=INT(FLOAT(N2-1)*X(2)+0.5)                                      
      J1=MAX(0,MIN(N1-3,L1-1))                                          
      J2=MAX(0,MIN(N2-3,L2-1))                                          
      DO 100 I1 = 1, 3                                                  
         DO 100 I2 = 1, 3                                               
  100        W(I1,I2) = A(I1+J1,I2+J2)                                  
      CALL T1UED ( X(1), L1, 3, N1-1, W, FY, F1Y )                      
C     CALL T1UED ( X(2), L2, 1, N2-1, FY, F, F2 )                       
C     CALL T1UEV ( X(2), L2, 1, N2-1, F1Y, F1 )                         
      N=N2-1                                                            
      J = 1                                                             
      IF(L2.LT.1)GOTO 250                                               
      IF(L2.LT.N)GOTO 290                                               
        J = 2                                                           
  250 Y = X(2) - FLOAT(J-1)*(1.-1./FLOAT(N))                            
      F2 = (FY(J+1)-FY(J))*FLOAT(N)                                     
      F = FY(J) + Y * F2                                                
      FD = (F1Y(J+1)-F1Y(J))*FLOAT(N)                                   
      F1 = F1Y(J) + Y * FD                                              
      RETURN                                                            
  290 Y=FLOAT(N)*X(2)+0.5-FLOAT(L2)                                     
      Z3=0.5*Y*Y                                                        
      Z2=(-2.*Z3+Y+0.5)                                                 
      Z1=(Z3-Y+0.5)                                                     
      F  = Z1*FY(1) + Z2*FY(2) + Z3*FY(3)                               
      F1 = Z1*F1Y(1) + Z2*F1Y(2) + Z3*F1Y(3)                            
      F2 = FLOAT(N)*(Y*FY(3) + (1.-2.*Y)*FY(2) + (Y-1.)*FY(1))          
      RETURN                                                            
      END                                                               
      SUBROUTINE T1UED ( X, L, S, N, A, F, FD )                         
C     X = EVALUATION POINT, SCALED FOR (0,1)                            
C     L = INT(N*X+0.5)                                                  
C     S = NUMBER OF REPETITIONS                                         
C     N = NUMBER OF INTERVALS  ( = DIM SPLINE SPACE - 1 )               
C     A(3,3,3) = SPLINE COEFFICIENTS                                    
C     F,FD = FUNCTION, DERIVATIVE                                       
      INTEGER N, S, R, J, L                                             
      REAL X, Y, A(1), F(S), FD(S), Z1,Z2,Z3,D1,D2,D3                   
      J = 1                                                             
      IF(L.LT.1)GOTO 50                                                 
      IF(L.LT.N)GOTO 90                                                 
        J = 2                                                           
   50 Y = X - FLOAT(J-1)*(1.-1./FLOAT(N))                               
      DO 60 R = 1, S                                                    
          FD(R) = (A(J+1)-A(J))*FLOAT(N)                                
          F(R) = A(J) + Y * FD(R)                                       
   60     J = J + 3                                                     
      RETURN                                                            
   90 Y=FLOAT(N)*X+0.5-FLOAT(L)                                         
      Z3=0.5*Y*Y                                                        
      Z2=(-2.*Z3+Y+0.5)                                                 
      Z1=(Z3-Y+0.5)                                                     
      D3=FLOAT(N)*Y                                                     
      D2=FLOAT(N)*(1.-2.*Y)                                             
      D1=FLOAT(N)*(Y-1.)                                                
      DO 190 R = 1, S                                                   
         F(R)  = Z1*A(J) + Z2*A(J+1) + Z3*A(J+2)                        
         FD(R) = D1*A(J) + D2*A(J+1) + D3*A(J+2)                        
  190    J = J + 3                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE T1UEV ( X, L, S, N, A, F )                             
      INTEGER N, S, R, J, L                                             
      REAL X, Y, A(1), F(S), FD, Z1,Z2,Z3                               
      J = 1                                                             
      IF(L.LT.1)GOTO 50                                                 
      IF(L.LT.N)GOTO 90                                                 
        J = 2                                                           
   50 Y = X - FLOAT(J-1)*(1.-1./FLOAT(N))                               
      DO 60 R = 1, S                                                    
          FD = (A(J+1)-A(J))*FLOAT(N)                                   
          F(R) = A(J) + Y * FD                                          
   60     J = J + 3                                                     
      RETURN                                                            
   90 Y=FLOAT(N)*X+0.5-FLOAT(L)                                         
      Z3=0.5*Y*Y                                                        
      Z2=(-2.*Z3+Y+0.5)                                                 
      Z1=(Z3-Y+0.5)                                                     
      DO 190 R = 1, S                                                   
         F(R)  = Z1*A(J) + Z2*A(J+1) + Z3*A(J+2)                        
  190    J = J + 3                                                      
      RETURN                                                            
      END                                                               
       SUBROUTINE DVDSS1 (X,N,U,L,A,F,FPRIME)                           
C DOUBLE PRECISION SPLINE PACKAGE                                       
C DRIVER FOR 1D                                                         
C X--POINT AT WHICH SPLINE IS TO BE EVALUATED                           
C N--NUMBER OF SAMPLES                                                  
C U--UPPER BOUND OF INTERVAL                                            
C L--LOWER BOUND OF INTERVAL                                            
C A--SPLINE COEFFICIENTS                                                
C F--FUNCTION                                                           
C FPRIME--DERIVATIVE                                                    
       INTEGER N,L1,J                                                   
       DOUBLE PRECISION X,A(1),F,FPRIME,U,L,X1,DIST                     
C ERROR CHECKING                                                        
C/6S                                                                    
C      IF (N.LE.0) CALL SETERR(14HDVDSS1- N.LE.0,13,1,2)                
C      IF (U.LE.L) CALL SETERR(14HDVDSS1- U.LE.L,13,2,2)                
C/7S                                                                    
       IF (N.LE.0) CALL SETERR('DVDSS1- N.LE.0',13,1,2)                 
       IF (U.LE.L) CALL SETERR('DVDSS1- U.LE.L',13,2,2)                 
C/                                                                      
C SCALE TO INTERVAL, 0,1, (INCLUDING ENDOINTS)                          
       X1 = X - L                                                       
       DIST = U-L                                                       
       X1 = X1 / DIST                                                   
       L1 = IDINT(DBLE(FLOAT(N-1))*X1 + 0.5D0)                          
       J = MAX(0,MIN(N-3,L1-1))                                         
C CALL SPLINE ROUTINE                                                   
       CALL DT1UED (X1,L1,1,N-1,A(J+1),F,FPRIME)                        
       FPRIME = FPRIME/DIST                                             
       RETURN                                                           
       END                                                              
       SUBROUTINE DVDSS2 (X,N1,N2,U,L,A,NA1,F,FPRIME)                   
C DRIVER FOR 2D                                                         
C X--POINT AT WHICH SPLINE IS TO BE EVALUATED                           
C N1,N2--NUMBER OF SAMPLES                                              
C U(2)--UPPER BOUND OF INTERVAL                                         
C L(2)--LOWER BOUND OF INTERVAL                                         
C A--SPLINE COEFFICIENTS                                                
C NA1--ACTUAL FIRST DIMENSION OF A                                      
C F--FUNCTION                                                           
C FPRIME(2)--PARTIALS                                                   
       INTEGER N1,N2,NA1,I                                              
       DOUBLE PRECISION X(2), A(NA1,N2), F, FPRIME(2), U(2), L(2), X1(2)
       DOUBLE PRECISION DIST(2)                                         
C ERROR CHECKING                                                        
C/6S                                                                    
C      IF (N1.LE.0) CALL SETERR(15HDVDSS2- N1.LE.0,13,1,2)              
C      IF (N2.LE.0) CALL SETERR(15HDVDSS2- N2.LE.0,13,1,2)              
C      IF (NA1.LT.N1) CALL SETERR(17HDVDSS2- NA1.LT.N1,13,2,2)          
C      IF (U(1).LE.L(1)) CALL SETERR(20HDVDSS2- U(1).LE.L(1),13,3,2)    
C      IF (U(2).LE.L(2)) CALL SETERR(20HDVDSS2- U(2).LE.L(2),13,3,2)    
C/7S                                                                    
       IF (N1.LE.0) CALL SETERR('DVDSS2- N1.LE.0',13,1,2)               
       IF (N2.LE.0) CALL SETERR('DVDSS2- N2.LE.0',13,1,2)               
       IF (NA1.LT.N1) CALL SETERR('DVDSS2- NA1.LT.N1',13,2,2)           
       IF (U(1).LE.L(1)) CALL SETERR('DVDSS2- U(1).LE.L(1)',13,3,2)     
       IF (U(2).LE.L(2)) CALL SETERR('DVDSS2- U(2).LE.L(2)',13,3,2)     
C/                                                                      
C SCALE TO UNIT SQUARE                                                  
       DO 100 I=1,2                                                     
              X1(I) = X(I) - L(I)                                       
              DIST(I) = U(I) - L(I)                                     
              X1(I) = X1(I) / DIST(I)                                   
 100   CONTINUE                                                         
C CALL SPLINE ROUTINE                                                   
       CALL DT2UEV(X1,N1,N2,A,NA1,F,FPRIME(1),FPRIME(2))                
       FPRIME(1) = FPRIME(1) /DIST(1)                                   
       FPRIME(2) = FPRIME(2) /DIST(2)                                   
       RETURN                                                           
       END                                                              
       SUBROUTINE DVDSS3(X,N1,N2,N3,U,L,A,NA1,NA2,F,FPRIME)             
C DRIVER FOR 3D                                                         
C X(3)--POINT AT WHICH SPLINE IS TO BE EVALUATED                        
C N1,N2,N3--NUMBER OF SAMPLES                                           
C U(3)--UPPER BOUND OF INTERVAL                                         
C L(3)--LOWER BOUND OF INTERVAL                                         
C A(NA1,NA2,N3)--SPLINE COEFFICIENTS                                    
C NA1,NA2--ACTUAL FIRST DIMENSION OF A                                  
C F--FUNCTION                                                           
C FPRIME(3)--PARTIALS                                                   
       INTEGER N1,N2,N3,NA1,NA2,I                                       
       DOUBLE PRECISION X(3),A(NA1,NA2,N3),F,FPRIME(3),X1(3)            
       DOUBLE PRECISION U(3),L(3),DIST(3)                               
C ERROR CHECKING                                                        
C/6S                                                                    
C      IF (N1.LE.0) CALL SETERR(15HDVDSS3- N1.LE.0,13,1,2)              
C      IF (N2.LE.0) CALL SETERR(15HDVDSS3- N2.LE.0,13,1,2)              
C      IF (N3.LE.0) CALL SETERR(15HDVDSS3- N3.LE.0,13,1,2)              
C      IF (NA1.LT.N1) CALL SETERR(17HDVDSS3- NA1.LT.N1,13,2,2)          
C      IF (NA2.LT.N2) CALL SETERR(17HDVDSS3- NA2.LT.N2,13,2,2)          
C      IF (U(1).LE.L(1)) CALL SETERR(20HDVDSS3- U(1).LE.L(1),13,3,2)    
C      IF (U(2).LE.L(2)) CALL SETERR(20HDVDSS3- U(2).LE.L(2),13,3,2)    
C      IF (U(3).LE.L(3)) CALL SETERR(20HDVDSS3- U(3).LE.L(3),13,3,2)    
C/7S                                                                    
       IF (N1.LE.0) CALL SETERR('DVDSS3- N1.LE.0',13,1,2)               
       IF (N2.LE.0) CALL SETERR('DVDSS3- N2.LE.0',13,1,2)               
       IF (N3.LE.0) CALL SETERR('DVDSS3- N3.LE.0',13,1,2)               
       IF (NA1.LT.N1) CALL SETERR('DVDSS3- NA1.LT.N1',13,2,2)           
       IF (NA2.LT.N2) CALL SETERR('DVDSS3- NA2.LT.N2',13,2,2)           
       IF (U(1).LE.L(1)) CALL SETERR('DVDSS3- U(1).LE.L(1)',13,3,2)     
       IF (U(2).LE.L(2)) CALL SETERR('DVDSS3- U(2).LE.L(2)',13,3,2)     
       IF (U(3).LE.L(3)) CALL SETERR('DVDSS3- U(3).LE.L(3)',13,3,2)     
C/                                                                      
C SCALE TO UNIT CUBE                                                    
       DO 100 I=1,3                                                     
              X1(I) = X(I) - L(I)                                       
              DIST(I) = U(I) - L(I)                                     
              X1(I) = X1(I) / DIST(I)                                   
 100   CONTINUE                                                         
C CALL SPLINE ROUTINE                                                   
       CALL DT3UEV(X1,N1,N2,N3,A,NA1,NA2,F,                             
     +      FPRIME(1),FPRIME(2),FPRIME(3))                              
       DO 110 I=1,3                                                     
              FPRIME(I) = FPRIME(I)/DIST(I)                             
 110   CONTINUE                                                         
       RETURN                                                           
       END                                                              
      SUBROUTINE DT3UEV ( X, N1,N2,N3, A, NA1, NA2, F,F1,F2,F3 )        
C     EVALUATE TENSOR PRODUCT SPLINE AND FIRST PARTIALS                 
C        3-DDIM, K=3, UNIF KNOT                                         
C     INPUT                                                             
C        X            POINT AT WHICH SPLINE IS TO BE EVALUATED          
C        N1,N2,N3     NUMBER OF SAMPLE POINTS                           
C        A            SPLINE COEFFICIENTS                               
C     OUTPUT                                                            
C        F,F1,F2,F3   VALUE (AND DERIVATIVES) OF SPLINE AT X            
      INTEGER N1,N2,N3,L1,L2,L3,J1,J2,J3,I1,I2,I3,N,J,NA1,NA2           
      DOUBLE PRECISION X(3), A(NA1,NA2,N3), F,F1,F2,F3, W(3,3,3)        
      DOUBLE PRECISION FYZ(9), F1YZ(9), FZ(3), F1Z(3), F2Z(3)           
      DOUBLE PRECISION Z1,Z2,Z3,FD,Y                                    
      L1=IDINT(DBLE(FLOAT(N1-1))*X(1)+0.5D0)                            
      L2=IDINT(DBLE(FLOAT(N2-1))*X(2)+0.5D0)                            
      L3=IDINT(DBLE(FLOAT(N3-1))*X(3)+0.5D0)                            
      J1=MAX(0,MIN(N1-3,L1-1))                                          
      J2=MAX(0,MIN(N2-3,L2-1))                                          
      J3=MAX(0,MIN(N3-3,L3-1))                                          
      DO 100 I1 = 1, 3                                                  
         DO 100 I2 = 1, 3                                               
            DO 100 I3 = 1, 3                                            
  100          W(I1,I2,I3) = A(I1+J1,I2+J2,I3+J3)                       
      CALL DT1UED ( X(1), L1, 9, N1-1, W, FYZ, F1YZ )                   
      CALL DT1UED ( X(2), L2, 3, N2-1, FYZ, FZ, F2Z )                   
      CALL DT1UEV ( X(2), L2, 3, N2-1, F1YZ, F1Z )                      
C     CALL DT1UED ( X(3), L3, 1, N3-1, FZ, F, F3 )                      
C     CALL DT1UEV ( X(3), L3, 1, N3-1, F1Z, F1 )                        
C     CALL DT1UEV ( X(3), L3, 1, N3-1, F2Z, F2 )                        
C       (THIS INLINE CODE SAVES 1MS ON SWIFT)                           
      N=N3-1                                                            
      J = 1                                                             
      IF(L3.LT.1)GOTO 250                                               
      IF(L3.LT.N)GOTO 290                                               
        J = 2                                                           
  250 Y = X(3) - DBLE(FLOAT(J-1))*(1.D0-1.D0/DBLE(FLOAT(N)))            
      F3 = (FZ(J+1)-FZ(J))*DBLE(FLOAT(N))                               
      F = FZ(J) + Y * F3                                                
      FD = (F1Z(J+1)-F1Z(J))*DBLE(FLOAT(N))                             
      F1 = F1Z(J) + Y * FD                                              
      FD = (F2Z(J+1)-F2Z(J))*DBLE(FLOAT(N))                             
      F2 = F2Z(J) + Y * FD                                              
      RETURN                                                            
  290 Y=DBLE(FLOAT(N))*X(3)+0.5D0-DBLE(FLOAT(L3))                       
      Z3=0.5D0*Y*Y                                                      
      Z2=(-2.D0*Z3+Y+0.5D0)                                             
      Z1=(Z3-Y+0.5D0)                                                   
      F  = Z1*FZ(1) + Z2*FZ(2) + Z3*FZ(3)                               
      F1 = Z1*F1Z(1) + Z2*F1Z(2) + Z3*F1Z(3)                            
      F2 = Z1*F2Z(1) + Z2*F2Z(2) + Z3*F2Z(3)                            
      F3 = DBLE(FLOAT(N))*(Y*FZ(3) + (1.D0-2.D0*Y)*FZ(2) +              
     1          (Y-1.D0)*FZ(1))                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE DT2UEV ( X, N1,N2, A, NA1, F,F1,F2 )                   
C     EVALUATE TENSOR PRODUCT SPLINE AND FIRST PARTIALS                 
C        2-DDIM, K=3, UNIF KNOT                                         
C     INPUT                                                             
C        X            POINT AT WHICH SPLINE IS TO BE EVALUATED          
C        N1,N2        NUMBER OF SAMPLE POINTS                           
C        A            SPLINE COEFFICIENTS                               
C     OUTPUT                                                            
C        F,F1,F2      VALUE (AND DERIVATIVES) OF SPLINE AT X            
      INTEGER N1,N2,L1,L2,J1,J2,I1,I2,N,J,NA1                           
      DOUBLE PRECISION X(2), A(NA1,N2), F,F1,F2, W(3,3)                 
      DOUBLE PRECISION FY(3), F1Y(3), Z1,Z2,Z3,FD,Y                     
      L1=IDINT(DBLE(FLOAT(N1-1))*X(1)+0.5D0)                            
      L2=IDINT(DBLE(FLOAT(N2-1))*X(2)+0.5D0)                            
      J1=MAX(0,MIN(N1-3,L1-1))                                          
      J2=MAX(0,MIN(N2-3,L2-1))                                          
      DO 100 I1 = 1, 3                                                  
         DO 100 I2 = 1, 3                                               
  100        W(I1,I2) = A(I1+J1,I2+J2)                                  
      CALL DT1UED ( X(1), L1, 3, N1-1, W, FY, F1Y )                     
C     CALL DT1UED ( X(2), L2, 1, N2-1, FY, F, F2 )                      
C     CALL DT1UEV ( X(2), L2, 1, N2-1, F1Y, F1 )                        
      N=N2-1                                                            
      J = 1                                                             
      IF(L2.LT.1)GOTO 250                                               
      IF(L2.LT.N)GOTO 290                                               
        J = 2                                                           
  250 Y = X(2) - DBLE(FLOAT(J-1))*(1.D0-1.D0/DBLE(FLOAT(N)))            
      F2 = (FY(J+1)-FY(J))*DBLE(FLOAT(N))                               
      F = FY(J) + Y * F2                                                
      FD = (F1Y(J+1)-F1Y(J))*DBLE(FLOAT(N))                             
      F1 = F1Y(J) + Y * FD                                              
      RETURN                                                            
  290 Y=DBLE(FLOAT(N))*X(2)+0.5D0-DBLE(FLOAT(L2))                       
      Z3=0.5D0*Y*Y                                                      
      Z2=(-2.D0*Z3+Y+0.5D0)                                             
      Z1=(Z3-Y+0.5D0)                                                   
      F  = Z1*FY(1) + Z2*FY(2) + Z3*FY(3)                               
      F1 = Z1*F1Y(1) + Z2*F1Y(2) + Z3*F1Y(3)                            
      F2 = DBLE(FLOAT(N))*(Y*FY(3) + (1.D0-2.D0*Y)*FY(2) +              
     1          (Y-1.D0)*FY(1))                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE DT1UED ( X, L, S, N, A, F, FD )                        
C     X = EVALUATION POINT, SCALED FOR (0,1)                            
C     L = IDINT(N*X+0.5D0)                                              
C     S = NUMBER OF REPETITIONS                                         
C     N = NUMBER OF INTERVALS  ( = DDIM SPLINE SPACE - 1 )              
C     A(3,3,3) = SPLINE COEFFICIENTS                                    
C     F,FD = FUNCTION, DERIVATIVE                                       
      INTEGER N, S, R, J, L                                             
      DOUBLE PRECISION X, Y, A(1), F(S), FD(S), Z1,Z2,Z3,D1,D2,D3       
      J = 1                                                             
      IF(L.LT.1)GOTO 50                                                 
      IF(L.LT.N)GOTO 90                                                 
        J = 2                                                           
   50 Y = X - DBLE(FLOAT(J-1))*(1.D0-1.D0/DBLE(FLOAT(N)))               
      DO 60 R = 1, S                                                    
          FD(R) = (A(J+1)-A(J))*DBLE(FLOAT(N))                          
          F(R) = A(J) + Y * FD(R)                                       
   60     J = J + 3                                                     
      RETURN                                                            
   90 Y=DBLE(FLOAT(N))*X+0.5D0-DBLE(FLOAT(L))                           
      Z3=0.5D0*Y*Y                                                      
      Z2=(-2.D0*Z3+Y+0.5D0)                                             
      Z1=(Z3-Y+0.5D0)                                                   
      D3=DBLE(FLOAT(N))*Y                                               
      D2=DBLE(FLOAT(N))*(1.D0-2.D0*Y)                                   
      D1=DBLE(FLOAT(N))*(Y-1.D0)                                        
      DO 190 R = 1, S                                                   
         F(R)  = Z1*A(J) + Z2*A(J+1) + Z3*A(J+2)                        
         FD(R) = D1*A(J) + D2*A(J+1) + D3*A(J+2)                        
  190    J = J + 3                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE DT1UEV ( X, L, S, N, A, F )                            
      INTEGER N, S, R, J, L                                             
      DOUBLE PRECISION X, Y, A(1), F(S), FD, Z1,Z2,Z3                   
      J = 1                                                             
      IF(L.LT.1)GOTO 50                                                 
      IF(L.LT.N)GOTO 90                                                 
        J = 2                                                           
   50 Y = X - DBLE(FLOAT(J-1))*(1.D0-1.D0/DBLE(FLOAT(N)))               
      DO 60 R = 1, S                                                    
          FD = (A(J+1)-A(J))*DBLE(FLOAT(N))                             
          F(R) = A(J) + Y * FD                                          
   60     J = J + 3                                                     
      RETURN                                                            
   90 Y=DBLE(FLOAT(N))*X+0.5D0-DBLE(FLOAT(L))                           
      Z3=0.5D0*Y*Y                                                      
      Z2=(-2.D0*Z3+Y+0.5D0)                                             
      Z1=(Z3-Y+0.5D0)                                                   
      DO 190 R = 1, S                                                   
         F(R)  = Z1*A(J) + Z2*A(J+1) + Z3*A(J+2)                        
  190    J = J + 3                                                      
      RETURN                                                            
      END                                                               
C****END OF ROUTINES NEEDED FOR PORT 3 APPROXIMATION CHAPTER************
