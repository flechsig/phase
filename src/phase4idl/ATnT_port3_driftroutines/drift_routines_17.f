        INTEGER FUNCTION ISAMAX(N,X,INCX)                               
        INTEGER N,INCX                                                  
        REAL X(INCX,1),SMAX                                             
C THIS FUNCTION RETURNS THE INDEX OF THE COMPONENT                      
C OF X HAVING MAXIMUM MAGNITUDE. ONLY EVERY                             
C INCXTH COMPONENT OF X IS CONSIDERED                                   
C                                                                       
        ISAMAX=0                                                        
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(13HISAMAX-N.LT.0,13,1,2)                 
C       IF(INCX.LT.1)CALL SETERR(16HISAMAX-INCX.LT.1,16,2,2)            
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('ISAMAX-N.LT.0',13,1,2)                  
        IF(INCX.LT.1)CALL SETERR('ISAMAX-INCX.LT.1',16,2,2)             
C/                                                                      
        SMAX=0.0                                                        
        ISAMAX=1                                                        
        DO 10 I=1,N                                                     
        IF(SMAX.GE.ABS(X(1,I))) GO TO 10                                
           SMAX=ABS(X(1,I))                                             
           ISAMAX=I                                                     
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        INTEGER FUNCTION IDAMAX(N,X,INCX)                               
        INTEGER N,INCX                                                  
        DOUBLE PRECISION X(INCX,1),SMAX                                 
C THIS FUNCTION RETURNS THE INDEX OF THE COMPONENT                      
C OF X HAVING MAXIMUM MAGNITUDE. ONLY EVERY                             
C INCXTH COMPONENT OF X IS CONSIDERED                                   
C                                                                       
        IDAMAX=0                                                        
       IF(N.EQ.0) RETURN                                                
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(13HIDAMAX-N.LT.0,13,1,2)                 
C       IF(INCX.LT.1)CALL SETERR(16HIDAMAX-INCX.LT.1,16,2,2)            
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('IDAMAX-N.LT.0',13,1,2)                  
        IF(INCX.LT.1)CALL SETERR('IDAMAX-INCX.LT.1',16,2,2)             
C/                                                                      
        SMAX=0.D0                                                       
        IDAMAX=1                                                        
        DO 10 I=1,N                                                     
        IF(SMAX.GE.DABS(X(1,I))) GO TO 10                               
           SMAX=DABS(X(1,I))                                            
           IDAMAX=I                                                     
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        INTEGER FUNCTION ICAMAX(N,X,INCX)                               
        INTEGER N,INCX                                                  
        COMPLEX X(INCX,1)                                               
        REAL SMAX                                                       
C THIS FUNCTION RETURNS THE INDEX OF THE COMPONENT                      
C OF X HAVING MAXIMUM MAGNITUDE. ONLY EVERY                             
C INCXTH COMPONENT OF X IS CONSIDERED                                   
C                                                                       
        ICAMAX=0                                                        
       IF(N.EQ.0) RETURN                                                
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(13HICAMAX-N.LT.0,13,1,2)                 
C       IF(INCX.LT.1)CALL SETERR(16HICAMAX-INCX.LT.1,16,2,2)            
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('ICAMAX-N.LT.0',13,1,2)                  
        IF(INCX.LT.1)CALL SETERR('ICAMAX-INCX.LT.1',16,2,2)             
C/                                                                      
        SMAX=0.0                                                        
        ICAMAX=1                                                        
        DO 10 I=1,N                                                     
        IF(SMAX.GE.CABS(X(1,I))) GO TO 10                               
           SMAX=CABS(X(1,I))                                            
           ICAMAX=I                                                     
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        INTEGER FUNCTION ISAMIN(N,X,INCX)                               
        INTEGER I,N,INCX                                                
        REAL X(INCX,1),SMIN                                             
C THIS FUNCTION RETURNS THE INDEX OF THE COMPONENT                      
C OF X HAVING MINIMUM MAGNITUDE. ONLY EVERY                             
C INCXTH COMPONENT OF X IS CONSIDERED                                   
C                                                                       
        ISAMIN=0                                                        
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(13HISAMIN-N.LT.0,13,1,2)                 
C       IF(INCX.LT.1)CALL SETERR(16HISAMIN-INCX.LT.1,16,2,2)            
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('ISAMIN-N.LT.0',13,1,2)                  
        IF(INCX.LT.1)CALL SETERR('ISAMIN-INCX.LT.1',16,2,2)             
C/                                                                      
        SMIN=ABS(X(1,1))                                                
        ISAMIN=1                                                        
        IF(SMIN .EQ. 0.0) RETURN                                        
        DO 10 I=1,N                                                     
        IF(SMIN.LE.ABS(X(1,I))) GO TO 10                                
           SMIN=ABS(X(1,I))                                             
           ISAMIN=I                                                     
           IF(SMIN .EQ. 0.0) RETURN                                     
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        INTEGER FUNCTION IDAMIN(N,X,INCX)                               
        INTEGER I,N,INCX                                                
        DOUBLE PRECISION X(INCX,1),DMIN                                 
C THIS FUNCTION RETURNS THE INDEX OF THE COMPONENT                      
C OF X HAVING MINIMUM MAGNITUDE. ONLY EVERY                             
C INCXTH COMPONENT OF X IS CONSIDERED                                   
C                                                                       
        IDAMIN=0                                                        
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(13HIDAMIN-N.LT.0,13,1,2)                 
C       IF(INCX.LT.1)CALL SETERR(16HIDAMIN-INCX.LT.1,16,2,2)            
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('IDAMIN-N.LT.0',13,1,2)                  
        IF(INCX.LT.1)CALL SETERR('IDAMIN-INCX.LT.1',16,2,2)             
C/                                                                      
        DMIN=DABS(X(1,1))                                               
        IDAMIN=1                                                        
        IF(DMIN .EQ. 0.D0) RETURN                                       
        DO 10 I=1,N                                                     
        IF(DMIN.LE.DABS(X(1,I))) GO TO 10                               
           DMIN=DABS(X(1,I))                                            
           IDAMIN=I                                                     
           IF(DMIN .EQ. 0.D0) RETURN                                    
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        INTEGER FUNCTION ICAMIN(N,X,INCX)                               
        INTEGER I,N,INCX                                                
        COMPLEX X(INCX,1)                                               
        REAL SMIN                                                       
C THIS FUNCTION RETURNS THE INDEX OF THE COMPONENT                      
C OF X HAVING MINIMUM MAGNITUDE. ONLY EVERY                             
C INCXTH COMPONENT OF X IS CONSIDERED                                   
C                                                                       
        ICAMIN=0                                                        
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(13HICAMIN-N.LT.0,13,1,2)                 
C       IF(INCX.LT.1)CALL SETERR(16HICAMIN-INCX.LT.1,16,2,2)            
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('ICAMIN-N.LT.0',13,1,2)                  
        IF(INCX.LT.1)CALL SETERR('ICAMIN-INCX.LT.1',16,2,2)             
C/                                                                      
        SMIN=CABS(X(1,I))                                               
        ICAMIN=1                                                        
        IF(SMIN .EQ. 0.0) RETURN                                        
        DO 10 I=1,N                                                     
        IF(SMIN.LE.CABS(X(1,I))) GO TO 10                               
           SMIN=CABS(X(1,1))                                            
           ICAMIN=I                                                     
           IF(SMIN .EQ. 0.0) RETURN                                     
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        INTEGER FUNCTION ISMAX(N,X,INCX)                                
        INTEGER I,N,INCX                                                
        REAL X(INCX,1),SMAX                                             
C                                                                       
C THIS FUNCTION RETURNS THE INDEX OF THE LARGEST (ALGEBRAIC)            
C COMPONENT OF X.                                                       
C ONLY EVERY INCXTH COMPONENT OF X IS CONSIDERED.                       
C                                                                       
        ISMAX=0                                                         
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(12HISMAX-N.LT.0,12,1,2)                  
C       IF(INCX.LT.1)CALL SETERR(15HISMAX-INCX.LT.1,15,2,2)             
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('ISMAX-N.LT.0',12,1,2)                   
        IF(INCX.LT.1)CALL SETERR('ISMAX-INCX.LT.1',15,2,2)              
C/                                                                      
        SMAX=X(1,1)                                                     
        ISMAX=1                                                         
        DO 10 I=1,N                                                     
        IF(SMAX.GE.X(1,I)) GO TO 10                                     
           SMAX=X(1,I)                                                  
           ISMAX=I                                                      
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        INTEGER FUNCTION IIMAX(N,X,INCX)                                
        INTEGER I,N,INCX                                                
        INTEGER X(INCX,1),IMAX                                          
C                                                                       
C THIS FUNCTION RETURNS THE INDEX OF THE LARGEST (ALGEBRAIC)            
C COMPONENT OF X.                                                       
C ONLY EVERY INCXTH COMPONENT OF X IS CONSIDERED.                       
C                                                                       
        IIMAX=0                                                         
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(12HIIMAX-N.LT.0,12,1,2)                  
C       IF(INCX.LT.1)CALL SETERR(15HIIMAX-INCX.LT.1,15,2,2)             
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('IIMAX-N.LT.0',12,1,2)                   
        IF(INCX.LT.1)CALL SETERR('IIMAX-INCX.LT.1',15,2,2)              
C/                                                                      
        IMAX=X(1,1)                                                     
        IIMAX=1                                                         
        DO 10 I=1,N                                                     
        IF(IMAX.GE.X(1,I)) GO TO 10                                     
           IMAX=X(1,I)                                                  
           IIMAX=I                                                      
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        INTEGER FUNCTION IDMAX(N,X,INCX)                                
        INTEGER I,N,INCX                                                
        DOUBLE PRECISION X(INCX,1),SMAX                                 
C                                                                       
C THIS FUNCTION RETURNS THE INDEX OF THE LARGEST (ALGEBRAIC)            
C COMPONENT OF X.                                                       
C ONLY EVERY INCXTH COMPONENT OF X IS CONSIDERED.                       
C                                                                       
        IDMAX=0                                                         
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(12HIDMAX-N.LT.0,12,1,2)                  
C       IF(INCX.LT.1)CALL SETERR(15HIDMAX-INCX.LT.1,15,2,2)             
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('IDMAX-N.LT.0',12,1,2)                   
        IF(INCX.LT.1)CALL SETERR('IDMAX-INCX.LT.1',15,2,2)              
C/                                                                      
        SMAX=X(1,1)                                                     
        IDMAX=1                                                         
        DO 10 I=1,N                                                     
        IF(SMAX.GE.X(1,I)) GO TO 10                                     
           SMAX=X(1,I)                                                  
           IDMAX=I                                                      
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        INTEGER FUNCTION ISMIN(N,X,INCX)                                
        INTEGER I,N,INCX                                                
        REAL X(INCX,1),SMIN                                             
C                                                                       
C THIS FUNCTION RETURNS THE INDEX OF THE SMALLEST (ALGEBRAIC)           
C COMPONENT OF X.                                                       
C ONLY EVERY INCXTH COMPONENT OF X IS CONSIDERED.                       
C                                                                       
        ISMIN=0                                                         
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(12HISMIN-N.LT.0,12,1,2)                  
C       IF(INCX.LT.1)CALL SETERR(15HISMIN-INCX.LT.1,15,2,2)             
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('ISMIN-N.LT.0',12,1,2)                   
        IF(INCX.LT.1)CALL SETERR('ISMIN-INCX.LT.1',15,2,2)              
C/                                                                      
        SMIN=X(1,1)                                                     
        ISMIN=1                                                         
        DO 10 I=1,N                                                     
        IF(SMIN.LE.X(1,I)) GO TO 10                                     
           SMIN=X(1,I)                                                  
           ISMIN=I                                                      
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        INTEGER FUNCTION IIMIN(N,X,INCX)                                
        INTEGER I,N,INCX                                                
        INTEGER X(INCX,1),IMIN                                          
C                                                                       
C THIS FUNCTION RETURNS THE INDEX OF THE LARGEST (ALGEBRAIC)            
C COMPONENT OF X.                                                       
C ONLY EVERY INCXTH COMPONENT OF X IS CONSIDERED.                       
C                                                                       
        IIMIN=0                                                         
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(12HIIMIN-N.LT.0,12,1,2)                  
C       IF(INCX.LT.1)CALL SETERR(15HIIMIN-INCX.LT.1,15,2,2)             
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('IIMIN-N.LT.0',12,1,2)                   
        IF(INCX.LT.1)CALL SETERR('IIMIN-INCX.LT.1',15,2,2)              
C/                                                                      
        IMIN=X(1,1)                                                     
        IIMIN=1                                                         
        DO 10 I=1,N                                                     
        IF(IMIN.LE.X(1,I)) GO TO 10                                     
           IMIN=X(1,I)                                                  
           IIMIN=I                                                      
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        INTEGER FUNCTION IDMIN(N,X,INCX)                                
        INTEGER I,N,INCX                                                
        DOUBLE PRECISION X(INCX,1),SMIN                                 
C                                                                       
C THIS FUNCTION RETURNS THE INDEX OF THE SMALLEST (ALGEBRAIC)           
C COMPONENT OF X.                                                       
C ONLY EVERY INCXTH COMPONENT OF X IS CONSIDERED.                       
C                                                                       
        IDMIN=0                                                         
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0) CALL SETERR(12HIDMIN-N.LT.0,12,1,2)                  
C       IF(INCX.LT.1)CALL SETERR(15HIDMIN-INCX.LT.1,15,2,2)             
C/7S                                                                    
        IF(N.LT.0) CALL SETERR('IDMIN-N.LT.0',12,1,2)                   
        IF(INCX.LT.1)CALL SETERR('IDMIN-INCX.LT.1',15,2,2)              
C/                                                                      
        SMIN=X(1,1)                                                     
        IDMIN=1                                                         
        DO 10 I=1,N                                                     
        IF(SMIN.LE.X(1,I)) GO TO 10                                     
           SMIN=X(1,I)                                                  
           IDMIN=I                                                      
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        REAL FUNCTION SAMAX(N,X,INCX)                                   
        INTEGER   N,INCX                                                
        REAL X(INCX,1)                                                  
C                                                                       
C THIS FUNCTIONS RETURNS THE MAGNITUDE OF THE COMPONENT                 
C OF X HAVING MAXIMUM MAGNITUDE. ONLY EVERY INCXTH                      
C COMPONENT OF X IS CONSIDERED.                                         
C                                                                       
        SAMAX=0.0                                                       
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0)CALL SETERR(12HSAMAX-N.LT.0,12,1,2)                   
C       IF (INCX.LT.1) CALL SETERR(15HSAMAX-INCX.LT.1,15,2,2)           
C/7S                                                                    
        IF(N.LT.0)CALL SETERR('SAMAX-N.LT.0',12,1,2)                    
        IF (INCX.LT.1) CALL SETERR('SAMAX-INCX.LT.1',15,2,2)            
C/                                                                      
        DO 10 I=1,N                                                     
           IF(SAMAX.LT.ABS(X(1,I)))SAMAX=ABS(X(1,I))                    
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        DOUBLE PRECISION FUNCTION DAMAX(N,X,INCX)                       
        INTEGER   N,INCX                                                
        DOUBLE PRECISION X(INCX,1)                                      
C                                                                       
C THIS FUNCTIONS RETURNS THE MAGNITUDE OF THE COMPONENT                 
C OF X HAVING MAXIMUM MAGNITUDE. ONLY EVERY INCXTH                      
C COMPONENT OF X IS CONSIDERED.                                         
C                                                                       
        DAMAX=0.0                                                       
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0)CALL SETERR(12HDAMAX-N.LT.0,12,1,2)                   
C       IF (INCX.LT.1) CALL SETERR(15HDAMAX-INCX.LT.1,15,2,2)           
C/7S                                                                    
        IF(N.LT.0)CALL SETERR('DAMAX-N.LT.0',12,1,2)                    
        IF (INCX.LT.1) CALL SETERR('DAMAX-INCX.LT.1',15,2,2)            
C/                                                                      
        DO 10 I=1,N                                                     
           IF(DAMAX.LT.DABS(X(1,I)))DAMAX=DABS(X(1,I))                  
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        REAL FUNCTION CAMAX(N,X,INCX)                                   
        INTEGER   N,INCX                                                
        COMPLEX X(INCX,1)                                               
C                                                                       
C THIS FUNCTIONS RETURNS THE MAGNITUDE OF THE COMPONENT                 
C OF X HAVING MAXIMUM MAGNITUDE. ONLY EVERY INCXTH                      
C COMPONENT OF X IS CONSIDERED.                                         
C                                                                       
        CAMAX=0.0                                                       
        IF(N.EQ.0) RETURN                                               
C/6S                                                                    
C       IF(N.LT.0)CALL SETERR(12HCAMAX-N.LT.0,12,1,2)                   
C       IF (INCX.LT.1) CALL SETERR(15HCAMAX-INCX.LT.1,15,2,2)           
C/7S                                                                    
        IF(N.LT.0)CALL SETERR('CAMAX-N.LT.0',12,1,2)                    
        IF (INCX.LT.1) CALL SETERR('CAMAX-INCX.LT.1',15,2,2)            
C/                                                                      
        DO 10 I=1,N                                                     
           IF(CAMAX.LT.CABS(X(1,I)))CAMAX=CABS(X(1,I))                  
 10     CONTINUE                                                        
        RETURN                                                          
        END                                                             
        REAL FUNCTION SASUM(N,X,INCX)                                   
C                                                                       
C THIS FUNCTION RETURNS THE SUM OF THE ABSOLUTE VALUES                  
C OF THE COMPONENTS OF X. ONLY EVERY INCXTH COMPONENT                   
C OF X IS CONSIDERED                                                    
       REAL X(INCX,1)                                                   
       SASUM=0.0                                                        
       IF(N.EQ.0) RETURN                                                
C/6S                                                                    
C      IF (N.LT.0) CALL SETERR(12HSASUM-N.LT.0,12,1,2)                  
C      IF(INCX.LT.1) CALL SETERR(15HSASUM-INCX.LT.1,15,2,2)             
C/7S                                                                    
       IF (N.LT.0) CALL SETERR('SASUM-N.LT.0',12,1,2)                   
       IF(INCX.LT.1) CALL SETERR('SASUM-INCX.LT.1',15,2,2)              
C/                                                                      
       DO 10 I=1,N                                                      
          SASUM=SASUM+ABS(X(1,I))                                       
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
        DOUBLE PRECISION FUNCTION DASUM(N,X,INCX)                       
C                                                                       
C THIS FUNCTION RETURNS THE SUM OF THE ABSOLUTE VALUES                  
C OF THE COMPONENTS OF X. ONLY EVERY INCXTH COMPONENT                   
C OF X IS CONSIDERED                                                    
       DOUBLE PRECISION X(INCX,1)                                       
       DASUM=0.D0                                                       
       IF(N.EQ.0) RETURN                                                
C/6S                                                                    
C      IF (N.LT.0) CALL SETERR(12HDASUM-N.LT.0,12,1,2)                  
C      IF(INCX.LT.1) CALL SETERR(15HDASUM-INCX.LT.1,15,2,2)             
C/7S                                                                    
       IF (N.LT.0) CALL SETERR('DASUM-N.LT.0',12,1,2)                   
       IF(INCX.LT.1) CALL SETERR('DASUM-INCX.LT.1',15,2,2)              
C/                                                                      
       DO 10 I=1,N                                                      
          DASUM=DASUM+DABS(X(1,I))                                      
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
        REAL FUNCTION SCASUM(N,X,INCX)                                  
C                                                                       
C THIS FUNCTION RETURNS THE SUM OF THE ABSOLUTE VALUES                  
C OF THE COMPONENTS OF X. ONLY EVERY INCXTH COMPONENT                   
C OF X IS CONSIDERED                                                    
       COMPLEX X(INCX,1)                                                
       SCASUM=0.0                                                       
       IF(N.EQ.0) RETURN                                                
C/6S                                                                    
C      IF (N.LT.0) CALL SETERR(13HSCASUM-N.LT.0,13,1,2)                 
C      IF(INCX.LT.1) CALL SETERR(16HSCASUM-INCX.LT.1,16,2,2)            
C/7S                                                                    
       IF (N.LT.0) CALL SETERR('SCASUM-N.LT.0',13,1,2)                  
       IF(INCX.LT.1) CALL SETERR('SCASUM-INCX.LT.1',16,2,2)             
C/                                                                      
       DO 10 I=1,N                                                      
          SCASUM=SCASUM+CABS1(X(1,I))                                   
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
       SUBROUTINE SAXPY(N,A,X,INCX,Y,INCY)                              
C                                                                       
C THIS SUBROUTINE MULTIPLIES THE X VECTOR BY A AND                      
C ADDS THE RESULT TO THE Y VECTOR                                       
C ONLY EVERY INCXTH COMPONENT OF X AND EVERY INCYTH                     
C COMPONENT OF Y ARE CONSIDERED                                         
       REAL X(INCX,1),Y(INCY,1),A                                       
       IF (N.EQ.0) RETURN                                               
C/6S                                                                    
C      IF(N.LT.0) CALL SETERR(12HSAXPY-N.LT.0,12,1,2)                   
C      IF(INCX.LT.1) CALL SETERR(15HSAXPY-INCX.LT.1,15,2,2)             
C      IF(INCY.LT.1) CALL SETERR(15HSAXPY-INCY.LT.1,15,3,2)             
C/7S                                                                    
       IF(N.LT.0) CALL SETERR('SAXPY-N.LT.0',12,1,2)                    
       IF(INCX.LT.1) CALL SETERR('SAXPY-INCX.LT.1',15,2,2)              
       IF(INCY.LT.1) CALL SETERR('SAXPY-INCY.LT.1',15,3,2)              
C/                                                                      
       DO 10 I=1,N                                                      
          Y(1,I)=Y(1,I)+A*X(1,I)                                        
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
       SUBROUTINE DAXPY(N,A,X,INCX,Y,INCY)                              
C                                                                       
C THIS SUBROUTINE MULTIPLIES THE X VECTOR BY A AND                      
C ADDS THE RESULT TO THE Y VECTOR                                       
C ONLY EVERY INCXTH COMPONENT OF X AND EVERY INCYTH                     
C COMPONENT OF Y ARE CONSIDERED                                         
       DOUBLE PRECISION X(INCX,1),Y(INCY,1),A                           
       IF (N.EQ.0) RETURN                                               
C/6S                                                                    
C      IF(N.LT.0) CALL SETERR(12HDAXPY-N.LT.0,12,1,2)                   
C      IF(INCX.LT.1) CALL SETERR(15HDAXPY-INCX.LT.1,15,2,2)             
C      IF(INCY.LT.1) CALL SETERR(15HDAXPY-INCY.LT.1,15,3,2)             
C/7S                                                                    
       IF(N.LT.0) CALL SETERR('DAXPY-N.LT.0',12,1,2)                    
       IF(INCX.LT.1) CALL SETERR('DAXPY-INCX.LT.1',15,2,2)              
       IF(INCY.LT.1) CALL SETERR('DAXPY-INCY.LT.1',15,3,2)              
C/                                                                      
       DO 10 I=1,N                                                      
          Y(1,I)=Y(1,I)+A*X(1,I)                                        
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
       SUBROUTINE CAXPY(N,A,X,INCX,Y,INCY)                              
C                                                                       
C THIS SUBROUTINE MULTIPLIES THE X VECTOR BY A AND                      
C ADDS THE RESULT TO THE Y VECTOR                                       
C ONLY EVERY INCXTH COMPONENT OF X AND EVERY INCYTH                     
C COMPONENT OF Y ARE CONSIDERED                                         
       COMPLEX X(INCX,1),Y(INCY,1),A                                    
       IF (N.EQ.0) RETURN                                               
C/6S                                                                    
C      IF(N.LT.0) CALL SETERR(12HCAXPY-N.LT.0,12,1,2)                   
C      IF(INCX.LT.1) CALL SETERR(15HCAXPY-INCX.LT.1,15,2,2)             
C      IF(INCY.LT.1) CALL SETERR(15HCAXPY-INCY.LT.1,15,3,2)             
C/7S                                                                    
       IF(N.LT.0) CALL SETERR('CAXPY-N.LT.0',12,1,2)                    
       IF(INCX.LT.1) CALL SETERR('CAXPY-INCX.LT.1',15,2,2)              
       IF(INCY.LT.1) CALL SETERR('CAXPY-INCY.LT.1',15,3,2)              
C/                                                                      
       DO 10 I=1,N                                                      
          Y(1,I)=Y(1,I)+A*X(1,I)                                        
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
      SUBROUTINE  SCOPY(N,SX,INCX,SY,INCY)                              
C                                                                       
C     COPIES A VECTOR, X, TO A VECTOR, Y.                               
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.                    
C     JACK DONGARRA, LINPACK, 3/11/78.                                  
C                                                                       
C     ADAPTED TO PORT 3 BY PHYL FOX, 11/8/83.                           
C                                                                       
      REAL SX(1),SY(1)                                                  
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N                                 
C                                                                       
      IF(N.EQ.0)RETURN                                                  
C/6S                                                                    
C     IF(N .LT. 0) CALL SETERR(12HSCOPY-N.LT.0, 12, 1, 2)               
C     IF(INCY .EQ. 0) CALL SETERR(15HSCOPY-INCY.EQ.0, 15, 3, 2)         
C/7S                                                                    
      IF(N .LT. 0) CALL SETERR('SCOPY-N.LT.0', 12, 1, 2)                
      IF(INCY .EQ. 0) CALL SETERR('SCOPY-INCY.EQ.0', 15, 3, 2)          
C/                                                                      
C                                                                       
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20                               
C                                                                       
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS                
C          NOT EQUAL TO 1                                               
C                                                                       
      IX = 1                                                            
      IY = 1                                                            
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 
      DO 10 I = 1,N                                                     
        SY(IY) = SX(IX)                                                 
        IX = IX + INCX                                                  
        IY = IY + INCY                                                  
   10 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C        CODE FOR BOTH INCREMENTS EQUAL TO 1                            
C                                                                       
C                                                                       
C        CLEAN-UP LOOP                                                  
C                                                                       
   20 M = MOD(N,7)                                                      
      IF( M .EQ. 0 ) GO TO 40                                           
      DO 30 I = 1,M                                                     
        SY(I) = SX(I)                                                   
   30 CONTINUE                                                          
      IF( N .LT. 7 ) RETURN                                             
   40 MP1 = M + 1                                                       
      DO 50 I = MP1,N,7                                                 
        SY(I) = SX(I)                                                   
        SY(I + 1) = SX(I + 1)                                           
        SY(I + 2) = SX(I + 2)                                           
        SY(I + 3) = SX(I + 3)                                           
        SY(I + 4) = SX(I + 4)                                           
        SY(I + 5) = SX(I + 5)                                           
        SY(I + 6) = SX(I + 6)                                           
   50 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE  DCOPY(N,DX,INCX,DY,INCY)                              
C                                                                       
C     COPIES A VECTOR, X, TO A VECTOR, Y.                               
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.                  
C     JACK DONGARRA, LINPACK, 3/11/78.                                  
C                                                                       
C     ADAPTED TO PORT 3 BY PHYL FOX, 11/8/83                            
C                                                                       
      DOUBLE PRECISION DX(1),DY(1)                                      
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N                                 
C                                                                       
      IF(N.EQ.0)RETURN                                                  
C/6S                                                                    
C     IF(N .LT. 0) CALL SETERR(12HDCOPY-N.LT.0, 12, 1, 2)               
C     IF(INCY .EQ. 0) CALL SETERR(15HDCOPY-INCY.EQ.0, 15, 3, 2)         
C/7S                                                                    
      IF(N .LT. 0) CALL SETERR('DCOPY-N.LT.0', 12, 1, 2)                
      IF(INCY .EQ. 0) CALL SETERR('DCOPY-INCY.EQ.0', 15, 3, 2)          
C/                                                                      
C                                                                       
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20                               
C                                                                       
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS                
C          NOT EQUAL TO 1                                               
C                                                                       
      IX = 1                                                            
      IY = 1                                                            
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 
      DO 10 I = 1,N                                                     
        DY(IY) = DX(IX)                                                 
        IX = IX + INCX                                                  
        IY = IY + INCY                                                  
   10 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C        CODE FOR BOTH INCREMENTS EQUAL TO 1                            
C                                                                       
C                                                                       
C        CLEAN-UP LOOP                                                  
C                                                                       
   20 M = MOD(N,7)                                                      
      IF( M .EQ. 0 ) GO TO 40                                           
      DO 30 I = 1,M                                                     
        DY(I) = DX(I)                                                   
   30 CONTINUE                                                          
      IF( N .LT. 7 ) RETURN                                             
   40 MP1 = M + 1                                                       
      DO 50 I = MP1,N,7                                                 
        DY(I) = DX(I)                                                   
        DY(I + 1) = DX(I + 1)                                           
        DY(I + 2) = DX(I + 2)                                           
        DY(I + 3) = DX(I + 3)                                           
        DY(I + 4) = DX(I + 4)                                           
        DY(I + 5) = DX(I + 5)                                           
        DY(I + 6) = DX(I + 6)                                           
   50 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE  CCOPY(N,CX,INCX,CY,INCY)                              
C                                                                       
C     COPIES A VECTOR, X, TO A VECTOR, Y.                               
C     JACK DONGARRA, LINPACK, 3/11/78.                                  
C                                                                       
C     ADAPTED TO PORT 3 BY PHYL FOX, 11/8/83                            
C                                                                       
      COMPLEX CX(1),CY(1)                                               
      INTEGER I,INCX,INCY,IX,IY,N                                       
C                                                                       
      IF(N.EQ.0)RETURN                                                  
C/6S                                                                    
C     IF(N .LT. 0) CALL SETERR(12HCCOPY-N.LT.0, 12, 1, 2)               
C     IF(INCY .EQ. 0) CALL SETERR(15HCCOPY-INCY.EQ.0, 15, 3, 2)         
C/7S                                                                    
      IF(N .LT. 0) CALL SETERR('CCOPY-N.LT.0', 12, 1, 2)                
      IF(INCY .EQ. 0) CALL SETERR('CCOPY-INCY.EQ.0', 15, 3, 2)          
C/                                                                      
C                                                                       
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20                               
C                                                                       
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS                
C          NOT EQUAL TO 1                                               
C                                                                       
      IX = 1                                                            
      IY = 1                                                            
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 
      DO 10 I = 1,N                                                     
        CY(IY) = CX(IX)                                                 
        IX = IX + INCX                                                  
        IY = IY + INCY                                                  
   10 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C        CODE FOR BOTH INCREMENTS EQUAL TO 1                            
C                                                                       
   20 DO 30 I = 1,N                                                     
        CY(I) = CX(I)                                                   
   30 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      REAL FUNCTION SDOT(N, SX, INCX, SY, INCY)                         
      INTEGER N, INCX, INCY                                             
      REAL SX(INCX, 1), SY(INCY, 1)                                     
      INTEGER I                                                         
C        RETURNS DOT PRODUCT OF SX AND SY                               
      SDOT = 0.                                                         
      IF (N .EQ. 0) RETURN                                              
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(17H  SDOT - N .LT. 0, 17, 1, 2)         
C     IF (INCX .LE. 0) CALL SETERR(20H  SDOT - INCX .LT. 0, 20, 1, 2)   
C     IF (INCY .LE. 0) CALL SETERR(20H  SDOT - INCY .LT. 0, 20, 1, 2)   
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('  SDOT - N .LT. 0', 17, 1, 2)          
      IF (INCX .LE. 0) CALL SETERR('  SDOT - INCX .LT. 0', 20, 1, 2)    
      IF (INCY .LE. 0) CALL SETERR('  SDOT - INCY .LT. 0', 20, 1, 2)    
C/                                                                      
      DO  1 I = 1, N                                                    
         SDOT = SDOT+SX(1, I)*SY(1, I)                                  
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DDOT(N, SX, INCX, SY, INCY)             
      INTEGER N, INCX, INCY                                             
      DOUBLE PRECISION SX(INCX, 1), SY(INCY, 1)                         
      INTEGER I                                                         
C        RETURNS DOT PRODUCT OF SX AND SY                               
      DDOT = 0.0D0                                                      
      IF (N .EQ. 0) RETURN                                              
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(17H  DDOT - N .LT. 0, 17, 1, 2)         
C     IF (INCX .LE. 0) CALL SETERR(19H  DDOT - INCX .LT.0, 20, 1, 2)    
C     IF (INCY .LE. 0) CALL SETERR(19H  DDOT - INCY .LT.0, 20, 1, 2)    
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('  DDOT - N .LT. 0', 17, 1, 2)          
      IF (INCX .LE. 0) CALL SETERR('  DDOT - INCX .LT.0', 20, 1, 2)     
      IF (INCY .LE. 0) CALL SETERR('  DDOT - INCY .LT.0', 20, 1, 2)     
C/                                                                      
      DO  1 I = 1, N                                                    
         DDOT = DDOT+SX(1, I)*SY(1, I)                                  
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      COMPLEX FUNCTION  CDOTC(N,DX,INCX,DY,INCY)                        
C THIS FUNCTION COMPUTES THE DOT PRODUCT OF X AND Y                     
C ONLY EVERY INCXTH COMPONENT OF X AND EVERY INCYTH                     
C COMPONENT OF Y IS CONSIDERED                                          
C                                                                       
      COMPLEX DX(INCX,1),DY(INCY,1)                                     
       CDOTC = CMPLX(0.0,0.0)                                           
       IF (N.EQ.0) RETURN                                               
C/6S                                                                    
C     IF (N.LT.0) CALL SETERR(13H CDOTC-N.LT.0,13,1,2)                  
C     IF(INCX.LT.1) CALL SETERR(16H CDOTC-INCX.LT.1,16,2,2)             
C     IF(INCY.LT.1) CALL SETERR(16H CDOTC-INCY.LT.1,16,3,2)             
C/7S                                                                    
      IF (N.LT.0) CALL SETERR(' CDOTC-N.LT.0',13,1,2)                   
      IF(INCX.LT.1) CALL SETERR(' CDOTC-INCX.LT.1',16,2,2)              
      IF(INCY.LT.1) CALL SETERR(' CDOTC-INCY.LT.1',16,3,2)              
C/                                                                      
      DO 10 I = 1,N                                                     
         CDOTC =  CDOTC + CONJG(DX(1,I))*DY(1,I)                        
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      COMPLEX FUNCTION  CDOTU(N,DX,INCX,DY,INCY)                        
C THIS FUNCTION COMPUTES THE DOT PRODUCT OF X AND Y                     
C ONLY EVERY INCXTH COMPONENT OF X AND EVERY INCYTH                     
C COMPONENT OF Y IS CONSIDERED                                          
C                                                                       
      COMPLEX DX(INCX,1),DY(INCY,1)                                     
       CDOTU = CMPLX(0.0,0.0)                                           
       IF (N.EQ.0) RETURN                                               
C/6S                                                                    
C     IF (N.LT.0) CALL SETERR(13H CDOTU-N.LT.0,13,1,2)                  
C     IF(INCX.LT.1) CALL SETERR(16H CDOTU-INCX.LT.1,16,2,2)             
C     IF(INCY.LT.1) CALL SETERR(16H CDOTU-INCY.LT.1,16,3,2)             
C/7S                                                                    
      IF (N.LT.0) CALL SETERR(' CDOTU-N.LT.0',13,1,2)                   
      IF(INCX.LT.1) CALL SETERR(' CDOTU-INCX.LT.1',16,2,2)              
      IF(INCY.LT.1) CALL SETERR(' CDOTU-INCY.LT.1',16,3,2)              
C/                                                                      
      DO 10 I = 1,N                                                     
         CDOTU =  CDOTU+ DX(1,I)*DY(1,I)                                
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      REAL FUNCTION SNRM2(N, X, INCX)                                   
      INTEGER N, INCX                                                   
      REAL X(INCX, 1)                                                   
      INTEGER I1MACH, NMAX, BETA, IEXP, J, EMIN                         
      INTEGER EMAX, T, MAX0, IOUT                                       
      REAL R1MACH, S2MACH, AX, ABIG, AMED, ASML                         
      REAL B1, B2, S1, S2, EPS, RELERR                                  
      REAL OVERFL, RBIG, SQRT, FLOAT, ABS, SNGL                         
      REAL AMIN1, AMAX1                                                 
      DOUBLE PRECISION DSML, DMED, DBIG, DX                             
      DATA B1/0.0/                                                      
      DATA B2/0.0/                                                      
      DATA S1/0.0/                                                      
      DATA S2/0.0/                                                      
      DATA RELERR/0.0/                                                  
      DATA RBIG/0.0/                                                    
      DATA OVERFL/0.0/                                                  
      DATA NMAX/0/                                                      
C CALCULATE 2-NORM OF ROW 1 OF X ARRAY.                                 
C AVOID ALL OVERFLOWS AND UNDERFLOWS                                    
C I1MACH( 9) = N, THE LARGEST INTEGER                                   
C I1MACH(10) = BETA, THE BASE FOR FLOATING-POINT NUMBERS                
C I1MACH(11) = T, THE NUMBER OF BASE-BETA DIGITS IN THE MANTISSA        
C I1MACH(12) = EMIN, THE MINIMUM EXPONENT                               
C I1MACH(13) = EMAX, THE MAXIMUM EXPONENT                               
C R1MACH( 2) = R, THE LARGEST FLOATING-POINT NUMBER                     
C EXTERNAL FUNCTION                                                     
C EXTERNAL FUNCTIONS                                                    
      IF (N .NE. 0) GOTO 1                                              
         SNRM2 = 0.                                                     
         RETURN                                                         
C/6S                                                                    
C  1  IF (N .LT. 0) CALL SETERR(17H SNRM2 - N .LT. 0, 17, 1, 2)         
C     IF (INCX .LE. 0) CALL SETERR(20H SNRM2 - INCX .LE. 0, 20, 2, 2)   
C/7S                                                                    
   1  IF (N .LT. 0) CALL SETERR(' SNRM2 - N .LT. 0', 17, 1, 2)          
      IF (INCX .LE. 0) CALL SETERR(' SNRM2 - INCX .LE. 0', 20, 2, 2)    
C/                                                                      
      IF (NMAX .GT. 0) GOTO 4                                           
         NMAX = I1MACH(9)                                               
C FIRST-TIME SWITCH                                                     
         BETA = I1MACH(10)                                              
         T = I1MACH(11)                                                 
         EMIN = I1MACH(12)                                              
         EMAX = I1MACH(13)                                              
         IEXP = -((1-EMIN)/2)                                           
C LOWER BOUNDARY OF MIDRANGE                                            
         B1 = S2MACH(1.0, BETA, IEXP)                                   
         IEXP = (EMAX+1-T)/2                                            
C UPPER BOUNDARY OF MIDRANGE                                            
         B2 = S2MACH(1.0, BETA, IEXP)                                   
         IEXP = -((2-EMIN)/2)                                           
C SCALING FACTOR FOR LOWER RANGE                                        
         S1 = S2MACH(1.0, BETA, IEXP)                                   
         IEXP = (EMAX+T)/2                                              
C SCALING FACTOR FOR UPPER RANGE                                        
         S2 = S2MACH(1.0, BETA, IEXP)                                   
         RBIG = R1MACH(2)                                               
         OVERFL = RBIG/S2                                               
         EPS = S2MACH(1.0, BETA, 1-T)                                   
         RELERR = SQRT(EPS)                                             
         ABIG = 1.0/EPS-1.0                                             
         IF (FLOAT(NMAX) .GT. ABIG) NMAX = ABIG                         
         IF (EMIN .LE. 1-2*T .AND. T+1 .LE. EMAX .AND. T .GE. MAX0(2, 6-
     1      BETA)) GOTO 3                                               
            IOUT = I1MACH(2)                                            
            WRITE (IOUT,  2)                                            
   2        FORMAT (43H SNRM2 - THE ALGORITHM CANNOT BE GUARANTEED,     
     1         17H ON THIS COMPUTER)                                    
   3     CONTINUE                                                       
C/6S                                                                    
C  4  IF (N .GT. NMAX) CALL SETERR(20H SNRM2 - N TOO LARGE, 20, 3, 2)   
C/7S                                                                    
   4  IF (N .GT. NMAX) CALL SETERR(' SNRM2 - N TOO LARGE', 20, 3, 2)    
C/                                                                      
      DSML = 0.0D0                                                      
      DMED = 0.0D0                                                      
      DBIG = 0.0D0                                                      
      DO  9 J = 1, N                                                    
         AX = ABS(X(1, J))                                              
         DX = AX                                                        
         IF (AX .LE. B2) GOTO 5                                         
            DBIG = DBIG+(DX/S2)**2                                      
            GOTO  8                                                     
   5        IF (AX .LE. B1) GOTO 6                                      
               DMED = DMED+DX**2                                        
               GOTO  7                                                  
   6           DSML = DSML+(DX/S1)**2                                   
   7     CONTINUE                                                       
   8     CONTINUE                                                       
   9     CONTINUE                                                       
      IF (DBIG .LE. 0.0D0) GOTO 11                                      
         ABIG = SQRT(SNGL(DBIG))                                        
         IF (ABIG .LE. OVERFL) GOTO 10                                  
            SNRM2 = RBIG                                                
C/6S                                                                    
C           CALL SETERR(17H SNRM2 - OVERFLOW, 17, 4, 1)                 
C/7S                                                                    
            CALL SETERR(' SNRM2 - OVERFLOW', 17, 4, 1)                  
C/                                                                      
            RETURN                                                      
  10     ABIG = ABIG*S2                                                 
         AMED = SQRT(SNGL(DMED))                                        
         GOTO  16                                                       
  11     IF (DSML .LE. 0.0D0) GOTO 14                                   
            IF (DMED .LE. 0.0D0) GOTO 12                                
               ABIG = SQRT(SNGL(DMED))                                  
               AMED = SQRT(SNGL(DSML))*S1                               
               GOTO  13                                                 
  12           SNRM2 = SQRT(SNGL(DSML))*S1                              
               RETURN                                                   
  13        CONTINUE                                                    
            GOTO  15                                                    
  14        SNRM2 = SQRT(SNGL(DMED))                                    
            RETURN                                                      
  15  CONTINUE                                                          
  16  ASML = AMIN1(ABIG, AMED)                                          
      ABIG = AMAX1(ABIG, AMED)                                          
      IF (ASML .GT. ABIG*RELERR) GOTO 17                                
         SNRM2 = ABIG                                                   
         GOTO  18                                                       
  17     SNRM2 = ABIG*SQRT((ASML/ABIG)**2+1.0)                          
  18  RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DNRM2(N, X, INCX)                       
      INTEGER N, INCX                                                   
      DOUBLE PRECISION X(INCX, 1)                                       
      INTEGER I1MACH, NMAX, BETA, IEXP, J, EMIN                         
      INTEGER EMAX, T, MAX0, IOUT                                       
      REAL FLOAT                                                        
      DOUBLE PRECISION D1MACH, S3MACH, B1, B2, S1, S2                   
      DOUBLE PRECISION EPS, RELERR, OVERFL, DSML, DMED, DBIG            
      DOUBLE PRECISION DX, DSQRT                                        
      DATA B1/0.0D0/                                                    
      DATA B2/0.0D0/                                                    
      DATA S1/0.0D0/                                                    
      DATA S2/0.0D0/                                                    
      DATA RELERR/0.0D0/                                                
      DATA OVERFL/0.0D0/                                                
      DATA NMAX/0/                                                      
C CALCULATE 2-NORM OF ROW 1 OF X ARRAY.                                 
C AVOID ALL OVERFLOWS AND UNDERFLOWS                                    
C I1MACH( 9) = N, THE LARGEST INTEGER                                   
C I1MACH(10) = BETA, THE BASE FOR FLOATING-POINT NUMBERS                
C I1MACH(14) = T, THE NUMBER OF BASE-BETA DIGITS IN THE MANTISSA        
C I1MACH(15) = EMIN, THE MINIMUM EXPONENT                               
C I1MACH(16) = EMAX, THE MAXIMUM EXPONENT                               
C D1MACH( 2) = R, THE LARGEST FLOATING-POINT NUMBER                     
C EXTERNAL FUNCTIONS                                                    
C EXTERNAL FUNCTIONS                                                    
      IF (N .NE. 0) GOTO 1                                              
         DNRM2 = 0.D0                                                   
         RETURN                                                         
C/6S                                                                    
C  1  IF (N .LT. 0) CALL SETERR(17H DNRM2 - N .LT. 0, 17, 1, 2)         
C     IF (INCX .LE. 0) CALL SETERR(20H DNRM2 - INCX .LE. 0, 20, 2, 2)   
C/7S                                                                    
   1  IF (N .LT. 0) CALL SETERR(' DNRM2 - N .LT. 0', 17, 1, 2)          
      IF (INCX .LE. 0) CALL SETERR(' DNRM2 - INCX .LE. 0', 20, 2, 2)    
C/                                                                      
      IF (NMAX .GT. 0) GOTO 4                                           
         NMAX = I1MACH(9)                                               
C FIRST-TIME SWITCH                                                     
         BETA = I1MACH(10)                                              
         T = I1MACH(14)                                                 
         EMIN = I1MACH(15)                                              
         EMAX = I1MACH(16)                                              
         IEXP = -((1-EMIN)/2)                                           
C LOWER BOUNDARY OF MIDRANGE                                            
         B1 = S3MACH(1.0D0, BETA, IEXP)                                 
         IEXP = (EMAX+1-T)/2                                            
C UPPER BOUNDARY OF MIDRANGE                                            
         B2 = S3MACH(1.0D0, BETA, IEXP)                                 
         IEXP = -((2-EMIN)/2)                                           
C SCALING FACTOR FOR LOWER RANGE                                        
         S1 = S3MACH(1.0D0, BETA, IEXP)                                 
         IEXP = (EMAX+T)/2                                              
C SCALING FACTOR FOR UPPER RANGE                                        
         S2 = S3MACH(1.0D0, BETA, IEXP)                                 
         OVERFL = D1MACH(2)/S2                                          
         EPS = S3MACH(1.0D0, BETA, 1-T)                                 
         RELERR = DSQRT(EPS)                                            
         DBIG = 1.0D0/EPS-1.0D0                                         
         IF (DBLE(FLOAT(NMAX)) .GT. DBIG) NMAX = DBIG                   
         IF (EMIN .LE. 1-2*T .AND. T+1 .LE. EMAX .AND. T .GE. MAX0(2, 6-
     1      BETA)) GOTO 3                                               
            IOUT = I1MACH(2)                                            
            WRITE (IOUT,  2)                                            
   2        FORMAT (43H DNRM2 - THE ALGORITHM CANNOT BE GUARANTEED,     
     1         17H ON THIS COMPUTER)                                    
   3     CONTINUE                                                       
C/6S                                                                    
C  4  IF (N .GT. NMAX) CALL SETERR(20H DNRM2 - N TOO LARGE, 20, 3, 2)   
C/7S                                                                    
   4  IF (N .GT. NMAX) CALL SETERR(' DNRM2 - N TOO LARGE', 20, 3, 2)    
C/                                                                      
      DSML = 0.0D0                                                      
      DMED = 0.0D0                                                      
      DBIG = 0.0D0                                                      
      DO  9 J = 1, N                                                    
         DX = DABS(X(1, J))                                             
         IF (DX .LE. B2) GOTO 5                                         
            DBIG = DBIG+(DX/S2)**2                                      
            GOTO  8                                                     
   5        IF (DX .LE. B1) GOTO 6                                      
               DMED = DMED+DX**2                                        
               GOTO  7                                                  
   6           DSML = DSML+(DX/S1)**2                                   
   7     CONTINUE                                                       
   8     CONTINUE                                                       
   9     CONTINUE                                                       
      IF (DBIG .LE. 0.0D0) GOTO 11                                      
         DBIG = DSQRT(DBIG)                                             
         IF (DBIG .LE. OVERFL) GOTO 10                                  
            DNRM2 = D1MACH(2)                                           
C/6S                                                                    
C           CALL SETERR(17H DNRM2 - OVERFLOW, 17, 4, 1)                 
C/7S                                                                    
            CALL SETERR(' DNRM2 - OVERFLOW', 17, 4, 1)                  
C/                                                                      
            RETURN                                                      
  10     DBIG = DBIG*S2                                                 
         DMED = DSQRT(DMED)                                             
         GOTO  16                                                       
  11     IF (DSML .LE. 0.0D0) GOTO 14                                   
            IF (DMED .LE. 0.0D0) GOTO 12                                
               DBIG = DSQRT(DMED)                                       
               DMED = DSQRT(DSML)*S1                                    
               GOTO  13                                                 
  12           DNRM2 = DSQRT(DSML)*S1                                   
               RETURN                                                   
  13        CONTINUE                                                    
            GOTO  15                                                    
  14        DNRM2 = DSQRT(DMED)                                         
            RETURN                                                      
  15  CONTINUE                                                          
  16  DSML = DMIN1(DBIG, DMED)                                          
      DBIG = DMAX1(DBIG, DMED)                                          
      IF (DSML .GT. DBIG*RELERR) GOTO 17                                
         DNRM2 = DBIG                                                   
         GOTO  18                                                       
  17     DNRM2 = DBIG*DSQRT((DSML/DBIG)**2+1.0D0)                       
  18  RETURN                                                            
      END                                                               
      SUBROUTINE SROT(N, SX, INCX, SY, INCY, SC, SS)                    
      INTEGER N, INCX, INCY                                             
      REAL SX(INCX, 1), SY(INCY, 1), SC, SS                             
      INTEGER J                                                         
      REAL SW                                                           
C     APPLY  ( SC SS)  TO THE 2 BY N MATRIX (SX(1) ... SX(N))           
C            (-SS SC)                       (SY(1) ... SY(N))           
      IF (N .EQ. 0) RETURN                                              
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(17H  SROT - N .LT. 0, 17, 1, 2)         
C     IF (INCX .LE. 0) CALL SETERR(20H  SROT - INCX .LT. 0, 20, 1, 2)   
C     IF (INCY .LE. 0) CALL SETERR(20H  SROT - INCY .LT. 0, 20, 1, 2)   
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('  SROT - N .LT. 0', 17, 1, 2)          
      IF (INCX .LE. 0) CALL SETERR('  SROT - INCX .LT. 0', 20, 1, 2)    
      IF (INCY .LE. 0) CALL SETERR('  SROT - INCY .LT. 0', 20, 1, 2)    
C/                                                                      
      DO  1 J = 1, N                                                    
         SW = SC*SX(1, J)+SS*SY(1, J)                                   
         SY(1, J) = (-SS)*SX(1, J)+SC*SY(1, J)                          
         SX(1, J) = SW                                                  
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DROT(N, SX, INCX, SY, INCY, SC, SS)                    
      INTEGER N, INCX, INCY                                             
      DOUBLE PRECISION SX(INCX, 1), SY(INCY, 1), SC, SS                 
      INTEGER J                                                         
      DOUBLE PRECISION SW                                               
C     APPLY  ( SC SS)  TO THE 2 BY N MATRIX (SX(1) ... SX(N))           
C            (-SS SC)                       (SY(1) ... SY(N))           
      IF (N .EQ. 0) RETURN                                              
C/6S                                                                    
C     IF (N .LT. 0) CALL SETERR(17H  DROT - N .LT. 0, 17, 1, 2)         
C     IF (INCX .LE. 0) CALL SETERR(19H  DROT - INCX .LT.0, 20, 1, 2)    
C     IF (INCY .LE. 0) CALL SETERR(19H  DROT - INCY .LT.0, 20, 1, 2)    
C/7S                                                                    
      IF (N .LT. 0) CALL SETERR('  DROT - N .LT. 0', 17, 1, 2)          
      IF (INCX .LE. 0) CALL SETERR('  DROT - INCX .LT.0', 20, 1, 2)     
      IF (INCY .LE. 0) CALL SETERR('  DROT - INCY .LT.0', 20, 1, 2)     
C/                                                                      
      DO  1 J = 1, N                                                    
         SW = SC*SX(1, J)+SS*SY(1, J)                                   
         SY(1, J) = (-SS)*SX(1, J)+SC*SY(1, J)                          
         SX(1, J) = SW                                                  
   1     CONTINUE                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE  CSROT (N,CX,INCX,CY,INCY,C,S)                         
C                                                                       
C     APPLIES A PLANE ROTATION, WHERE THE COS AND SIN (C AND S) ARE REAL
C     AND THE VECTORS CX AND CY ARE COMPLEX.                            
C     JACK DONGARRA, LINPACK, 3/11/78.                                  
C                                                                       
C     ADAPTED TO PORT 3 BY PHYL FOX, 11/8/83                            
C                                                                       
      COMPLEX CX(1),CY(1),CTEMP                                         
      REAL C,S                                                          
      INTEGER I,INCX,INCY,IX,IY,N                                       
C                                                                       
      IF(N.EQ.0)RETURN                                                  
C/6S                                                                    
C     IF(N .LT. 0) CALL SETERR(12HCSROT-N.LT.0, 12, 1, 2)               
C     IF(INCX .EQ. 0) CALL SETERR(15HCSROT-INCX.EQ.0, 15, 1, 2)         
C     IF(INCY .EQ. 0) CALL SETERR(15HCSROT-INCY.EQ.0, 15, 1, 2)         
C/7S                                                                    
      IF(N .LT. 0) CALL SETERR('CSROT-N.LT.0', 12, 1, 2)                
      IF(INCX .EQ. 0) CALL SETERR('CSROT-INCX.EQ.0', 15, 1, 2)          
      IF(INCY .EQ. 0) CALL SETERR('CSROT-INCY.EQ.0', 15, 1, 2)          
C/                                                                      
C                                                                       
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20                               
C                                                                       
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL       
C         TO 1                                                          
C                                                                       
      IX = 1                                                            
      IY = 1                                                            
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 
      DO 10 I = 1,N                                                     
        CTEMP = C*CX(IX) + S*CY(IY)                                     
        CY(IY) = C*CY(IY) - S*CX(IX)                                    
        CX(IX) = CTEMP                                                  
        IX = IX + INCX                                                  
        IY = IY + INCY                                                  
   10 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C       CODE FOR BOTH INCREMENTS EQUAL TO 1                             
C                                                                       
   20 DO 30 I = 1,N                                                     
        CTEMP = C*CX(I) + S*CY(I)                                       
        CY(I) = C*CY(I) - S*CX(I)                                       
        CX(I) = CTEMP                                                   
   30 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE SROTG(SA, SB, SC, SS)                                  
      REAL SA, SB, SC, SS                                               
      REAL ONE, XR, YR, ZERO, ABS, SQRT                                 
      REAL SIGN                                                         
      DATA ZERO/0./                                                     
      DATA ONE/1./                                                      
C COMPUTE.. MATRIX ( SC SS) SO  ( SC SS)(SA) = (SQRT(SA**2+SB**2))      
C                  (-SS SC)     (-SS SC)(SB)   (   0             )      
C  SQRT(SA**2+SB**2) REPLACES SA IN STORAGE.                            
C  ZERO REPLACES SB                                                     
      IF (ABS(SA) .LE. ABS(SB)) GOTO 1                                  
         XR = SB/SA                                                     
         YR = SQRT(ONE+XR**2)                                           
         SC = SIGN(ONE/YR, SA)                                          
         SS = SC*XR                                                     
         SA = ABS(SA)*YR                                                
         GOTO  4                                                        
   1     IF (SB .EQ. ZERO) GOTO 2                                       
            XR = SA/SB                                                  
            YR = SQRT(ONE+XR**2)                                        
            SS = SIGN(ONE/YR, SB)                                       
            SC = SS*XR                                                  
            SA = ABS(SB)*YR                                             
            GOTO  3                                                     
   2        SC = ONE                                                    
            SS = ZERO                                                   
   3  CONTINUE                                                          
   4  SB = ZERO                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE DROTG(SA, SB, SC, SS)                                  
      DOUBLE PRECISION SA, SB, SC, SS                                   
      DOUBLE PRECISION ONE, XR, YR, ZERO, DSQRT                         
      DATA ZERO/0.0D0/                                                  
      DATA ONE/1.0D0/                                                   
C COMPUTE.. MATRIX ( SC SS) SO  ( SC SS)(SA) = (DSQRT(SA**2+SB**2))     
C                  (-SS SC)     (-SS SC)(SB)   (   0             )      
C  DSQRT(SA**2+SB**2) REPLACES SA IN STORAGE.                           
C  ZERO REPLACES SB                                                     
      IF (DABS(SA) .LE. DABS(SB)) GOTO 1                                
         XR = SB/SA                                                     
         YR = DSQRT(ONE+XR**2)                                          
         SC = DSIGN(ONE/YR, SA)                                         
         SS = SC*XR                                                     
         SA = DABS(SA)*YR                                               
         GOTO  4                                                        
   1     IF (SB .EQ. ZERO) GOTO 2                                       
            XR = SA/SB                                                  
            YR = DSQRT(ONE+XR**2)                                       
            SS = DSIGN(ONE/YR, SB)                                      
            SC = SS*XR                                                  
            SA = DABS(SB)*YR                                            
            GOTO  3                                                     
   2        SC = ONE                                                    
            SS = ZERO                                                   
   3  CONTINUE                                                          
   4  SB = ZERO                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE CROTG(CA,CB,C,S)                                       
C                                                                       
C     ADAPTED TO PORT 3 BY PHYL FOX, 11/8/83                            
C                                                                       
      COMPLEX CA,CB,S                                                   
      REAL C                                                            
      REAL NORM,SCALE                                                   
      COMPLEX ALPHA                                                     
C                                                                       
      IF (CABS(CA) .NE. 0.) GO TO 10                                    
      IF (CABS(CB) .EQ. 0.) GO TO  5                                    
         C = 0.                                                         
         S = (1.,0.)                                                    
         CA = CB                                                        
         GO TO 20                                                       
    5    C = 1.0                                                        
         S = (0.0,0.0)                                                  
         GO TO 20                                                       
   10 CONTINUE                                                          
         SCALE = CABS(CA) + CABS(CB)                                    
         NORM = SCALE * SQRT((CABS(CA/SCALE))**2 + (CABS(CB/SCALE))**2) 
         ALPHA = CA /CABS(CA)                                           
         C = CABS(CA) / NORM                                            
         S = ALPHA * CONJG(CB) / NORM                                   
         CA = ALPHA * NORM                                              
   20 CONTINUE                                                          
         CB = (0.0,0.0)                                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE SROT2(N,X,INCX,Y,INCY,A,B)                             
C                                                                       
C THIS SUBROUTINE IS SROT WITH THE OPTION OF GOING                      
C A VARIED AMOUNT IN INCREMENTS                                         
C                                                                       
        REAL X(1),Y(1)                                                  
        REAL A,B,W,V                                                    
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
       SUBROUTINE SSCAL(N,A,X,INCX)                                     
C                                                                       
C THIS SUBROUTINE SCALES EVERY INCXTH COMPONENT OF X                    
C BY A                                                                  
      REAL A,X(INCX,1)                                                  
      IF(N.EQ.0) RETURN                                                 
C/6S                                                                    
C     IF(N.LT.0) CALL SETERR(12HSSCAL-N.LT.0,12,1,2)                    
C     IF(INCX.LT.1)CALL SETERR(15HSSCAL-INCX.LT.1,15,2,2)               
C/7S                                                                    
      IF(N.LT.0) CALL SETERR('SSCAL-N.LT.0',12,1,2)                     
      IF(INCX.LT.1)CALL SETERR('SSCAL-INCX.LT.1',15,2,2)                
C/                                                                      
      DO 10 I=1,N                                                       
         X(1,I)=X(1,I)*A                                                
 10   CONTINUE                                                          
      RETURN                                                            
      END                                                               
       SUBROUTINE DSCAL(N,A,X,INCX)                                     
C                                                                       
C THIS SUBROUTINE SCALES EVERY INCXTH COMPONENT OF X                    
C BY A                                                                  
      DOUBLE PRECISION A,X(INCX,1)                                      
      IF(N.EQ.0) RETURN                                                 
C/6S                                                                    
C     IF(N.LT.0) CALL SETERR(12HDSCAL-N.LT.0,12,1,2)                    
C     IF(INCX.LT.1)CALL SETERR(15HDSCAL-INCX.LT.1,15,2,2)               
C/7S                                                                    
      IF(N.LT.0) CALL SETERR('DSCAL-N.LT.0',12,1,2)                     
      IF(INCX.LT.1)CALL SETERR('DSCAL-INCX.LT.1',15,2,2)                
C/                                                                      
      DO 10 I=1,N                                                       
         X(1,I)=X(1,I)*A                                                
 10   CONTINUE                                                          
      RETURN                                                            
      END                                                               
       SUBROUTINE CSCAL(N,A,X,INCX)                                     
C                                                                       
C THIS SUBROUTINE SCALES EVERY INCXTH COMPONENT OF X                    
C BY A                                                                  
      COMPLEX A,X(INCX,1)                                               
      IF(N.EQ.0) RETURN                                                 
C/6S                                                                    
C     IF(N.LT.0) CALL SETERR(12HCSCAL-N.LT.0,12,1,2)                    
C     IF(INCX.LT.1)CALL SETERR(15HCSCAL-INCX.LT.1,15,2,2)               
C/7S                                                                    
      IF(N.LT.0) CALL SETERR('CSCAL-N.LT.0',12,1,2)                     
      IF(INCX.LT.1)CALL SETERR('CSCAL-INCX.LT.1',15,2,2)                
C/                                                                      
      DO 10 I=1,N                                                       
         X(1,I)=X(1,I)*A                                                
 10   CONTINUE                                                          
      RETURN                                                            
      END                                                               
       SUBROUTINE CSSCAL(N,A,X,INCX)                                    
C                                                                       
C THIS SUBROUTINE SCALES EVERY INCXTH COMPONENT OF X                    
C BY A                                                                  
      COMPLEX X(INCX,1)                                                 
       REAL A                                                           
      IF(N.EQ.0) RETURN                                                 
C/6S                                                                    
C     IF(N.LT.0) CALL SETERR(13HCSSCAL-N.LT.0,13,1,2)                   
C     IF(INCX.LT.1)CALL SETERR(16HCSSCAL-INCX.LT.1,16,2,2)              
C/7S                                                                    
      IF(N.LT.0) CALL SETERR('CSSCAL-N.LT.0',13,1,2)                    
      IF(INCX.LT.1)CALL SETERR('CSSCAL-INCX.LT.1',16,2,2)               
C/                                                                      
      DO 10 I=1,N                                                       
         X(1,I)=X(1,I)*A                                                
 10   CONTINUE                                                          
      RETURN                                                            
      END                                                               
      FUNCTION SSUM(N,X,INCX)                                           
C                                                                       
C  PROGRAM TO FIND THE SUM OF N COMPONENTS OF A VECTOR.                 
C  THE PROGRAM GUARDS AGAINST OVERFLOW.                                 
C                                                                       
C  ADAPTED BY PHYLLIS FOX                                               
C  FROM W. STANLEY BROWNS ALGORITHM FOR FINDING THE MEAN                
C  AUGUST 23, 1982.                                                     
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C  N             - THE NUMBER OF ELEMENTS TO BE SUMMED                  
C                                                                       
C  X             - VECTOR OF LENGTH AT LEAST N                          
C                                                                       
C  INCX          - THE PROGRAM SUMS ONLY THE COMPONENTS SEPARATED BY    
C                  SPACING, INCX, I.E.                                  
C                  X(1) + X(1+INCX) + ... +X(1+(N-1)*INCX)              
C                                                                       
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE                                       
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C  1 - N .LT. 0                                                         
C                                                                       
C  2 - INCX .LT. 1                                                      
C                                                                       
C  3 - N IS TOO BIG (CANNOT GUARANTEE NO OVERFLOW)                      
C      (RECOVERABLE ERROR)                                              
C                                                                       
C  4 - THE RESULTANT SUM IS TOO BIG AND WOULD OVERFLOW                  
C      WHEN UNSCALED.                                                   
C      INFINITY (R1MACH(2)) WITH THE APPROPRIATE SIGN IS RETURNED.      
C      (RECOVERABLE ERROR)                                              
C                                                                       
C  NOTE - WHEN N IS ZERO, SSUM=0.0 IS RETURNED.                         
C                                                                       
      INTEGER J, K, N, INCX                                             
      REAL X(INCX,1), BASE, BIG, SMALL, EPS, EPSIN, BNDRY, FLTN, TEMP   
C                                                                       
      SSUM = 0.                                                         
      IF(N .EQ. 0 ) RETURN                                              
C/6S                                                                    
C     IF(N.LT.0) CALL SETERR(11HSSUM-N.LT.0, 11, 1, 2)                  
C     IF(INCX.LT.1) CALL SETERR (14HSSUM-INCX.LT.1, 14, 2, 2)           
C/7S                                                                    
      IF(N.LT.0) CALL SETERR('SSUM-N.LT.0', 11, 1, 2)                   
      IF(INCX.LT.1) CALL SETERR ('SSUM-INCX.LT.1', 14, 2, 2)            
C/                                                                      
C                                                                       
C  IF N IS NOT LESS THAN 1/EPS, OVERFLOW MAY OCCUR                      
C                                                                       
      EPS = R1MACH(4)                                                   
      EPSIN = 1.0/EPS                                                   
      FLTN = FLOAT(N)                                                   
      IF (FLTN .LT. EPSIN) GO TO 10                                     
C/6S                                                                    
C     CALL SETERR(14HSSUM-N TOO BIG, 14, 3, 1)                          
C/7S                                                                    
      CALL SETERR('SSUM-N TOO BIG', 14, 3, 1)                           
C/                                                                      
      RETURN                                                            
C                                                                       
C  NOW SUM ELEMENTS UNTIL ONE GREATER THAN BNDRY IS FOUND               
C                                                                       
C  BNDRY IS (BASE)*(SMALL)/(EPS**2)                                     
C                                                                       
 10   BASE = FLOAT(I1MACH(10))                                          
      SMALL = R1MACH(1)                                                 
      BIG = R1MACH(2)                                                   
C                                                                       
      BNDRY = BASE*SMALL*EPSIN*EPSIN                                    
C                                                                       
      DO 20 J=1,N                                                       
C                                                                       
C  IF A LARGE ONE IS FOUND, GO TO THE PROCEDURE FOR BIG ADDENDS.        
C                                                                       
      IF(ABS(X(1,J)) .GE. BNDRY) GO TO 30                               
      SSUM = SSUM + EPSIN*X(1,J)                                        
 20   CONTINUE                                                          
C                                                                       
C  IF NO LARGE ELEMENTS HAVE BEEN FOUND,                                
C  TAKE OUT THE EPS SCALING FACTOR FROM THE SUM, AND RETURN.            
C                                                                       
      SSUM = EPS*SSUM                                                   
      RETURN                                                            
C                                                                       
C  COMES HERE AFTER A LARGE ADDEND IS FOUND.                            
C  (AND STAYS IN THIS LOOP)                                             
C                                                                       
 30   SSUM = EPS*EPS*SSUM                                               
      DO 40 K=J,N                                                       
      SSUM = SSUM + EPS*X(1,K)                                          
 40   CONTINUE                                                          
C                                                                       
C  HERE SSUM IS EQUAL TO THE SUM SCALED BY EPS.                         
C                                                                       
C  CHECK FOR OVERFLOW ON RESCALING                                      
C                                                                       
      TEMP = EPS*BIG                                                    
      IF (SSUM .GE. TEMP .OR. SSUM .LE. (-TEMP)) GO TO 50               
C                                                                       
C  HERE ALL IS WELL                                                     
C                                                                       
      SSUM = SSUM*EPSIN                                                 
      RETURN                                                            
C                                                                       
C  COMES HERE TO A RECOVERABLE ERROR BECAUSE THE SUM WOULD OVERFLOW.    
C  RETURN THE VALUE BIG WITH THE CORRECT SIGN.                          
C                                                                       
 50   SSUM = SIGN(BIG, SSUM)                                            
C/6S                                                                    
C     CALL SETERR(                                                      
C    1  39HSSUM-SUM TOO BIG, +OR-INFINITY RETURNED, 39, 4, 1)           
C/7S                                                                    
      CALL SETERR(                                                      
     1  'SSUM-SUM TOO BIG, +OR-INFINITY RETURNED', 39, 4, 1)            
C/                                                                      
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DSUM(N,X,INCX)                          
C                                                                       
C  PROGRAM TO FIND THE SUM OF N COMPONENTS OF A VECTOR.                 
C  THE PROGRAM GUARDS AGAINST OVERFLOW.                                 
C                                                                       
C  ADAPTED BY PHYLLIS FOX                                               
C  FROM W. STANLEY BROWNS ALGORITHM FOR FINDING THE MEAN                
C  AUGUST 23, 1982.                                                     
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C  N             - THE NUMBER OF ELEMENTS TO BE SUMMED                  
C                                                                       
C  X             - VECTOR OF LENGTH AT LEAST N                          
C                                                                       
C  INCX          - THE PROGRAM SUMS ONLY THE COMPONENTS SEPARATED BY    
C                  SPACING, INCX, I.E.                                  
C                  X(1) + X(1+INCX) + ... +X(1+(N-1)*INCX)              
C                                                                       
C                                                                       
C  SCRATCH SPACE ALLOCATED - NONE                                       
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C  1 - N .LT. 0                                                         
C                                                                       
C  2 - INCX .LT. 1                                                      
C                                                                       
C  3 - N IS TOO BIG (CANNOT GUARANTEE NO OVERFLOW)                      
C      (RECOVERABLE ERROR)                                              
C                                                                       
C  4 - THE RESULTANT SUM IS TOO BIG AND WOULD OVERFLOW                  
C      WHEN UNSCALED.                                                   
C      INFINITY (D1MACH(2)) WITH THE APPROPRIATE SIGN IS RETURNED.      
C      (RECOVERABLE ERROR)                                              
C                                                                       
C  NOTE - WHEN N IS ZERO, DSUM=0.0D0 IS RETURNED.                       
C                                                                       
      INTEGER J, K, N, INCX                                             
      DOUBLE PRECISION X(INCX,1), BASE, BIG, SMALL, EPS                 
      DOUBLE PRECISION EPSIN, BNDRY, FLTN, TEMP, DFLOAT, D1MACH         
C                                                                       
      DSUM = 0.                                                         
      IF(N .EQ. 0 ) RETURN                                              
C/6S                                                                    
C     IF(N.LT.0) CALL SETERR(11HDSUM-N.LT.0, 11, 1, 2)                  
C     IF(INCX.LT.1) CALL SETERR (14HDSUM-INCX.LT.1, 14, 2, 2)           
C/7S                                                                    
      IF(N.LT.0) CALL SETERR('DSUM-N.LT.0', 11, 1, 2)                   
      IF(INCX.LT.1) CALL SETERR ('DSUM-INCX.LT.1', 14, 2, 2)            
C/                                                                      
C                                                                       
C  IF N IS NOT LESS THAN 1/EPS, OVERFLOW MAY OCCUR                      
C                                                                       
      EPS = D1MACH(4)                                                   
      EPSIN = 1.0D0/EPS                                                 
      FLTN = DFLOAT(N)                                                  
      IF (FLTN .LT. EPSIN) GO TO 10                                     
C/6S                                                                    
C     CALL SETERR(14HDSUM-N TOO BIG, 14, 3, 1)                          
C/7S                                                                    
      CALL SETERR('DSUM-N TOO BIG', 14, 3, 1)                           
C/                                                                      
      RETURN                                                            
C                                                                       
C  NOW SUM ELEMENTS UNTIL ONE GREATER THAN BNDRY IS FOUND               
C                                                                       
C  BNDRY IS (BASE)*(SMALL)/(EPS**2)                                     
C                                                                       
 10   BASE = FLOAT(I1MACH(10))                                          
      SMALL = D1MACH(1)                                                 
      BIG = D1MACH(2)                                                   
C                                                                       
      BNDRY = BASE*SMALL*EPSIN*EPSIN                                    
C                                                                       
      DO 20 J=1,N                                                       
C                                                                       
C  IF A LARGE ONE IS FOUND, GO TO THE PROCEDURE FOR BIG ADDENDS.        
C                                                                       
      IF(DABS(X(1,J)) .GE. BNDRY) GO TO 30                              
      DSUM = DSUM + EPSIN*X(1,J)                                        
 20   CONTINUE                                                          
C                                                                       
C  IF NO LARGE ELEMENTS HAVE BEEN FOUND,                                
C  TAKE OUT THE EPS SCALING FACTOR FROM THE SUM, AND RETURN.            
C                                                                       
      DSUM = EPS*DSUM                                                   
      RETURN                                                            
C                                                                       
C  COMES HERE AFTER A LARGE ADDEND IS FOUND.                            
C  (AND STAYS IN THIS LOOP)                                             
C                                                                       
 30   DSUM = EPS*EPS*DSUM                                               
      DO 40 K=J,N                                                       
      DSUM = DSUM + EPS*X(1,K)                                          
 40   CONTINUE                                                          
C                                                                       
C  HERE DSUM IS EQUAL TO THE SUM SCALED BY EPS.                         
C                                                                       
C  CHECK FOR OVERFLOW ON RESCALING                                      
C                                                                       
      TEMP = EPS*BIG                                                    
      IF (DSUM .GE. TEMP .OR. DSUM .LE. (-TEMP)) GO TO 50               
C                                                                       
C  HERE ALL IS WELL                                                     
C                                                                       
      DSUM = DSUM*EPSIN                                                 
      RETURN                                                            
C                                                                       
C  COMES HERE TO A RECOVERABLE ERROR BECAUSE THE SUM WOULD OVERFLOW.    
C  RETURN THE VALUE BIG WITH THE CORRECT SIGN.                          
C                                                                       
 50   DSUM = DSIGN(BIG, DSUM)                                           
C/6S                                                                    
C     CALL SETERR(                                                      
C    1  39HDSUM-SUM TOO BIG, +OR-INFINITY RETURNED, 39, 4, 1)           
C/7S                                                                    
      CALL SETERR(                                                      
     1  'DSUM-SUM TOO BIG, +OR-INFINITY RETURNED', 39, 4, 1)            
C/                                                                      
      RETURN                                                            
      END                                                               
        SUBROUTINE SSWAP(N,X,INCX,Y,INCY)                               
C                                                                       
C THIS SUBROUTINE SWAPS EVERY INCXTH AND INCYTH COMPONENT               
C OF X AND Y                                                            
       REAL X(INCX,1),Y(INCY,1),TEMP                                    
       IF(N.EQ.0) RETURN                                                
C/6S                                                                    
C      IF(N.LT.0) CALL SETERR(12HSSWAP-N.LT.0,12,1,2)                   
C      IF(INCX.LT.1) CALL SETERR(15HSSWAP-INCX.LT.1,15,2,2)             
C      IF(INCY.LT.1)CALL SETERR(15HSSWAP-INCY.LT.1,15,3,2)              
C/7S                                                                    
       IF(N.LT.0) CALL SETERR('SSWAP-N.LT.0',12,1,2)                    
       IF(INCX.LT.1) CALL SETERR('SSWAP-INCX.LT.1',15,2,2)              
       IF(INCY.LT.1)CALL SETERR('SSWAP-INCY.LT.1',15,3,2)               
C/                                                                      
       DO 10 I=1,N                                                      
          TEMP=X(1,I)                                                   
          X(1,I)=Y(1,I)                                                 
          Y(1,I)=TEMP                                                   
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
        SUBROUTINE DSWAP(N,X,INCX,Y,INCY)                               
C                                                                       
C THIS SUBROUTINE SWAPS EVERY INCXTH AND INCYTH COMPONENT               
C OF X AND Y                                                            
       DOUBLE PRECISION X(INCX,1),Y(INCY,1),TEMP                        
       IF(N.EQ.0) RETURN                                                
C/6S                                                                    
C      IF(N.LT.0) CALL SETERR(12HDSWAP-N.LT.0,12,1,2)                   
C      IF(INCX.LT.1) CALL SETERR(15HDSWAP-INCX.LT.1,15,2,2)             
C      IF(INCY.LT.1)CALL SETERR(15HDSWAP-INCY.LT.1,15,3,2)              
C/7S                                                                    
       IF(N.LT.0) CALL SETERR('DSWAP-N.LT.0',12,1,2)                    
       IF(INCX.LT.1) CALL SETERR('DSWAP-INCX.LT.1',15,2,2)              
       IF(INCY.LT.1)CALL SETERR('DSWAP-INCY.LT.1',15,3,2)               
C/                                                                      
       DO 10 I=1,N                                                      
          TEMP=X(1,I)                                                   
          X(1,I)=Y(1,I)                                                 
          Y(1,I)=TEMP                                                   
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
        SUBROUTINE CSWAP(N,X,INCX,Y,INCY)                               
C                                                                       
C THIS SUBROUTINE SWAPS EVERY INCXTH AND INCYTH COMPONENT               
C OF X AND Y                                                            
       COMPLEX X(INCX,1),Y(INCY,1),TEMP                                 
       IF(N.EQ.0) RETURN                                                
C/6S                                                                    
C      IF(N.LT.0) CALL SETERR(12HCSWAP-N.LT.0,12,1,2)                   
C      IF(INCX.LT.1) CALL SETERR(15HCSWAP-INCX.LT.1,15,2,2)             
C      IF(INCY.LT.1)CALL SETERR(15HCSWAP-INCY.LT.1,15,3,2)              
C/7S                                                                    
       IF(N.LT.0) CALL SETERR('CSWAP-N.LT.0',12,1,2)                    
       IF(INCX.LT.1) CALL SETERR('CSWAP-INCX.LT.1',15,2,2)              
       IF(INCY.LT.1)CALL SETERR('CSWAP-INCY.LT.1',15,3,2)               
C/                                                                      
       DO 10 I=1,N                                                      
          TEMP=X(1,I)                                                   
          X(1,I)=Y(1,I)                                                 
          Y(1,I)=TEMP                                                   
 10    CONTINUE                                                         
       RETURN                                                           
       END                                                              
       REAL FUNCTION CABS1(X)                                           
       COMPLEX X                                                        
       CABS1=ABS(REAL(X))+ABS(AIMAG(X))                                 
       RETURN                                                           
       END                                                              
C****END OF ROUTINES NEEDED FOR PORT 3 (PARTIAL) BLAS*******************
