      SUBROUTINE ENTER(IRNEW)                                           
C                                                                       
C  THIS ROUTINE SAVES                                                   
C                                                                       
C    1) THE CURRENT NUMBER OF OUTSTANDING STORAGE ALLOCATIONS, LOUT, AND
C    2) THE CURRENT RECOVERY LEVEL, LRECOV,                             
C                                                                       
C  IN AN ENTER-BLOCK IN THE STACK.                                      
C                                                                       
C  IT ALSO SETS LRECOV = IRNEW IF IRNEW = 1 OR 2.                       
C  IF IRNEW = 0, THEN THE RECOVERY LEVEL IS NOT ALTERED.                
C                                                                       
C  SCRATCH SPACE ALLOCATED - 3 INTEGER WORDS ARE LEFT ON THE STACK.     
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MUST HAVE IRNEW = 0, 1 OR 2.                                   
C                                                                       
      COMMON /CSTAK/DSTACK                                              
      DOUBLE PRECISION DSTACK(500)                                      
      INTEGER ISTACK(1000)                                              
      EQUIVALENCE (DSTACK(1),ISTACK(1))                                 
      EQUIVALENCE (ISTACK(1),LOUT)                                      
C                                                                       
C/6S                                                                    
C     IF (0.GT.IRNEW .OR. IRNEW.GT.2)                                   
C    1  CALL SETERR(35HENTER - MUST HAVE IRNEW = 0, 1 OR 2,35,1,2)      
C/7S                                                                    
      IF (0.GT.IRNEW .OR. IRNEW.GT.2)                                   
     1  CALL SETERR('ENTER - MUST HAVE IRNEW = 0, 1 OR 2',35,1,2)       
C/                                                                      
C                                                                       
C  ALLOCATE SPACE FOR SAVING THE ABOVE 2 ITEMS                          
C  AND A BACK-POINTER FOR CHAINING THE ENTER-BLOCKS TOGETHER.           
C                                                                       
      INOW=ISTKGT(3,2)                                                  
C                                                                       
C  SAVE THE CURRENT NUMBER OF OUTSTANDING ALLOCATIONS.                  
C                                                                       
      ISTACK(INOW)=LOUT                                                 
C                                                                       
C  SAVE THE CURRENT RECOVERY LEVEL.                                     
C                                                                       
      CALL ENTSRC(ISTACK(INOW+1),IRNEW)                                 
C                                                                       
C  SAVE A BACK-POINTER TO THE START OF THE PREVIOUS ENTER-BLOCK.        
C                                                                       
      ISTACK(INOW+2)=I8TSEL(INOW)                                       
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE LEAVE                                                  
C                                                                       
C  THIS ROUTINE                                                         
C                                                                       
C    1) DE-ALLOCATES ALL SCRATCH SPACE ALLOCATED SINCE THE LAST ENTER,  
C       INCLUDING THE LAST ENTER-BLOCK.                                 
C    2) RESTORES THE RECOVERY LEVEL TO ITS VALUE                        
C       AT THE TIME OF THE LAST CALL TO ENTER.                          
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - CANNOT LEAVE BEYOND THE FIRST ENTER.                           
C    2 - ISTACK(INOW) HAS BEEN OVERWRITTEN.                             
C    3 - TOO MANY ISTKRLS OR ISTACK(1 AND/OR INOW) CLOBBERED.           
C    4 - ISTACK(INOW+1) HAS BEEN OVERWRITTEN.                           
C    5 - ISTACK(INOW+2) HAS BEEN OVERWRITTEN.                           
C                                                                       
      COMMON /CSTAK/DSTACK                                              
      DOUBLE PRECISION DSTACK(500)                                      
      INTEGER ISTACK(1000)                                              
      EQUIVALENCE (DSTACK(1),ISTACK(1))                                 
      EQUIVALENCE (ISTACK(1),LOUT)                                      
C                                                                       
C  GET THE POINTER TO THE CURRENT ENTER-BLOCK.                          
C                                                                       
      INOW=I8TSEL(-1)                                                   
C                                                                       
C/6S                                                                    
C     IF (INOW.EQ.0)                                                    
C    1  CALL SETERR(43HLEAVE - CANNOT LEAVE BEYOND THE FIRST ENTER,43,  
C    2              1,2)                                                
C     IF (ISTACK(INOW).LT.1)                                            
C    1  CALL SETERR(41HLEAVE - ISTACK(INOW) HAS BEEN OVERWRITTEN,41,2,2)
C     IF (LOUT.LT.ISTACK(INOW)) CALL SETERR(                            
C    1  59HLEAVE - TOO MANY ISTKRLS OR ISTACK(1 AND/OR INOW) CLOBBERED, 
C    2  59,3,2)                                                         
C     IF (ISTACK(INOW+1).LT.1 .OR. ISTACK(INOW+1).GT.2)                 
C    1  CALL SETERR(43HLEAVE - ISTACK(INOW+1) HAS BEEN OVERWRITTEN,     
C    2              43,4,2)                                             
C     IF (ISTACK(INOW+2).GT.INOW-3 .OR. ISTACK(INOW+2).LT.0)            
C    1  CALL SETERR(43HLEAVE - ISTACK(INOW+2) HAS BEEN OVERWRITTEN,     
C    2              43,5,2)                                             
C/7S                                                                    
      IF (INOW.EQ.0)                                                    
     1  CALL SETERR('LEAVE - CANNOT LEAVE BEYOND THE FIRST ENTER',43,   
     2              1,2)                                                
      IF (ISTACK(INOW).LT.1)                                            
     1  CALL SETERR('LEAVE - ISTACK(INOW) HAS BEEN OVERWRITTEN',41,2,2) 
      IF (LOUT.LT.ISTACK(INOW)) CALL SETERR(                            
     1  'LEAVE - TOO MANY ISTKRLS OR ISTACK(1 AND/OR INOW) CLOBBERED',  
     2  59,3,2)                                                         
      IF (ISTACK(INOW+1).LT.1 .OR. ISTACK(INOW+1).GT.2)                 
     1  CALL SETERR('LEAVE - ISTACK(INOW+1) HAS BEEN OVERWRITTEN',      
     2              43,4,2)                                             
      IF (ISTACK(INOW+2).GT.INOW-3 .OR. ISTACK(INOW+2).LT.0)            
     1  CALL SETERR('LEAVE - ISTACK(INOW+2) HAS BEEN OVERWRITTEN',      
     2              43,5,2)                                             
C/                                                                      
C                                                                       
C  DE-ALLOCATE THE SCRATCH SPACE.                                       
C                                                                       
      CALL ISTKRL(LOUT-ISTACK(INOW)+1)                                  
C                                                                       
C  RESTORE THE RECOVERY LEVEL.                                          
C                                                                       
      CALL RETSRC(ISTACK(INOW+1))                                       
C                                                                       
C  LOWER THE BACK-POINTER.                                              
C                                                                       
      ITEMP=I8TSEL(ISTACK(INOW+2))                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE ENTSRC(IROLD,IRNEW)                                    
C                                                                       
C  THIS ROUTINE RETURNS IROLD = LRECOV AND SETS LRECOV = IRNEW.         
C                                                                       
C  IF THERE IS AN ACTIVE ERROR STATE, THE MESSAGE IS PRINTED            
C  AND EXECUTION STOPS.                                                 
C                                                                       
C  IRNEW = 0 LEAVES LRECOV UNCHANGED, WHILE                             
C  IRNEW = 1 GIVES RECOVERY AND                                         
C  IRNEW = 2 TURNS RECOVERY OFF.                                        
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - ILLEGAL VALUE OF IRNEW.                                        
C    2 - CALLED WHILE IN AN ERROR STATE.                                
C                                                                       
C/6S                                                                    
C     IF (IRNEW.LT.0 .OR. IRNEW.GT.2)                                   
C    1   CALL SETERR(31HENTSRC - ILLEGAL VALUE OF IRNEW,31,1,2)         
C/7S                                                                    
      IF (IRNEW.LT.0 .OR. IRNEW.GT.2)                                   
     1   CALL SETERR('ENTSRC - ILLEGAL VALUE OF IRNEW',31,1,2)          
C/                                                                      
C                                                                       
      IROLD=I8SAVE(2,IRNEW,IRNEW.NE.0)                                  
C                                                                       
C  IF HAVE AN ERROR STATE, STOP EXECUTION.                              
C                                                                       
C/6S                                                                    
C     IF (I8SAVE(1,0,.FALSE.) .NE. 0) CALL SETERR                       
C    1   (39HENTSRC - CALLED WHILE IN AN ERROR STATE,39,2,2)            
C/7S                                                                    
      IF (I8SAVE(1,0,.FALSE.) .NE. 0) CALL SETERR                       
     1   ('ENTSRC - CALLED WHILE IN AN ERROR STATE',39,2,2)             
C/                                                                      
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE RETSRC(IROLD)                                          
C                                                                       
C  THIS ROUTINE SETS LRECOV = IROLD.                                    
C                                                                       
C  IF THE CURRENT ERROR BECOMES UNRECOVERABLE,                          
C  THE MESSAGE IS PRINTED AND EXECUTION STOPS.                          
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - ILLEGAL VALUE OF IROLD.                                        
C                                                                       
C/6S                                                                    
C     IF (IROLD.LT.1 .OR. IROLD.GT.2)                                   
C    1  CALL SETERR(31HRETSRC - ILLEGAL VALUE OF IROLD,31,1,2)          
C/7S                                                                    
      IF (IROLD.LT.1 .OR. IROLD.GT.2)                                   
     1  CALL SETERR('RETSRC - ILLEGAL VALUE OF IROLD',31,1,2)           
C/                                                                      
C                                                                       
      ITEMP=I8SAVE(2,IROLD,.TRUE.)                                      
C                                                                       
C  IF THE CURRENT ERROR IS NOW UNRECOVERABLE, PRINT AND STOP.           
C                                                                       
      IF (IROLD.EQ.1 .OR. I8SAVE(1,0,.FALSE.).EQ.0) RETURN              
C                                                                       
        CALL EPRINT                                                     
        STOP                                                            
C                                                                       
      END                                                               
      INTEGER FUNCTION I8TSEL(INOW)                                     
C                                                                       
C  TO RETURN I8TSEL = THE POINTER TO THE CURRENT ENTER-BLOCK AND        
C  SET THE CURRENT POINTER TO INOW.                                     
C                                                                       
C  START WITH NO BACK-POINTER.                                          
C                                                                       
      DATA IENTER/0/                                                    
C                                                                       
      I8TSEL=IENTER                                                     
      IF (INOW.GE.0) IENTER=INOW                                        
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      INTEGER FUNCTION ISTKQU(ITYPE)                                    
C                                                                       
C  RETURNS THE NUMBER OF ITEMS OF TYPE ITYPE THAT REMAIN                
C  TO BE ALLOCATED IN ONE REQUEST.                                      
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - LNOW, LUSED, LMAX OR LBOOK OVERWRITTEN                         
C    2 - ITYPE .LE. 0 .OR. ITYPE .GE. 6                                 
C                                                                       
      COMMON /CSTAK/DSTAK                                               
C                                                                       
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER ISTAK(1000)                                               
      INTEGER ISIZE(5)                                                  
C                                                                       
      LOGICAL INIT                                                      
C                                                                       
      EQUIVALENCE (DSTAK(1),ISTAK(1))                                   
      EQUIVALENCE (ISTAK(2),LNOW)                                       
      EQUIVALENCE (ISTAK(3),LUSED)                                      
      EQUIVALENCE (ISTAK(4),LMAX)                                       
      EQUIVALENCE (ISTAK(5),LBOOK)                                      
      EQUIVALENCE (ISTAK(6),ISIZE(1))                                   
C                                                                       
      DATA INIT/.TRUE./                                                 
C                                                                       
      IF (INIT) CALL I0TK00(INIT,500,4)                                 
C                                                                       
C/6S                                                                    
C     IF (LNOW.LT.LBOOK.OR.LNOW.GT.LUSED.OR.LUSED.GT.LMAX) CALL SETERR  
C    1   (47HISTKQU - LNOW, LUSED, LMAX OR LBOOK OVERWRITTEN,           
C    2    47,1,2)                                                       
C/7S                                                                    
      IF (LNOW.LT.LBOOK.OR.LNOW.GT.LUSED.OR.LUSED.GT.LMAX) CALL SETERR  
     1   ('ISTKQU - LNOW, LUSED, LMAX OR LBOOK OVERWRITTEN',            
     2    47,1,2)                                                       
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (ITYPE.LE.0.OR.ITYPE.GE.6) CALL SETERR                         
C    1   (33HISTKQU - ITYPE.LE.0.OR.ITYPE.GE.6,33,2,2)                  
C/7S                                                                    
      IF (ITYPE.LE.0.OR.ITYPE.GE.6) CALL SETERR                         
     1   ('ISTKQU - ITYPE.LE.0.OR.ITYPE.GE.6',33,2,2)                   
C/                                                                      
C                                                                       
      ISTKQU = MAX0( ((LMAX-2)*ISIZE(2))/ISIZE(ITYPE)                   
     1             - (LNOW*ISIZE(2)-1)/ISIZE(ITYPE)                     
     2             - 1, 0 )                                             
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      INTEGER FUNCTION ISTKMD(NITEMS)                                   
C                                                                       
C  CHANGES THE LENGTH OF THE FRAME AT THE TOP OF THE STACK              
C  TO NITEMS.                                                           
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - LNOW OVERWRITTEN                                               
C    2 - ISTAK(LNOWO-1) OVERWRITTEN                                     
C                                                                       
      COMMON /CSTAK/DSTAK                                               
C                                                                       
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER ISTAK(1000)                                               
C                                                                       
      EQUIVALENCE (DSTAK(1),ISTAK(1))                                   
      EQUIVALENCE (ISTAK(2),LNOW)                                       
C                                                                       
      LNOWO = LNOW                                                      
      CALL ISTKRL(1)                                                    
C                                                                       
      ITYPE = ISTAK(LNOWO-1)                                            
C                                                                       
C/6S                                                                    
C     IF (ITYPE.LE.0.OR.ITYPE.GE.6) CALL SETERR                         
C    1   (35HISTKMD - ISTAK(LNOWO-1) OVERWRITTEN,35,1,2)                
C/7S                                                                    
      IF (ITYPE.LE.0.OR.ITYPE.GE.6) CALL SETERR                         
     1   ('ISTKMD - ISTAK(LNOWO-1) OVERWRITTEN',35,1,2)                 
C/                                                                      
C                                                                       
      ISTKMD = ISTKGT(NITEMS,ITYPE)                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE ISTKRL(NUMBER)                                         
C                                                                       
C  DE-ALLOCATES THE LAST (NUMBER) ALLOCATIONS MADE IN THE STACK         
C  BY ISTKGT.                                                           
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - NUMBER .LT. 0                                                  
C    2 - LNOW, LUSED, LMAX OR LBOOK OVERWRITTEN                         
C    3 - ATTEMPT TO DE-ALLOCATE NON-EXISTENT ALLOCATION                 
C    4 - THE POINTER AT ISTAK(LNOW) OVERWRITTEN                         
C                                                                       
      COMMON /CSTAK/DSTAK                                               
C                                                                       
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER ISTAK(1000)                                               
      LOGICAL INIT                                                      
C                                                                       
      EQUIVALENCE (DSTAK(1),ISTAK(1))                                   
      EQUIVALENCE (ISTAK(1),LOUT)                                       
      EQUIVALENCE (ISTAK(2),LNOW)                                       
      EQUIVALENCE (ISTAK(3),LUSED)                                      
      EQUIVALENCE (ISTAK(4),LMAX)                                       
      EQUIVALENCE (ISTAK(5),LBOOK)                                      
C                                                                       
      DATA INIT/.TRUE./                                                 
C                                                                       
      IF (INIT) CALL I0TK00(INIT,500,4)                                 
C                                                                       
C/6S                                                                    
C     IF (NUMBER.LT.0) CALL SETERR(20HISTKRL - NUMBER.LT.0,20,1,2)      
C/7S                                                                    
      IF (NUMBER.LT.0) CALL SETERR('ISTKRL - NUMBER.LT.0',20,1,2)       
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (LNOW.LT.LBOOK.OR.LNOW.GT.LUSED.OR.LUSED.GT.LMAX) CALL SETERR  
C    1   (47HISTKRL - LNOW, LUSED, LMAX OR LBOOK OVERWRITTEN,           
C    2    47,2,2)                                                       
C/7S                                                                    
      IF (LNOW.LT.LBOOK.OR.LNOW.GT.LUSED.OR.LUSED.GT.LMAX) CALL SETERR  
     1   ('ISTKRL - LNOW, LUSED, LMAX OR LBOOK OVERWRITTEN',            
     2    47,2,2)                                                       
C/                                                                      
C                                                                       
      IN = NUMBER                                                       
 10      IF (IN.EQ.0) RETURN                                            
C                                                                       
C/6S                                                                    
C        IF (LNOW.LE.LBOOK) CALL SETERR                                 
C    1   (55HISTKRL - ATTEMPT TO DE-ALLOCATE NON-EXISTENT ALLOCATION,   
C    2    55,3,2)                                                       
C/7S                                                                    
         IF (LNOW.LE.LBOOK) CALL SETERR                                 
     1   ('ISTKRL - ATTEMPT TO DE-ALLOCATE NON-EXISTENT ALLOCATION',    
     2    55,3,2)                                                       
C/                                                                      
C                                                                       
C     CHECK TO MAKE SURE THE BACK POINTERS ARE MONOTONE.                
C                                                                       
C/6S                                                                    
C        IF (ISTAK(LNOW).LT.LBOOK.OR.ISTAK(LNOW).GE.LNOW-1) CALL SETERR 
C    1   (47HISTKRL - THE POINTER AT ISTAK(LNOW) OVERWRITTEN,           
C    2    47,4,2)                                                       
C/7S                                                                    
         IF (ISTAK(LNOW).LT.LBOOK.OR.ISTAK(LNOW).GE.LNOW-1) CALL SETERR 
     1   ('ISTKRL - THE POINTER AT ISTAK(LNOW) OVERWRITTEN',            
     2    47,4,2)                                                       
C/                                                                      
C                                                                       
         LOUT = LOUT-1                                                  
         LNOW = ISTAK(LNOW)                                             
         IN = IN-1                                                      
         GO TO 10                                                       
C                                                                       
      END                                                               
      INTEGER FUNCTION ISTKGT(NITEMS,ITYPE)                             
C                                                                       
C  ALLOCATES SPACE OUT OF THE INTEGER ARRAY ISTAK (IN COMMON            
C  BLOCK CSTAK) FOR AN ARRAY OF LENGTH NITEMS AND OF TYPE               
C  DETERMINED BY ITYPE AS FOLLOWS                                       
C                                                                       
C    1 - LOGICAL                                                        
C    2 - INTEGER                                                        
C    3 - REAL                                                           
C    4 - DOUBLE PRECISION                                               
C    5 - COMPLEX                                                        
C                                                                       
C  ON RETURN, THE ARRAY WILL OCCUPY                                     
C                                                                       
C    STAK(ISTKGT), STAK(ISTKGT+1), ..., STAK(ISTKGT-NITEMS+1)           
C                                                                       
C  WHERE STAK IS AN ARRAY OF TYPE ITYPE EQUIVALENCED TO ISTAK.          
C                                                                       
C  (FOR THOSE WANTING TO MAKE MACHINE DEPENDENT MODIFICATIONS           
C  TO SUPPORT OTHER TYPES, CODES 6,7,8,9,10,11 AND 12 HAVE              
C  BEEN RESERVED FOR 1/4 LOGICAL, 1/2 LOGICAL, 1/4 INTEGER,             
C  1/2 INTEGER, QUAD PRECISION, DOUBLE COMPLEX AND QUAD                 
C  COMPLEX, RESPECTIVELY.)                                              
C                                                                       
C  THE ALLOCATOR RESERVES THE FIRST TEN INTEGER WORDS OF THE STACK      
C  FOR ITS OWN INTERNAL BOOK-KEEPING. THESE ARE INITIALIZED BY          
C  THE INITIALIZING SUBPROGRAM I0TK00 UPON THE FIRST CALL               
C  TO A SUBPROGRAM IN THE ALLOCATION PACKAGE.                           
C                                                                       
C  THE USE OF THE FIRST FIVE WORDS IS DESCRIBED BELOW.                  
C                                                                       
C    ISTAK( 1) - LOUT,  THE NUMBER OF CURRENT ALLOCATIONS.              
C    ISTAK( 2) - LNOW,  THE CURRENT ACTIVE LENGTH OF THE STACK.         
C    ISTAK( 3) - LUSED, THE MAXIMUM VALUE OF ISTAK(2) ACHIEVED.         
C    ISTAK( 4) - LMAX,  THE MAXIMUM LENGTH THE STACK.                   
C    ISTAK( 5) - LBOOK, THE NUMBER OF WORDS USED FOR BOOKEEPING.        
C                                                                       
C  THE NEXT FIVE WORDS CONTAIN INTEGERS DESCRIBING THE AMOUNT           
C  OF STORAGE ALLOCATED BY THE FORTRAN SYSTEM TO THE VARIOUS            
C  DATA TYPES.  THE UNIT OF MEASUREMENT IS ARBITRARY AND MAY            
C  BE WORDS, BYTES OR BITS OR WHATEVER IS CONVENIENT.  THE              
C  VALUES CURRENTLY ASSUMED CORRESPOND TO AN ANS FORTRAN                
C  ENVIRONMENT.  FOR SOME MINI-COMPUTER SYSTEMS THE VALUES MAY          
C  HAVE TO BE CHANGED (SEE I0TK00).                                     
C                                                                       
C    ISTAK( 6) - THE NUMBER OF UNITS ALLOCATED TO LOGICAL               
C    ISTAK( 7) - THE NUMBER OF UNITS ALLOCATED TO INTEGER               
C    ISTAK( 8) - THE NUMBER OF UNITS ALLOCATED TO REAL                  
C    ISTAK( 9) - THE NUMBER OF UNITS ALLOCATED TO DOUBLE PRECISION      
C    ISTAK(10) - THE NUMBER OF UNITS ALLOCATED TO COMPLEX               
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - NITEMS .LT. 0                                                  
C    2 - ITYPE .LE. 0 .OR. ITYPE .GE. 6                                 
C    3 - LNOW, LUSED, LMAX OR LBOOK OVERWRITTEN                         
C    4 - STACK OVERFLOW                                                 
C                                                                       
      COMMON /CSTAK/DSTAK                                               
C                                                                       
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER ISTAK(1000)                                               
      INTEGER ISIZE(5)                                                  
C                                                                       
      LOGICAL INIT                                                      
C                                                                       
      EQUIVALENCE (DSTAK(1),ISTAK(1))                                   
      EQUIVALENCE (ISTAK(1),LOUT)                                       
      EQUIVALENCE (ISTAK(2),LNOW)                                       
      EQUIVALENCE (ISTAK(3),LUSED)                                      
      EQUIVALENCE (ISTAK(4),LMAX)                                       
      EQUIVALENCE (ISTAK(5),LBOOK)                                      
      EQUIVALENCE (ISTAK(6),ISIZE(1))                                   
C                                                                       
      DATA INIT/.TRUE./                                                 
C                                                                       
      IF (INIT) CALL I0TK00(INIT,500,4)                                 
C                                                                       
C/6S                                                                    
C     IF (NITEMS.LT.0) CALL SETERR(20HISTKGT - NITEMS.LT.0,20,1,2)      
C/7S                                                                    
      IF (NITEMS.LT.0) CALL SETERR('ISTKGT - NITEMS.LT.0',20,1,2)       
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (ITYPE.LE.0 .OR. ITYPE.GE.6) CALL SETERR                       
C    1   (33HISTKGT - ITYPE.LE.0.OR.ITYPE.GE.6,33,2,2)                  
C/7S                                                                    
      IF (ITYPE.LE.0 .OR. ITYPE.GE.6) CALL SETERR                       
     1   ('ISTKGT - ITYPE.LE.0.OR.ITYPE.GE.6',33,2,2)                   
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (LNOW.LT.LBOOK.OR.LNOW.GT.LUSED.OR.LUSED.GT.LMAX) CALL SETERR  
C    1   (47HISTKGT - LNOW, LUSED, LMAX OR LBOOK OVERWRITTEN,           
C    2    47,3,2)                                                       
C/7S                                                                    
      IF (LNOW.LT.LBOOK.OR.LNOW.GT.LUSED.OR.LUSED.GT.LMAX) CALL SETERR  
     1   ('ISTKGT - LNOW, LUSED, LMAX OR LBOOK OVERWRITTEN',            
     2    47,3,2)                                                       
C/                                                                      
C                                                                       
      ISTKGT = (LNOW*ISIZE(2)-1)/ISIZE(ITYPE) + 2                       
      I = ( (ISTKGT-1+NITEMS)*ISIZE(ITYPE) - 1 )/ISIZE(2) + 3           
C                                                                       
C  STACK OVERFLOW IS AN UNRECOVERABLE ERROR.                            
C                                                                       
C/6S                                                                    
C     IF (I.GT.LMAX) CALL SETERR(69HISTKGT - STACK TOO SHORT. ENLARGE IT
C    1 AND CALL ISTKIN IN MAIN PROGRAM.,69,4,2)                         
C/7S                                                                    
      IF (I.GT.LMAX) CALL SETERR('ISTKGT - STACK TOO SHORT. ENLARGE IT A
     *ND CALL ISTKIN IN MAIN PROGRAM.',69,4,2)                          
C/                                                                      
C                                                                       
C  ISTAK(I-1) CONTAINS THE TYPE FOR THIS ALLOCATION.                    
C  ISTAK(I  ) CONTAINS A POINTER TO THE END OF THE PREVIOUS             
C             ALLOCATION.                                               
C                                                                       
      ISTAK(I-1) = ITYPE                                                
      ISTAK(I  ) = LNOW                                                 
      LOUT = LOUT+1                                                     
      LNOW = I                                                          
      LUSED = MAX0(LUSED,LNOW)                                          
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE ISTKIN(NITEMS,ITYPE)                                   
C                                                                       
C  INITIALIZES THE STACK ALLOCATOR, SETTING THE LENGTH OF THE STACK.    
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - NITEMS .LE. 0                                                  
C    2 - ITYPE .LE. 0 .OR. ITYPE .GE. 6                                 
C                                                                       
      LOGICAL INIT                                                      
C                                                                       
      DATA INIT/.TRUE./                                                 
C                                                                       
C/6S                                                                    
C     IF (NITEMS.LE.0) CALL SETERR(20HISTKIN - NITEMS.LE.0,20,1,2)      
C/7S                                                                    
      IF (NITEMS.LE.0) CALL SETERR('ISTKIN - NITEMS.LE.0',20,1,2)       
C/                                                                      
C                                                                       
C/6S                                                                    
C     IF (ITYPE.LE.0.OR.ITYPE.GE.6) CALL SETERR                         
C    1   (33HISTKIN - ITYPE.LE.0.OR.ITYPE.GE.6,33,2,2)                  
C/7S                                                                    
      IF (ITYPE.LE.0.OR.ITYPE.GE.6) CALL SETERR                         
     1   ('ISTKIN - ITYPE.LE.0.OR.ITYPE.GE.6',33,2,2)                   
C/                                                                      
C                                                                       
      IF (INIT) CALL I0TK00(INIT,NITEMS,ITYPE)                          
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      INTEGER FUNCTION ISTKST(NFACT)                                    
C                                                                       
C  RETURNS CONTROL INFORMATION AS FOLLOWS                               
C                                                                       
C  NFACT    ITEM RETURNED                                               
C                                                                       
C    1         LOUT,  THE NUMBER OF CURRENT ALLOCATIONS                 
C    2         LNOW,  THE CURRENT ACTIVE LENGTH                         
C    3         LUSED, THE MAXIMUM USED                                  
C    4         LMAX,  THE MAXIMUM ALLOWED                               
C                                                                       
      COMMON /CSTAK/DSTAK                                               
C                                                                       
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER ISTAK(1000)                                               
      INTEGER ISTATS(4)                                                 
      LOGICAL INIT                                                      
C                                                                       
      EQUIVALENCE (DSTAK(1),ISTAK(1))                                   
      EQUIVALENCE (ISTAK(1),ISTATS(1))                                  
C                                                                       
      DATA INIT/.TRUE./                                                 
C                                                                       
      IF (INIT) CALL I0TK00(INIT,500,4)                                 
C                                                                       
C/6S                                                                    
C     IF (NFACT.LE.0.OR.NFACT.GE.5) CALL SETERR                         
C    1   (33HISTKST - NFACT.LE.0.OR.NFACT.GE.5,33,1,2)                  
C/7S                                                                    
      IF (NFACT.LE.0.OR.NFACT.GE.5) CALL SETERR                         
     1   ('ISTKST - NFACT.LE.0.OR.NFACT.GE.5',33,1,2)                   
C/                                                                      
C                                                                       
      ISTKST = ISTATS(NFACT)                                            
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE N5ERR(MESSG, NMESSG, NERR, IOPT)                       
      INTEGER NMESSG, NERR, IOPT                                        
C/6S                                                                    
C     INTEGER MESSG(1)                                                  
C/7S                                                                    
      CHARACTER*1 MESSG(NMESSG)                                         
C/                                                                      
C  N5ERR IS A PROCEDURE USED TO REDEFINE AN ERROR STATE.                
      CALL ERROFF                                                       
      CALL SETERR(MESSG, NMESSG, NERR, IOPT)                            
      RETURN                                                            
      END                                                               
      INTEGER FUNCTION NERROR(NERR)                                     
C                                                                       
C  RETURNS NERROR = NERR = THE VALUE OF THE ERROR FLAG LERROR.          
C                                                                       
      NERROR=I8SAVE(1,0,.FALSE.)                                        
      NERR=NERROR                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE ERROFF                                                 
C                                                                       
C  TURNS OFF THE ERROR STATE OFF BY SETTING LERROR=0.                   
C                                                                       
      I=I8SAVE(1,0,.TRUE.)                                              
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE SETERR(MESSG,NMESSG,NERR,IOPT)                         
C                                                                       
C  SETERR SETS LERROR = NERR, OPTIONALLY PRINTS THE MESSAGE AND DUMPS   
C  ACCORDING TO THE FOLLOWING RULES...                                  
C                                                                       
C    IF IOPT = 1 AND RECOVERING      - JUST REMEMBER THE ERROR.         
C    IF IOPT = 1 AND NOT RECOVERING  - PRINT AND STOP.                  
C    IF IOPT = 2                     - PRINT, DUMP AND STOP.            
C                                                                       
C  INPUT                                                                
C                                                                       
C    MESSG  - THE ERROR MESSAGE.                                        
C    NMESSG - THE LENGTH OF THE MESSAGE, IN CHARACTERS.                 
C    NERR   - THE ERROR NUMBER. MUST HAVE NERR NON-ZERO.                
C    IOPT   - THE OPTION. MUST HAVE IOPT=1 OR 2.                        
C                                                                       
C  ERROR STATES -                                                       
C                                                                       
C    1 - MESSAGE LENGTH NOT POSITIVE.                                   
C    2 - CANNOT HAVE NERR=0.                                            
C    3 - AN UNRECOVERED ERROR FOLLOWED BY ANOTHER ERROR.                
C    4 - BAD VALUE FOR IOPT.                                            
C                                                                       
C  ONLY THE FIRST 72 CHARACTERS OF THE MESSAGE ARE PRINTED.             
C                                                                       
C  THE ERROR HANDLER CALLS A SUBROUTINE NAMED SDUMP TO PRODUCE A        
C  SYMBOLIC DUMP.                                                       
C                                                                       
C/6S                                                                    
C     INTEGER MESSG(1)                                                  
C/7S                                                                    
      CHARACTER*1 MESSG(NMESSG)                                         
C/                                                                      
C                                                                       
C  THE UNIT FOR ERROR MESSAGES.                                         
C                                                                       
      IWUNIT=I1MACH(4)                                                  
C                                                                       
      IF (NMESSG.GE.1) GO TO 10                                         
C                                                                       
C  A MESSAGE OF NON-POSITIVE LENGTH IS FATAL.                           
C                                                                       
        WRITE(IWUNIT,9000)                                              
 9000   FORMAT(52H1ERROR    1 IN SETERR - MESSAGE LENGTH NOT POSITIVE.) 
        GO TO 60                                                        
C                                                                       
C  NW IS THE NUMBER OF WORDS THE MESSAGE OCCUPIES.                      
C  (I1MACH(6) IS THE NUMBER OF CHARACTERS PER WORD.)                    
C                                                                       
 10   NW=(MIN0(NMESSG,72)-1)/I1MACH(6)+1                                
C                                                                       
      IF (NERR.NE.0) GO TO 20                                           
C                                                                       
C  CANNOT TURN THE ERROR STATE OFF USING SETERR.                        
C  (I8SAVE SETS A FATAL ERROR HERE.)                                    
C                                                                       
        WRITE(IWUNIT,9001)                                              
 9001   FORMAT(42H1ERROR    2 IN SETERR - CANNOT HAVE NERR=0//          
     1         34H THE CURRENT ERROR MESSAGE FOLLOWS///)                
        CALL E9RINT(MESSG,NW,NERR,.TRUE.)                               
        ITEMP=I8SAVE(1,1,.TRUE.)                                        
        GO TO 50                                                        
C                                                                       
C  SET LERROR AND TEST FOR A PREVIOUS UNRECOVERED ERROR.                
C                                                                       
 20   IF (I8SAVE(1,NERR,.TRUE.).EQ.0) GO TO 30                          
C                                                                       
        WRITE(IWUNIT,9002)                                              
 9002   FORMAT(23H1ERROR    3 IN SETERR -,                              
     1         48H AN UNRECOVERED ERROR FOLLOWED BY ANOTHER ERROR.//    
     2         48H THE PREVIOUS AND CURRENT ERROR MESSAGES FOLLOW.///)  
        CALL EPRINT                                                     
        CALL E9RINT(MESSG,NW,NERR,.TRUE.)                               
        GO TO 50                                                        
C                                                                       
C  SAVE THIS MESSAGE IN CASE IT IS NOT RECOVERED FROM PROPERLY.         
C                                                                       
 30   CALL E9RINT(MESSG,NW,NERR,.TRUE.)                                 
C                                                                       
      IF (IOPT.EQ.1 .OR. IOPT.EQ.2) GO TO 40                            
C                                                                       
C  MUST HAVE IOPT = 1 OR 2.                                             
C                                                                       
        WRITE(IWUNIT,9003)                                              
 9003   FORMAT(42H1ERROR    4 IN SETERR - BAD VALUE FOR IOPT//          
     1         34H THE CURRENT ERROR MESSAGE FOLLOWS///)                
        GO TO 50                                                        
C                                                                       
C  IF THE ERROR IS FATAL, PRINT, DUMP, AND STOP                         
C                                                                       
 40   IF (IOPT.EQ.2) GO TO 50                                           
C                                                                       
C  HERE THE ERROR IS RECOVERABLE                                        
C                                                                       
C  IF THE RECOVERY MODE IS IN EFFECT, OK, JUST RETURN                   
C                                                                       
      IF (I8SAVE(2,0,.FALSE.).EQ.1) RETURN                              
C                                                                       
C  OTHERWISE PRINT AND STOP                                             
C                                                                       
      CALL EPRINT                                                       
      STOP                                                              
C                                                                       
 50   CALL EPRINT                                                       
 60   CALL SDUMP                                                        
      STOP                                                              
C                                                                       
      END                                                               
      SUBROUTINE SDUMP                                                  
C   THIS IS THE STANDARD DUMP ROUTINE FOR THE PORT LIBRARY.             
C   FIRST IT PROVIDES A FORMATTED DUMP OF THE PORT STACK.               
C   THEN IT CALLS THE LOCAL (PREFERABLY SYMBOLIC) DUMP ROUTINE.         
      CALL STKDMP                                                       
      CALL FDUMP                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE STKDMP                                                 
C                                                                       
C  THIS PROCEDURE PROVIDES A DUMP OF THE PORT STACK.                    
C                                                                       
C  WRITTEN BY D. D. WARNER.                                             
C                                                                       
C  MOSTLY REWRITTEN BY P. A. FOX, OCTOBER 13, 1982                      
C  AND COMMENTS ADDED.                                                  
C                                                                       
C  ALLOCATED REGIONS OF THE STACK ARE PRINTED OUT IN THE APPROPRIATE    
C  FORMAT, EXCEPT IF THE STACK APPEARS TO HAVE BEEN OVERWRITTEN.        
C  IF OVERWRITE SEEMS TO HAVE HAPPENED, THE ENTIRE STACK IS PRINTED OUT 
C  IN UNSTRUCTURED FORM, ONCE FOR EACH OF THE POSSIBLE                  
C  (LOGICAL, INTEGER, REAL, DOUBLE PRECISION, OR COMPLEX) FORMATS.      
C                                                                       
      COMMON /CSTAK/ DSTAK                                              
      DOUBLE PRECISION DSTAK(500)                                       
      REAL RSTAK(1000)                                                  
C/R                                                                     
C     REAL CMSTAK(2,500)                                                
C/C                                                                     
      COMPLEX CMSTAK(500)                                               
C/                                                                      
      INTEGER ISTAK(1000)                                               
      LOGICAL LSTAK(1000)                                               
C                                                                       
      INTEGER LOUT, LNOW, LUSED, LMAX, LBOOK                            
      INTEGER LLOUT, BPNTR                                              
      INTEGER IPTR, ERROUT, MCOL, NITEMS                                
      INTEGER WR, DR, WD, DD, WI                                        
      INTEGER LNG(5), ISIZE(5)                                          
      INTEGER I, LNEXT, ITYPE, I1MACH                                   
C                                                                       
      LOGICAL INIT, TRBL1, TRBL2                                        
C                                                                       
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), LSTAK(1))                                  
      EQUIVALENCE (DSTAK(1), RSTAK(1))                                  
C/R                                                                     
C     EQUIVALENCE (DSTAK(1), CMSTAK(1,1))                               
C/C                                                                     
      EQUIVALENCE (DSTAK(1), CMSTAK(1))                                 
C/                                                                      
      EQUIVALENCE (ISTAK(1), LOUT)                                      
      EQUIVALENCE (ISTAK(2), LNOW)                                      
      EQUIVALENCE (ISTAK(3), LUSED)                                     
      EQUIVALENCE (ISTAK(4), LMAX)                                      
      EQUIVALENCE (ISTAK(5), LBOOK)                                     
      EQUIVALENCE (ISTAK(6), ISIZE(1))                                  
C                                                                       
      DATA MCOL/132/                                                    
      DATA INIT/.TRUE./                                                 
C                                                                       
C  I0TK00 CHECKS TO SEE IF THE FIRST TEN, BOOKKEEPING, LOCATIONS OF     
C  THE STACK HAVE BEEN INITIALIZED (AND DOES IT, IF NEEDED).            
C                                                                       
      IF (INIT) CALL I0TK00(INIT, 500, 4)                               
C                                                                       
C                                                                       
C  I1MACH(4) IS THE STANDARD ERROR MESSAGE WRITE UNIT.                  
C                                                                       
      ERROUT = I1MACH(4)                                                
      WRITE (ERROUT,  9901)                                             
 9901   FORMAT (11H1STACK DUMP)                                         
C                                                                       
C                                                                       
C  FIND THE MACHINE-DEPENDENT FORMATS FOR PRINTING - BUT ADD 1 TO       
C  THE WIDTH TO GET SEPARATION BETWEEN ITEMS, AND SUBTRACT 1 FROM       
C  THE NUMBER OF DIGITS AFTER THE DECIMAL POINT TO ALLOW FOR THE        
C  1P IN THE DUMP FORMAT OF 1PEW.D                                      
C                                                                       
C  (NOTE, THAT ALTHOUGH IT IS NOT NECESSARY, 2 HAS BEEN ADDED TO        
C   THE INTEGER WIDTH, WI, TO CONFORM WITH DAN WARNERS PREVIOUS         
C   USAGE - SO PEOPLE CAN COMPARE DUMPS WITH ONES THEY HAVE HAD         
C   AROUND FOR A LONG TIME.)                                            
C                                                                       
       CALL FRMATR(WR,DR)                                               
       CALL FRMATD(WD,DD)                                               
       CALL FRMATI(WI)                                                  
C                                                                       
       WR = WR+1                                                        
       WD = WD+1                                                        
       WI = WI+2                                                        
       DR = DR-1                                                        
       DD = DD-1                                                        
C                                                                       
C  CHECK, IN VARIOUS WAYS, THE BOOKKEEPING PART OF THE STACK TO SEE     
C  IF THINGS WERE OVERWRITTEN.                                          
C                                                                       
C  LOUT  IS THE NUMBER OF CURRENT ALLOCATIONS                           
C  LNOW  IS THE CURRENT ACTIVE LENGTH OF THE STACK                      
C  LUSED IS THE MAXIMUM VALUE OF LNOW ACHIEVED                          
C  LMAX  IS THE MAXIMUM LENGTH OF THE STACK                             
C  LBOOK IS THE NUMBER OF WORDS USED FOR BOOK-KEEPING                   
C                                                                       
      TRBL1 = LBOOK .NE. 10                                             
      IF (.NOT. TRBL1) TRBL1 = LMAX .LT. 12                             
      IF (.NOT. TRBL1) TRBL1 = LMAX .LT. LUSED                          
      IF (.NOT. TRBL1) TRBL1 = LUSED .LT. LNOW                          
      IF (.NOT. TRBL1) TRBL1 = LNOW .LT. LBOOK                          
      IF (.NOT. TRBL1) TRBL1 = LOUT .LT. 0                              
      IF (.NOT. TRBL1) GO TO 10                                         
C                                                                       
         WRITE (ERROUT,  9902)                                          
 9902      FORMAT (29H0STACK HEADING IS OVERWRITTEN)                    
         WRITE (ERROUT,  9903)                                          
 9903      FORMAT (47H UNSTRUCTURED DUMP OF THE DEFAULT STACK FOLLOWS)  
C                                                                       
C  SINCE INFORMATION IS LOST, SIMPLY SET THE USUAL DEFAULT VALUES FOR   
C  THE LENGTH OF THE ENTIRE STACK IN TERMS OF EACH (LOGICAL, INTEGER,   
C  ETC.,) TYPE.                                                         
C                                                                       
      LNG(1) = 1000                                                     
      LNG(2) = 1000                                                     
      LNG(3) = 1000                                                     
      LNG(4) = 500                                                      
      LNG(5) = 500                                                      
C                                                                       
C                                                                       
         CALL U9DMP(LNG, MCOL, WI, WR, DR, WD, DD)                      
         GO TO  110                                                     
C                                                                       
C  WRITE OUT THE STORAGE UNITS USED BY EACH TYPE OF VARIABLE            
C                                                                       
   10    WRITE (ERROUT,  9904)                                          
 9904      FORMAT (19H0STORAGE PARAMETERS)                              
         WRITE (ERROUT,  9905) ISIZE(1)                                 
 9905      FORMAT (18H LOGICAL          , I7, 14H STORAGE UNITS)        
         WRITE (ERROUT,  9906) ISIZE(2)                                 
 9906      FORMAT (18H INTEGER          , I7, 14H STORAGE UNITS)        
         WRITE (ERROUT,  9907) ISIZE(3)                                 
 9907      FORMAT (18H REAL             , I7, 14H STORAGE UNITS)        
         WRITE (ERROUT,  9908) ISIZE(4)                                 
 9908      FORMAT (18H DOUBLE PRECISION , I7, 14H STORAGE UNITS)        
         WRITE (ERROUT,  9909) ISIZE(5)                                 
 9909      FORMAT (18H COMPLEX          , I7, 14H STORAGE UNITS)        
C                                                                       
C  WRITE OUT THE CURRENT STACK STATISTICS (I.E. USAGE)                  
C                                                                       
         WRITE (ERROUT,  9910)                                          
 9910      FORMAT (17H0STACK STATISTICS)                                
         WRITE (ERROUT,  9911) LMAX                                     
 9911      FORMAT (23H STACK SIZE            , I7)                      
         WRITE (ERROUT,  9912) LUSED                                    
 9912      FORMAT (23H MAXIMUM STACK USED    , I7)                      
         WRITE (ERROUT,  9913) LNOW                                     
 9913      FORMAT (23H CURRENT STACK USED    , I7)                      
         WRITE (ERROUT,  9914) LOUT                                     
 9914      FORMAT (23H NUMBER OF ALLOCATIONS , I7)                      
C                                                                       
C  HERE AT LEAST THE BOOKKEEPING PART OF THE STACK HAS NOT BEEN         
C  OVERWRITTEN.                                                         
C                                                                       
C  STACKDUMP WORKS BACKWARDS FROM THE END (MOST RECENT ALLOCATION) OF   
C  THE STACK, PRINTING INFORMATION, BUT ALWAYS CHECKING TO SEE IF       
C  THE POINTERS FOR AN ALLOCATION HAVE BEEN OVERWRITTEN.                
C                                                                       
C  LLOUT COUNTS THE NUMBER OF ALLOCATIONS STILL LEFT TO PRINT           
C  SO LLOUT IS INITIALLY LOUT OR ISTAK(1).                              
C                                                                       
C  THE STACK ALLOCATION ROUTINE PUTS, AT THE END OF EACH ALLOCATION,    
C  TWO EXTRA SPACES - ONE FOR THE TYPE OF THE ALLOCATION AND THE NEXT   
C  TO HOLD A BACK POINTER TO THE PREVIOUS ALLOCATION.                   
C  THE BACK POINTER IS THEREFORE INITIALLY LOCATED AT THE INITIAL END,  
C  LNOW, OF THE STACK.                                                  
C  CALL THIS LOCATION BPNTR.                                            
C                                                                       
          LLOUT = LOUT                                                  
          BPNTR = LNOW                                                  
C                                                                       
C  IF WE ARE DONE, THE BACK POINTER POINTS BACK INTO THE BOOKKEEPING    
C  PART OF THE STACK.                                                   
C                                                                       
C  IF WE ARE NOT DONE, OBTAIN THE NEXT REGION TO PRINT AND GET ITS TYPE.
C                                                                       
   20    IF (BPNTR .LE. LBOOK) GO TO  110                               
C                                                                       
            LNEXT = ISTAK(BPNTR)                                        
            ITYPE = ISTAK(BPNTR-1)                                      
C                                                                       
C  SEE IF ANY OF THESE NEW DATA ARE INCONSISTENT - WHICH WOULD SIGNAL   
C  AN OVERWRITE.                                                        
C                                                                       
            TRBL2 = LNEXT .LT. LBOOK                                    
            IF (.NOT. TRBL2) TRBL2 = BPNTR .LE. LNEXT                   
            IF (.NOT. TRBL2) TRBL2 = ITYPE .LT. 0                       
            IF (.NOT. TRBL2) TRBL2 = 5 .LT. ITYPE                       
            IF (.NOT. TRBL2) GO TO 40                                   
C                                                                       
C  HERE THERE SEEMS TO HAVE BEEN A PARTIAL OVERWRITE.                   
C  COMPUTE THE LENGTH OF THE ENTIRE STACK IN TERMS OF THE VALUES GIVEN  
C  IN THE BOOKKEEPING PART OF THE STACK (WHICH, AT LEAST, SEEMS NOT TO  
C  HAVE BEEN OVERWRITTEN), AND DO AN UNFORMATTED DUMP, AND RETURN.      
C                                                                       
               WRITE (ERROUT,  9915)                                    
 9915            FORMAT (28H0STACK PARTIALLY OVERWRITTEN)               
               WRITE (ERROUT,  9916)                                    
 9916          FORMAT (45H UNSTRUCTURED DUMP OF REMAINING STACK FOLLOWS)
C                                                                       
         DO  30 I = 1, 5                                                
            LNG(I) = (BPNTR*ISIZE(2)-1)/ISIZE(I)+1                      
   30    CONTINUE                                                       
C                                                                       
               CALL U9DMP(LNG, MCOL, WI, WR, DR, WD, DD)                
               GO TO  110                                               
C                                                                       
C                                                                       
C  COMES HERE EACH TIME TO PRINT NEXT (BACK) ALLOCATION.                
C                                                                       
C  AT THIS POINT BPNTR POINTS TO THE END OF THE ALLOCATION ABOUT TO     
C  BE PRINTED, LNEXT = ISTAK(BPNTR) POINTS BACK TO THE END OF THE       
C  PREVIOUS ALLOCATION, AND ITYPE = ISTAK(BPNTR-1) GIVES THE TYPE OF    
C  THE ALLOCATION ABOUT TO BE PRINTED.                                  
C                                                                       
C  THE PRINTING ROUTINES NEED TO KNOW THE START OF THE ALLOCATION AND   
C  THE NUMBER OF ITEMS.                                                 
C  THESE ARE COMPUTED FROM THE EQUATIONS USED WHEN THE FUNCTION ISTKGT  
C  COMPUTED THE ORIGINAL ALLOCATION - THE POINTER TO THE                
C  START OF THE ALLOCATION WAS COMPUTED BY ISTKGT FROM THE (THEN)       
C  END OF THE PREVIOUS ALLOCATION VIA THE FORMULA,                      
C                                                                       
C           ISTKGT = (LNOW*ISIZE(2)-1)/ISIZE(ITYPE) + 2                 
C                                                                       
   40       IPTR   = (LNEXT*ISIZE(2)-1)/ISIZE(ITYPE) + 2                
C                                                                       
C  THE FUNCTION ISTKGT THEN FOUND NEW END OF THE STACK, LNOW, FROM THE  
C  FORMULA                                                              
C                                                                       
C          I = ( (ISTKGT-1+NITEMS)*ISIZE(ITYPE) - 1 )/ISIZE(2) + 3      
C                                                                       
C  HERE WE SOLVE THIS FOR NITEMS TO DETERMINE THE NUMBER OF LOCATIONS   
C  IN THIS ALLOCATION.                                                  
C                                                                       
            NITEMS = 1-IPTR + ((BPNTR-3)*ISIZE(2)+1)/ISIZE(ITYPE)       
C                                                                       
C                                                                       
C  USE THE TYPE (INTEGER, REAL, ETC.) TO DTERMINE WHICH PRINTING        
C  ROUTINE TO USE.                                                      
C                                                                       
               IF (ITYPE .EQ. 1) GO TO  50                              
               IF (ITYPE .EQ. 2) GO TO  60                              
               IF (ITYPE .EQ. 3) GO TO  70                              
               IF (ITYPE .EQ. 4) GO TO  80                              
               IF (ITYPE .EQ. 5) GO TO  90                              
C                                                                       
   50          WRITE (ERROUT,  9917) LLOUT, IPTR                        
 9917            FORMAT (13H0ALLOCATION =, I7, 20H,          POINTER =, 
     1            I7, 23H,          TYPE LOGICAL)                       
               CALL A9RNTL(LSTAK(IPTR), NITEMS, ERROUT, MCOL)           
               GO TO  100                                               
C                                                                       
   60          WRITE (ERROUT,  9918) LLOUT, IPTR                        
 9918            FORMAT (13H0ALLOCATION =, I7, 20H,          POINTER =, 
     1            I7, 23H,          TYPE INTEGER)                       
               CALL A9RNTI(ISTAK(IPTR), NITEMS, ERROUT, MCOL, WI)       
               GO TO  100                                               
C                                                                       
   70          WRITE (ERROUT,  9919) LLOUT, IPTR                        
 9919            FORMAT (13H0ALLOCATION =, I7, 20H,          POINTER =, 
     1            I7, 20H,          TYPE REAL)                          
               CALL A9RNTR(RSTAK(IPTR), NITEMS, ERROUT, MCOL, WR, DR)   
               GO TO  100                                               
C                                                                       
   80          WRITE (ERROUT,  9920) LLOUT, IPTR                        
 9920            FORMAT (13H0ALLOCATION =, I7, 20H,          POINTER =, 
     1            I7, 32H,          TYPE DOUBLE PRECISION)              
               CALL A9RNTD(DSTAK(IPTR), NITEMS, ERROUT, MCOL, WD, DD)   
               GO TO  100                                               
C                                                                       
   90          WRITE (ERROUT,  9921) LLOUT, IPTR                        
 9921            FORMAT (13H0ALLOCATION =, I7, 20H,          POINTER =, 
     1            I7, 23H,          TYPE COMPLEX)                       
C/R                                                                     
C              CALL A9RNTC(CMSTAK(1,IPTR), NITEMS, ERROUT, MCOL, WR,DR) 
C/C                                                                     
               CALL A9RNTC(CMSTAK(IPTR), NITEMS, ERROUT, MCOL, WR, DR)  
C/                                                                      
C                                                                       
 100        BPNTR = LNEXT                                               
            LLOUT = LLOUT-1                                             
            GO TO 20                                                    
C                                                                       
  110  WRITE (ERROUT,  9922)                                            
 9922   FORMAT (18H0END OF STACK DUMP)                                  
      RETURN                                                            
      END                                                               
      SUBROUTINE U9DMP(LNG, NCOL, WI, WR, DR, WD, DD)                   
C                                                                       
C  THIS SUBROUTINE ASSUMES THAT THE TYPE (INTEGER, ETC.) OF THE DATA    
C  IN THE PORT STACK IS NOT KNOWN - SO IT PRINTS OUT, IN ALL FORMATS    
C  THE STACK CONTENTS, USING THE ARRAY OUTPUT ROUTINES APRNTX.          
C                                                                       
C  WRITTEN BY DAN WARNER, REVISED BY PHYL FOX, NOVEMBER 8, 1982.        
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C    LNG      - AN INTEGER VECTOR ARRAY CONTAINING IN                   
C               LNG(1) THE LENGTH OF THE ARRAY IF LOGICAL               
C               LNG(2) THE LENGTH OF THE ARRAY IF INTEGER               
C               LNG(3) THE LENGTH OF THE ARRAY IF REAL                  
C               LNG(4) THE LENGTH OF THE ARRAY IF DOUBLE PRECISION      
C               LNG(5) THE LENGTH OF THE ARRAY IF COMPLEX               
C                                                                       
C    NCOL     - THE NUMBER OF SPACES ACROSS A PRINTED LINE              
C                                                                       
C    WI       - THE FORMAT WIDTH FOR AN INTEGER                         
C                                                                       
C    WR       - THE FORMAT WIDTH FOR A REAL (W IN 1PEW.D)               
C                                                                       
C    DR       - THE NUMBER OF DIGITS AFTER THE DECIMAL POINT            
C               (THE D IN THE 1PEW.D FORMULA)                           
C                                                                       
C    WD       - THE FORMAT WIDTH FOR A REAL (W IN 1PDW.D)               
C                                                                       
C    DD       - THE NUMBER OF DIGITS AFTER THE DECIMAL POINT            
C               (THE D IN THE 1PDW.D FORMULA)                           
C                                                                       
C                                                                       
C  ERROR STATES - NONE.  U9DMP IS CALLED BY SETERR,                     
C  SO IT CANNOT CALL SETERR.                                            
C                                                                       
C                                                                       
      INTEGER LNG(5), NCOL, WI, WR, DR, WD                              
      INTEGER DD                                                        
      COMMON /CSTAK/ DSTAK                                              
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER ERROUT, ISTAK(1000), I1MACH                               
      REAL RSTAK(1000)                                                  
      LOGICAL LSTAK(1000)                                               
C/R                                                                     
C     REAL CMSTAK(2,500)                                                
C     EQUIVALENCE (DSTAK(1), CMSTAK(1,1))                               
C/C                                                                     
      COMPLEX CMSTAK(500)                                               
      EQUIVALENCE (DSTAK(1), CMSTAK(1))                                 
C/                                                                      
      EQUIVALENCE (DSTAK(1), ISTAK(1))                                  
      EQUIVALENCE (DSTAK(1), LSTAK(1))                                  
      EQUIVALENCE (DSTAK(1), RSTAK(1))                                  
C                                                                       
      ERROUT = I1MACH(4)                                                
C                                                                       
      WRITE (ERROUT,  1)                                                
   1  FORMAT (14H0LOGICAL STACK)                                        
      CALL A9RNTL(LSTAK, LNG(1), ERROUT, NCOL)                          
      WRITE (ERROUT,  2)                                                
   2  FORMAT (14H0INTEGER STACK)                                        
      CALL A9RNTI(ISTAK, LNG(2), ERROUT, NCOL, WI)                      
      WRITE (ERROUT,  3)                                                
   3  FORMAT (11H0REAL STACK)                                           
      CALL A9RNTR(RSTAK, LNG(3), ERROUT, NCOL, WR, DR)                  
      WRITE (ERROUT,  4)                                                
   4  FORMAT (23H0DOUBLE PRECISION STACK)                               
      CALL A9RNTD(DSTAK, LNG(4), ERROUT, NCOL, WD, DD)                  
      WRITE (ERROUT,  5)                                                
   5  FORMAT (14H0COMPLEX STACK)                                        
      CALL A9RNTC(CMSTAK, LNG(5), ERROUT, NCOL, WR, DR)                 
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE A9RNTC(A, NITEMS, IOUT, MCOL, W, D)                    
C                                                                       
C  THIS IS THE DOCUMENTED ROUTINE APRNTC, BUT WITHOUT THE CALLS TO      
C  SETERR- BECAUSE IT IS CALLED BY SETERR.                              
C                                                                       
C  THIS SUBROUTINE PRINTS OUT NITEMS FROM THE COMPLEX ARRAY, A, ON      
C  OUTPUT UNIT IOUT, USING A MAXIMUM OF MCOL PRINT SPACES.              
C  THE OUTPUT FORMAT IS 2(1PEW.D).                                      
C  THE PROGRAM PUTS AS MANY VALUES ON A LINE AS POSSIBLE.               
C  W SHOULD BE INPUT AS THE ACTUAL WIDTH +1 FOR A SPACE BETWEEN VALUES. 
C                                                                       
C  DUPLICATE LINES ARE NOT ALL PRINTED, BUT ARE INDICATED BY ASTERISKS. 
C                                                                       
C  WRITTEN BY DAN WARNER, REVISED BY PHYL FOX, OCTOBER 21, 1982.        
C                                                                       
C  THE LINE WIDTH IS COMPUTED AS THE MINIMUM OF THE INPUT MCOL AND 160. 
C  IF THE LINE WIDTH IS TO BE INCREASED ABOVE 160, THE BUFFERS LINE()   
C  AND LAST(), WHICH THE VALUES TO BE PRINTED ON ONE LINE, MUST         
C  BE DIMENSIONED ACCORDINGLY.                                          
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C    A        - THE START OF THE COMPLEX ARRAY TO BE PRINTED            
C                                                                       
C    NITEMS   - THE NUMBER OF ITEMS TO BE PRINTED                       
C                                                                       
C    IOUT     - THE OUTPUT UNIT FOR PRINTING                            
C                                                                       
C    MCOL     - THE NUMBER OF SPACES ACROSS THE LINE                    
C                                                                       
C    W        - THE WIDTH OF THE PRINTED VALUE (1PEW.D)                 
C                                                                       
C    D        - THE NUMBER OF DIGITS AFTER THE DECIMAL POINT (1PEW.D)   
C                                                                       
C                                                                       
C  ERROR STATES - NONE.  LOWER LEVEL ROUTINE CALLED BY                  
C  SETERR, SO IT CANNOT CALL SETERR.                                    
C                                                                       
      INTEGER  NITEMS, IOUT, MCOL, W, D                                 
C/R                                                                     
C     REAL A(2,NITEMS)                                                  
C/C                                                                     
      COMPLEX  A(NITEMS)                                                
C/                                                                      
C                                                                       
      INTEGER  MAX0, MIN0, WW, DD, EMIN, EMAX,                          
     1         EXPENT, I1MACH, ICEIL, IABS, I10WID                      
C/6S                                                                    
C     INTEGER  IFMT1(20), IFMT2(18), BLANK, STAR                        
C     INTEGER IFMT1C(20), IFMT2C(18)                                    
C     EQUIVALENCE (IFMT1(1),IFMT1C(1)), (IFMT2(1),IFMT2C(1))            
C/7S                                                                    
      CHARACTER*1  IFMT1(20), IFMT2(18), BLANK, STAR                    
      CHARACTER*20 IFMT1C                                               
      CHARACTER*18 IFMT2C                                               
      EQUIVALENCE (IFMT1(1),IFMT1C), (IFMT2(1),IFMT2C)                  
C/                                                                      
      INTEGER  INDW, NCOL, COUNT, I, J, K, ILINE, ILAST                 
      LOGICAL  DUP                                                      
C/R                                                                     
C     REAL LINE(2,18), LAST(2,18)                                       
C/C                                                                     
      COMPLEX  LINE(18), LAST(18)                                       
C/                                                                      
      REAL  LOGETA                                                      
C                                                                       
C/6S                                                                    
C     DATA BLANK/1H /, STAR/1H*/, INDW/7/, EXPENT/0/                    
C/7S                                                                    
      DATA BLANK/' '/, STAR/'*'/, INDW/7/, EXPENT/0/                    
C/                                                                      
C                                                                       
C  IFMT1 IS FOR THE ASTERISK LINES- IFMT2 FOR THE DATA LINES            
C                                                                       
C/6S                                                                    
C     DATA IFMT1( 1) /1H(/,  IFMT2( 1) /1H(/                            
C     DATA IFMT1( 2) /1H1/,  IFMT2( 2) /1H1/                            
C     DATA IFMT1( 3) /1HA/,  IFMT2( 3) /1HA/                            
C     DATA IFMT1( 4) /1H1/,  IFMT2( 4) /1H1/                            
C     DATA IFMT1( 5) /1H,/,  IFMT2( 5) /1H,/                            
C     DATA IFMT1( 6) /1H5/,  IFMT2( 6) /1HI/                            
C     DATA IFMT1( 7) /1HX/,  IFMT2( 7) /1H7/                            
C     DATA IFMT1( 8) /1H,/,  IFMT2( 8) /1H,/                            
C     DATA IFMT1( 9) /1H2/,  IFMT2( 9) /1H1/                            
C     DATA IFMT1(10) /1HA/,  IFMT2(10) /1HP/                            
C     DATA IFMT1(11) /1H1/,  IFMT2(11) /1H /                            
C     DATA IFMT1(12) /1H,/,  IFMT2(12) /1HE/                            
C     DATA IFMT1(13) /1H /,  IFMT2(13) /1H /                            
C     DATA IFMT1(14) /1H /,  IFMT2(14) /1H /                            
C     DATA IFMT1(15) /1HX/,  IFMT2(15) /1H./                            
C     DATA IFMT1(16) /1H,/,  IFMT2(16) /1H /                            
C     DATA IFMT1(17) /1H2/,  IFMT2(17) /1H /                            
C     DATA IFMT1(18) /1HA/,  IFMT2(18) /1H)/                            
C     DATA IFMT1(19) /1H1/                                              
C     DATA IFMT1(20) /1H)/                                              
C/7S                                                                    
      DATA IFMT1( 1) /'('/,  IFMT2( 1) /'('/                            
      DATA IFMT1( 2) /'1'/,  IFMT2( 2) /'1'/                            
      DATA IFMT1( 3) /'A'/,  IFMT2( 3) /'A'/                            
      DATA IFMT1( 4) /'1'/,  IFMT2( 4) /'1'/                            
      DATA IFMT1( 5) /','/,  IFMT2( 5) /','/                            
      DATA IFMT1( 6) /'5'/,  IFMT2( 6) /'I'/                            
      DATA IFMT1( 7) /'X'/,  IFMT2( 7) /'7'/                            
      DATA IFMT1( 8) /','/,  IFMT2( 8) /','/                            
      DATA IFMT1( 9) /'2'/,  IFMT2( 9) /'1'/                            
      DATA IFMT1(10) /'A'/,  IFMT2(10) /'P'/                            
      DATA IFMT1(11) /'1'/,  IFMT2(11) /' '/                            
      DATA IFMT1(12) /','/,  IFMT2(12) /'E'/                            
      DATA IFMT1(13) /' '/,  IFMT2(13) /' '/                            
      DATA IFMT1(14) /' '/,  IFMT2(14) /' '/                            
      DATA IFMT1(15) /'X'/,  IFMT2(15) /'.'/                            
      DATA IFMT1(16) /','/,  IFMT2(16) /' '/                            
      DATA IFMT1(17) /'2'/,  IFMT2(17) /' '/                            
      DATA IFMT1(18) /'A'/,  IFMT2(18) /')'/                            
      DATA IFMT1(19) /'1'/                                              
      DATA IFMT1(20) /')'/                                              
C/                                                                      
C                                                                       
C     EXPENT IS USED AS A FIRST-TIME SWITCH TO SIGNAL IF THE            
C     MACHINE-VALUE CONSTANTS HAVE BEEN COMPUTED.                       
C                                                                       
      IF (EXPENT .GT. 0) GO TO 10                                       
         LOGETA = ALOG10(FLOAT(I1MACH(10)))                             
         EMIN   = ICEIL(LOGETA*FLOAT(IABS(I1MACH(12)-1)))               
         EMAX   = ICEIL(LOGETA*FLOAT(I1MACH(13)))                       
         EXPENT = I10WID(MAX0(EMIN, EMAX))                              
C                                                                       
C     COMPUTE THE FORMATS.                                              
C                                                                       
   10 WW = MIN0(99, MAX0(W, 5+EXPENT))                                  
      CALL S88FMT(2, WW, IFMT2(13))                                     
      DD = MIN0(D, (WW-(5+EXPENT)))                                     
      CALL S88FMT(2, DD, IFMT2(16))                                     
C                                                                       
C  NCOL IS THE NUMBER OF VALUES TO BE PRINTED ACROSS THE LINE.          
C                                                                       
      NCOL = MAX0(1, MIN0(9, (MIN0(MCOL,160)-INDW)/(2*WW)))             
      CALL S88FMT(1, (2*NCOL), IFMT2(11))                               
      WW = WW-2                                                         
C                                                                       
C  THE ASTERISKS ARE POSITIONED RIGHT-ADJUSTED IN THE W-WIDTH SPACE.    
      CALL S88FMT(2, WW, IFMT1(13))                                     
C                                                                       
C  I COUNTS THE NUMBER OF ITEMS TO BE PRINTED,                          
C  J COUNTS THE NUMBER ON A GIVEN LINE,                                 
C  COUNT COUNTS THE NUMBER OF DUPLICATE LINES.                          
C                                                                       
      I = 1                                                             
      J = 0                                                             
      COUNT = 0                                                         
C                                                                       
C  THE LOGICAL OF THE FOLLOWING IS ROUGHLY THIS -                       
C  IF THERE ARE STILL MORE ITEMS TO BE PRINTED, A LINE-                 
C  FULL IS PUT INTO THE ARRAY, LINE.                                    
C  WHENEVER A LINE IS PRINTED OUT, IT IS ALSO STUFFED INTO              
C  THE ARRAY, LAST, TO COMPARE WITH THE NEXT ONE COMING IN              
C  TO CHECK FOR REPEAT OR DUPLICATED LINES.                             
C  ALSO WHENEVER A LINE IS WRITTEN OUT, THE DUPLICATION                 
C  COUNTER, COUNT, IS SET TO ONE.                                       
C  THE ONLY MILDLY TRICKY PART IS TO NOTE THAT COUNT HAS TO             
C  GO TO 3 BEFORE A LINE OF ASTERISKS IS PRINTED BECAUSE                
C  OF COURSE NO SUCH LINE IS PRINTED FOR JUST A PAIR OF                 
C  DUPLICATE LINES.                                                     
C                                                                       
C  ILINE IS PRINTED AS THE INDEX OF THE FIRST ARRAY ELEMENT             
C  IN A LINE.                                                           
C                                                                       
C                                                                       
   20 IF (I .GT. NITEMS)  GO TO 90                                      
        J = J+1                                                         
C/R                                                                     
C       LINE(1,J) = A(1,I)                                              
C       LINE(2,J) = A(2,I)                                              
C/C                                                                     
        LINE(J) = A(I)                                                  
C/                                                                      
        IF (J .EQ. 1) ILINE = I                                         
        IF (J .LT. NCOL .AND. I .LT. NITEMS) GO TO 80                   
          IF (COUNT .EQ. 0) GO TO 50                                    
            DUP = .TRUE.                                                
            DO 30 K=1,NCOL                                              
C/R                                                                     
C             IF (LAST(1,K) .NE. LINE(1,K)  .OR.                        
C    1            LAST(2,K) .NE. LINE(2,K))                             
C    2            DUP = .FALSE.                                         
C/C                                                                     
              IF (REAL(LAST(K)) .NE. REAL(LINE(K))  .OR.                
     1            AIMAG(LAST(K)) .NE. AIMAG(LINE(K)))                   
     2            DUP = .FALSE.                                         
C/                                                                      
   30       CONTINUE                                                    
            IF (I .EQ. NITEMS  .AND.  J .LT. NCOL) DUP = .FALSE.        
            IF (.NOT. DUP .AND. COUNT .EQ. 1) GO TO 50                  
              IF (.NOT. DUP) GO TO 40                                   
                COUNT = COUNT+1                                         
                IF (COUNT .EQ. 3) WRITE(IOUT, IFMT1C) BLANK,            
     1                                 STAR, STAR, STAR, STAR           
                IF (I .EQ. NITEMS)  GO TO 50                            
                  GO TO 70                                              
C/R                                                                     
C  40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(1,K),             
C    1              LAST(2,K), K=1,NCOL)                                
C  50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(1,K),                 
C    1              LINE(2,K), K=1,J)                                   
C/C                                                                     
   40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(K), K=1,NCOL)     
   50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(K), K=1,J)            
C/                                                                      
          COUNT = 1                                                     
          DO 60 K=1,NCOL                                                
C/R                                                                     
C           LAST(1,K) = LINE(1,K)                                       
C  60       LAST(2,K) = LINE(2,K)                                       
C/C                                                                     
   60       LAST(K) = LINE(K)                                           
C/                                                                      
   70     ILAST = ILINE                                                 
          J = 0                                                         
   80   I = I+1                                                         
        GO TO 20                                                        
   90 RETURN                                                            
      END                                                               
      SUBROUTINE A9RNTD(A, NITEMS, IOUT, MCOL, W, D)                    
C                                                                       
C  THIS IS THE DOCUMENTED ROUTINE APRNTD, BUT WITHOUT THE CALLS TO      
C  SETERR - BECAUSE IT IS CALLED BY SETERR.                             
C                                                                       
C  THIS SUBROUTINE PRINTS OUT NITEMS FROM THE DOUBLE PRECISION ARRAY,   
C  A, ON OUTPUT UNIT IOUT, USING A MAXIMUM OF MCOL PRINT SPACES.        
C  THE OUTPUT FORMAT IS 1PDW.D.                                         
C  THE PROGRAM PUTS AS MANY VALUES ON A LINE AS POSSIBLE.               
C  W SHOULD BE INPUT AS THE ACTUAL WIDTH +1 FOR A SPACE BETWEEN VALUES. 
C                                                                       
C  DUPLICATE LINES ARE NOT ALL PRINTED, BUT ARE INDICATED BY ASTERISKS. 
C                                                                       
C  WRITTEN BY DAN WARNER, REVISED BY PHYL FOX, OCTOBER 21, 1982.        
C                                                                       
C  THE LINE WIDTH IS COMPUTED AS THE MINIMUM OF THE INPUT MCOL AND 160. 
C  IF THE LINE WIDTH IS TO BE INCREASED ABOVE 160, THE BUFFERS LINE()   
C  AND LAST(), WHICH THE VALUES TO BE PRINTED ON ONE LINE, MUST         
C  BE DIMENSIONED ACCORDINGLY.                                          
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C    A        - THE START OF THE DOUBLE PRECISION ARRAY TO BE PRINTED   
C                                                                       
C    NITEMS   - THE NUMBER OF ITEMS TO BE PRINTED                       
C                                                                       
C    IOUT     - THE OUTPUT UNIT FOR PRINTING                            
C                                                                       
C    MCOL     - THE NUMBER OF SPACES ACROSS THE LINE                    
C                                                                       
C    W        - THE WIDTH OF THE PRINTED VALUE (1PDW.D)                 
C                                                                       
C    D        - THE NUMBER OF DIGITS AFTER THE DECIMAL POINT (1PDW.D)   
C                                                                       
C                                                                       
C  ERROR STATES - NONE.  LOWER LEVEL ROUTINE CALLED BY                  
C  SETERR, SO IT CANNOT CALL SETERR.                                    
C                                                                       
      INTEGER  NITEMS, IOUT, MCOL, W, D                                 
      DOUBLE PRECISION  A(NITEMS)                                       
C                                                                       
      INTEGER  MAX0, MIN0, WW, DD, EMIN, EMAX,                          
     1         EXPENT, I1MACH, ICEIL, IABS, I10WID                      
C/6S                                                                    
C     INTEGER  IFMT1(20), IFMT1C(20), IFMT2(18), IFMT2C(18), BLANK, STAR
C     EQUIVALENCE (IFMT1(1), IFMT1C(1)), (IFMT2(1), IFMT2C(1))          
C/7S                                                                    
      CHARACTER*1  IFMT1(20), IFMT2(18), BLANK, STAR                    
      CHARACTER*20 IFMT1C                                               
      CHARACTER*18 IFMT2C                                               
      EQUIVALENCE (IFMT1(1), IFMT1C), (IFMT2(1), IFMT2C)                
C/                                                                      
      INTEGER  INDW, NCOL, COUNT, I, J, K, ILINE, ILAST                 
      LOGICAL  DUP                                                      
      DOUBLE PRECISION  LINE(18), LAST(18)                              
      REAL  LOGETA                                                      
C                                                                       
C/6S                                                                    
C     DATA BLANK/1H /, STAR/1H*/, INDW/7/, EXPENT/0/                    
C/7S                                                                    
      DATA BLANK/' '/, STAR/'*'/, INDW/7/, EXPENT/0/                    
C/                                                                      
C                                                                       
C  IFMT1 IS FOR THE ASTERISK LINES- IFMT2 FOR THE DATA LINES            
C                                                                       
C/6S                                                                    
C     DATA IFMT1( 1) /1H(/,  IFMT2( 1) /1H(/                            
C     DATA IFMT1( 2) /1H1/,  IFMT2( 2) /1H1/                            
C     DATA IFMT1( 3) /1HA/,  IFMT2( 3) /1HA/                            
C     DATA IFMT1( 4) /1H1/,  IFMT2( 4) /1H1/                            
C     DATA IFMT1( 5) /1H,/,  IFMT2( 5) /1H,/                            
C     DATA IFMT1( 6) /1H5/,  IFMT2( 6) /1HI/                            
C     DATA IFMT1( 7) /1HX/,  IFMT2( 7) /1H7/                            
C     DATA IFMT1( 8) /1H,/,  IFMT2( 8) /1H,/                            
C     DATA IFMT1( 9) /1H2/,  IFMT2( 9) /1H1/                            
C     DATA IFMT1(10) /1HA/,  IFMT2(10) /1HP/                            
C     DATA IFMT1(11) /1H1/,  IFMT2(11) /1H /                            
C     DATA IFMT1(12) /1H,/,  IFMT2(12) /1HD/                            
C     DATA IFMT1(13) /1H /,  IFMT2(13) /1H /                            
C     DATA IFMT1(14) /1H /,  IFMT2(14) /1H /                            
C     DATA IFMT1(15) /1HX/,  IFMT2(15) /1H./                            
C     DATA IFMT1(16) /1H,/,  IFMT2(16) /1H /                            
C     DATA IFMT1(17) /1H2/,  IFMT2(17) /1H /                            
C     DATA IFMT1(18) /1HA/,  IFMT2(18) /1H)/                            
C     DATA IFMT1(19) /1H1/                                              
C     DATA IFMT1(20) /1H)/                                              
C/7S                                                                    
      DATA IFMT1( 1) /'('/,  IFMT2( 1) /'('/                            
      DATA IFMT1( 2) /'1'/,  IFMT2( 2) /'1'/                            
      DATA IFMT1( 3) /'A'/,  IFMT2( 3) /'A'/                            
      DATA IFMT1( 4) /'1'/,  IFMT2( 4) /'1'/                            
      DATA IFMT1( 5) /','/,  IFMT2( 5) /','/                            
      DATA IFMT1( 6) /'5'/,  IFMT2( 6) /'I'/                            
      DATA IFMT1( 7) /'X'/,  IFMT2( 7) /'7'/                            
      DATA IFMT1( 8) /','/,  IFMT2( 8) /','/                            
      DATA IFMT1( 9) /'2'/,  IFMT2( 9) /'1'/                            
      DATA IFMT1(10) /'A'/,  IFMT2(10) /'P'/                            
      DATA IFMT1(11) /'1'/,  IFMT2(11) /' '/                            
      DATA IFMT1(12) /','/,  IFMT2(12) /'D'/                            
      DATA IFMT1(13) /' '/,  IFMT2(13) /' '/                            
      DATA IFMT1(14) /' '/,  IFMT2(14) /' '/                            
      DATA IFMT1(15) /'X'/,  IFMT2(15) /'.'/                            
      DATA IFMT1(16) /','/,  IFMT2(16) /' '/                            
      DATA IFMT1(17) /'2'/,  IFMT2(17) /' '/                            
      DATA IFMT1(18) /'A'/,  IFMT2(18) /')'/                            
      DATA IFMT1(19) /'1'/                                              
      DATA IFMT1(20) /')'/                                              
C/                                                                      
C                                                                       
C     EXPENT IS USED AS A FIRST-TIME SWITCH TO SIGNAL IF THE            
C     MACHINE-VALUE CONSTANTS HAVE BEEN COMPUTED.                       
C                                                                       
      IF (EXPENT .GT. 0) GO TO 10                                       
         LOGETA = ALOG10(FLOAT(I1MACH(10)))                             
         EMIN = ICEIL(LOGETA*FLOAT(IABS(I1MACH(15)-1)))                 
         EMAX = ICEIL(LOGETA*FLOAT(I1MACH(16)))                         
         EXPENT = I10WID(MAX0(EMIN, EMAX))                              
C                                                                       
C     COMPUTE THE FORMATS.                                              
C                                                                       
   10 WW = MIN0(99, MAX0(W, 5+EXPENT))                                  
      CALL S88FMT(2, WW, IFMT2(13))                                     
      DD = MIN0(D, (WW-(5+EXPENT)))                                     
      CALL S88FMT(2, DD, IFMT2(16))                                     
C                                                                       
C  NCOL IS THE NUMBER OF VALUES TO BE PRINTED ACROSS THE LINE.          
C                                                                       
      NCOL = MAX0(1, MIN0(9, (MIN0(MCOL,160)-INDW)/WW))                 
      CALL S88FMT(1, NCOL, IFMT2(11))                                   
      WW = WW-2                                                         
C  THE ASTERISKS ARE POSITIONED RIGHT-ADJUSTED IN THE W-WIDTH SPACE.    
      CALL S88FMT(2, WW, IFMT1(13))                                     
C                                                                       
C  I COUNTS THE NUMBER OF ITEMS TO BE PRINTED,                          
C  J COUNTS THE NUMBER ON A GIVEN LINE,                                 
C  COUNT COUNTS THE NUMBER OF DUPLICATE LINES.                          
C                                                                       
      I = 1                                                             
      J = 0                                                             
      COUNT = 0                                                         
C                                                                       
C  THE LOGICAL OF THE FOLLOWING IS ROUGHLY THIS -                       
C  IF THERE ARE STILL MORE ITEMS TO BE PRINTED, A LINE-                 
C  FULL IS PUT INTO THE ARRAY, LINE.                                    
C  WHENEVER A LINE IS PRINTED OUT, IT IS ALSO STUFFED INTO              
C  THE ARRAY, LAST, TO COMPARE WITH THE NEXT ONE COMING IN              
C  TO CHECK FOR REPEAT OR DUPLICATED LINES.                             
C  ALSO WHENEVER A LINE IS WRITTEN OUT, THE DUPLICATION                 
C  COUNTER, COUNT, IS SET TO ONE.                                       
C  THE ONLY MILDLY TRICKY PART IS TO NOTE THAT COUNT HAS TO             
C  GO TO 3 BEFORE A LINE OF ASTERISKS IS PRINTED BECAUSE                
C  OF COURSE NO SUCH LINE IS PRINTED FOR JUST A PAIR OF                 
C  DUPLICATE LINES.                                                     
C                                                                       
C  ILINE IS PRINTED AS THE INDEX OF THE FIRST ARRAY ELEMENT             
C  IN A LINE.                                                           
C                                                                       
   20 IF (I .GT. NITEMS)  GO TO 90                                      
        J = J+1                                                         
        LINE(J) = A(I)                                                  
        IF (J .EQ. 1) ILINE = I                                         
        IF (J .LT. NCOL .AND. I .LT. NITEMS) GO TO 80                   
          IF (COUNT .EQ. 0) GO TO 50                                    
            DUP = .TRUE.                                                
            DO 30 K=1,NCOL                                              
   30         IF (LAST(K) .NE. LINE(K)) DUP = .FALSE.                   
            IF (I .EQ. NITEMS  .AND.  J .LT. NCOL) DUP = .FALSE.        
            IF (.NOT. DUP .AND. COUNT .EQ. 1) GO TO 50                  
              IF (.NOT. DUP) GO TO 40                                   
                COUNT = COUNT+1                                         
                IF (COUNT .EQ. 3) WRITE(IOUT, IFMT1C) BLANK,            
     1                                 STAR, STAR, STAR, STAR           
                IF (I .EQ. NITEMS)  GO TO 50                            
                  GO TO 70                                              
   40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(K), K=1,NCOL)     
   50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(K), K=1,J)            
          COUNT = 1                                                     
          DO 60 K=1,NCOL                                                
   60       LAST(K) = LINE(K)                                           
   70     ILAST = ILINE                                                 
          J = 0                                                         
   80   I = I+1                                                         
        GO TO 20                                                        
   90 RETURN                                                            
      END                                                               
      SUBROUTINE A9RNTI(A, NITEMS, IOUT, MCOL, W)                       
C                                                                       
C  THIS IS THE DOCUMENTED ROUTINE APRNTI, BUT WITHOUT THE CALLS TO      
C  SETERR - BECAUSE IT IS CALLED BY SETERR.                             
C                                                                       
C  THIS SUBROUTINE PRINTS OUT NITEMS FROM THE INTEGER ARRAY, A, ON      
C  OUTPUT UNIT IOUT, USING A MAXIMUM OF MCOL PRINT SPACES.              
C  THE OUTPUT FORMAT IS IW.                                             
C  THE PROGRAM PUTS AS MANY VALUES ON A LINE AS POSSIBLE.               
C  W SHOULD BE INPUT AS THE ACTUAL WIDTH +1 FOR A SPACE BETWEEN VALUES. 
C                                                                       
C  DUPLICATE LINES ARE NOT ALL PRINTED, BUT ARE INDICATED BY ASTERISKS. 
C                                                                       
C  WRITTEN BY DAN WARNER, REVISED BY PHYL FOX, OCTOBER 21, 1982.        
C                                                                       
C  THE LINE WIDTH IS COMPUTED AS THE MINIMUM OF THE INPUT MCOL AND 160. 
C  IF THE LINE WIDTH IS TO BE INCREASED ABOVE 160, THE BUFFERS LINE()   
C  AND LAST(), WHICH THE VALUES TO BE PRINTED ON ONE LINE, MUST         
C  BE DIMENSIONED ACCORDINGLY.                                          
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C    A        - THE START OF THE INTEGER ARRAY TO BE PRINTED            
C                                                                       
C    NITEMS   - THE NUMBER OF ITEMS TO BE PRINTED                       
C                                                                       
C    IOUT     - THE OUTPUT UNIT FOR PRINTING                            
C                                                                       
C    MCOL     - THE NUMBER OF SPACES ACROSS THE LINE                    
C                                                                       
C    W        - THE WIDTH OF THE PRINTED VALUE (IW)                     
C                                                                       
C                                                                       
C  ERROR STATES - NONE. LOWER LEVEL ROUTINE CALLED BY                   
C  SETERR, SO IT CANNOT CALL SETERR.                                    
C                                                                       
C                                                                       
      INTEGER  NITEMS, IOUT, MCOL, W                                    
      INTEGER  A(NITEMS)                                                
C                                                                       
      INTEGER  MAX0, MIN0, WW                                           
C/6S                                                                    
C     INTEGER  IFMT1(20), IFMT1C(20), IFMT2(14), IFMT2C(14), BLANK, STAR
C     EQUIVALENCE (IFMT1(1), IFMT1C(1)), (IFMT2(1), IFMT2C(1))          
C/7S                                                                    
      CHARACTER*1  IFMT1(20), IFMT2(14), BLANK, STAR                    
      CHARACTER*20 IFMT1C                                               
      CHARACTER*14 IFMT2C                                               
      EQUIVALENCE (IFMT1(1), IFMT1C), (IFMT2(1), IFMT2C)                
C/                                                                      
      INTEGER  INDW, NCOL, COUNT, I, J, K, ILINE, ILAST                 
      LOGICAL  DUP                                                      
      INTEGER  LINE(40), LAST(40)                                       
C                                                                       
C/6S                                                                    
C     DATA BLANK/1H /, STAR/1H*/, INDW/7/                               
C/7S                                                                    
      DATA BLANK/' '/, STAR/'*'/, INDW/7/                               
C/                                                                      
C                                                                       
C  IFMT1 IS FOR THE ASTERISK LINES- IFMT2 FOR THE DATA LINES            
C                                                                       
C/6S                                                                    
C     DATA IFMT1( 1) /1H(/,  IFMT2( 1) /1H(/                            
C     DATA IFMT1( 2) /1H1/,  IFMT2( 2) /1H1/                            
C     DATA IFMT1( 3) /1HA/,  IFMT2( 3) /1HA/                            
C     DATA IFMT1( 4) /1H1/,  IFMT2( 4) /1H1/                            
C     DATA IFMT1( 5) /1H,/,  IFMT2( 5) /1H,/                            
C     DATA IFMT1( 6) /1H5/,  IFMT2( 6) /1HI/                            
C     DATA IFMT1( 7) /1HX/,  IFMT2( 7) /1H7/                            
C     DATA IFMT1( 8) /1H,/,  IFMT2( 8) /1H,/                            
C     DATA IFMT1( 9) /1H2/,  IFMT2( 9) /1H /                            
C     DATA IFMT1(10) /1HA/,  IFMT2(10) /1H /                            
C     DATA IFMT1(11) /1H1/,  IFMT2(11) /1HI/                            
C     DATA IFMT1(12) /1H,/,  IFMT2(12) /1H /                            
C     DATA IFMT1(13) /1H /,  IFMT2(13) /1H /                            
C     DATA IFMT1(14) /1H /,  IFMT2(14) /1H)/                            
C     DATA IFMT1(15) /1HX/                                              
C     DATA IFMT1(16) /1H,/                                              
C     DATA IFMT1(17) /1H2/                                              
C     DATA IFMT1(18) /1HA/                                              
C     DATA IFMT1(19) /1H1/                                              
C     DATA IFMT1(20) /1H)/                                              
C/7S                                                                    
      DATA IFMT1( 1) /'('/,  IFMT2( 1) /'('/                            
      DATA IFMT1( 2) /'1'/,  IFMT2( 2) /'1'/                            
      DATA IFMT1( 3) /'A'/,  IFMT2( 3) /'A'/                            
      DATA IFMT1( 4) /'1'/,  IFMT2( 4) /'1'/                            
      DATA IFMT1( 5) /','/,  IFMT2( 5) /','/                            
      DATA IFMT1( 6) /'5'/,  IFMT2( 6) /'I'/                            
      DATA IFMT1( 7) /'X'/,  IFMT2( 7) /'7'/                            
      DATA IFMT1( 8) /','/,  IFMT2( 8) /','/                            
      DATA IFMT1( 9) /'2'/,  IFMT2( 9) /' '/                            
      DATA IFMT1(10) /'A'/,  IFMT2(10) /' '/                            
      DATA IFMT1(11) /'1'/,  IFMT2(11) /'I'/                            
      DATA IFMT1(12) /','/,  IFMT2(12) /' '/                            
      DATA IFMT1(13) /' '/,  IFMT2(13) /' '/                            
      DATA IFMT1(14) /' '/,  IFMT2(14) /')'/                            
      DATA IFMT1(15) /'X'/                                              
      DATA IFMT1(16) /','/                                              
      DATA IFMT1(17) /'2'/                                              
      DATA IFMT1(18) /'A'/                                              
      DATA IFMT1(19) /'1'/                                              
      DATA IFMT1(20) /')'/                                              
C/                                                                      
C                                                                       
C     COMPUTE THE FORMATS.                                              
C                                                                       
        WW = MIN0(99, MAX0(W, 2))                                       
        CALL S88FMT(2, WW, IFMT2(12))                                   
        NCOL = MAX0(1, MIN0(99, (MIN0(MCOL,160) - INDW)/WW))            
        CALL S88FMT(2, NCOL, IFMT2(9))                                  
        WW = WW-2                                                       
        CALL S88FMT(2, WW, IFMT1(13))                                   
C                                                                       
C  THE ASTERISKS ARE POSITIONED RIGHT-ADJUSTED IN THE W-WIDTH SPACE.    
      CALL S88FMT(2, WW, IFMT1(13))                                     
C                                                                       
C  I COUNTS THE NUMBER OF ITEMS TO BE PRINTED,                          
C  J COUNTS THE NUMBER ON A GIVEN LINE,                                 
C  COUNT COUNTS THE NUMBER OF DUPLICATE LINES.                          
C                                                                       
  10  I = 1                                                             
      J = 0                                                             
      COUNT = 0                                                         
C                                                                       
C  THE LOGICAL OF THE FOLLOWING IS ROUGHLY THIS -                       
C  IF THERE ARE STILL MORE ITEMS TO BE PRINTED, A LINE-                 
C  FULL IS PUT INTO THE ARRAY, LINE.                                    
C  WHENEVER A LINE IS PRINTED OUT, IT IS ALSO STUFFED INTO              
C  THE ARRAY, LAST, TO COMPARE WITH THE NEXT ONE COMING IN              
C  TO CHECK FOR REPEAT OR DUPLICATED LINES.                             
C  ALSO WHENEVER A LINE IS WRITTEN OUT, THE DUPLICATION                 
C  COUNTER, COUNT, IS SET TO ONE.                                       
C  THE ONLY MILDLY TRICKY PART IS TO NOTE THAT COUNT HAS TO             
C  GO TO 3 BEFORE A LINE OF ASTERISKS IS PRINTED BECAUSE                
C  OF COURSE NO SUCH LINE IS PRINTED FOR JUST A PAIR OF                 
C  DUPLICATE LINES.                                                     
C                                                                       
C  ILINE IS PRINTED AS THE INDEX OF THE FIRST ARRAY ELEMENT             
C  IN A LINE.                                                           
C                                                                       
   20 IF (I .GT. NITEMS)  GO TO 90                                      
        J = J+1                                                         
        LINE(J) = A(I)                                                  
        IF (J .EQ. 1) ILINE = I                                         
        IF (J .LT. NCOL .AND. I .LT. NITEMS) GO TO 80                   
          IF (COUNT .EQ. 0) GO TO 50                                    
            DUP = .TRUE.                                                
            DO 30 K=1,NCOL                                              
   30         IF (LAST(K) .NE. LINE(K)) DUP = .FALSE.                   
            IF (I .EQ. NITEMS  .AND.  J .LT. NCOL) DUP = .FALSE.        
            IF (.NOT. DUP .AND. COUNT .EQ. 1) GO TO 50                  
              IF (.NOT. DUP) GO TO 40                                   
                COUNT = COUNT+1                                         
                IF (COUNT .EQ. 3) WRITE(IOUT, IFMT1C) BLANK,            
     1                                 STAR, STAR, STAR, STAR           
                IF (I .EQ. NITEMS)  GO TO 50                            
                  GO TO 70                                              
   40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(K), K=1,NCOL)     
   50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(K), K=1,J)            
          COUNT = 1                                                     
          DO 60 K=1,NCOL                                                
   60       LAST(K) = LINE(K)                                           
   70     ILAST = ILINE                                                 
          J = 0                                                         
   80   I = I+1                                                         
        GO TO 20                                                        
   90 RETURN                                                            
      END                                                               
      SUBROUTINE A9RNTL(A, NITEMS, IOUT, MCOL)                          
C                                                                       
C  THIS IS THE DOCUMENTED ROUTINE APRNTL, BUT WITHOUT THE CALLS TO      
C  SETERR - BECAUSE IT IS CALLED BY SETERR.                             
C                                                                       
C  THIS SUBROUTINE PRINTS OUT NITEMS FROM THE LOGICAL ARRAY, A, ON      
C  OUTPUT UNIT IOUT, USING A MAXIMUM OF MCOL PRINT SPACES.              
C  THE T OR F VALUES ARE PRINTED RIGHT-ADJUSTED IN A FIELD OF WIDTH 4.  
C                                                                       
C  DUPLICATE LINES ARE NOT ALL PRINTED, BUT ARE INDICATED BY ASTERISKS. 
C                                                                       
C  WRITTEN BY DAN WARNER, REVISED BY PHYL FOX, OCTOBER 21, 1982.        
C                                                                       
C  THE LINE WIDTH IS COMPUTED AS THE MINIMUM OF THE INPUT MCOL AND 160. 
C  IF THE LINE WIDTH IS TO BE INCREASED ABOVE 160, THE BUFFERS LINE()   
C  AND LAST(), WHICH THE VALUES TO BE PRINTED ON ONE LINE, MUST         
C  BE DIMENSIONED ACCORDINGLY.                                          
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C    A        - THE START OF THE LOGICAL ARRAY TO BE PRINTED            
C                                                                       
C    NITEMS   - THE NUMBER OF ITEMS TO BE PRINTED                       
C                                                                       
C    IOUT     - THE OUTPUT UNIT FOR PRINTING                            
C                                                                       
C    MCOL     - THE NUMBER OF SPACES ACROSS THE LINE                    
C                                                                       
C                                                                       
C  ERROR STATES - NONE.  LOWER LEVEL ROUTINE CALLED BY                  
C  SETERR, SO IT CANNOT CALL SETERR.                                    
C                                                                       
C                                                                       
      INTEGER  NITEMS, IOUT, MCOL                                       
      LOGICAL  A(NITEMS)                                                
C                                                                       
      INTEGER  MAX0, MIN0                                               
C/6S                                                                    
C     INTEGER  IFMT1(20), IFMT1C(20), IFMT2(19), IFMT2C(19), BLANK,     
C    1         STAR, TCHAR, FCHAR                                       
C     INTEGER  LINE(40), LAST(40)                                       
C     EQUIVALENCE (IFMT1(1), IFMT1C(1)), (IFMT2(1), IFMT2C(1))          
C/7S                                                                    
      CHARACTER*1  IFMT1(20), IFMT2(19), BLANK, STAR, TCHAR, FCHAR      
      CHARACTER*20 IFMT1C                                               
      CHARACTER*19 IFMT2C                                               
      EQUIVALENCE (IFMT1(1), IFMT1C), (IFMT2(1), IFMT2C)                
      CHARACTER*1  LINE(40), LAST(40)                                   
C/                                                                      
      INTEGER  INDW, NCOL, COUNT, I, J, K, ILINE, ILAST                 
      LOGICAL  DUP                                                      
C                                                                       
C/6S                                                                    
C     DATA BLANK/1H /, STAR/1H*/, TCHAR/1HT/, FCHAR/1HF/, INDW/7/       
C/7S                                                                    
      DATA BLANK/' '/, STAR/'*'/, TCHAR/'T'/, FCHAR/'F'/, INDW/7/       
C/                                                                      
C                                                                       
C                                                                       
C  IFMT1 IS FOR THE ASTERISK LINES- IFMT2 FOR THE DATA LINES            
C                                                                       
C/6S                                                                    
C     DATA IFMT1( 1) /1H(/,  IFMT2( 1) /1H(/                            
C     DATA IFMT1( 2) /1H1/,  IFMT2( 2) /1H1/                            
C     DATA IFMT1( 3) /1HA/,  IFMT2( 3) /1HA/                            
C     DATA IFMT1( 4) /1H1/,  IFMT2( 4) /1H1/                            
C     DATA IFMT1( 5) /1H,/,  IFMT2( 5) /1H,/                            
C     DATA IFMT1( 6) /1H5/,  IFMT2( 6) /1HI/                            
C     DATA IFMT1( 7) /1HX/,  IFMT2( 7) /1H7/                            
C     DATA IFMT1( 8) /1H,/,  IFMT2( 8) /1H,/                            
C     DATA IFMT1( 9) /1H2/,  IFMT2( 9) /1H /                            
C     DATA IFMT1(10) /1HA/,  IFMT2(10) /1H /                            
C     DATA IFMT1(11) /1H1/,  IFMT2(11) /1H(/                            
C     DATA IFMT1(12) /1H,/,  IFMT2(12) /1H3/                            
C     DATA IFMT1(13) /1H /,  IFMT2(13) /1HX/                            
C     DATA IFMT1(14) /1H2/,  IFMT2(14) /1H,/                            
C     DATA IFMT1(15) /1HX/,  IFMT2(15) /1H1/                            
C     DATA IFMT1(16) /1H,/,  IFMT2(16) /1HA/                            
C     DATA IFMT1(17) /1H2/,  IFMT2(17) /1H1/                            
C     DATA IFMT1(18) /1HA/,  IFMT2(18) /1H)/                            
C     DATA IFMT1(19) /1H1/,  IFMT2(19) /1H)/                            
C     DATA IFMT1(20) /1H)/                                              
C/7S                                                                    
      DATA IFMT1( 1) /'('/,  IFMT2( 1) /'('/                            
      DATA IFMT1( 2) /'1'/,  IFMT2( 2) /'1'/                            
      DATA IFMT1( 3) /'A'/,  IFMT2( 3) /'A'/                            
      DATA IFMT1( 4) /'1'/,  IFMT2( 4) /'1'/                            
      DATA IFMT1( 5) /','/,  IFMT2( 5) /','/                            
      DATA IFMT1( 6) /'5'/,  IFMT2( 6) /'I'/                            
      DATA IFMT1( 7) /'X'/,  IFMT2( 7) /'7'/                            
      DATA IFMT1( 8) /','/,  IFMT2( 8) /','/                            
      DATA IFMT1( 9) /'2'/,  IFMT2( 9) /' '/                            
      DATA IFMT1(10) /'A'/,  IFMT2(10) /' '/                            
      DATA IFMT1(11) /'1'/,  IFMT2(11) /'('/                            
      DATA IFMT1(12) /','/,  IFMT2(12) /'3'/                            
      DATA IFMT1(13) /' '/,  IFMT2(13) /'X'/                            
      DATA IFMT1(14) /'2'/,  IFMT2(14) /','/                            
      DATA IFMT1(15) /'X'/,  IFMT2(15) /'1'/                            
      DATA IFMT1(16) /','/,  IFMT2(16) /'A'/                            
      DATA IFMT1(17) /'2'/,  IFMT2(17) /'1'/                            
      DATA IFMT1(18) /'A'/,  IFMT2(18) /')'/                            
      DATA IFMT1(19) /'1'/,  IFMT2(19) /')'/                            
      DATA IFMT1(20) /')'/                                              
C/                                                                      
C                                                                       
C                                                                       
C  COMPUTE THE NUMBER OF FIELDS OF 4 ACROSS A LINE.                     
C                                                                       
      NCOL = MAX0(1, MIN0(99, (MIN0(MCOL,160)-INDW)/4))                 
C                                                                       
C  THE ASTERISKS ARE POSITIONED RIGHT-ADJUSTED IN THE 4-CHARACTER SPACE.
      CALL S88FMT(2, NCOL, IFMT2(9))                                    
C                                                                       
C  I COUNTS THE NUMBER OF ITEMS TO BE PRINTED,                          
C  J COUNTS THE NUMBER ON A GIVEN LINE,                                 
C  COUNT COUNTS THE NUMBER OF DUPLICATE LINES.                          
C                                                                       
  10  I = 1                                                             
      J = 0                                                             
      COUNT = 0                                                         
C                                                                       
C  THE LOGICAL OF THE FOLLOWING IS ROUGHLY THIS -                       
C  IF THERE ARE STILL MORE ITEMS TO BE PRINTED, A LINE-                 
C  FULL IS PUT INTO THE ARRAY, LINE.                                    
C  WHENEVER A LINE IS PRINTED OUT, IT IS ALSO STUFFED INTO              
C  THE ARRAY, LAST, TO COMPARE WITH THE NEXT ONE COMING IN              
C  TO CHECK FOR REPEAT OR DUPLICATED LINES.                             
C  ALSO WHENEVER A LINE IS WRITTEN OUT, THE DUPLICATION                 
C  COUNTER, COUNT, IS SET TO ONE.                                       
C  THE ONLY MILDLY TRICKY PART IS TO NOTE THAT COUNT HAS TO             
C  GO TO 3 BEFORE A LINE OF ASTERISKS IS PRINTED BECAUSE                
C  OF COURSE NO SUCH LINE IS PRINTED FOR JUST A PAIR OF                 
C  DUPLICATE LINES.                                                     
C                                                                       
C  ILINE IS PRINTED AS THE INDEX OF THE FIRST ARRAY ELEMENT             
C  IN A LINE.                                                           
C                                                                       
   20 IF (I .GT. NITEMS)  GO TO 90                                      
        J = J+1                                                         
        LINE(J) = FCHAR                                                 
        IF ( A(I) )  LINE(J) = TCHAR                                    
        IF (J .EQ. 1) ILINE = I                                         
        IF (J .LT. NCOL .AND. I .LT. NITEMS) GO TO 80                   
          IF (COUNT .EQ. 0) GO TO 50                                    
            DUP = .TRUE.                                                
            DO 30 K=1,NCOL                                              
   30         IF (LAST(K) .NE. LINE(K)) DUP = .FALSE.                   
            IF (I .EQ. NITEMS  .AND.  J .LT. NCOL) DUP = .FALSE.        
            IF (.NOT. DUP .AND. COUNT .EQ. 1) GO TO 50                  
              IF (.NOT. DUP) GO TO 40                                   
                COUNT = COUNT+1                                         
                IF (COUNT .EQ. 3) WRITE(IOUT, IFMT1C) BLANK,            
     1                                 STAR, STAR, STAR, STAR           
                IF (I .EQ. NITEMS)  GO TO 50                            
                  GO TO 70                                              
   40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(K), K=1,NCOL)     
   50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(K), K=1,J)            
          COUNT = 1                                                     
          DO 60 K=1,NCOL                                                
   60       LAST(K) = LINE(K)                                           
   70     ILAST = ILINE                                                 
          J = 0                                                         
   80   I = I+1                                                         
        GO TO 20                                                        
   90 RETURN                                                            
      END                                                               
      SUBROUTINE A9RNTR(A, NITEMS, IOUT, MCOL, W, D)                    
C                                                                       
C  THIS IS THE DOCUMENTED ROUTINE APRNTR, BUT WITHOUT THE CALLS TO      
C  SETERR - BECAUSE IT IS CALLED BY SETERR.                             
C                                                                       
C  THIS SUBROUTINE PRINTS OUT NITEMS FROM THE REAL ARRAY, A, ON         
C  OUTPUT UNIT IOUT, USING A MAXIMUM OF MCOL PRINT SPACES.              
C  THE OUTPUT FORMAT IS 1PEW.D.                                         
C  THE PROGRAM PUTS AS MANY VALUES ON A LINE AS POSSIBLE.               
C  W SHOULD BE INPUT AS THE ACTUAL WIDTH +1 FOR A SPACE BETWEEN VALUES. 
C                                                                       
C  DUPLICATE LINES ARE NOT ALL PRINTED, BUT ARE INDICATED BY ASTERISKS. 
C                                                                       
C  WRITTEN BY DAN WARNER, REVISED BY PHYL FOX, OCTOBER 21, 1982.        
C                                                                       
C  THE LINE WIDTH IS COMPUTED AS THE MINIMUM OF THE INPUT MCOL AND 160. 
C  IF THE LINE WIDTH IS TO BE INCREASED ABOVE 160, THE BUFFERS LINE()   
C  AND LAST(), WHICH THE VALUES TO BE PRINTED ON ONE LINE, MUST         
C  BE DIMENSIONED ACCORDINGLY.                                          
C                                                                       
C  INPUT PARAMETERS -                                                   
C                                                                       
C    A        - THE START OF THE REAL ARRAY TO BE PRINTED               
C                                                                       
C    NITEMS   - THE NUMBER OF ITEMS TO BE PRINTED                       
C                                                                       
C    IOUT     - THE OUTPUT UNIT FOR PRINTING                            
C                                                                       
C    MCOL     - THE NUMBER OF SPACES ACROSS THE LINE                    
C                                                                       
C    W        - THE WIDTH OF THE PRINTED VALUE (1PEW.D)                 
C                                                                       
C    D        - THE NUMBER OF DIGITS AFTER THE DECIMAL POINT (1PEW.D)   
C                                                                       
C                                                                       
C  ERROR STATES - NONE.  LOWER LEVEL ROUTINE CALLED BY                  
C  SETERR, SO IT CANNOT CALL SETERR.                                    
C                                                                       
C                                                                       
      INTEGER  NITEMS, IOUT, MCOL, W, D                                 
      REAL     A(NITEMS)                                                
C                                                                       
      INTEGER  MAX0, MIN0, WW, DD, EMIN, EMAX,                          
     1         EXPENT, I1MACH, ICEIL, IABS, I10WID                      
C/6S                                                                    
C     INTEGER  IFMT1(20), IFMT1C(20), IFMT2(18), IFMT2C(18), BLANK, STAR
C     EQUIVALENCE (IFMT1(1), IFMT1C(1)), (IFMT2(1), IFMT2C(1))          
C/7S                                                                    
      CHARACTER*1  IFMT1(20), IFMT2(18), BLANK, STAR                    
      CHARACTER*20 IFMT1C                                               
      CHARACTER*18 IFMT2C                                               
      EQUIVALENCE (IFMT1(1), IFMT1C), (IFMT2(1), IFMT2C)                
C/                                                                      
      INTEGER  INDW, NCOL, COUNT, I, J, K, ILINE, ILAST                 
      LOGICAL  DUP                                                      
      REAL     LINE(18), LAST(18), LOGETA                               
C                                                                       
C/6S                                                                    
C     DATA BLANK/1H /, STAR/1H*/, INDW/7/, EXPENT/0/                    
C/7S                                                                    
      DATA BLANK/' '/, STAR/'*'/, INDW/7/, EXPENT/0/                    
C/                                                                      
C                                                                       
C  IFMT1 IS FOR THE ASTERISK LINES- IFMT2 FOR THE DATA LINES            
C                                                                       
C/6S                                                                    
C     DATA IFMT1( 1) /1H(/,  IFMT2( 1) /1H(/                            
C     DATA IFMT1( 2) /1H1/,  IFMT2( 2) /1H1/                            
C     DATA IFMT1( 3) /1HA/,  IFMT2( 3) /1HA/                            
C     DATA IFMT1( 4) /1H1/,  IFMT2( 4) /1H1/                            
C     DATA IFMT1( 5) /1H,/,  IFMT2( 5) /1H,/                            
C     DATA IFMT1( 6) /1H5/,  IFMT2( 6) /1HI/                            
C     DATA IFMT1( 7) /1HX/,  IFMT2( 7) /1H7/                            
C     DATA IFMT1( 8) /1H,/,  IFMT2( 8) /1H,/                            
C     DATA IFMT1( 9) /1H2/,  IFMT2( 9) /1H1/                            
C     DATA IFMT1(10) /1HA/,  IFMT2(10) /1HP/                            
C     DATA IFMT1(11) /1H1/,  IFMT2(11) /1H /                            
C     DATA IFMT1(12) /1H,/,  IFMT2(12) /1HE/                            
C     DATA IFMT1(13) /1H /,  IFMT2(13) /1H /                            
C     DATA IFMT1(14) /1H /,  IFMT2(14) /1H /                            
C     DATA IFMT1(15) /1HX/,  IFMT2(15) /1H./                            
C     DATA IFMT1(16) /1H,/,  IFMT2(16) /1H /                            
C     DATA IFMT1(17) /1H2/,  IFMT2(17) /1H /                            
C     DATA IFMT1(18) /1HA/,  IFMT2(18) /1H)/                            
C     DATA IFMT1(19) /1H1/                                              
C     DATA IFMT1(20) /1H)/                                              
C/7S                                                                    
      DATA IFMT1( 1) /'('/,  IFMT2( 1) /'('/                            
      DATA IFMT1( 2) /'1'/,  IFMT2( 2) /'1'/                            
      DATA IFMT1( 3) /'A'/,  IFMT2( 3) /'A'/                            
      DATA IFMT1( 4) /'1'/,  IFMT2( 4) /'1'/                            
      DATA IFMT1( 5) /','/,  IFMT2( 5) /','/                            
      DATA IFMT1( 6) /'5'/,  IFMT2( 6) /'I'/                            
      DATA IFMT1( 7) /'X'/,  IFMT2( 7) /'7'/                            
      DATA IFMT1( 8) /','/,  IFMT2( 8) /','/                            
      DATA IFMT1( 9) /'2'/,  IFMT2( 9) /'1'/                            
      DATA IFMT1(10) /'A'/,  IFMT2(10) /'P'/                            
      DATA IFMT1(11) /'1'/,  IFMT2(11) /' '/                            
      DATA IFMT1(12) /','/,  IFMT2(12) /'E'/                            
      DATA IFMT1(13) /' '/,  IFMT2(13) /' '/                            
      DATA IFMT1(14) /' '/,  IFMT2(14) /' '/                            
      DATA IFMT1(15) /'X'/,  IFMT2(15) /'.'/                            
      DATA IFMT1(16) /','/,  IFMT2(16) /' '/                            
      DATA IFMT1(17) /'2'/,  IFMT2(17) /' '/                            
      DATA IFMT1(18) /'A'/,  IFMT2(18) /')'/                            
      DATA IFMT1(19) /'1'/                                              
      DATA IFMT1(20) /')'/                                              
C/                                                                      
C                                                                       
C                                                                       
C     EXPENT IS USED AS A FIRST-TIME SWITCH TO SIGNAL IF THE            
C     MACHINE-VALUE CONSTANTS HAVE BEEN COMPUTED.                       
C                                                                       
      IF (EXPENT .GT. 0) GO TO 10                                       
         LOGETA = ALOG10(FLOAT(I1MACH(10)))                             
         EMIN   = ICEIL(LOGETA*FLOAT(IABS(I1MACH(12)-1)))               
         EMAX   = ICEIL(LOGETA*FLOAT(I1MACH(13)))                       
         EXPENT = I10WID(MAX0(EMIN, EMAX))                              
C                                                                       
C     COMPUTE THE FORMATS.                                              
C                                                                       
   10 WW = MIN0(99, MAX0(W, 5+EXPENT))                                  
      CALL S88FMT(2, WW, IFMT2(13))                                     
      DD = MIN0(D, (WW-(5+EXPENT)))                                     
      CALL S88FMT(2, DD, IFMT2(16))                                     
C                                                                       
C  NCOL IS THE NUMBER OF VALUES TO BE PRINTED ACROSS THE LINE.          
C                                                                       
      NCOL = MAX0(1, MIN0(9, (MIN0(MCOL,160)-INDW)/WW))                 
      CALL S88FMT(1, NCOL, IFMT2(11))                                   
      WW = WW-2                                                         
C                                                                       
C  THE ASTERISKS ARE POSITIONED RIGHT-ADJUSTED IN THE W-WIDTH SPACE.    
      CALL S88FMT(2, WW, IFMT1(13))                                     
C                                                                       
C  I COUNTS THE NUMBER OF ITEMS TO BE PRINTED,                          
C  J COUNTS THE NUMBER ON A GIVEN LINE,                                 
C  COUNT COUNTS THE NUMBER OF DUPLICATE LINES.                          
C                                                                       
      I = 1                                                             
      J = 0                                                             
      COUNT = 0                                                         
C                                                                       
C  THE LOGICAL OF THE FOLLOWING IS ROUGHLY THIS -                       
C  IF THERE ARE STILL MORE ITEMS TO BE PRINTED, A LINE-                 
C  FULL IS PUT INTO THE ARRAY, LINE.                                    
C  WHENEVER A LINE IS PRINTED OUT, IT IS ALSO STUFFED INTO              
C  THE ARRAY, LAST, TO COMPARE WITH THE NEXT ONE COMING IN              
C  TO CHECK FOR REPEAT OR DUPLICATED LINES.                             
C  ALSO WHENEVER A LINE IS WRITTEN OUT, THE DUPLICATION                 
C  COUNTER, COUNT, IS SET TO ONE.                                       
C  THE ONLY MILDLY TRICKY PART IS TO NOTE THAT COUNT HAS TO             
C  GO TO 3 BEFORE A LINE OF ASTERISKS IS PRINTED BECAUSE                
C  OF COURSE NO SUCH LINE IS PRINTED FOR JUST A PAIR OF                 
C  DUPLICATE LINES.                                                     
C                                                                       
C  ILINE IS PRINTED AS THE INDEX OF THE FIRST ARRAY ELEMENT             
C  IN A LINE.                                                           
C                                                                       
   20 IF (I .GT. NITEMS)  GO TO 90                                      
        J = J+1                                                         
        LINE(J) = A(I)                                                  
        IF (J .EQ. 1) ILINE = I                                         
        IF (J .LT. NCOL .AND. I .LT. NITEMS) GO TO 80                   
          IF (COUNT .EQ. 0) GO TO 50                                    
            DUP = .TRUE.                                                
            DO 30 K=1,NCOL                                              
   30         IF (LAST(K) .NE. LINE(K)) DUP = .FALSE.                   
            IF (I .EQ. NITEMS  .AND.  J .LT. NCOL) DUP = .FALSE.        
            IF (.NOT. DUP .AND. COUNT .EQ. 1) GO TO 50                  
              IF (.NOT. DUP) GO TO 40                                   
                COUNT = COUNT+1                                         
                IF (COUNT .EQ. 3) WRITE(IOUT, IFMT1C) BLANK,            
     1                                 STAR, STAR, STAR, STAR           
                IF (I .EQ. NITEMS)  GO TO 50                            
                  GO TO 70                                              
   40         WRITE(IOUT, IFMT2C) BLANK, ILAST, (LAST(K), K=1,NCOL)     
   50     WRITE(IOUT, IFMT2C) BLANK, ILINE, (LINE(K), K=1,J)            
          COUNT = 1                                                     
          DO 60 K=1,NCOL                                                
   60       LAST(K) = LINE(K)                                           
   70     ILAST = ILINE                                                 
          J = 0                                                         
   80   I = I+1                                                         
        GO TO 20                                                        
   90 RETURN                                                            
      END                                                               
      SUBROUTINE FRMATD(WWIDTH, EWIDTH)                                 
C                                                                       
C  THIS SUBROUTINE COMPUTES, FOR THE FORMAT SPECIFICATION, DW.E, THE    
C  NUMBER OF DIGITS TO THE RIGHT OF THE DECIMAL POINT, E=EWIDTH, AND    
C  THE FIELD WIDTH, W=WWIDTH.                                           
C                                                                       
C  WWIDTH INCLUDES THE FIVE POSITIONS NEEDED FOR THE SIGN OF THE        
C  MANTISSA, THE SIGN OF THE EXPONENT, THE 0, THE DECIMAL POINT AND THE 
C  CHARACTER IN THE OUTPUT - +0.XXXXXXXXXD+YYYY                         
C                                                                       
C  THE FOLLOWING MACHINE-DEPENDENT VALUES ARE USED -                    
C                                                                       
C  I1MACH(10) - THE BASE, B                                             
C  I1MACH(14) - THE NUMBER OF BASE-B DIGITS IN THE MANTISSA             
C  I1MACH(15) - THE SMALLEST EXPONENT, EMIN                             
C  I1MACH(16) - THE LARGEST EXPONENT, EMAX                              
C                                                                       
      INTEGER I1MACH, ICEIL, IFLR, EWIDTH, WWIDTH                       
      INTEGER DEMIN, DEMAX, EXPWID                                      
      REAL BASE                                                         
C                                                                       
      BASE = I1MACH(10)                                                 
C                                                                       
      EWIDTH = ICEIL( ALOG10(BASE)*FLOAT(I1MACH(14)) )                  
C                                                                       
      DEMIN =  IFLR( ALOG10(BASE)*FLOAT(I1MACH(15)-1) ) + 1             
      DEMAX = ICEIL( ALOG10(BASE)*FLOAT(I1MACH(16)) )                   
      EXPWID = IFLR( ALOG10(FLOAT(MAX0(IABS(DEMIN),IABS(DEMAX)))) ) + 1 
      WWIDTH = EWIDTH + EXPWID + 5                                      
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE FRMATI(IWIDTH)                                         
C                                                                       
C  THIS SUBROUTINE COMPUTES THE WIDTH, W=IWIDTH, IN THE FORMAT          
C  SPECIFICATION FOR INTEGER VARIABLES.                                 
C                                                                       
C  FRMATI SETS IWIDTH TO THE NUMBER OF CHARACTER POSITIONS NEEDED       
C  FOR WRITING OUT THE LARGEST INTEGER PLUS ONE POSITION FOR THE SIGN.  
C                                                                       
C  I1MACH(7) IS THE BASE, A, FOR INTEGER REPRESENTATION IN THE MACHINE. 
C  I1MACH(8) IS THE (MAXIMUM) NUMBER OF BASE A DIGITS.                  
C                                                                       
      INTEGER I1MACH, ICEIL, IWIDTH                                     
C                                                                       
      IWIDTH = ICEIL( ALOG10(FLOAT(I1MACH(7)))*FLOAT(I1MACH(8)) ) + 1   
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE FRMATR(WWIDTH, EWIDTH)                                 
C                                                                       
C  THIS SUBROUTINE COMPUTES, FOR THE FORMAT SPECIFICATION, EW.E, THE    
C  NUMBER OF DIGITS TO THE RIGHT OF THE DECIMAL POINT, E=EWIDTH, AND    
C  THE FIELD WIDTH, W=WWIDTH.                                           
C                                                                       
C  WWIDTH INCLUDES THE FIVE POSITIONS NEEDED FOR THE SIGN OF THE        
C  MANTISSA, THE SIGN OF THE EXPONENT, THE 0, THE DECIMAL POINT AND THE 
C  CHARACTER IN THE OUTPUT - +0.XXXXXXXXXE+YYYY                         
C                                                                       
C  THE FOLLOWING MACHINE-DEPENDENT VALUES ARE USED -                    
C                                                                       
C  I1MACH(10) - THE BASE, B                                             
C  I1MACH(11) - THE NUMBER OF BASE-B DIGITS IN THE MANTISSA             
C  I1MACH(12) - THE SMALLEST EXPONENT, EMIN                             
C  I1MACH(13) - THE LARGEST EXPONENT, EMAX                              
C                                                                       
      INTEGER I1MACH, ICEIL, IFLR, EWIDTH, WWIDTH                       
      INTEGER DEMIN, DEMAX, EXPWID                                      
      REAL BASE                                                         
C                                                                       
      BASE = I1MACH(10)                                                 
C                                                                       
      EWIDTH = ICEIL( ALOG10(BASE)*FLOAT(I1MACH(11)) )                  
C                                                                       
      DEMIN =  IFLR( ALOG10(BASE)*FLOAT(I1MACH(12)-1) ) + 1             
      DEMAX = ICEIL( ALOG10(BASE)*FLOAT(I1MACH(13)) )                   
      EXPWID = IFLR( ALOG10(FLOAT(MAX0(IABS(DEMIN),IABS(DEMAX)))) ) + 1 
      WWIDTH = EWIDTH + EXPWID + 5                                      
C                                                                       
      RETURN                                                            
      END                                                               
      INTEGER FUNCTION I10WID(IX)                                       
      INTEGER IX                                                        
      INTEGER IABS, IY, DIGITS                                          
C     THIS FUNCTION RETURNS THE NUMBER OF DECIMAL                       
C     DIGITS REQUIRED TO REPRESENT THE INTEGER, IX.                     
      DIGITS = 0                                                        
      IY = IABS(IX)                                                     
   1  IF (IY .LT. 1) GOTO  2                                            
         DIGITS = DIGITS+1                                              
         IY = IY/10                                                     
         GOTO  1                                                        
   2  I10WID = DIGITS                                                   
      RETURN                                                            
      END                                                               
      SUBROUTINE I0TK00(LARG,NITEMS,ITYPE)                              
C                                                                       
C  INITIALIZES THE STACK TO NITEMS OF TYPE ITYPE                        
C                                                                       
      COMMON /CSTAK/DSTAK                                               
C                                                                       
      DOUBLE PRECISION DSTAK(500)                                       
      INTEGER ISTAK(1000)                                               
      LOGICAL LARG,INIT                                                 
      INTEGER ISIZE(5)                                                  
C                                                                       
      EQUIVALENCE (DSTAK(1),ISTAK(1))                                   
      EQUIVALENCE (ISTAK(1),LOUT)                                       
      EQUIVALENCE (ISTAK(2),LNOW)                                       
      EQUIVALENCE (ISTAK(3),LUSED)                                      
      EQUIVALENCE (ISTAK(4),LMAX)                                       
      EQUIVALENCE (ISTAK(5),LBOOK)                                      
      EQUIVALENCE (ISTAK(6),ISIZE(1))                                   
C                                                                       
      DATA INIT/.FALSE./                                                
C                                                                       
      LARG = .FALSE.                                                    
      IF (INIT) RETURN                                                  
C                                                                       
C  HERE TO INITIALIZE                                                   
C                                                                       
      INIT = .TRUE.                                                     
C                                                                       
C  SET DATA SIZES APPROPRIATE FOR A STANDARD CONFORMING                 
C  FORTRAN SYSTEM USING THE FORTRAN STORAGE UNIT AS THE                 
C  MEASURE OF SIZE.                                                     
C                                                                       
C  LOGICAL                                                              
      ISIZE(1) = 1                                                      
C  INTEGER                                                              
      ISIZE(2) = 1                                                      
C  REAL                                                                 
      ISIZE(3) = 1                                                      
C  DOUBLE PRECISION                                                     
      ISIZE(4) = 2                                                      
C  COMPLEX                                                              
      ISIZE(5) = 2                                                      
C                                                                       
      LBOOK = 10                                                        
      LNOW  = LBOOK                                                     
      LUSED = LBOOK                                                     
      LMAX  = MAX0( (NITEMS*ISIZE(ITYPE))/ISIZE(2), 12 )                
      LOUT  = 0                                                         
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      REAL FUNCTION CEIL(X)                                             
C                                                                       
C  CEIL RETURNS CEIL(X)                                                 
C                                                                       
      CEIL = FLOAT( INT(X) )                                            
      IF (X .LE. 0.0) RETURN                                            
      IF (CEIL .NE. X) CEIL = CEIL + 1.0                                
C                                                                       
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DCEIL(X)                                
C                                                                       
C  DCEIL RETURNS CEIL(X)                                                
C                                                                       
      DOUBLE PRECISION X                                                
C                                                                       
      DCEIL = DBLE( FLOAT ( IDINT(X) ) )                                
      IF (X .LE. 0.0D0) RETURN                                          
      IF (DCEIL .NE. X) DCEIL = DCEIL + 1.0D0                           
C                                                                       
      RETURN                                                            
      END                                                               
      INTEGER FUNCTION ICEIL(X)                                         
C                                                                       
C  ICEIL RETURNS CEIL(X)                                                
C                                                                       
      ICEIL = INT(X)                                                    
      IF (X .LE. 0.0) RETURN                                            
      IF (FLOAT(ICEIL) .NE. X) ICEIL = ICEIL + 1                        
C                                                                       
      RETURN                                                            
      END                                                               
      INTEGER FUNCTION IDCEIL(X)                                        
C                                                                       
C  IDCEIL RETURNS CEIL(X)                                               
C                                                                       
      DOUBLE PRECISION X                                                
C                                                                       
      IDCEIL = IDINT(X)                                                 
      IF (X .LE. 0.0D0) RETURN                                          
      IF (DBLE(FLOAT(IDCEIL)) .NE. X) IDCEIL = IDCEIL + 1               
C                                                                       
      RETURN                                                            
      END                                                               
      REAL FUNCTION FLR(X)                                              
C                                                                       
C  FLR RETURNS FLR(X)                                                   
C                                                                       
      FLR = FLOAT( INT(X) )                                             
      IF (X .GE. 0.0) RETURN                                            
      IF (FLR .NE. X) FLR = FLR - 1.0                                   
C                                                                       
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DFLR(X)                                 
C                                                                       
C  DFLR RETURNS FLR(X)                                                  
C                                                                       
      DOUBLE PRECISION X                                                
C                                                                       
      DFLR = DBLE( FLOAT ( IDINT(X) ) )                                 
      IF (X .GE. 0.0D0) RETURN                                          
      IF (DFLR .NE. X) DFLR = DFLR - 1.0D0                              
C                                                                       
      RETURN                                                            
      END
	                                                               
      INTEGER FUNCTION IFLR(X)                                          
C                                                                       
C  IFLR RETURNS FLR(X)                                                  
C                                                                       
      IFLR = INT(X)                                                     
      IF (X .GE. 0.0) RETURN                                            
      IF (FLOAT(IFLR) .NE. X) IFLR = IFLR - 1                           
C                                                                       
      RETURN                                                            
      END
	                                                               
      INTEGER FUNCTION IDFLR(X)                                         
C                                                                       
C  IDFLR RETURNS FLR(X)                                                 
C                                                                       
      DOUBLE PRECISION X                                                
C                                                                       
      IDFLR = IDINT(X)                                                  
      IF (X .GE. 0.0D0) RETURN                                          
      IF (DBLE(FLOAT(IDFLR)) .NE. X) IDFLR = IDFLR - 1                  
C                                                                       
      RETURN                                                            
      END
	                                                               
      SUBROUTINE EPRINT                                                 
C                                                                       
C  THIS SUBROUTINE PRINTS THE LAST ERROR MESSAGE, IF ANY.               
C                                                                       
C/6S                                                                    
C     INTEGER MESSG(1)                                                  
C/7S                                                                    
      CHARACTER*1 MESSG(1)                                              
C/                                                                      
C                                                                       
      CALL E9RINT(MESSG,1,1,.FALSE.)                                    
      RETURN                                                            
C                                                                       
      END
	                                                               
      SUBROUTINE E9RINT(MESSG,NW,NERR,SAVE)                             
C                                                                       
C  THIS ROUTINE STORES THE CURRENT ERROR MESSAGE OR PRINTS THE OLD ONE, 
C  IF ANY, DEPENDING ON WHETHER OR NOT SAVE = .TRUE. .                  
C                                                                       
C  CHANGED, BY P.FOX, MAY 18, 1983, FROM THE ORIGINAL VERSION IN ORDER  
C  TO GET RID OF THE FORTRAN CARRIAGE CONTROL LINE OVERWRITE            
C  CHARACTER +, WHICH HAS ALWAYS CAUSED TROUBLE.                        
C  FOR THE RECORD, THE PREVIOUS VERSION HAD THE FOLLOWING ARRAY         
C  AND CALLS -   (WHERE CCPLUS WAS DECLARED OF TYPE INTEGER)            
C                                                                       
C      DATA CCPLUS  / 1H+ /                                             
C                                                                       
C      DATA FMT( 1) / 1H( /                                             
C      DATA FMT( 2) / 1HA /                                             
C      DATA FMT( 3) / 1H1 /                                             
C      DATA FMT( 4) / 1H, /                                             
C      DATA FMT( 5) / 1H1 /                                             
C      DATA FMT( 6) / 1H4 /                                             
C      DATA FMT( 7) / 1HX /                                             
C      DATA FMT( 8) / 1H, /                                             
C      DATA FMT( 9) / 1H7 /                                             
C      DATA FMT(10) / 1H2 /                                             
C      DATA FMT(11) / 1HA /                                             
C      DATA FMT(12) / 1HX /                                             
C      DATA FMT(13) / 1HX /                                             
C      DATA FMT(14) / 1H) /                                             
C                                                                       
C        CALL S88FMT(2,I1MACH(6),FMT(12))                               
C        WRITE(IWUNIT,FMT) CCPLUS,(MESSGP(I),I=1,NWP)                   
C                                                                       
C/6S                                                                    
C     INTEGER MESSG(NW)                                                 
C/7S                                                                    
      CHARACTER*1 MESSG(NW)                                             
C/                                                                      
      LOGICAL SAVE                                                      
C                                                                       
C  MESSGP STORES AT LEAST THE FIRST 72 CHARACTERS OF THE PREVIOUS       
C  MESSAGE. ITS LENGTH IS MACHINE DEPENDENT AND MUST BE AT LEAST        
C                                                                       
C       1 + 71/(THE NUMBER OF CHARACTERS STORED PER INTEGER WORD).      
C                                                                       
C/6S                                                                    
C     INTEGER MESSGP(36),FMT(10), FMT10(10)                             
C     EQUIVALENCE (FMT(1),FMT10(1))                                     
C/7S                                                                    
      CHARACTER*1 MESSGP(72),FMT(10)                                    
      CHARACTER*10 FMT10                                                
      EQUIVALENCE (FMT(1),FMT10)                                        
C/                                                                      
C                                                                       
C  START WITH NO PREVIOUS MESSAGE.                                      
C                                                                       
C/6S                                                                    
C     DATA MESSGP(1)/1H1/, NWP/0/, NERRP/0/                             
C/7S                                                                    
      DATA MESSGP(1)/'1'/, NWP/0/, NERRP/0/                             
C/                                                                      
C                                                                       
C  SET UP THE FORMAT FOR PRINTING THE ERROR MESSAGE.                    
C  THE FORMAT IS SIMPLY (A1,14X,72AXX) WHERE XX=I1MACH(6) IS THE        
C  NUMBER OF CHARACTERS STORED PER INTEGER WORD.                        
C                                                                       
C/6S                                                                    
C     DATA FMT( 1) / 1H( /                                              
C     DATA FMT( 2) / 1H3 /                                              
C     DATA FMT( 3) / 1HX /                                              
C     DATA FMT( 4) / 1H, /                                              
C     DATA FMT( 5) / 1H7 /                                              
C     DATA FMT( 6) / 1H2 /                                              
C     DATA FMT( 7) / 1HA /                                              
C     DATA FMT( 8) / 1HX /                                              
C     DATA FMT( 9) / 1HX /                                              
C     DATA FMT(10) / 1H) /                                              
C/7S                                                                    
      DATA FMT( 1) / '(' /                                              
      DATA FMT( 2) / '3' /                                              
      DATA FMT( 3) / 'X' /                                              
      DATA FMT( 4) / ',' /                                              
      DATA FMT( 5) / '7' /                                              
      DATA FMT( 6) / '2' /                                              
      DATA FMT( 7) / 'A' /                                              
      DATA FMT( 8) / 'X' /                                              
      DATA FMT( 9) / 'X' /                                              
      DATA FMT(10) / ')' /                                              
C/                                                                      
C                                                                       
      IF (.NOT.SAVE) GO TO 20                                           
C                                                                       
C  SAVE THE MESSAGE.                                                    
C                                                                       
        NWP=NW                                                          
        NERRP=NERR                                                      
        DO 10 I=1,NW                                                    
 10     MESSGP(I)=MESSG(I)                                              
C                                                                       
        GO TO 30                                                        
C                                                                       
 20   IF (I8SAVE(1,0,.FALSE.).EQ.0) GO TO 30                            
C                                                                       
C  PRINT THE MESSAGE.                                                   
C                                                                       
        IWUNIT=I1MACH(4)                                                
        WRITE(IWUNIT,9000) NERRP                                        
 9000   FORMAT(7H ERROR ,I4,4H IN )                                     
C                                                                       
        CALL S88FMT(2,I1MACH(6),FMT( 8))                                
        WRITE(IWUNIT,FMT10) (MESSGP(I),I=1,NWP)                         
C                                                                       
 30   RETURN                                                            
C                                                                       
      END
	                                                               
      INTEGER FUNCTION I8SAVE(ISW,IVALUE,SET)                           
C                                                                       
C  IF (ISW = 1) I8SAVE RETURNS THE CURRENT ERROR NUMBER AND             
C               SETS IT TO IVALUE IF SET = .TRUE. .                     
C                                                                       
C  IF (ISW = 2) I8SAVE RETURNS THE CURRENT RECOVERY SWITCH AND          
C               SETS IT TO IVALUE IF SET = .TRUE. .                     
C                                                                       
      LOGICAL SET                                                       
C                                                                       
      INTEGER IPARAM(2)                                                 
      EQUIVALENCE (IPARAM(1),LERROR) , (IPARAM(2),LRECOV)               
C                                                                       
C  START EXECUTION ERROR FREE AND WITH RECOVERY TURNED OFF.             
C                                                                       
      DATA LERROR/0/ , LRECOV/2/                                        
C                                                                       
      I8SAVE=IPARAM(ISW)                                                
      IF (SET) IPARAM(ISW)=IVALUE                                       
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE FDUMP                                                  
C  THIS IS A DUMMY ROUTINE TO BE SENT OUT ON                            
C  THE PORT SEDIT TAPE                                                  
C                                                                       
      RETURN                                                            
      END                                                               
C****END OF ROUTINES NEEDED FOR PORT 3 FRAMEWORK CHAPTER****************


