         SUBROUTINE CHKGOF
     G                     (NS    , DVAL
     Y                     ,DFLG  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE CHKGOF
C    G                     (NS    , DVAL
C    Y                     ,DFLG  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO DETERMINE IF THE CALCULATED KOLMOGOROV  DISTANCE IS SIGNIFI-
C        CANT
C   DESCRIPTION
C        THE ROUTINE PERFORMS A TABLE LOOK-UP (WITH LINEAR INTERPOLATION
C        IF NEEDED) FOR SAMPLE SIZES UNDER 50, OR COMPUTES THE VALUE US-
C        ING THE ASYMPTOTIC FORMULA FOR SAMPLE SIZES OVER 50.
C
C        THAT OBTAINED POINT OF SIGNIFICANCE (AT THE 0.05 LEVEL) IS THEN
C        COMPARED TO THE  GIVEN KOLMOGOROV DISTANCE  TO DETERMINE IF THE
C        MODEL SHOULD BE REJECTED.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             DVAL   (R) = CALCULATED KOLMOGOROV DISTANCE
C             NS     (I) = NUMBER OF TBF ENTRIES TO BE CREATED
C        YIELDED
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL, WHERE:
C                          1 - THE VALUE OF D IS LARGER  THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY NOT PROVIDE AN
C                              ADEQUATE FIT.
C                          2 - THE VALUE OF D IS SMALLER THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY PROVIDE AN AD-
C                              EQUATE FIT.
C   LOCAL GLOSSARY
C             INTPNT (I) = VECTOR OF SAMPLE SIZES UNDER 50
C             INTVAL (R) = VECTOR OF ASSOCIATED CRITICAL VALUES
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              DFLG          ,NS
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,INTPNT(25)
         DIMENSION            INTVAL(25)
C
C                             LOCAL ASSIGNMENTS
         DATA                 (INTPNT(I), I = 1, 25)
     1                        /    1,     2,     3,     4,     5
     2                        ,    6,     7,     8,     9,    10
     3                        ,   11,    12,    13,    14,    15
     4                        ,   16,    17,    18,    19,    20
     5                        ,   25,    30,    35,    40,    50/
         DATA                 (INTVAL(I), I = 1, 25)
     1                        /0.975, 0.842, 0.708, 0.624, 0.563
     2                        ,0.521, 0.486, 0.457, 0.432, 0.409
     3                        ,0.391, 0.375, 0.361, 0.349, 0.338
     4                        ,0.328, 0.318, 0.309, 0.301, 0.294
     5                        ,0.264, 0.242, 0.230, 0.210, 0.190/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         IF (NS.LE.50) THEN
C            DETERMINE THE INDEX TO THE SAMPLE SIZE WHICH IS IS EQUAL TO
C            OR LARGER THAN THE THE GIVEN SAMPLE SIZE.
             I = 0
C            DOUNTIL (INDEXED VALUE .GE. NS)
 1000            I = I + 1
                 IF (INTPNT(I).LT.NS) GO TO 1000
C            ENDUNTIL
C
             IF (INTPNT(I).EQ.NS) THEN
C                USE A STRAIGHT TABLE LOOK-UP AS SAMPLE SIZE WAS FOUND.
                 TMP = INTVAL(I)
             ELSE
C                INTERPOLATE TO FIND THE CRITICAL VALUE.
                 PT1 = REAL(NS        - INTPNT(I-1))
                 PT2 = REAL(INTPNT(I) - INTPNT(I-1))
                 TMP = INTVAL(I-1) + (INTVAL(I) - INTVAL(I-1))
     1               * (PT1 / PT2)
             ENDIF
         ELSE
C            USE THE ASYMPTOTIC FORMULA FOR SAMPLE SIZES OVER 50.
             TMP = 1.36 / SQRT(REAL(NS))
         ENDIF
C
         IF (DVAL.GE.TMP) THEN
C            THE MODEL MAY NOT PROVIDE AN ADEQUATE FIT.
             DFLG = 1
         ELSE
C            THE MODEL MAY PROVIDE AN ADEQUATE FIT.
             DFLG = 2
         ENDIF
         RETURN
         END
         SUBROUTINE CMPMAX
     G                     (NSR   , SRTFLG
     B                     ,DPMAX , V     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE CMPMAX
C    G                     (NSR   , SRTFLG
C    B                     ,DPMAX , V     )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO COMPUTE THE  KOLMOGOROV DISTANCE  FOR THE GIVEN VECTOR.  THE
C        ARGUMENT SRTFLG IS USED TO INDICATE WHETHER THE VECTOR REQUIRES
C        SORTING.
C   DESCRIPTION
C        THE ROUTINE FIRST SORTS THE DATA OF THE U-PLOT VECTOR. WHEN THE
C        ROUTINE IS ACCESSED WITH Y-PLOT DATA THE SORTING IS NOT NEEDED;
C        HENCE, THE ROUTINE IS ALWAYS ACCESSED WITH  SRTFLG=1 FOR U-PLOT
C        PROCESSING AND SRTFLG=0 FOR Y-PLOT PROCESSING.
C
C        THEN A LOOP IS INITIATED TO  COMPUTE THE VALUES,  AND DETERMINE
C        THE MAXIMUM DISTANCE.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             NSR    (I) = NUMBER OF POINTS FROM WHICH STATISTICS GOTTEN
C             SRTFLG (I) = FLAG CONTROLLING THE DATA SORT (1=SORT FIRST)
C        BOTH
C             DPMAX  (R) = CALCULATED KOLMOGOROV DISTANCE
C             V      (R) = ARRAY OF DATA FOR U-PLOT OR Y-PLOT
C   LOCAL GLOSSARY
C             MAX1   (R) = INTERMEDIATE CALCULATION 1
C             MAX2   (R) = INTERMEDIATE CALCULATION 2
C             TMP    (R) = TEMPORARY TO PERFORM THE SORT
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              NSR           ,SRTFLG
         DIMENSION            V(NSR)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,IP1           ,J
     1                       ,NSM1
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         IF (SRTFLG.EQ.1) THEN
C            SORT THE DATA ARRAY INTO  SMALLEST TO LARGEST ORDER FOR THE
C            U-PLOT.
             NSM1 = NSR - 1
             DO 1025 I = 1, NSM1
                 IP1 = I + 1
                 DO 1000 J = IP1, NSR
                     IF (V(J).LT.V(I)) THEN
                         TMP  = V(I)
                         V(I) = V(J)
                         V(J) = TMP
                     ENDIF
 1000                CONTINUE
C                ENDDO
 1025            CONTINUE
C            ENDDO
         ENDIF
C
C        COMPUTE THE TWO VALUES FOR PROCESSING.
         DO 1050 I = 1, NSR
             MAX1  = (REAL(I) / REAL(NSR)) - V(I)
             MAX2  = V(I) - ((REAL(I) - 1.0) / REAL(NSR))
C
C            REPLACE THE MAXIMUM IF INDICATED.
             DPMAX = MAX(DPMAX , MAX1  , MAX2  )
 1050        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE CONMIN
     G                     (G     , H     , N     , TMAX  , TMIN  , X
     Y                     ,Y     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE CONMIN
C    G                     (G     , H     , N     , TMAX  , TMIN  , X
C    Y                     ,Y     )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO FIND THE CONSTRAINED MINIMUM OF A  QUADRATIC FUNCTION WITHIN
C        A TRUST REGION.
C   DESCRIPTION
C        THIS ROUTINE FINDS THE CONSTRAINED MINIMUM, Y OF THE  QUADRATIC
C        FUNCTION   Q(Y) = G * (X - Y) + 0.5 * (Y - X) * H * (Y - X)  IN
C        THE REGION BOUNDED BY THE  TRUST REGION.  X CONTAINS THE VALUES
C        OF THE  PARAMETER  ESTIMATES,  G  CONTAINS THE  GRADIENT OF THE
C        MAXIMUM  LIKELIHOOD OR  LEAST SQUARES FUNCTION,  AND H CONTAINS
C        THE HESSIAN OF THE APPROPRIATE FUNCTION.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             G      (R) = GRADIENT OF ML OR LS FUNCTION
C             H      (R) = HESSIAN OF ML OR LS FUNCTION
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C             TMAX   (R) = TRUST REGION MAXIMUM
C             TMIN   (R) = TRUST REGION MINIMUM
C             X      (R) = PARAMETER ESTIMATE VALUES
C        YIELDED
C             Y      (R) = PARAMETER ESTIMATES AT CONSTRAINED MINIMUM
C   LOCAL GLOSSARY
C             GQ     (R) = GRADIENT OF THE QUADRATIC FUNCTION
C             ICONS  (I) = CONSTRAINT VECTOR
C             IFLAG  (I) = CONSTRAINT STATUS FLAG, WHERE 0 INDICATES THE
C                          CONSTRAINTS DO NOT EXIST AND 1 INDICATES THAT
C                          CONSTRAINTS DO EXIST
C             IRELES (I) = CONSTRAINT RELEASE FLAG,  WHERE  0  INDICATES
C                          CONSTRAINTS  CANNOT BE  RELEASED  AND 1 INDI-
C                          CATES THAT CONSTRAINTS CAN BE RELEASED
C             XTMP   (R) = INTERMEDIATE  STORAGE VECTOR  FOR CONSTRAINED
C                          POINT
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              N
         DIMENSION            G(3)          ,H(3,3)        ,TMAX(3)
     1                       ,TMIN(3)       ,X(3)          ,Y(3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,ICONS(3)      ,IFLAG
     1                       ,IRELES        ,J
         DIMENSION            GQ(3)         ,XTMP(3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE THE CONSTRAINT VECTOR  AND  SET THE CURRENT POINT TO
C        POINT X.
         DO 1000 I = 1, N
             ICONS(I) = 0
             Y(I)     = X(I)
 1000        CONTINUE
C        ENDDO
C
C        INITIALIZE THE CONSTRAINT RELEASE FLAG.
         IRELES = 1
C        DOWHILE (IRELES .EQ. 1)
C            INITIALIZE THE CONSTRAINT STATUS FLAG.
 1025        IFLAG = 1
C
C            DOWHILE (IFLAG .EQ. 1)
 1050            CONTINUE
C
C                SAVE THE CURRENT CONSTRAINED POINT.
                 DO 1075 I = 1, N
                     XTMP(I) = Y(I)
 1075                CONTINUE
C                ENDDO
C
C                FIND THE  CONSTRAINED MINIMUM ON THE SURFACE DEFINED BY
C                THE CONSTRAINT VECTOR.
                 CALL CONSOP
     G                       (G     , H     , ICONS , N     , TMAX
     G                       ,TMIN  , X
     B                       ,Y     )
C
C                CHECK TO SEE IF CONSTRAINTS  ARE  VIOLATED BY THIS  NEW
C                MINIMUM.
                 CALL CONSTR
     G                       (N     , TMAX  , TMIN  , XTMP
     B                       ,Y
     Y                       ,ICONS , IFLAG )
                 IF (IFLAG.EQ.1) GO TO 1050
C            ENDWHILE
C
C            COMPUTE THE GRADIENT OF THE QUADRATIC FUNCTION.
             DO 1125 I = 1, N
                 GQ(I) = G(I)
                 DO 1100 J = 1, N
                     GQ(I) = GQ(I) + H(I,J) * (Y(J) - X(J))
 1100                CONTINUE
C                ENDDO
 1125            CONTINUE
C            ENDDO
C
C            CHECK TO SEE IF CONSTRAINTS CAN BE RELEASED.
             CALL RELESE
     G                   (GQ    , N
     B                   ,ICONS
     Y                   ,IRELES)
             IF (IRELES.EQ.1) GO TO 1025
C        ENDWHILE
         RETURN
         END
         SUBROUTINE CONSOP
     G                   (G     , H     , ICONS , N     , TMAX  , TMIN
     G                   ,X
     B                   ,Y     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE CONSOP
C    G                   (G     , H     , ICONS , N     , TMAX  , TMIN
C    G                   ,X
C    B                   ,Y     )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO FIND THE MINIMUM OF THE  QUADRATIC FUNCTION  ON THE  SURFACE
C        DEFINED BY THE CONSTRAINT VECTOR.
C   DESCRIPTION
C        THIS ROUTINE UTILIZES THE CONSTRAINT VECTOR TO DETERMINE IF ANY
C        CONSTRAINTS  EXIST.  IF NOT, THE MINIMUM OF THE QUADRATIC FUNC-
C        TION IS SET TO THE COMPUTED NEWTON POINT. IF CONSTRAINTS EXIST,
C        THE CONSTRAINED MINIMUM OF THE QUADRATIC FUNCTION IS COMPUTED.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS  SUBPROGRAM  CONTAINS  CODE WHICH DOES NOT COMPLY WITH THE
C        PROGRAMMING STANDARDS; GROUP LEADER APPROVAL HAS BEEN OBTAINED.
C       -SPECIFICALLY:
C        THE ARGUMENT Y  IS ACTUALLY INITIALIZED WITHIN THIS ROUTINE, IF
C        IFLAG IS SET TO ONE; OTHERWISE, IT IS A "BOTH" AS DEFINED.
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             G      (R) = GRADIENT
C             H      (R) = HESSIAN
C             ICONS  (I) = CONSTRAINT VECTOR
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C             TMAX   (R) = TRUST REGION MAXIMUM
C             TMIN   (R) = TRUST REGION MINIMUM
C             X      (R) = VECTOR OF PARAMETER ESTIMATES
C        BOTH
C             Y      (R) = NEWTON  POINT OR CONSTRAINED  MINIMUM  OF THE
C                          QUADRATIC FUNCTION
C   LOCAL GLOSSARY
C             DXCONS (R) = STEP VECTOR
C             HTMP   (R) = INTERMEDIATE HESSIAN
C             IFLAG  (I) = CONSTRAINT STATUS FLAG, WHERE 0 INDICATES THE
C                          CONSTRAINTS DO NOT EXIST AND 1 INDICATES THAT
C                          CONSTRAINTS DO EXIST
C             SM     (R) = TEMPORARY SUMMATION
C             TMP1   (R) = INTERMEDIATE CALCULATION 1
C             TMP2   (R) = INTERMEDIATE CALCULATION 2
C             TMP3   (R) = INTERMEDIATE CALCULATION 3
C             U      (R) = UPPER TRIANGULAR FACTORS OF H-MATRIX
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              ICONS(3)      ,N
         DIMENSION            G(3)          ,H(3,3)        ,TMAX(3)
     1                       ,TMIN(3)       ,X(3)          ,Y(3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,IFLAG         ,J
         DIMENSION            DXCONS(3)     ,HTMP(3,3)     ,TMP1(3)
     1                       ,TMP2(3)       ,TMP3(3)       ,U(3,3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        DETERMINE IF ANY CONSTRAINTS EXIST.
         IFLAG = 0
         DO 1000 I = 1, N
             IF (ICONS(I).NE.0) THEN
                 IFLAG = 1
             ENDIF
 1000        CONTINUE
C        ENDDO
C
         IF (IFLAG.EQ.0) THEN
C            CONSTRAINTS DO NOT EXIST.  SET  MINIMUM  OF  THE  QUADRATIC
C            FUNCTION TO THE COMPUTED NEWTON POINT.<<SEE RESTRICTIONS <<
             CALL FACTOR
     G                   (H     , N
     Y                   ,U     )
             CALL SOLVE
     G                   (N     , U     , G
     Y                   ,TMP1  )
             DO 1025 I = 1, N
                 Y(I) = X(I) - TMP1(I)
 1025            CONTINUE
C            ENDDO
         ELSE
C            CONSTRAINTS DO EXIST. STORE  THE HESSIAN IN AN INTERMEDIATE
C            MATRIX.
             DO 1075 I = 1, N
                 DO 1050 J = 1, N
                     HTMP(I,J) = H(I,J)
 1050                CONTINUE
C                ENDDO
 1075            CONTINUE
C            ENDDO
C
C            DETERMINE THE STEP TO THE BOUNDARY.
             DO 1100 I = 1, N
                 IF (ICONS(I).EQ.0) THEN
                     DXCONS(I) = 0.0
                 ELSE
                     DXCONS(I) = Y(I) - X(I)
                 ENDIF
 1100            CONTINUE
C            ENDDO
C
C            COMPUTE THE PRODUCT OF THE HESSIAN AND THE STEP VECTOR.
             DO 1150 I = 1, N
                 SM = 0.0
                 DO 1125 J = 1, N
                     SM = SM + H(I,J) * DXCONS(J)
 1125                CONTINUE
C                ENDDO
                 TMP1(I) = SM
 1150            CONTINUE
C            ENDDO
C
C            ADJUST THE GRADIENT FOR ALL  CONSTRAINED  ELEMENTS  OF  THE
C            CONSTRAINT VECTOR.
             DO 1175 I = 1, N
                 IF (ICONS(I).NE.0) THEN
                     TMP2(I) = 0.0
                 ELSE
                     TMP2(I) = G(I) + TMP1(I)
                 ENDIF
 1175            CONTINUE
C            ENDDO
C
C            ADJUST THE HESSIAN FOR  ALL  CONSTRAINED  ELEMENTS  OF  THE
C            CONSTRAINT VECTOR.
             DO 1225 I = 1, N
                 IF (ICONS(I).NE.0) THEN
                     DO 1200 J = 1, N
                         HTMP(I,J) = 0.0
                         HTMP(J,I) = 0.0
 1200                    CONTINUE
C                    ENDDO
                     HTMP(I,I) = 1.0E30
                 ENDIF
 1225            CONTINUE
C            ENDDO
C
C            COMPUTE THE CONSTRAINED MINIMUM OF THE QUADRATIC FUNCTION.
             CALL FACTOR
     G                   (HTMP  , N
     Y                   ,U     )
             CALL SOLVE
     G                   (N     , U     , TMP2
     Y                   ,TMP3  )
             DO 1250 I = 1, N
                 IF (ICONS(I).EQ.1) THEN
                     Y(I) = TMAX(I)
                 ELSE
                     IF (ICONS(I).EQ.-1) THEN
                         Y(I) = TMIN(I)
                     ELSE
                         Y(I) = X(I) - TMP3(I)
                     ENDIF
                 ENDIF
 1250            CONTINUE
C            ENDDO
         ENDIF
         RETURN
         END
         SUBROUTINE CONSTR
     G                     (N     , TMAX  , TMIN  , X
     B                     ,Y
     Y                     ,ICONS , IFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE CONSTR
C    G                     (N     , TMAX  , TMIN  , X
C    B                     ,Y
C    Y                     ,ICONS , IFLAG )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CONSTRAIN THE POINT Y TO BE ON THE LINE FROM X  TO  Y INSIDE
C        OR ON THE BOUNDARIES OF THE TRUST REGION.
C   DESCRIPTION
C        THIS ROUTINE UTILIZES THE POINTS X AND Y  AND THE  TRUST REGION
C        BOUNDARIES TO DETERMINE IF  THE POINT Y  SHOULD BE  CONSTRAINED
C        TO BE  ON A LINE FROM X TO Y INSIDE OR ON THE BOUNDARIES OF THE
C        TRUST REGION. IF SO, THE CONSTRAINT STATUS FLAG IS SET TO INDI-
C        CATE CONSTRAINTS  EXIST,  THE COMPONENTS OF Y ARE ADJUSTED, AND
C        THE CONSTRAINT VECTOR IS SET TO INDICATE THE COMPONENT OF POINT
C        Y WHICH HAS VIOLATED ITS TRUST REGION  BOUNDARIES BY THE GREAT-
C        EST  MARGIN.  IF NOT, THE  CONSTRAINT  STATUS  FLAG IS  SET  TO
C        INDICATE CONSTRAINTS DO NOT EXIST.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C             TMAX   (R) = TRUST REGION MAXIMUM
C             TMIN   (R) = TRUST REGION MINIMUM
C             X      (R) = VECTOR OF PARAMETER ESTIMATES
C        BOTH
C             Y      (R) = NEWTON POINT
C        YIELDED
C             ICONS  (I) = CONSTRAINT VECTOR
C             IFLAG  (I) = CONSTRAINT STATUS FLAG, WHERE 0 INDICATES THE
C                          CONSTRAINTS DO NOT EXIST AND 1 INDICATES THAT
C                          CONSTRAINTS DO EXIST
C   LOCAL GLOSSARY
C             COMPY  (I) = COMPONENT OF POINT Y  WHICH HAS  VIOLATED ITS
C                          CONSTRAINTS BY THE GREATEST MARGIN
C             DELP   (R) = DIFFERENCE BETWEEN POINTS X AND Y
C             NEWT   (R) = NEWTON POINT ADJUSTMENT
C             TMPNEW (R) = INTERMEDIATE NEWTON POINT ADJUSTMENT
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              ICONS(3)      ,IFLAG         ,N
         DIMENSION            TMAX(3)       ,TMIN(3)       ,X(3)
     1                       ,Y(3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              COMPY         ,I
         DIMENSION            DELP(3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE THE NEWTON POINT ADJUSTMENT.
         NEWT = 1.0
C
C        DETERMINE IF THE COMPONENTS OF POINT Y HAVE TO BE CONSTRAINED.
         DO 1000 I = 1, N
             DELP(I) = Y(I) - X(I)
             IF (DELP(I).NE.0.0) THEN
                 IF (DELP(I).LT.0.0) THEN
                     TMPNEW = (TMIN(I) - X(I)) / DELP(I)
                 ELSE
                     TMPNEW = (TMAX(I) - X(I)) / DELP(I)
                 ENDIF
                 IF (TMPNEW.LE.NEWT) THEN
                     NEWT  = TMPNEW
                     COMPY = I
                 ENDIF
             ENDIF
 1000        CONTINUE
C        ENDDO
C
         IF (NEWT.NE.1.0) THEN
C            SET THE CONSTRAINTS STATUS  FLAG  TO  INDICATE  CONSTRAINTS
C            DO EXIST.
             IFLAG = 1
C
C            ADJUST THE COMPONENTS  OF Y BY THE NEWTON POINT ADJUSTMENT.
             DO 1025 I = 1, N
                 Y(I) = X(I) + NEWT * DELP(I)
 1025            CONTINUE
C            ENDDO
C
C            SET THE CONSTRAINT VECTOR FOR THE COMPONENT OF Y WHICH  HAS
C            VIOLATED ITS TRUST REGION BOUNDARIES BY THE  GREATEST  MAR-
C            GIN.
             IF (DELP(COMPY).LT.0.0) THEN
                 ICONS(COMPY) = -1
             ELSE
                 ICONS(COMPY) =  1
             ENDIF
         ELSE
C            SET THE CONSTRAINT STATUS FLAG TO  INDICATE  CONSTRAINTS DO
C            NOT EXIST.
             IFLAG = 0
         ENDIF
         RETURN
         END
         FUNCTION D1MACH
     G                   ()
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION D1MACH
C    G                   ()
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO FIND THE RELATIVE PRECISION OF  FLOATING  POINT  ARITHMETIC.
C   DESCRIPTION
C        THIS FUNCTION COMPUTES THE RELATIVE MACHINE PRECISION OF FLOAT-
C        ING POINT ARITHMETIC FOR THE MACHINE ON WHICH SMERFS  IS  BEING
C        RUN.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        YIELDED
C             D1MACH (R) = RELATIVE MACHINE PRECISION
C   LOCAL GLOSSARY
C             EPS    (R) = TEMPORARY RELATIVE MACHINE PRECISION
C             TOL    (R) = CONSTANT PLUS RELATIVE MACHINE PRECISION
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
C        (NONE)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        COMPUTE RELATIVE MACHINE PRECISION.
         EPS=1.0
C
C        DOWHILE (TOL .GT. 1.0)
 1000        D1MACH = EPS
             EPS    = EPS / 2.0
             TOL    = 1.0 + EPS
             IF (TOL.GT.1.0) GO TO 1000
C        ENDWHILE
         RETURN
         END
         SUBROUTINE EGNVAL
     G                     (NM    , N     , A     , FV1   , FV2
     Y                     ,W     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE EGNVAL
C    G                     (NM    , N     , A     , FV1   , FV2
C    Y                     ,W     )
C
C   AUTHOR
C        B. S. GARBOW (APPLIED MATH. DIVISION, ARGONNE NATIONAL LAB.)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE  THE  EIGENVALUES  OF  A  REAL  SYMMETRIC  MATRIX.
C   DESCRIPTION
C        THIS ROUTINE CALLS THE RECOMMENDED SEQUENCE OF ROUTINES TO FIND
C        THE EIGENVALUES OF A REAL SYMMETRIC MATRIX.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             A      (R) = CONTAINS THE REAL SYMMETRIC ARRAY
C             FV1    (R) = DUMMY STORAGE ARRAY 1
C             FV2    (R) = DUMMY STORAGE ARRAY 2
C             N      (I) = THE ORDER OF MATRIX A
C             NM     (I) = ROW DIMENSION OF THE 2-DIMENSIONAL ARRAY PAR-
C                          AMETER AS DECLARED IN THE CALLING ROUTINE
C        YIELDED
C             W      (R) = CONTAINS THE EIGENVALUES IN ASCENDING ORDER
C   LOCAL GLOSSARY
C             (NONE)
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              N             ,NM
         DIMENSION            A(NM,N)       ,FV1(N)        ,FV2(N)
     1                       ,W(N)
C
C                             LOCAL SPECIFICATIONS
C        (NONE)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        FIND EIGENVALUES.
         CALL TRED1
     G               (NM    , N
     B               ,A
     Y               ,W     , FV1   , FV2   )
         CALL TQLRAT
     G               (N
     B               ,FV2   , W     )
         RETURN
         END
         SUBROUTINE FACTOR
     G                     (H     , N
     Y                     ,U     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE FACTOR
C    G                     (H     , N
C    Y                     ,U     )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO COMPUTE THE UPPER TRIANGULAR FACTORS OF A SYMMETRIC POSITIVE
C        DEFINITE MATRIX.
C   DESCRIPTION
C        THIS ROUTINE UTILIZES THE VALUES FROM  THE  SYMMETRIC  POSITIVE
C        DEFINITE MATRIX-H TO COMPUTE THE UPPER TRIANGULAR FACTORS-U.
C
C        THIS ROUTINE ASSUMES THAT THE INPUT MATRIX-H IS  POSITIVE  DEF-
C        INITE.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THE INPUT MATRIX-H IS POSITIVE DEFINITE.
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             H      (R) = SYMMETRIC POSITIVE DEFINITE MATRIX
C             N      (I) = ROW AND COLUMN DIMENSION OF H
C        YIELDED
C             U      (R) = UPPER TRIANGULAR FACTORS
C   LOCAL GLOSSARY
C             ALPHA  (R) = ALPHA VALUE
C             BETA   (R) = BETA VALUE
C             HTMP   (R) = INTERMEDIATE MATRIX FOR H
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              N
         DIMENSION            H(3,3)        ,U(3,3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J             ,K
         DIMENSION            HTMP(3,3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        STORE THE SYMMETRIC POSITIVE DEFINITE MATRIX-H IN HTMP.
         DO 1025 I = 1, N
             DO 1000 J = 1, N
                 HTMP(I,J) = H(I,J)
 1000            CONTINUE
C            ENDDO
 1025        CONTINUE
C        ENDDO
C
C        COMPUTE THE UPPER TRIANGULAR FACTORS OF MATRIX-H AND  STORE  IN
C        THE UPPER TRIANGULAR PORTION OF MATRIX-U.
         DO 1100 I = N, 2, -1
             U(I,I) = SQRT(HTMP(I,I))
             ALPHA  = 1.0 / U(I,I)
             DO 1075 J = 1, I - 1
                 U(J,I) = ALPHA * HTMP(J,I)
                 BETA   = U(J,I)
                 DO 1050 K = 1, J
                     HTMP(K,J) = HTMP(K,J) - BETA * U(K,I)
 1050                CONTINUE
C                ENDDO
 1075            CONTINUE
C            ENDDO
 1100        CONTINUE
C        ENDDO
C
C        COMPUTE THE  UPPER  TRIANGULAR  FACTOR OF  THE FIRST ELEMENT OF
C        MATRIX-H.
         U(1,1) = SQRT(HTMP(1,1))
         RETURN
         END
         SUBROUTINE RELESE
     G                      (G     , N
     B                      ,ICONS
     Y                      ,IRELES)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE RELESE
C    G                      (G     , N
C    B                      ,ICONS
C    Y                      ,IRELES)
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CHECK THE GRADIENT TO DETERMINE IF ANY OF THE CONSTRAINTS ON
C        THE CONSTRAINT SURFACE CAN BE RELEASED.
C   DESCRIPTION
C        THIS ROUTINE UTILIZES THE GRADIENT OF  THE  QUADRATIC  FUNCTION
C        AND  THE  CONSTRAINT  VECTOR  TO  DETERMINE  IF ANY OF THE CON-
C        STRAINTS ON THE CONSTRAINT SURFACE CAN BE  RELEASED. IF SO, THE
C        CONSTRAINT VECTOR IS ADJUSTED AND THE  CONSTRAINT  RELEASE FLAG
C        IS SET TO INDICATE THAT CONSTRAINTS WERE RELEASED.  IF NOT, THE
C        CONSTRAINT RELEASE FLAG IS SET TO INDICATE THAT NO  CONSTRAINTS
C        WERE RELEASED. THIS MEANS  THAT THE  CONSTRAINED MINIMUM ON THE
C        CONSTRAINT SURFACE HAS BEEN FOUND.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             G      (R) = GRADIENT OF THE QUADRATIC FUNCTION
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C        BOTH
C             ICONS  (I) = CONSTRAINT VECTOR
C        YIELDED
C             IRELES (I) = CONSTRAINT RELEASE FLAG,  WHERE  0  INDICATES
C                          CONSTRAINTS  CANNOT BE  RELEASED  AND 1 INDI-
C                          CATES THAT CONSTRAINTS CAN BE RELEASED
C   LOCAL GLOSSARY
C             (NONE)
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              N             ,ICONS(3)      ,IRELES
         DIMENSION            G(3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE THE CONSTRAINT RELEASE FLAG.
         IRELES = 0
C
C        DETERMINE IF ANY CONSTRAINTS CAN BE RELEASED.
         DO 1000 I = 1, N
             IF (((ICONS(I).EQ.-1) .AND.(G(I).LT.0.0))  .OR.
     1           ((ICONS(I).EQ. 1) .AND.(G(I).GT.0.0))) THEN
                 ICONS(I) = 0
                 IRELES   = 1
             ENDIF
 1000        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE SMFDST
     G                     (NS    , NST
     B                     ,DAT
     Y                     ,STATS )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMFDST
C    G                     (NS    , NST
C    B                     ,DAT
C    Y                     ,STATS )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H, FARR (B10, NSWCDD)
C   PURPOSE
C        TO GENERATE VECTOR OF LENGTH  6 OR 11 CONTAINING THE STATISTICS
C        OF THE GIVEN VECTOR'S DATA.
C   DESCRIPTION
C        THE STATISTICS ROUTINE WILL  GENERATE VALUES FOR THE  SUM, MED-
C        IAN, HINGES, MAXIMUM AND MINIMUM STATISTICS.  ADDITIONALLY, THE
C        ROUTINE CAN BE USED TO COMPUTE THE MEAN, VARIANCE, STANDARD DE-
C        VIATION, SKEWNESS, AND KURTOSIS (BY SETTING THE NST ARGUMENT OF
C        THE CALL LINE TO 11).
C
C        THE DATA VECTOR IS SORTED IN THIS ROUTINE; THEREFORE, IT SHOULD
C        BE STORED PRIOR TO THE ROUTINE ACCESS.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             NS     (I) = NUMBER OF ENTRIES IN THE VECTOR
C             NST    (I) = NUMBER OF STATISTICS DESIRED
C        BOTH
C             DAT    (R) = THE DATA VECTOR
C        YIELDED
C             STATS  (R) = VECTOR OF COMPUTED STATISTICS
C   LOCAL GLOSSARY
C             NSM1   (I) = NS MINUS 1
C             SE1    (R) = SUM OF DATA
C             SE2    (R) = SUM OF DATA WITH EXPONENT 2
C             SE3    (R) = SUM OF DATA WITH EXPONENT 3
C             SE4    (R) = SUM OF DATA WITH EXPONENT 4
C             RN     (R) = REAL VARIABLE STORAGE OF NS
C             TMP    (R) = INTERMEDIATE CALCULATION
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              NS            ,NST
         DIMENSION            DAT(NS)       ,STATS(NST)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J             ,K
     1                       ,L             ,M             ,NSM1
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        ZERO THE VARIABLE USED FOR THE DATA SUMMATIONS.
         SE1 = 0.0
         SE2 = 0.0
         SE3 = 0.0
         SE4 = 0.0
C
C        COMPUTE THE NEEDED  SUMMATIONS.  NOTE THAT  SE2 THROUGH SE4 ARE
C        ONLY NEEDED FOR THE EXPANDED STATISTICS.
         DO 1000 I = 1, NS
             SE1 = SE1 + DAT(I)
             IF (NST.EQ.11) THEN
                 SE2 = SE2 + DAT(I)**2
                 SE3 = SE3 + DAT(I)**3
                 SE4 = SE4 + DAT(I)**4
             ENDIF
 1000        CONTINUE
C        ENDDO
C
C        PERFORM THE SORTING OF THE DATA VECTOR.
         NSM1 = NS - 1
         DO 1050 I = 1, NSM1
             M = I + 1
             DO 1025 J = M, NS
                 IF (DAT(I).GT.DAT(J)) THEN
                     TMP    = DAT(I)
                     DAT(I) = DAT(J)
                     DAT(J) = TMP
                 ENDIF
 1025            CONTINUE
C            ENDDO
 1050        CONTINUE
C        ENDDO
C
C        BEGIN THE FILLING OF THE STATS VECTOR WITH THE SUM OF DATA.
         STATS(1) = SE1
C
C        MEDIAN
         I        = (NS / 2) + 1
         J        = NS + 1 - I
         STATS(2) = (DAT(I) + DAT(J)) / 2.0
C
C        HINGES
         K        = (NS + 1) / 2
         I        = K / 2 + 1
         J        = K + 1 - I
         STATS(3) = (DAT(I) + DAT(J)) / 2.0
         L        = NS - K + I
         M        = NS + 1 - I
         STATS(4) = (DAT(L) + DAT(M)) / 2.0
C
C        MINIMUM AND MAXIMUM
         STATS(5) = DAT(1)
         STATS(6) = DAT(NS)
C
         IF (NST.EQ.11) THEN
C            THE OTHER FIVE  STATISTICS ARE ALSO DESIRED; START BY STOR-
C            ING NS IN THE REAL LOCATION.
             RN = NS
C
C            MEAN
             STATS(7) = SE1 / RN
C
C            VARIANCE AND STANDARD DEVIATION
             STATS(9) = (SE2 - ((SE1**2) / RN)) / (RN - 1.0)
             STATS(8) = SQRT(STATS(9))
C
C            SKEWNESS
             TMP       = SE3 - (3.0 / RN) * SE1 * SE2 + (2.0 / RN**2)
     1                 * SE1**3
             STATS(10) = SQRT(RN) * TMP / (SQRT(SE2 - SE1**2 / RN)**3)
C
C            KURTOSIS
             TMP       = SE4 - (4.0 / RN) * SE1 * SE3 + (6.0 / RN**2)
     1                 * SE1**2 * SE2 - (3.0 / RN**3) * SE1**4
             STATS(11) = RN * TMP / ((SE2 - SE1**2 / RN)**2) - 3.0
         ENDIF
         RETURN
         END
         SUBROUTINE SMFEDT
     G                     (PRC   , ADA   , ASZ   , DSZ
     B                     ,DT1   , DT2   , USZ
     Y                     ,ERRFLG)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMFEDT
C    G                     (PRC   , ADA   , ASZ   , DSZ
C    B                     ,DT1   , DT2   , USZ
C    Y                     ,ERRFLG)
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM THE DATA EDIT  (AS INDICATED IN THE FIRST ELEMENT OF
C        THE ARGUMENT PRC)  OF THE PASSED DATA VECTORS DT1 AND DT2.  THE
C        EDITING OF THE SECOND DATA VECTOR IS OPTIONAL AND IS CONTROLLED
C        BY THE VALUE OF THE SECOND ELEMENT OF THE PRC VECTOR.
C   DESCRIPTION
C        THIS ONE SMFLIB  ROUTINE CAN BE UTILIZED  TO PERFORM FOUR TYPES
C        OF DATA EDITING  (OVER 1 OR 2  GIVEN DATA VECTORS).  BECAUSE OF
C        THIS FLEXIBILITY WITHIN  THE ROUTINE, CERTAIN  ARGUMENTS OF THE
C        CALL LINE TAKE ON DIFFERENT MEANINGS DEPENDING ON THE EDIT TYPE
C        BEING PERFORMED.
C
C        THE PRC VECTOR CONTAINS: (1) THE EDIT TYPE (CHANGE, DELETE, IN-
C        SERT, OR COMBINE), (2) THE NUMBER OF VECTORS TO BE ALTERED, (3)
C        THE STARTING  ADDRESS, AND (4) THE  ENDING ADDRESS.  THE FOURTH
C        ELEMENT WILL BE IN THE FORM OF THE NUMBER OF ENTRIES BEING ADD-
C        ED IF AN INSERTION TYPE EDIT IS BEING PERFORMED.
C
C        THE ADA ARRAY  (OF SIZE 2 BY ASZ)  CONTAINS THE NEW DATA, WHERE
C        THE FIRST ROW IS FOR DT1 AND THE SECOND ROW IS FOR DT2. THE TWO
C        DATA VECTORS MUST BOTH BE OF SIZE DSZ.  NOTE THAT THIS ARRAY IS
C        UNUSED FOR DELETION AND COMBINATION TYPE EDITS.
C
C        THE USZ ARGUMENT  WILL BE CHANGED  TO REFLECT THE NUMBER OF EN-
C        TRIES AFTER THE EDIT.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             ADA    (R) = ALTERATION DATA ARRAY
C             ASZ    (I) = ROW DIMENSION OF ADA
C             DSZ    (I) = DIMENSION OF THE DT1 & DT2 VECTORS
C             PRC    (I) = PROCESSING INFORMATION
C        BOTH
C             DT1    (R) = FIRST DATA VECTOR
C             DT2    (R) = SECOND DATA VECTOR
C             USZ    (I) = USED (ASSIGNED) ELEMENTS IN DT1 & DT2
C        YIELDED
C             ERRFLG (I) = EDIT ERROR FLAG
C   LOCAL GLOSSARY
C             FWAP1  (I) = FIRST WORD ADDRESS PLUS ONE
C             NUMB   (I) = NUMBER TO INSERT OR DELETE (FOR SMFSHF)
C   ERRORS
C        ERRFLG = 1 : EDIT EXCEEDS THE BOUNDS OF THE CURRENT DATA
C        ERRFLG = 2 : INSERTION WILL EXCEED ARRAY SIZE
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              ASZ           ,DSZ           ,ERRFLG
     1                       ,PRC(4)        ,USZ
         DIMENSION            ADA(2,ASZ)    ,DT1(DSZ)      ,DT2(DSZ)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              FWAP1         ,I             ,J
     1                       ,K             ,NUMB
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE ERROR FLAG TO NON-ERRORED STATUS.
         ERRFLG = 0
C
C        STORE THE EDIT  TYPE FLAG IN THE  VARIABLE I,  AND TEST FOR THE
C        POSSIBILITY THAT THE  ARRAY INDEX VALUES  FROM THE CALL LINE DO
C        NOT AGREE WITH THE AMOUNT OF DATA IN DT1 AND DT2.
         I = PRC(1)
         IF (((I.EQ.1) .AND. ((PRC(3).LT.1) .OR. (PRC(3).GT.USZ)))  .OR.
     1       ((I.EQ.2) .AND. ((PRC(3).LT.1) .OR. (PRC(4).GT.USZ)))  .OR.
     2       ((I.EQ.3) .AND. ((PRC(3).LT.0) .OR. (PRC(3).GT.USZ)))  .OR.
     3       ((I.EQ.4) .AND. ((PRC(3).LT.1) .OR. (PRC(4).GT.USZ)))) THEN
             ERRFLG = 1
         ENDIF
C
         IF (ERRFLG.EQ.0) THEN
C            CASE ENTRY
C
                 GO TO (1000, 1025, 1050, 1100), I
C
C                CASE 1 - CHANGE DATA ELEMENT
 1000                I = PRC(3)
C                    WITH THE ADDRESS  STORED IN I,  UPDATE THE DT1 ELE-
C                    MENT; AND FOR DT2, IF BOTH VECTORS ARE SPECIFIED.
                     DT1(I) = ADA(1,1)
                     IF (PRC(2).EQ.2) THEN
                         DT2(I) = ADA(2,1)
                     ENDIF
                     GO TO 1150
C
C                CASE 2 - DELETE SPECIFIED DATA ELEMENT(S)
 1025                NUMB = 0 - (PRC(4) - PRC(3) + 1)
C                    WITH THE NUMBER OF ELEMENTS TO BE DELETED STORED IN
C                    THE NUMB  VARIABLE,  ACCESS THE  SMFSHF  ROUTINE TO
C                    PERFORM THE DATA SHIFTING. THE NEGATIVE OF NUMB IN-
C                    DICATES  DELETION,  AS OPPOSED TO  INSERTION. USING
C                    THE SAME ROUTINE REDUCE THE DT2 VECTOR IF BOTH VEC-
C                    TORS HAVE BEEN SPECIFIED.
                     CALL SMFSHF
     G                           (PRC(3), NUMB  , DSZ   , USZ
     B                           ,DT1   )
                     IF (PRC(2).EQ.2) THEN
                         CALL SMFSHF
     G                               (PRC(3), NUMB  , DSZ   , USZ
     B                               ,DT2   )
                     ENDIF
                     USZ = USZ + NUMB
                     GO TO 1150
C
C                CASE 3 - INSERT SPECIFIED DATA ELEMENT(S).
 1050                I = USZ + ASZ
                     IF (I.GT.DSZ) THEN
C                        THE INDICATED INSERTION  WILL EXCEED THE DIMEN-
C                        SION OF THE ARRAY.
                         ERRFLG = 2
                     ELSE
                         FWAP1 = PRC(3) + 1
C                        WITH THE  LOCATION  OF THE FIRST  ELEMENT TO BE
C                        SHIFTED STORED IN FWAP1, ACCESS THE SMFSHF ROU-
C                        TINE TO PERFORM THE  DATA SHIFTING, THEN INSERT
C                        THE ADA VECTOR DATA. AT THE SAME TIME ALTER THE
C                        THE DT2 VECTOR, IF BOTH  VECTORS HAVE BEEN SPE-
C                        CIFIED.
                         CALL SMFSHF
     G                               (FWAP1 , PRC(4), DSZ   , USZ
     B                               ,DT1   )
                         IF (PRC(2).EQ.2) THEN
                             CALL SMFSHF
     G                                   (FWAP1 , PRC(4), DSZ   , USZ
     B                                   ,DT2   )
                         ENDIF
                         J = PRC(4)
                         K = PRC(3)
                         DO 1075 I = 1, J
                             K = K + 1
                             DT1(K) = ADA(1,I)
                             IF (PRC(2).EQ.2) THEN
                                 DT2(K) = ADA(2,I)
                             ENDIF
 1075                        CONTINUE
C                        ENDDO
                         USZ = USZ + PRC(4)
                     ENDIF
                     GO TO 1150
C
C                CASE 4 - COMBINING OF ADJACENT ELEMENTS
 1100                FWAP1 = PRC(3) + 1
C                    WITH THE ADDRESS  OF THE FIRST  ELEMENT TO BE ADDED
C                    TO THE FWA ELEMENT STORED IN FWAP1, PERFORM THE DA-
C                    TA ADDITIONS.  THEN USING THE  SMFSHF ROUTINE SHIFT
C                    OUT THE ELEMENTS WHICH ARE NO LONGER TREATED AS ER-
C                    ROR TIMES. AT THE SAME TIME ALTER THE DT2 VECTOR IF
C                    BOTH VECTORS HAVE BEEN SPECIFIED.
                     J = PRC(3)
                     K = PRC(4)
                     DO 1125 I = FWAP1, K
                         DT1(J) = DT1(J) + DT1(I)
                         IF (PRC(2).EQ.2) THEN
                             DT2(J) = DT2(J) + DT2(I)
                         ENDIF
 1125                    CONTINUE
C                    ENDDO
                     NUMB = 0 - (PRC(4) - PRC(3))
                     CALL SMFSHF
     G                           (FWAP1 , NUMB  , DSZ   , USZ
     B                           ,DT1   )
                     IF (PRC(2).EQ.2) THEN
                         CALL SMFSHF
     G                               (FWAP1 , NUMB  , DSZ   , USZ
     B                               ,DT2   )
                     ENDIF
                     USZ = USZ + NUMB
 1150                CONTINUE
C            END CASE
         ENDIF
         RETURN
         END
         SUBROUTINE SMFGOF
     G                     (DAT   , PDAT  , NS    , EST   , CCN
     Y                     ,CHI   , DOF   , LIF   , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMFGOF
C    G                     (DAT   , PDAT  , NS    , EST   , CCN
C    Y                     ,CHI   , DOF   , LIF   , RFLAG )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE A GOODNESS-OF-FIT (USING CHI-SQUARE) FOR THE PREDIC-
C        TION OF THE INTERVAL MODELS.
C   DESCRIPTION
C        THE ROUTINE  WILL ATTEMPT TO  ACHIEVE THE DESIRED CELL COMBINA-
C        TION  FREQUENCY  NUMBER (CCN).  IF IT IS  UNSUCCESSFUL, IT WILL
C        CONSTRUCT A PARTITION  FOR WHICH ALL  BUT THE LAST INTERVAL HAS
C        THE DESIRED FREQUENCY.  THE USER ALSO HAS THE OPTION OF EXECUT-
C        ING THE CHI-SQUARE WITH NO CELLS COMBINED (CCN = -1.0).
C
C        THE LAST ARGUMENT OF THE CALL LINE (RFLAG) WILL BE SET TO INDI-
C        CATE TO THE CALLING ROUTINE THE SUCCESSFULNESS OF THE GOODNESS-
C        OF-FIT EXECUTION. THE THREE POSSIBLE RETURN STATES ARE:
C             0 - SUCCESSFUL CHI AND DOF COMPUTATIONS
C             1 - SUCCESSFUL CHI AND DOF COMPUTATIONS, BUT THE USER-SPE-
C                 CIFIED CELL COMBINATION COULD  NOT BE ACHIEVED FOR THE
C                 LAST INTERVAL.  THE ARGUMENT LIF WILL CONTAIN THE LAST
C                 INTERVAL'S FREQUENCY.
C             2 - THE DEGREES-OF-FREEDOM WAS  CALCULATED TO BE LESS THAN
C                 ONE.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             CCN    (R) = CELL COMBINATION FREQUENCY NUMBER
C             DAT    (R) = ORIGINAL FAULT COUNTS
C             EST    (I) = NUMBER OF ESTIMATIONS FROM MODEL
C             NS     (I) = SIZE OF DAT AND PDAT
C             PDAT   (R) = PREDICTED FAULT COUNTS
C        YIELDED
C             CHI    (R) = CHI-SQUARE STATISTIC
C             DOF    (I) = DEGREES-OF-FREEDOM
C             LIF    (R) = LAST INTERVAL'S FREQUENCY
C             RFLAG  (I) = RETURN STATUS FLAG
C   LOCAL GLOSSARY
C             CCNT   (I) = CELL COUNTER
C             ICNT   (I) = INTERNAL COUNTER
C             SOD    (R) = TEMPORARY SUMMATION OF DAT CELLS
C             SOP    (R) = TEMPORARY SUMMATION OF PDAT CELLS
C   ERRORS
C        RFLAG  = 1 : CCN COULD NOT BE ACHIEVED FOR THE LAST INTERVAL
C        RFLAG  = 2 : THE CALCULATED DOF WAS LESS THAN 1
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              DOF           ,EST           ,NS
     1                       ,RFLAG
         DIMENSION            DAT(NS)       ,PDAT(NS)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CCNT          ,ICNT
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE THE RETURN  STATUS FLAG,  THE INTERVAL COUNTER,  THE
C        CELL COUNTER, AND CHI.
         RFLAG = 0
         ICNT  = 0
         CCNT  = 0
         CHI   = 0.0
C
C        DOWHILE (ICNT .LT. NS)
 1000        SOD = 0.0
             SOP = 0.0
C
C            SUM UP THE INTERVAL FREQUENCY COUNTS (PREDICTED & ORIGINAL)
C            SUCH THAT THE RESULTANT SOP IS GREATER THAN OR EQUAL TO THE
C            DESIRED CCN.
C            DOWHILE ((SOP .LT. CCN) .AND. (ICNT .LT. NS))
 1025            ICNT = ICNT + 1
                 SOP  = SOP + PDAT(ICNT)
                 SOD  = SOD + DAT(ICNT)
                 IF ((SOP.LT.CCN) .AND. (ICNT.LT.NS)) GO TO 1025
C            ENDWHILE
C
C            VERIFY THAT  THE SOP WAS  ALLOWED TO REACH THE DESIRED CCN.
C            THE LOOP COULD  HAVE BEEN TERMINATED  BY ICNT  REACHING NS,
C            LEAVING SOP LESS THAN CCN.  IF THIS IS THE CASE  RFLAG WILL
C            BE SET TO A 1 (AS EXPLAINED IN THE DESCRIPTION).
             IF ((SOP.LT.CCN) .AND. (ICNT.EQ.NS)) THEN
                 RFLAG = 1
             ENDIF
C
C            INCREMENT THE  CELL COUNTER, AND  ADD THIS CELL'S CONTRIBU-
C            TION TO THE CHI-SQUARE STATISTIC.
             CCNT = CCNT + 1
             CHI  = CHI + (((SOD - SOP)**2) / SOP)
             IF (ICNT.LT.NS) GO TO 1000
C        ENDWHILE
C
C        SET THE  ARGUMENT LIF  TO THE LAST  INTERVAL'S FREQUENCY OF THE
C        PREDICTED DATA.
         LIF = SOP
C
C        COMPUTE THE DEGREES-OF-FREEDOM, AND VERIFY  THAT IT IS AT LEAST
C        ONE. IF IT ISN'T, SET THE ERROR FLAG SHOWING THAT THE STATISTIC
C        CANNOT BE COMPUTED USING THE CURRENT CCN.
         DOF = CCNT - 1 - EST
         IF (DOF.LT.1) THEN
             RFLAG = 2
         ENDIF
         RETURN
         END
         SUBROUTINE SMFSHF
     G                     (FWA   , NE    , NS    , USZ
     B                     ,DAT   )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMFSHF
C    G                     (FWA   , NE    , NS    , USZ
C    B                     ,DAT   )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO SHIFT THE CONTENTS  OF THE  DAT VECTOR  (OF DIMENSION NS, OF
C        WHICH USZ ELEMENTS  ARE USED/ASSIGNED)  FOR INSERTIONS OR DELE-
C        TIONS AS DIRECTED BY THE SMFEDT ROUTINE.
C   DESCRIPTION
C        INSERTION - THE ELEMENT(S)  OF DAT WILL BE SHIFTED RIGHT NE LO-
C                    CATIONS STARTING AT ELEMENT FWA.
C        DELETION  - THE DATA VECTOR WILL BE  SHIFTED LEFT IN ACCORDANCE
C                    WITH THE ARGUMENT VALUES.
C
C        THE DECISION OF AN  INSERTION OR DELETION  IS BASED ON THE SIGN
C        OF THE NE ARGUMENT.  A POSITIVE NE  DENOTES AN  INSERTION AND A
C        NEGATIVE NE DENOTES A DELETION.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             FWA    (I) = THE LOWER ADDRESS OF THE SHIFTING
C             NE     (I) = THE NUMBER OF ELEMENTS EFFECTED
C             NS     (I) = DIMENSION OF DAT VECTOR
C             USZ    (I) = USED (ASSIGNED) ELEMENTS IN DAT
C        BOTH
C             DAT    (R) = THE DATA VECTOR
C   LOCAL GLOSSARY
C             LWA    (I) = INDEX OF THE LAST DELETED ELEMENT
C             NNS    (I) = NEW NUMBER OF ELEMENTS IN DELETED SAMPLE
C             NUM    (I) = NUMBER OF ELEMENTS TO SHIFT - ABS(NE)
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              FWA           ,NE            ,NS
     1                       ,USZ
         DIMENSION            DAT(NS)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,LWA           ,NNS
     1                       ,NUM
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         NUM = ABS(NE)
         IF (NUM.NE.NE) THEN
C            A DATA DELETION IS REQUESTED; COMPUTE THE INDEX OF THE LAST
C            ELEMENT TO BE DELETED,  AND THE NEW SAMPLE SIZE.  NOTE THAT
C            THE NEW SAMPLE SIZE IS  RETAINED LOCALLY,  AND NOT RETURNED
C            TO SMFEDT. THE ACTUAL MODIFICATION OF THE ARGUMENT USZ WILL
C            BE PERFORMED WITHIN THE SMFEDT ROUTINE.
             LWA = FWA + NUM - 1
             NNS = USZ - NUM
             IF (LWA.NE.USZ) THEN
C                THE INDICATED DELETION IS NOT FROM  THE END OF THE DATA
C                VECTOR; THEREFORE THE DATA ELEMENTS FROM THE HIGHER END
C                MUST BE SHIFTED TO THE LOWER END TO FILL IN THE DELETED
C                AREA.
                 DO 1000 I = FWA, NNS
                     DAT(I) = DAT(I+NUM)
 1000                CONTINUE
C                ENDDO
             ENDIF
C
C            NOW ZERO-FILL THE END OF THE VECTOR.
             NNS = NNS + 1
             DO 1025 I = NNS, USZ
                 DAT(I) = 0.0
 1025            CONTINUE
C            ENDDO
         ELSE
C            A DATA INSERTION  HAS BEEN INDICATED;  DETERMINE IF THE IN-
C            SERTION IS TO BE MADE AT THE  END OF THE VECTOR.  IF IT IS,
C            THEN NO SHIFTING IS REQUIRED, AND CONTROL CAN RETURN TO THE
C            CALLING ROUTINE.
             IF (FWA.LE.USZ) THEN
C                PERFORM A DATA SHIFT TO THE RIGHT.  NOTE THAT THE SHIFT
C                MUST BE PERFORMED FROM THE HIGH-TO-LOW ELEMENT.
                 I = USZ + 1
C                DOWHILE (I .GE. FWA)
 1050                I = I - 1
                     DAT(I+NUM) = DAT(I)
                     IF (I.GT.FWA) GO TO 1050
C                ENDWHILE
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE SMFTRN
     G                     (NS    , A     , B     , TYPE
     B                     ,DAT
     Y                     ,ERRFLG)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMFTRN
C    G                     (NS    , A     , B     , TYPE
C    B                     ,DAT
C    Y                     ,ERRFLG)
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM  SIX POSSIBLE  TYPES OF  TRANSFORMATIONS OF THE DATA
C        VECTOR.
C   DESCRIPTION
C        THE VECTOR DAT OF SIZE NS CAN BE TRANSFORMED VIA SIX TYPES. THE
C        ELEMENTS OF THE DATA VECTOR CAN BE TRANSFORMED BY:
C          (1) DAT(I) = LOG(A * DAT(I) + B)
C          (2) DAT(I) = EXP(A * DAT(I) + B)
C          (3) DAT(I) = DAT(I) ** A
C          (4) DAT(I) = DAT(I) + A
C          (5) DAT(I) = DAT(I) * A
C          (6) DAT(I) = DAT(I) + THE SUM OF THE PREVIOUS DAT(J'S)
C
C        ERROR CODE  IS INCORPORATED IN THE ROUTINE TO PREVENT THE FATAL
C        ERROR OF ATTEMPTING  TO TAKE THE  LOG OF A NEGATIVE  NUMBER AND
C        THE FATAL ERROR OF RAISING A NEGATIVE OR ZERO  REAL NUMBER TO A
C        REAL POWER.  BOTH OF THESE POSSIBILITIES ARE REMOTE CONSIDERING
C        THE TYPE OF DATA.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             A      (R) = TRANSFORMATION VARIABLE 1
C             B      (R) = TRANSFORMATION VARIABLE 2
C             NS     (I) = NUMBER OF ENTRIES
C             TYPE   (I) = TRANSFORMATION TYPE
C        BOTH
C             DAT    (R) = THE DATA VECTOR
C        YIELDED
C             ERRFLG (I) = ERROR FLAG
C   LOCAL GLOSSARY
C             IEXP   (I) = INTEGER EXPONENT
C             ZTEST  (R) = ZERO TEST CONSTANT
C   ERRORS
C        ERRFLG = 1 : A NEGATIVE X(I) WAS FOUND; AND REAL EXP. DESIRED
C        ERRFLG = 2 : LOG ATTEMPTED ON COMPUTED NEGATIVE OR ZERO VALUE
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              ERRFLG        ,NS            ,TYPE
         DIMENSION            DAT(NS)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,IEXP
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE ZERO TEST CONSTANT.
         ZTEST = 0.0000000001
C
         ERRFLG = 0
         IF (TYPE.EQ.1) THEN
C            A LOG TRANSFORMATION  IS DESIRED; POTENTIAL ERROR CONDITION
C            MUST BE CHECKED, AND ERRFLG SET IF VIOLATIONS OCCUR.
             DO 1000 I = 1, NS
                 TMP = A * DAT(I) + B
                 IF (TMP.LT.ZTEST) THEN
                     ERRFLG = 2
                 ENDIF
 1000            CONTINUE
C            ENDDO
         ELSEIF (TYPE.EQ.3) THEN
C            AN EXPONENTIAL  TRANSFORMATION IS DESIRED;  POTENTIAL ERROR
C            CONDITION MUST BE CHECKED, AND ERRFLG SET IF VIOLATIONS OC-
C            CUR. THE ERROR IS THE RESULT OF ATTEMPTING TO RAISE A NEGA-
C            TIVE NUMBER TO A NON-INTEGER POWER.
             IEXP = A
             TMP  = A - REAL(IEXP)
             IF (TMP.NE.0.0) THEN
                 TMP = 0.5
             ENDIF
             IF (TMP.EQ.0.5) THEN
C                EXP. WAS REAL;  THE ELEMENTS OF DAT MUST BE VERIFIED TO
C                BE GREATER THAN OR EQUAL TO ZERO.
                 DO 1025 I = 1, NS
                     IF (DAT(I).LT.ZTEST) THEN
                         ERRFLG = 1
                     ENDIF
 1025                CONTINUE
C                ENDDO
             ENDIF
         ENDIF
C
         IF (ERRFLG.EQ.0) THEN
C            NO ERRORS WERE FOUND, PROCEED WITH PROCESSING.
C
             DO 1225 I = 1, NS
C
C                CASE ENTRY
                     GO TO (1050, 1075, 1100, 1125, 1150, 1175), TYPE
C
C                    CASE 1 - LOG(A * DAT(I) + B)
 1050                    DAT(I) = LOG(A * DAT(I) + B)
                         GO TO 1200
C
C                    CASE 2 - EXP(A * DAT(I) + B)
 1075                    DAT(I) = EXP(A * DAT(I) + B)
                         GO TO 1200
C
C                    CASE 3 - DAT(I) ** A OR DAT(I) ** IEXP
 1100                    IF (TMP.EQ.0.5) THEN
                             DAT(I) = DAT(I)**A
                         ELSE
                             DAT(I) = DAT(I)**IEXP
                         ENDIF
                         GO TO 1200
C
C                    CASE 4 - DAT(I) + A
 1125                    DAT(I) = DAT(I) + A
                         GO TO 1200
C
C                    CASE 5 - DAT(I) * A
 1150                    DAT(I) = DAT(I) * A
                         GO TO 1200
C
C                    CASE 6 - DAT(I) = DAT(I) + DAT(J)
 1175                    IF (I.GT.1) THEN
                             DAT(I) = DAT(I) + DAT(I-1)
                         ENDIF
C
 1200                    CONTINUE
C                END CASE
 1225            CONTINUE
C            ENDDO
         ENDIF
         RETURN
         END
         SUBROUTINE SOLVE
     G                     (N     , U    , Y
     Y                     ,X     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SOLVE
C    G                     (N     , U    , Y
C    Y                     ,X     )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO SOLVE THE SYSTEM OF EQUATIONS HX = Y FOR X.
C   DESCRIPTION
C        THIS ROUTINE  UTILIZES  THE  UPPER  TRIANGULAR  FACTORS-U  OF A
C        SYMMETRIC POSITIVE DEFINITE MATRIX TO SOLVE THE SYSTEM OF EQUA-
C        TIONS HX = Y FOR X.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             N      (I) = DIMENSION VARIABLE FOR U, X, AND Y
C             U      (R) = UPPER TRIANGULAR FACTORS
C             Y      (R) = Y-VECTOR IN EQUATION HX = Y
C        YIELDED
C             X      (R) = X-VECTOR IN EQUATION HX = Y
C   LOCAL GLOSSARY
C             (NONE )
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              N
         DIMENSION            U(3,3)        ,X(3)          ,Y(3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SOLVE THE SYSTEM OF EQUATIONS HX = Y  FOR  X  USING  THE  UPPER
C        TRIANGULAR FACTORS-U OF MATRIX-H.
         DO 1025 I = N, 1, -1
             X(I) = Y(I)
             IF (I.NE.N) THEN
                 DO 1000 J = I + 1, N
                     X(I) = X(I) - U(I,J) * X(J)
 1000                CONTINUE
C                ENDDO
             ENDIF
             X(I) = X(I) / U(I,I)
 1025        CONTINUE
C        ENDDO
C
         DO 1075 I = 1, N
             IF (I.NE.1) THEN
                 DO 1050 J = 1, I-1
                     X(I) = X(I) - U(J,I) * X(J)
 1050                CONTINUE
C                ENDDO
             ENDIF
             X(I) = X(I) / U(I,I)
 1075        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE TQLRAT
     G                     (N
     B                     ,E2    , D     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE TQLRAT
C    G                     (N
C    B                     ,E2    , D     )
C
C   AUTHOR
C        B.S. GARBOW (APPLIED MATH. DIVISION, ARGONNE NATIONAL LAB.)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO FIND  THE  EIGENVALUES  OF A  SYMMETRIC  TRIDIAGONAL MATRIX.
C   DESCRIPTION
C        THE SYMMETRICAL TRIDIAGONAL MATRIX IS STORED  IN D  WHICH  CON-
C        TAINS THE DIAGONAL ELEMENTS OF THE MATRIX AND E2 WHICH CONTAINS
C        THE SUBDIAGONAL ELEMENTS OF THE MATRIX.  THE EIGENVALUES OF THE
C        SYMMETRIC TRIDIAGONAL MATRIX ARE  COMPUTED BY  THE  RATIONAL QL
C        METHOD AND STORED IN VECTOR D IN ASCENDING ORDER.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS  SUBPROGRAM  CONTAINS  CODE WHICH DOES NOT COMPLY WITH THE
C        PROGRAMMING STANDARDS; GROUP LEADER APPROVAL HAS BEEN OBTAINED.
C       -SPECIFICALLY:
C        THE DOWHILE CONTAINS A HIGHLY NESTED LOOP-BACK TO 1075
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             N      (I) = THE ORDER OF MATRIX A
C        BOTH
C             D      (R) = ON INPUT,  CONTAINS THE DIAGONAL  ELEMENTS OF
C                          THE INPUT MATRIX. ON OUTPUT, CONTAINS THE EI-
C                          GENVALUES IN ASCENDING ORDER.
C             E2     (R) = CONTAINS THE SQUARES OF  THE  SUBDIAGONAL EL-
C                          EMENTS  OF THE INPUT  MATRIX IN ITS  LAST N-1
C                          POSITIONS. E2(1) IS ARBITRARY
C   LOCAL GLOSSARY
C             B      (R) = INTERMEDIATE CALCULATION
C             C      (R) = INTERMEDIATE CALCULATION
C             F      (R) = INTERMEDIATE CALCULATION
C             G      (R) = INTERMEDIATE CALCULATION
C             H      (R) = INTERMEDIATE CALCULATION
C             IFLAG  (I) = INTERMEDIATE STATUS FLAG
C             J      (I) = ITERATION COUNTER
C             MACHEP (R) = RELATIVE MACHINE PRECISION
C             P      (R) = INTERMEDIATE CALCULATION
C             R      (R) = INTERMEDIATE CALCULATION
C             S      (R) = INTERMEDIATE CALCULATION
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              N
         DIMENSION            D(N)          ,E2(N)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,IFLAG         ,II
     1                       ,J             ,L             ,L1
     2                       ,M             ,MML
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        COMPUTE RELATIVE MACHINE PRECISION.
         MACHEP = D1MACH()
         DO 1000 I = 2, N
             E2 (I - 1) = E2(I)
 1000        CONTINUE
C        ENDDO
         F     = 0.0
         B     = 0.0
         E2(N) = 0.0
         L     = 0
C
C        DOWHILE (L .LT. N)
 1025        IFLAG = 0
             L     = L + 1
             J     = 0
             H     = MACHEP * (ABS(D(L)) + SQRT(E2(L)))
             IF (B.LE.H) THEN
                 B = H
                 C = B * B
             ENDIF
C
C            LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT.
             M = L - 1
C            DOWHILE ((E2(M) .GT. C) .AND. (M .LE. N))
 1050            M = M + 1
                 IF ((E2(M).GT.C) .AND. (M.LT.N)) GO TO 1050
C            ENDWHILE
             IF (M.NE.L) THEN
C                DOWHILE (E2(L) .NE. 0)            <<SEE RESTRICTIONS <<
 1075                J = J + 1
C
C                    FORM SHIFT.
                     L1   = L + 1
                     S    = SQRT(E2(L))
                     G    = D(L)
                     P    = (D(L1) - G) / (2.0 * S)
                     R    = SQRT(P * P  + 1.0)
                     D(L) = S / (P + SIGN(R,P))
                     H    = G - D(L)
                     DO 1100 I = L1, N
                         D(I) = D(I) - H
 1100                    CONTINUE
C                    ENDDO
                     F = F + H
C
C                    RATIONAL QL TRANSFORMATION.
                     G = D(M)
                     IF (G.EQ.0.0) THEN
                         G = B
                     ENDIF
                     H   = G
                     S   = 0.0
                     MML = M - L
                     DO 1125 II = 1, MML
                         I       = M     - II
                         P       = G     * H
                         R       = P     + E2(I)
                         E2(I+1) = S     * R
                         S       = E2(I) / R
                         D(I+1)  = H + S * (H    + D(I))
                         G       = D(I)  - E2(I) / G
                         IF (G.EQ.0.0) THEN
                             G = B
                         ENDIF
                         H = G * P / R
 1125                    CONTINUE
C                    ENDDO
                     E2(L) = S * G
                     D(L)  = H
C
C                    GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST.
                     IF (H.NE.0.0) THEN
                         IF (ABS(E2(L)).GT.ABS(C/H)) THEN
                             E2(L) = H * E2(L)
                             IF (E2(L).NE.0.0) GO TO 1075
                         ENDIF
                     ENDIF
C                ENDWHILE
             ENDIF
             P = D(L) + F
C
C            ORDER EIGENVALUES.
             IF (L.NE.1) THEN
                 DO 1150 II = 2,L
                     IF (IFLAG.EQ.0) THEN
                         I = L + 2 - II
                         IF (P.LT.D(I-1)) THEN
                             D(I) = D(I-1)
                         ELSE
                             IFLAG = 1
                         ENDIF
                     ENDIF
 1150                CONTINUE
C                ENDDO
             ENDIF
             IF (IFLAG.EQ.0) THEN
                 I = 1
             ENDIF
             D(I) = P
             IF (L.LT.N) GO TO 1025
C        ENDWHILE
         RETURN
         END
         SUBROUTINE TRED1
     G                    (NM    , N
     B                    ,A
     Y                    ,D     , E     , E2    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE TRED1
C    G                    (NM    , N
C    B                    ,A
C    Y                    ,D     , E     , E2    )
C
C   AUTHOR
C        B.S. GARBOW (APPLIED MATH. DIVISION, ARGONNE NATIONAL LAB.)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO REDUCE A REAL SYMMETRIC  MATRIX TO A SYMMETRICAL TRIDIAGONAL
C        MATRIX.
C   DESCRIPTION
C        THE REAL SYMMETRIC MATRIX A IS REDUCED TO A SYMMETRICAL  TRIDI-
C        AGONAL  MATRIX  USING  ORTHOGONAL  SIMILARITY  TRANSFORMATIONS.
C        THE  DIAGONAL  ELEMENTS OF THE TRIDIAGONAL MATRIX ARE STORED IN
C        D AND THE  SUBDIAGONAL  ELEMENTS  OF THE TRIDIAGONAL MATRIX ARE
C        STORED IN E.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             N      (I) = THE ORDER OF MATRIX A
C             NM     (I) = ROW DIMENSION OF THE 2-DIMENSIONAL ARRAY PAR-
C                          AMETER AS DECLARED IN THE CALLING ROUTINE
C        BOTH
C             A      (R) = ON INPUT,  CONTAINS THE REAL SYMMETRIC ARRAY.
C                          ON OUTPUT, CONTAINS INFORMATION ABOUT THE OR-
C                          THOGONAL  TRANSFORMATIONS  USED IN THE REDUC-
C                          TION IN ITS STRICT LOWER TRIANGLE.
C        YIELDED
C             D      (R) = CONTAINS THE  DIAGONAL ELEMENTS OF THE TRIDI-
C                          AGONAL MATRIX
C             E      (R) = CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRI-
C                          DIAGONAL MATRIX IN ITS LAST N-1 POSITIONS.
C                          E(1) IS SET TO ZERO
C             E2     (R) = CONTAINS THE SQUARES OF THE CORRESPONDING EL-
C                          EMENTS OF E.  E2 MAY COINCIDE WITH  E  IF THE
C                          SQUARES ARE NOT NEEDED
C   LOCAL GLOSSARY
C             F      (R) = INTERMEDIATE CALCULATION
C             G      (R) = INTERMEDIATE CALCULATION
C             H      (R) = INTERMEDIATE CALCULATION
C             SCALE  (R) = ROW SCALE FACTOR
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              N             ,NM
         DIMENSION            A(NM,N)       ,D(N)          ,E(N)
     1                       ,E2(N)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,II            ,J
     1                       ,JP1           ,K             ,L
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         DO 1000 I = 1, N
             D(I) = A(I,I)
 1000        CONTINUE
C        ENDDO
C
C        COMPUTE TRIDIAGONAL MATRIX.
         DO 1225 II = 1, N
             I      = N + 1 - II
             L      = I - 1
             H      = 0.0
             SCALE  = 0.0
C
             IF (L.LT.1) THEN
                 E(I)  = 0.0
                 E2(I) = 0.0
             ELSE
C                SCALE ROW.
                 DO 1025 K = 1, L
                     SCALE = SCALE + ABS(A(I,K))
 1025                CONTINUE
C                ENDDO
C
                 IF (SCALE.EQ.0.0) THEN
                     E(I)  = 0.0
                     E2(I) = 0.0
                 ELSE
                     DO 1050 K = 1, L
                         A(I,K) = A(I,K) / SCALE
                         H      = H      + A(I,K) * A(I,K)
 1050                    CONTINUE
C                    ENDDO
                     E2(I)  = SCALE**2 * H
                     F      = A(I,L)
                     G      = -SIGN(SQRT(H),F)
                     E(I)   = SCALE * G
                     H      = H     - F * G
                     A(I,L) = F     - G
                     IF (L.NE.1) THEN
                         F = 0.0
                         DO 1125 J = 1, L
                             G = 0.0
C
C                            FORM ELEMENT OF A*U.
                             DO 1075 K = 1, J
                                 G = G + A(J,K) * A(I,K)
 1075                            CONTINUE
C                            ENDDO
                             JP1 = J + 1
                             IF (L.GE.JP1) THEN
                                 DO 1100 K = JP1, L
                                     G = G + A(K,J) * A(I,K)
 1100                                CONTINUE
C                                ENDDO
                             ENDIF
C
C                            FORM ELEMENT OF P.
                             E(J) = G / H
                             F    = F + E(J) * A(I,J)
 1125                        CONTINUE
C                        ENDDO
                         H = F / (H + H)
C
C                        FORM REDUCED A.
                         DO 1175 J = 1, L
                             F    = A(I,J)
                             G    = E(J) - H * F
                             E(J) = G
                             DO 1150 K = 1, J
                                 A(J,K) = A(J,K) - F * E(K) - G
     1                                  * A(I,K)
 1150                            CONTINUE
C                            ENDDO
 1175                        CONTINUE
C                        ENDDO
                     ENDIF
                     DO 1200 K = 1, L
                         A(I,K) = SCALE * A(I,K)
 1200                    CONTINUE
C                    ENDDO
                 ENDIF
             ENDIF
             H      = D(I)
             D(I)   = A(I,I)
             A(I,I) = H
 1225        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE ZERO
     B                   (A     , B     , FA    , FB    , FC    , ICOUNT
     Y                   ,IFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE ZERO
C    B                   (A     , B     , FA    , FB    , FC    , ICOUNT
C    Y                   ,IFLAG )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        COMPUTES THE ZERO OF THE FUNCTION USING THE DEKKER-BRENT  ALGO-
C        RITHM.
C   DESCRIPTION
C        THIS  ROUTINE COMPUTES THE ZERO OF THE FUNCTION IN THE INTERVAL
C        A AND B.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS  SUBPROGRAM  CONTAINS  CODE WHICH DOES NOT COMPLY WITH THE
C        PROGRAMMING STANDARDS; GROUP LEADER APPROVAL HAS BEEN OBTAINED.
C       -SPECIFICALLY:
C        THE ARGUMENT FC IS ACTUALLY INITIALIZED WITHIN THIS ROUTINE, IF
C        ICOUNT IS SET TO ONE; OTHERWISE, IT IS A "BOTH" AS DEFINED.
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        BOTH
C             A      (R) = NEWEST POINT
C             B      (R) = PREVIOUS POINT
C             FA     (R) = VALUE OF FUNCTION AT POINT A
C             FB     (R) = VALUE OF FUNCTION AT POINT B
C             FC     (R) = VALUE OF FUNCTION AT POINT C
C             ICOUNT (I) = STEP COUNTER
C        YIELDED
C             IFLAG  (I) = CONVERGENCE FLAG,  WHERE -1 INDICATES CONVER-
C                          GENCE TO THE ZERO OF THE FUNCTION HAS NOT OC-
C                          CURRED  AND  0 INDICATES THAT CONVERGENCE HAS
C                          OCCURRED
C   LOCAL GLOSSARY
C             C      (R) = MOST RECENT POINT  FOR WHICH  FB AND FC  HAVE
C                          OPPOSITE SIGNS
C             D      (R) = VARIABLE USED FOR STORAGE OF A POINT
C             E      (R) = VARIABLE USED FOR STORAGE OF A POINT
C             EPS    (R) = RELATIVE MACHINE PRECISION
C             P      (R) = VARIABLE USED FOR INTERPOLATION ALGORITHM
C             Q      (R) = VARIABLE USED FOR INTERPOLATION ALGORITHM
C             R      (R) = VARIABLE USED FOR INTERPOLATION ALGORITHM
C             S      (R) = VARIABLE USED FOR INTERPOLATION ALGORITHM
C             TOL    (R) = DESIRED LENGTH OF THE INTERVAL OF UNCERTAINTY
C                          OF THE FINAL RESULT
C             TOL1   (R) = TOLERANCE
C             XM     (R) = DIFFERENCE BETWEEN POINTS A AND B
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   REFERENCES
C        SEE PROLOGUE OF MAIN PROGRAM
C   LANGUAGE
C        FORTRAN 77
C
C *********************** START OF DECLARATIONS ************************
C
C                             PRE-DECLARATION DEFAULT ASSIGNMENT
         IMPLICIT             DOUBLE PRECISION (A-Z)
C
C                             PARAMETER SPECIFICATIONS
C        (NONE)
C
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              ICOUNT        ,IFLAG
C
C                             LOCAL SPECIFICATIONS
C        (NONE)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        PERFORM INITIALIZATION.
         EPS = D1MACH()
         TOL = 1.0E-10
C
C        PERFORM FIRST-TIME INITIALIZATIONS.      <<SEE RESTRICTIONS <<
         IF (ICOUNT.EQ.1) THEN
             C      = A
             FC     = FA
             D      = B - A
             E      = D
             ICOUNT = 2
         ENDIF
C
         IF ((FB * (FC / ABS(FC))).GT.0.0) THEN
             C      = A
             FC     = FA
             D      = B - A
             E      = D
             ICOUNT = 2
         ENDIF
C
         IF (ABS(FC).LT.ABS(FB)) THEN
             A      = B
             B      = C
             C      = A
             FA     = FB
             FB     = FC
             FC     = FA
         ENDIF
C
C        PERFORM CONVERGENCE TEST.
         TOL1 = 2.0 * EPS * ABS(B) + 0.5 * TOL
         XM   = 0.5 * (C - B)
         IF ((ABS(XM).LE.TOL1) .OR. (FB.EQ.0.0)) THEN
             IFLAG = 0
         ELSE
C            DETERMINE IF BISECTION IS NECESSARY.
             IF ((ABS(E).LT.TOL1) .OR. (ABS(FA).LE.ABS(FB))) THEN
                 D = XM
                 E = D
             ELSE
C                DETERMINE IF QUADRATIC INTERPOLATION IS POSSIBLE.
                 IF (A.EQ.C) THEN
C                    PERFORM LINEAR INTERPOLATION.
                     S = FB  / FA
                     P = 2.0 * XM * S
                     Q = 1.0 - S
                 ELSE
C                    PERFORM INVERSE QUADRATIC INTERPOLATION.
                     Q = FA / FC
                     R = FB / FC
                     S = FB / FA
                     P =  S        * (2.0      * XM       * Q * (Q - R)
     1                 - (B - A)   * (R - 1.0))
                     Q = (Q - 1.0) * (R - 1.0) * (S - 1.0)
                 ENDIF
C
C                ADJUST THE SIGNS.
                 IF (P.GT.0.0) THEN
                     Q = -Q
                 ENDIF
                 P = ABS(P)
C
C                DETERMINE IF INTERPOLATION IS ACCEPTABLE.
                 IF (((2.0 * P).LT.(3.0 * XM * Q - ABS(TOL1 * Q)))
     1               .OR. (P.LT.ABS(0.5  * E  * Q))) THEN
                     E = D
                     D = P / Q
                 ELSE
C                    PERFORM BISECTION.
                     D = XM
                     E = D
                 ENDIF
             ENDIF
C
C            COMPLETE THE STEP.
             A  = B
             FA = FB
             IF (ABS(D).GT.TOL1) THEN
                 B = B + D
             ELSE
                 B = B + SIGN(TOL1,XM)
             ENDIF
         ENDIF
         RETURN
         END
