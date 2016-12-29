1
PROGRAM SERFS
C   PURPOSE
C        THE  STATISTICAL MODELING  AND ESTIMATION OF  RELIABILITY FUNC-
C        TIONS FOR SOFTWARE (SMERFS) PROGRAM ACTS AS A VEHICLE TO ACCESS
C        THE SMERFS LIBRARY (SMFLIB).  IT IS IN THOSE ROUTINES  THAT THE
C        REAL PROCESSING FOR THE RESULTANT  SOFTWARE RELIABILITY STATIS-
C        TICS WILL BE CONDUCTED.

3
SUBROUTINE MDLLAV
C   PURPOSE
C        TO PROVIDE THE  INTERACTIVE PROMPTS AND INPUTS  FOR THE LITTLE-
C        WOOD AND VERRALL BAYESIAN RELIABILITY GROWTH MODEL. ADDITIONAL-
C        LY, THE ROUTINE WILL PERFORM THE  ACCESS TO THE SMFLIB ROUTINES
C        NEEDED TO PERFORM THE MODEL  EXECUTIONS AND PREDICTED DATA VEC-
C        TOR CREATION.

LT
SUBROUTINE SLAVFN
C   PURPOSE
C        TO CALCULATE EITHER THE  LIKELIHOOD FUNCTION FOR X IF ESF=1, OR
C        THE SUMS-OF-SQUARES IF ESF=2,  FOR THE  LITTLEWOOD AND  VERRALL
C        BAYESIAN RELIABILITY GROWTH MODEL.

SUBROUTINE SLAVGD
C   PURPOSE
C        TO CALCULATE THE GRADIENT OF THE LIKELIHOOD FUNCTION FOR  X FOR
C        THE  LITTLEWOOD AND VERRALL BAYESIAN  RELIABILITY GROWTH MODEL.

SUBROUTINE SLAVHS
C   PURPOSE
C        TO CALCULATE THE  HESSIAN OF THE  LIKELIHOOD FUNCTION FOR X FOR
C        THE  LITTLEWOOD AND VERRALL BAYESIAN RELIABILITY GROWTH  MODEL.

SUBROUTINE SLAVMA
C   PURPOSE
C        TO PERFORM FOUR TYPES OF  MODEL APPLICABILITY ANALYSIS FOR  THE
C        LITTLEWOOD AND VERRALL BAYESIAN RELIABILITY GROWTH MODEL.
C
C        THESE INCLUDE:
C             1) THE MODEL ACCURACY;
C             2) THE MODEL BIAS;
C             3) THE MODEL NOISE; AND
C             4) THE MODEL TREND

SUBROUTINE SLAVMD
C   PURPOSE
C        TO CALCULATE THE ESTIMATES IN THE  LITTLEWOOD AND VERRALL BAYE-
C        SIAN RELIABILITY GROWTH MODEL  USING THE TRUST REGION MINIMIZA-
C        TION ALGORITHM.

SUBROUTINE SLAVPR
C   PURPOSE
C        TO COMPUTE THE PREDICTED MTBNF AND TO CREATE THE PREDICTED DATA
C        VECTOR FOR  LITTLEWOOD AND VERRALL BAYESIAN  RELIABILITY GROWTH
C        MODEL.  DURING THE PREDICTED VECTOR CREATION, THIS ROUTINE WILL
C        ALSO COMPUTE THE KOLMOGOROV DISTANCE.



         SUBROUTINE SLAVMD
     G                     (DAT   , ESF   , MAXIC , NS    , N
     G                     ,PHIIND, BETA
     Y                     ,ALPHA , COUNT , RFLAG , X     , Z     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SLAVMD
C    G                     (DAT   , ESF   , MAXIC , NS    , N
C    G                     ,PHIIND, BETA
C    Y                     ,ALPHA , COUNT , RFLAG , X     , Z     )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   TRUST REGION AND CLOSED FORM IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   TRUST REGION AND CLOSED FORM CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE ESTIMATES IN THE  LITTLEWOOD AND VERRALL BAYE-
C        SIAN RELIABILITY GROWTH MODEL  USING THE TRUST REGION MINIMIZA-
C        TION ALGORITHM.
C   DESCRIPTION
C        THE SECOND ARGUMENT OF THE CALL LINE (ESF)  WILL BE USED BY THE
C        ROUTINE TO DETERMINE WHICH OF THE THREE METHODS OF EXECUTION IS
C        DESIRED (MAXIMUM LIKELIHOOD, LEAST SQUARES, OR MAXIMUM  LIKELI-
C        HOOD INITIAL ESTIMATES).
C
C        ONCE THE  ITERATIVE EXECUTION IS  INITIATED FOR MAXIMUM LIKELI-
C        HOOD,  PROCESSING  WILL  CONTINUE  UNTIL  ONE OF THREE POSSIBLE
C        TERMINATION  STATES  IS  REACHED.   THE TENTH  ARGUMENT  OF THE
C        CALL  LINE  (RFLAG)  WILL  BE SET TO  INDICATE TO  THE  CALLING
C        ROUTINE THE  REASON FOR EXIT.  THE POSSIBLE REASONS  FOR SLAVMD
C        EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             1 - MAXIMUM ITERATIONS
C             2 - THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY
C
C        FOR BOTH THE  LEAST SQUARES AND  MAXIMUM LIKELIHOOD  METHODS OF
C        EXECUTION, RFLAG MAY ALSO BE RETURNED AS FOUR  TO INDICATE THAT
C        THE MODEL ESTIMATES WERE DEEMED INVALID BECAUSE THE REQUIREMENT
C        THAT THE SUM OF BETA0 AND BETA1 BE POSITIVE AS NOT ACHIEVED.
C
C        THE NUMBER OF ITERATIONS  NEEDED FOR THE SUCCESSFUL CONVERGENCE
C        WILL BE RETURNED IN THE NINTH ARGUMENT (COUNT).
C
C        THE  RESULTANT  STATISTICS WILL BE RETURNED IN X, Z, AND  ALPHA
C        FOR MAXIMUM LIKELIHOOD,  X AND Z FOR LEAST SQUARES,  AND X ONLY
C        FOR  MAXIMUM  LIKELIHOOD  INITIAL  ESTIMATES.   THE VALUES IN X
C        CORRESPOND TO THE MODEL ESTIMATES FOR  BETA(0) AND BETA(1), AND
C        Z  CORRESPONDS TO THE  VALUE OF THE  OBJECTIVE FUNCTION FOR THE
C        ESTIMATES.
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
C             BETA   (R) = INITIAL BETA ESTIMATES
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C             NS     (I) = SIZE OF DAT
C             PHIIND (I) = FLAG CONTROLLING PHI FUNCTION USED
C        YIELDED
C             ALPHA  (R) = ALPHA ESTIMATE
C             COUNT  (I) = ITERATION COUNTER
C             RFLAG  (I) = RETURN STATUS FLAG
C             X      (R) = BETA ESTIMATES
C             Z      (R) = MINIMIZED FUNCTION VALUE
C   LOCAL GLOSSARY
C             EPSD   (R) = FUNCTION VALUE DIFFERENCE MINIMUM
C             EPSG   (R) = GRADIENT VALUE DIFFERENCE MINIMUM
C             ERR    (R) = TRUST REGION ADJUSTMENT VALUE
C             FACT   (R) = TRUST REGION RADIUS FACTOR
C             FUNUP  (R) = FUNCTION DECREASING VALUE
C             FX     (R) = FUNCTION VALUE AT XP POINTS
C             FY     (R) = FUNCTION VALUE AT Y POINTS
C             G      (R) = GRADIENT OF THE FUNCTION
C             GPROD  (R) = GRADIENT PRODUCT
C             H      (R) = HESSIAN OF THE FUNCTION
C             HPROD  (R) = HESSIAN PRODUCT
C             IRED   (I) = NUMBER OF TRUST REGION REDUCTIONS
C             NDIFF  (R) = FUNCTION VALUE AT DIFFERENCE
C             NEWSTP (R) = NEWTON STEP VALUE
C             NGRAD  (R) = GRADIENT VALUE AT DIFFERENCE
C             PHI    (R) = PHI FUNCTION VALUE
C             R      (R) = TRUST REGION RADIUS
C             RNEW   (R) = TRUST REGION RADIUS FACTOR FOR 1ST REDUCTION
C             RTMP   (R) = INTERMEDIATE TRUST REGION RADIUS
C             S      (R) = DIAGONALS OF THE HESSIAN INVERSE
C             SM1    (R) = TEMPORARY SUMMATION 1
C             SM2    (R) = TEMPORARY SUMMATION 2
C             SM3    (R) = TEMPORARY SUMMATION 3
C             SM4    (R) = TEMPORARY SUMMATION 4
C             TAU    (R) = TRUST REGION RADIUS TAU VALUE
C             TMP    (R) = INTERMEDIATE CALCULATION
C             TMPMI  (R) = INTERMEDIATE TRUST REGION MINIMUM
C             TMPMX  (R) = INTERMEDIATE TRUST REGION MAXIMUM
C             TMAX   (R) = TRUST REGION MAXIMUM
C             TMIN   (R) = TRUST REGION MINIMUM
C             U      (R) = UPPER TRIANGULAR FACTORS OF HESSIAN
C             XMAX   (R) = UPPER BOUNDS OF THE SOLUTION
C             XMIN   (R) = LOWER BOUNDS OF THE SOLUTION
C             XP     (R) = PARAMETER ESTIMATE VALUES
C             Y      (R) = UPDATED PARAMETER ESTIMATE VALUES
C   ERRORS
C        RFLAG  = 1 : MAXIMUM ITERATIONS
C        RFLAG  = 2 : THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY
C        RFLAG  = 4 : THE MODEL ESTIMATES WERE DEEMED INVALID
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
         INTEGER              COUNT         ,ESF           ,MAXIC
     1                       ,N             ,NS            ,PHIIND
     2                       ,RFLAG
         DIMENSION            BETA(2)       ,DAT(NS)       ,X(2)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,IRED          ,J
         DIMENSION            G(3)          ,H(3,3)        ,NEWSTP(3)
     1                       ,S(3)          ,TMAX(3)       ,TMIN(3)
     2                       ,U(3,3)        ,XMAX(3)       ,XMIN(3)
     3                       ,XP(3)         ,Y(3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         IF (ESF.GT.1) THEN
C
C            COMPUTE  CLOSED FORM  SOLUTION OF BETA(0)  AND BETA(1) FOR
C            LEAST SQUARES OR INITIAL ESTIMATES OF  BETA(0) AND BETA(1)
C            FOR MAXIMUM LIKELIHOOD.
             RFLAG = 0
             SM1   = 0.0
             SM2   = 0.0
             SM3   = 0.0
             SM4   = 0.0
             DO 1000 I = 1, NS
                 SM1 = SM1 +          (REAL(I)**PHIIND)**2
                 SM2 = SM2 +  DAT(I)
                 SM3 = SM3 +           REAL(I)**PHIIND
                 SM4 = SM4 +  DAT(I) * REAL(I)**PHIIND
 1000            CONTINUE
C            ENDDO
             X(1) = (SM1      * SM2 - SM3 * SM4)
     1            / (REAL(NS) * SM1 - SM3**2)
             X(2) = (REAL(NS) * SM4 - SM3 * SM2)
     1            / (REAL(NS) * SM1 - SM3**2)
C
C            IF LEAST SQUARES,  SET THE PARAMETER  ESTIMATE VALUES  FOR
C            MINIMIZED FUNCTION VALUE COMPUTATION.
             IF (ESF.EQ.2) THEN
                 ALPHA = 2.0
                 XP(1) = 2.0
                 XP(2) = X(1)
                 XP(3) = X(2)
             ENDIF
         ELSE
C
C            COMPUTE ESTIMATES OF ALPHA, BETA(0), AND BETA(1) FOR MAXI-
C            MUM LIKELIHOOD USING THE TRUST REGION ALGORITHM.  BEGIN BY
C            SETTING  THE COUNTER,  RETURN STATUS FLAG,  FUNCTION VALUE
C            DIFFERENCE AND GRADIENT VALUE DIFFERENCE.
             COUNT =  1
             RFLAG = -1
             EPSD  = 1.0E-4
             EPSG  = 1.0E-8
C
C            SET THE UPPER AND LOWER BOUNDS FOR THE SOLUTION.
             XMAX(1) = 1.0E10
             XMAX(2) = 1.0E10
             XMAX(3) = 1.0E10
             XMIN(1) = 1.0E-10
             XMIN(2) = 1.0E-10
             XMIN(3) = 1.0E-10
C
C            SET THE INITIAL GUESSES FOR BETA(0) AND BETA(1).
             XP(2) = BETA(1)
             IF (XP(2).LT.XMIN(2)) THEN
                 XP(2)  = XMIN(2)
             ELSE
                 IF (XP(2).GT.XMAX(2)) THEN
                     XP(2)  = XMAX(2)
                 ENDIF
             ENDIF
             XP(3) = BETA(2)
             IF (XP(3).LT.XMIN(3)) THEN
                 XP(3)  = XMIN(3)
             ELSE
                 IF (XP(3).GT.XMAX(3)) THEN
                     XP(3)  = XMAX(3)
                 ENDIF
             ENDIF
C
C            COMPUTE THE INITIAL GUESS FOR ALPHA.
             TMP = 0.0
             SM1 = 0.0
             DO 1025 I = 1, NS
                 PHI = XP(2) +  XP(3)  * REAL(I)**PHIIND
                 TMP = PHI   / (DAT(I) + PHI)
                 SM1 = SM1   +  LOG(TMP)
 1025            CONTINUE
C            ENDDO
             XP(1) = -REAL(NS) / SM1
             IF (XP(1).LT.XMIN(1)) THEN
                 XP(1)  = XMIN(1)
             ELSE
                 IF (XP(1).GT.XMAX(1)) THEN
                     XP(1)  = XMAX(1)
                 ENDIF
             ENDIF
C
C            INITIALIZE THE TRUST REGION.
             CALL SLAVFN
     G                   (DAT   , ESF   , N     , NS    , PHIIND, XP
     Y                   ,FX    )
             CALL SLAVGD
     G                   (DAT   , N     , NS    , PHIIND, XP
     Y                   ,G     )
             CALL SLAVHS
     G                   (DAT   , N     , NS    , PHIIND, XP
     Y                   ,H     )
C
C            COMPUTE THE NEWTON STEP.
             CALL FACTOR
     G                   (H     , N
     Y                   ,U     )
             CALL SOLVE
     G                   (N     , U     , G
     Y                   ,NEWSTP)
C
C            COMPUTE THE DIAGONALS OF H INVERSE.
             DO 1075 I = 1, N
                 DO 1050 J = 1, N
                     Y(J) = 0.0
 1050                CONTINUE
C                ENDDO
                 Y(I) = 1.0
                 CALL SOLVE
     G                      (N     , U     , Y
     Y                      ,Y     )
                 S(I) = SQRT(Y(I))
 1075            CONTINUE
C            ENDDO
C
C            COMPUTE THE TRUST REGION RADIUS.
             R = 0.0
             DO 1100 I = 1, N
                 RTMP = ABS(NEWSTP(I) / S(I))
                 R    = MAX(R,RTMP)
 1100            CONTINUE
C            ENDDO
             R = 0.5 * R
C
C            DOWHILE (RFLAG .EQ. -1)
 1125            IRED = 0
                 IF (COUNT.NE.1) THEN
                     CALL SLAVHS
     G                           (DAT   , N     , NS   , PHIIND, XP
     Y                           ,H     )
                 ENDIF
C
C                COMPUTE  THE  MINIMUM  WITHIN THE  TRUST  REGION OF THE
C                QUADRATIC MODEL.
C                DOWHILE ((IRED .LE. 10) .AND. (FY .GT. FUNUP))
 1150                CONTINUE
C
C                    SET UP THE TRUST REGION BOUNDARIES.
                     DO 1175 I = 1, N
                         TMPMI   = XP(I) - R * S(I)
                         TMPMX   = XP(I) + R * S(I)
                         TMIN(I) = MAX(XMIN(I),TMPMI)
                         TMAX(I) = MIN(XMAX(I),TMPMX)
 1175                    CONTINUE
C                    ENDDO
C
C                    COMPUTE THE MINIMUM WITHIN THE TRUST REGION.
                     CALL CONMIN
     G                           (G     , H     , N     , TMAX  , TMIN
     G                           ,XP
     Y                           ,Y     )
                     CALL SLAVFN
     G                           (DAT   , ESF   , N     , NS    , PHIIND
     G                           ,Y
     Y                           ,FY    )
C
C                    CHECK WHETHER  THE FUNCTION HAS  DECREASED  SUFFIC-
C                    IENTLY.
                     GPROD = 0.0
                     DO 1200 I = 1, N
                         GPROD = GPROD + G(I) * (Y(I) - XP(I))
 1200                    CONTINUE
C                    ENDDO
                     FUNUP = FX + 0.0001 * GPROD
C
                     IF (FY.GT.FUNUP) THEN
C                        REDUCE THE TRUST REGION AND TRY AGAIN.
                         IRED = IRED + 1
C
                         IF (IRED.EQ.1) THEN
C                            MAKE SURE THE FIRST REDUCTION  IS ENOUGH TO
C                            CAUSE A  CHANGE IN THE CONSTRAINED MINIMUM.
                             RNEW = R
                             DO 1225 I = 1, N
                                 FACT = ABS(XP(I) - Y(I))
                                 IF (FACT.NE.0.0) THEN
                                     IF (FACT.LT.(RNEW * S(I))) THEN
                                         RNEW = FACT / S(I)
                                     ENDIF
                                 ENDIF
 1225                            CONTINUE
C                            ENDDO
                             RTMP = R
                             IF (RNEW.LT.R) THEN
                                 R = 0.9 * RNEW
                             ENDIF
                         ENDIF
C
C                        REDUCE THE TRUST REGION RADIUS.
                         IF (((IRED.EQ.1) .AND. (RNEW.GE.RTMP)) .OR.
     1                       ((IRED.GT.1) .AND. (IRED.LE.10)))  THEN
                             TAU  = -GPROD / (2.0 * (FY - FX - GPROD))
                             FACT = TAU
                             IF (TAU.LE.0.1) THEN
                                 FACT = 0.1
                             ENDIF
                             IF (TAU.GT.0.5) THEN
                                 FACT = 0.5
                             ENDIF
                             R = R * FACT
                         ENDIF
                     ENDIF
                     IF ((IRED.LE.10) .AND. (FY.GT.FUNUP)) GO TO 1150
C                ENDWHILE
C
C                SET RFLAG IF TRUST REGION COULD NOT BE ADJUSTED PROPER-
C                LY.
                 IF (IRED.GT.10) THEN
                     RFLAG = 2
                 ELSE
C                    AN ACCEPTABLE POINT, Y HAS BEEN FOUND. SET RFLAG IF
C                    MAXIMUM ITERATIONS WAS REACHED.
                     COUNT = COUNT + 1
                     IF (COUNT.GT.MAXIC) THEN
                         RFLAG = 1
                     ELSE
C                        EVALUATE THE GRADIENT AT THE NEW POINT.
                         CALL SLAVGD
     G                               (DAT   , N    , NS   , PHIIND, Y
     Y                               ,G     )
C
C                        TO  TEST FOR CONVERGENCE,  COMPUTE THE  NORM OF
C                        THE GRADIENT AND THE NORM OF THE DIFFERENCE BE-
C                        TWEEN THIS POINT AND THE PREVIOUS POINT.
                         NDIFF = 0.0
                         NGRAD = 0.0
                         DO 1250 I = 1, N
                             NDIFF = NDIFF + (XP(I) - Y(I))**2
                             NGRAD = NGRAD + G(I)**2
 1250                        CONTINUE
C                        ENDDO
                         NDIFF = SQRT(NDIFF)
                         NGRAD = SQRT(NGRAD)
C
C                        SET RFLAG IF CONVERGENCE OCCURRED.
                         IF ((NDIFF.LT.EPSD) .OR. (NGRAD.LT.EPSG)) THEN
                             RFLAG = 0
                         ELSE
C                            ADJUST THE TRUST REGION.
                             HPROD = 0.0
                             DO 1300 I = 1, N
                                 DO 1275 J  = 1, N
                                     HPROD = HPROD + H(I,J)
     1                                     * (Y(I) - XP(I))
     2                                     * (Y(J) - XP(J))
 1275                                CONTINUE
C                                ENDDO
 1300                            CONTINUE
C                            ENDDO
                             ERR = ABS(FX + GPROD + 0.5 * HPROD - FY)
                             IF (ERR.LE.(0.1 * ABS(FY))) THEN
                                 R = 2.0 * R
                             ENDIF
                             IF (ERR.GE.(0.75 * ABS(FY))) THEN
                                 R = 0.5 * R
                             ENDIF
C
C                            SWAP POINTS AND FUNCTION VALUES.
                             DO 1325 I = 1, N
                                 XP(I) = Y(I)
 1325                            CONTINUE
C                            ENDDO
                             FX = FY
                         ENDIF
                     ENDIF
                 ENDIF
                 IF (RFLAG.EQ.-1) GO TO 1125
C            ENDWHILE
C
C            SET THE RESULTANT STATISTICS.
             ALPHA = XP(1)
             X(1)  = XP(2)
             X(2)  = XP(3)
         ENDIF
C
C        IF MAXIMUM LIKELIHOOD  OR  LEAST SQUARES, COMPUTE THE MINIMIZED
C        FUNCTION VALUE.
         IF (ESF.LT.3) THEN
             CALL SLAVFN
     G                   (DAT   , ESF   , N     , NS    , PHIIND, XP
     Y                   ,Z     )
         ENDIF
C
C        RESET THE ERROR FLAG IF THE ESTIMATES DO NOT ALLOW FOR THE PRE-
C        DICTIONS TO BE MADE.
         IF ((RFLAG.EQ.0) .AND. ((X(1)+X(2)).LT.0.0)) THEN
             RFLAG = 4
         ENDIF
         RETURN
         END





         SUBROUTINE SLAVFN
     G                     (DAT   , ESF   , N     , NS    , PHIIND, X
     Y                     ,FX    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SLAVFN
C    G                     (DAT   , ESF   , N     , NS    , PHIIND, X
C    Y                     ,FX    )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   TRUST REGION IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   TRUST REGION CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE EITHER THE  LIKELIHOOD FUNCTION FOR X IF ESF=1, OR
C        THE SUMS-OF-SQUARES IF ESF=2,  FOR THE  LITTLEWOOD AND  VERRALL
C        BAYESIAN RELIABILITY GROWTH MODEL.
C   DESCRIPTION
C        IF  ESF IS SET TO 1, THE LIKELIHOOD FUNCTION  IS CALCULATED AND
C        IF  ESF IS SET TO 2, THE LEAST SQUARES FUNCTION  IS CALCULATED.
C        FOR EITHER TYPE OF ESTIMATION, THE RESULT IS STORED IN FX.
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
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C             NS     (I) = SIZE OF DAT
C             PHIIND (I) = FLAG CONTROLLING PHI FUNCTION USED
C             X      (R) = PARAMETER ESTIMATE VALUES
C        YIELDED
C             FX     (R) = MINIMIZED FUNCTION VALUE
C   LOCAL GLOSSARY
C             PHI    (R) = PHI FUNCTION VALUE
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
         INTEGER              ESF           ,N             ,NS
     1                       ,PHIIND
         DIMENSION            DAT(NS)       ,X(N)
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
C        INITIALIZE THE MINIMIZED FUNCTION VALUE.
         IF (ESF.EQ.1) THEN
             FX = -REAL(NS) * LOG(X(1))
         ELSE
             FX = 0.0
         ENDIF
C
C        CALCULATE THE MINIMIZED FUNCTION  VALUE FOR THE MAXIMUM LIKELI-
C        HOOD FUNCTION OR LEAST SQUARES FUNCTION.
         DO 1000 I = 1, NS
             PHI = X(2) + X(3) * REAL(I)**PHIIND
             IF (ESF.EQ.1) THEN
                 FX = FX - X(1) * LOG(PHI) + (X(1) + 1.0)
     1              * LOG(DAT(I) + PHI)
             ELSE
                 FX = FX + (DAT(I) - PHI / (X(1) - 1.0))**2
             ENDIF
 1000        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE SLAVGD
     G                     (DAT   , N     , NS    , PHIIND, X
     Y                     ,G     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SLAVGD
C    G                     (DAT   , N     , NS    , PHIIND, X
C    Y                     ,G     )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE GRADIENT OF THE LIKELIHOOD FUNCTION FOR  X FOR
C        THE  LITTLEWOOD AND VERRALL BAYESIAN  RELIABILITY GROWTH MODEL.
C   DESCRIPTION
C        THE  GRADIENT OF THE  LIKELIHOOD FUNCTION IS CALCULATED AND THE
C        THE RESULT IS STORED IN G.
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
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C             NS     (I) = SIZE OF DAT
C             PHIIND (I) = FLAG CONTROLLING PHI FUNCTION USED
C             X      (R) = PARAMETER ESTIMATE VALUES
C        YIELDED
C             G      (R) = GRADIENT VALUES FOR THE FUNCTION
C   LOCAL GLOSSARY
C             EX     (R) = EXPONENT VALUE
C             PHI    (R) = PHI FUNCTION VALUE
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
         INTEGER              N             ,NS            ,PHIIND
         DIMENSION            DAT(NS)       ,G(N)          ,X(N)
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
C        INITIALIZE THE GRADIENT VALUES.
         G(1) = -REAL(NS) / X(1)
         G(2) = 0.0
         G(3) = 0.0
C
C        CALCULATE THE GRADIENT VALUES FOR THE  MAXIMUM LIKELIHOOD FUNC-
C        TION.
         DO 1000 I = 1, NS
             EX   =  REAL(I)**PHIIND
             PHI  =  X(2)   + X(3) * EX
             TMP  =  DAT(I) +  PHI
             G(1) =  G(1)   -  LOG(PHI / TMP)
             G(2) =  G(2)   - (X(1)    / PHI) + (X(1) + 1.0) / TMP
             PHI  =  X(1)   /  PHI
             TMP  = (X(1)   +  1.0) / TMP
             G(3) =  G(3)   - (PHI  - TMP) * EX
 1000        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE SLAVHS
     G                     (DAT   , N     , NS    , PHIIND, X
     Y                     ,H     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SLAVHS
C    G                     (DAT   , N     , NS    , PHIIND, X
C    Y                     ,H     )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE  HESSIAN OF THE  LIKELIHOOD FUNCTION FOR X FOR
C        THE  LITTLEWOOD AND VERRALL BAYESIAN RELIABILITY GROWTH  MODEL.
C   DESCRIPTION
C        THE  HESSIAN OF THE  LIKELIHOOD FUNCTION IS CALCULATED  AND THE
C        RESULT IS STORED IN H.
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
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C             NS     (I) = SIZE OF DAT
C             PHIIND (I) = FLAG CONTROLLING PHI FUNCTION USED
C             X      (R) = PARAMETER ESTIMATE VALUES
C        YIELDED
C             H      (R) = HESSIAN VALUES FOR THE FUNCTION
C   LOCAL GLOSSARY
C             D      (R) = EIGENVALUES OF THE HESSIAN
C             DUM1   (R) = DUMMY STORAGE ARRAY 1 FOR EGNVAL CALL
C             DUM2   (R) = DUMMY STORAGE ARRAY 2 FOR EGNVAL CALL
C             EMIN   (R) = VALUE TO BE  ADDED TO THE  DIAGONAL  ELEMENTS
C                          OF THE HESSIAN  WHEN THE HESSIAN IS NOT POSI-
C                          TIVE DEFINITE.
C             EX     (R) = EXPONENT VALUE
C             HTMP   (R) = INTER. STORAGE OF HESSIAN FOR EGNVAL CALL
C             PHI    (R) = PHI FUNCTION VALUE
C             PHINV  (R) = INVERSE OF PHI FUNCTION VALUE
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
         INTEGER              N             ,NS            ,PHIIND
         DIMENSION            DAT(NS)       ,H(N,N)        ,X(N)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J
         DIMENSION            D(3)          ,DUM1(3,3)     ,DUM2(3,3)
     1                       ,HTMP(3,3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE THE HESSIAN VALUES.
         H(1,1) = REAL(NS) / X(1)**2
         H(1,2) = 0.0
         H(1,3) = 0.0
         H(2,2) = 0.0
         H(2,3) = 0.0
         H(3,3) = 0.0
C
C        CALCULATE THE HESSIAN VALUES FOR THE  MAXIMUM  LIKELIHOOD FUNC-
C        TION.
         DO 1000 I = 1, NS
             EX     = REAL(I)**PHIIND
             PHI    = X(2)    +  X(3)  * EX
             PHINV  = 1.0     /  PHI
             TMP    = 1.0     / (PHI   + DAT(I))
             H(1,2) = H(1,2)  - (PHINV - TMP)
             H(1,3) = H(1,3)  - (PHINV - TMP) * EX
             H(2,2) = H(2,2)  +  X(1)  * PHINV**2 - (X(1) + 1.0)
     1              * TMP**2
             H(2,3) = H(2,3)  +  X(1)  * PHINV**2 * EX    - (X(1) + 1.0)
     1              * TMP**2  *  EX
             H(3,3) = H(3,3)  + (X(1)  * PHINV**2 - (X(1) + 1.0)
     1              * TMP**2) *  EX**2
 1000        CONTINUE
C        ENDDO
C
C        STORE THE SYMMETRICAL HESSIAN VALUES.
         H(2,1) = H(1,2)
         H(3,1) = H(1,3)
         H(3,2) = H(2,3)
C
C        COMPUTE THE EIGENVALUES OF THE HESSIAN.
         DO 1050 I = 1, N
             DO 1025 J = 1, N
                 HTMP(I,J) = H(I,J)
 1025            CONTINUE
C            ENDDO
 1050        CONTINUE
C        ENDDO
         CALL EGNVAL
     G           (N      , N     , HTMP  , DUM1  , DUM2
     Y           ,D      )
C
C        WHEN THE HESSIAN IS NOT POSITIVE DEFINITE, REPLACE THE DIAGONAL
C        ELEMENTS OF THE  HESSIAN WITH THE  HESSIAN  VALUE + EMIN.  THIS
C        FORCES THE HESSIAN TO BE POSITIVE DEFINITE.
         IF (D(1).LE.0.0) THEN
             EMIN = ABS(D(1)) + D(N) * 0.0001
             DO 1075 I = 1, N
                 H(I,I) = H(I,I) + EMIN
 1075            CONTINUE
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