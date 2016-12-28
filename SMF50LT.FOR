         FUNCTION SGEOLS
     G                   (AB    , DAT   , NS    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION SGEOLS
C    G                   (AB    , DAT   , NS    )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   DEKKER-BRENT IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   DEKKER-BRENT CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE VALUE OF THE FUNCTION FOR THE GEOMETRIC  MODEL
C        (REF: NSWCDD TR 82-171)  USING  THE  LEAST  SQUARES  ESTIMATION
C        METHOD.
C   DESCRIPTION
C        THIS ROUTINE CALCULATES THE VALUE OF THE  FUNCTION AT THE POINT
C        AB (ESTIMATE OF THE PROPORTIONALITY CONSTANT).
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
C             AB     (R) = ESTIMATE OF PROPORTIONALITY CONSTANT
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             NS     (I) = SIZE OF DAT
C        YIELDED
C             SGEOLS (R) = FUNCTION VALUE AT POINT AB
C   LOCAL GLOSSARY
C             SM1    (R) = TEMPORARY SUMMATION 1
C             SM2    (R) = TEMPORARY SUMMATION 2
C             SM3    (R) = TEMPORARY SUMMATION 3
C             SM4    (R) = TEMPORARY SUMMATION 4
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
         INTEGER              NS
         DIMENSION            DAT(NS)
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
C        INITIALIZE THE SUMMATION VARIABLES.
         SM1 = 0.0
         SM2 = 0.0
         SM3 = 0.0
         SM4 = 0.0
C
C        PERFORM THE SUMMATIONS OVER ALL ENTRIES.
         DO 1000 I = 1, NS
             TMP = AB**(2 *   (I - 1))
             SM1 = SM1    + (1.0 / TMP)
             SM4 = SM4    + (REAL(I - 1) / TMP)
             TMP = AB**(I - 1)
             SM2 = SM2    + ((DAT(I) * REAL(I - 1)) / TMP)
             SM3 = SM3    +  (DAT(I) / TMP)
 1000        CONTINUE
C        ENDDO
C
C        CALCULATE THE FUNCTION VALUE.
         SGEOLS = SM1 * SM2 - SM3 * SM4
         RETURN
         END
         SUBROUTINE SGEOMA
     G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
     Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SGEOMA
C    G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
C    Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM FOUR TYPES OF  MODEL APPLICABILITY ANALYSIS FOR  THE
C        GEOMETRIC MODEL.
C
C        THESE INCLUDE:
C             1) THE MODEL ACCURACY;
C             2) THE MODEL BIAS;
C             3) THE MODEL NOISE; AND
C             4) THE MODEL TREND
C   DESCRIPTION
C        THE ROUTINE  CONTROLS THE EXECUTIONS  OF THE  APPLICABLE SMFLIB
C        ROUTINE, SGEOMD.  THE PROCESSING, AS DEFINED  IN THE LITTLEWOOD
C        PAPER (SEE REFERENCES IN THE MAIN PROGRAM) BASICALLY INVOLVES A
C        SERIES OF EXECUTIONS WHERE  NSB TO NSE DATA POINTS ARE UTILIZED
C        IN THE MODEL.
C
C        FOR EACH ITERATION,  THE MODEL PARAMETERS (ESTIMATES)  FROM THE
C        CURRENT ITERATION ARE USED IN CONJUNCTION WITH THE OBSERVED DA-
C        TA FROM THE NEXT ITERATION POINT  TO DETERMINE AN OVER-ALL PIC-
C        TURE OF THE APPLICABILITY OF THE MODEL  FOR THE PARTICULAR DATA
C        SET.
C
C        THIS ONE ROUTINE IS USED TO OBTAIN ALL FOUR ANALYSES; THE SIXTH
C        ARGUMENT, TYP, IS USED TO INDICATE HOW THE MODEL PARAMETERS ARE
C        TO BE USED.
C
C        NOTE, THE VECTOR, V, IS NOT USED IN THE NOISE ANALYSIS  AND THE
C        VECTOR, VPRE, IS ONLY USED IN THE BIAS ANALYSIS;  HOWEVER, THEY
C        SHOULD BE AVAILABLE TO THE ROUTINE FOR ALL ACCESSES.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED TBF DATA
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C             TYP    (I) = ANALYSIS TYPE TO BE PERFORMED (SEE PURPOSE)
C        YIELDED
C             INDX   (I) = ANALYSIS ITERATION COUNTER (AT RETURN)
C             RFLAG  (I) = MODEL RESULTS ERROR FLAG
C             STAT   (R) = ANALYSIS RESULTS STATISTIC
C             V      (R) = ANALYSIS RESULTS ARRAY OF SIZE NSR
C             VPRE   (R) = VECTOR OF BIAS (U-PLOT) DATA PRIOR TO SORTING
C   LOCAL GLOSSARY
C             ESF    (I) = MODEL ESTIMATION FLAG (ML ONLY)
C             STATS  (R) = MODEL STATISTICS RESULTS ARRAY
C             SUMY   (R) = SUM OF THE PREVIOUS Y'S
C             TMP    (R) = INTERMEDIATE CALCULATION
C             TOTY   (R) = SUM OF ALL THE Y'S
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
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              INDX          ,NS            ,NSB
     1                       ,NSE           ,NSR           ,RFLAG
     2                       ,TYP
         DIMENSION            DAT(NS)       ,V(NSR)        ,VPRE(NSR)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              ESF           ,I
         DIMENSION            STATS(4,3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE  THE ITERATION COUNTER,  STORAGE INDEX,  MODEL ERROR
C        FLAG, RESULTS STATISTIC, AND SUM OF THE Y-PLOT VALUES LOCATION.
         I     = NSB - 1
         INDX  = 0
         RFLAG = 0
         STAT  = 0.0
         TOTY  = 0.0
C
C        INITIALIZE THE MODEL SELECTION FLAG TO INDICATE MAXIMUM LIKELI-
C        HOOD ANALYSIS (LEAST SQUARES IS NOT ALLOWED).
         ESF   = 1
C
         IF (TYP.EQ.3) THEN
C            INITIALIZE THE CONSTANT PORTION OF THE NOISE CALCULATION.
             TMP = -LOG(0.5)
         ENDIF
C
C        DOWHILE (PROCESSING LEFT AND NO ERRORS)
 1000        I = I + 1
             IF ((I.GT.NSE) .OR. (RFLAG.NE.0)) GO TO 1025
C
C            COMPUTE MODEL ESTIMATES.
             CALL SGEOMD
     G                   (ESF   , I     ,  DAT
     Y                   ,STATS , RFLAG )
C
C            INCREMENT THE ITERATION COUNTER FOR STORAGE.
             INDX = INDX + 1
C
             IF (RFLAG.EQ.0) THEN
                 IF     (TYP.EQ.1) THEN
C                    ACCURACY ANALYSIS IS DESIRED;  APPEND THE CONTRIBU-
C                    TION OF THE  CURRENT  ITERATION TO  THE PREQUENTIAL
C                    LIKELIHOOD STATISTIC.
                     V(INDX) = 0.0  - (LOG(STATS(2,1))
     1                       + REAL(I) * LOG(STATS(1,1))
     2                       - STATS(2,1) * STATS(1,1)**I * DAT(I+1))
                     STAT    = STAT + V(INDX)
C
                 ELSEIF ((TYP.EQ.2) .OR. (TYP.EQ.4)) THEN
C                    BIAS OR TREND ANALYSIS IS DESIRED;  STORE THE FUNC-
C                    TIONAL VALUE FOR THIS ELEMENT OF THE U-PLOT.
                     V(INDX) = 1.0
     1                       - EXP(-STATS(2,1) * STATS(1,1)**I
     2                       * DAT(I+1))
                     IF (TYP.EQ.4) THEN
C                        TREND ANALYSIS IS DESIRED; TRANSFORM THE U-PLOT
C                        VALUE TO THE Y-PLOT REPRESENTATION.
                         IF (V(INDX).LT.1.0) THEN
                             V(INDX) = -LOG(1.0 - V(INDX))
                             TOTY    = TOTY + V(INDX)
                         ELSE
                             RFLAG   = 5
                         ENDIF
                     ENDIF
C
                 ELSE
C                    NOISE ANALYSIS IS DESIRED; COMPUTE THE FUNCTION FOR
C                    THE NEXT POINT.
                     MNXT = TMP / (STATS(2,1) * STATS(1,1)**I)
C
C                    PERFORM THE ADDITION OF THE NOISE ON ALL ITERATIONS
C                    EXCEPT THE FIRST; AND THEN PREPARE FOR THE NEXT.
                     IF (I.GT.NSB) THEN
                         STAT = STAT + ABS((MNXT - MCUR) / MCUR)
                     ENDIF
                     MCUR = MNXT
C
                 ENDIF
             ENDIF
             GO TO 1000
 1025        CONTINUE
C        ENDWHILE
C
         IF ((TYP.EQ.2) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE U-PLOT VECTOR  AND COMPUTE
C            THE KOLMOGOROV STATISTIC. FIRST STORE THE UNSORTED DATA.
             DO 1050 I = 1, NSR
                 VPRE(I) = V(I)
 1050            CONTINUE
C            ENDDO
             CALL CMPMAX
     G                   (NSR   , 1
     B                   ,STAT  , V     )
         ENDIF
C
         IF ((TYP.EQ.4) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE Y-PLOT VECTOR.
             SUMY = 0.0
             DO 1075 I = 1, NSR
                 SUMY = SUMY + V(I)
                 V(I) = SUMY / TOTY
 1075            CONTINUE
C            ENDDO
C
C            COMPUTE THE KOLMOGOROV STATISTIC;  NOTE, SORTING IS NOT RE-
C            QUIRED FOR THE Y-PLOT DATA.
             CALL CMPMAX
     G                   (NSR   , 0
     B                   ,STAT  , V     )
         ENDIF
         RETURN
         END
         SUBROUTINE SGEOMD
     G                     (ESF   , NS    , DAT
     Y                     ,STATS , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SGEOMD
C    G                     (ESF   , NS    , DAT
C    Y                     ,STATS , RFLAG )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   DEKKER-BRENT IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   DEKKER-BRENT CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE  THE ESTIMATES  USING THE  MAXIMUM  LIKELIHOOD AND
C        LEAST SQUARES METHODS OF THE GEOMETRIC MODEL.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE CALL LINE (ESF)  WILL BE USED BY THE
C        ROUTINE TO DETERMINE  WHICH OF THE TWO METHODS  OF EXECUTION IS
C        DESIRED.
C
C        ONCE THE ITERATIVE EXECUTION IS INITIATED, PROCESSING WILL CON-
C        TINUE UNTIL ONE OF TWO  POSSIBLE TERMINATION STATES IS REACHED.
C        THE LAST ARGUMENT OF THE CALL LINE (RFLAG) WILL BE SET TO INDI-
C        CATE TO THE CALLING  ROUTINE THE REASON FOR EXIT.  THE POSSIBLE
C        REASONS FOR SGEOMD EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
C
C        THE RESULTANT STATISTICS WILL BE RETURNED IN THE STATS ARGUMENT
C        FOR  A  RFLAG  VALUE  OF 0  ONLY.   FOR THE  MAXIMUM LIKELIHOOD
C        METHOD,  THIS WILL  INCLUDE THE  FOUR STATISTICS  AND THEIR 95%
C        CONFIDENCE INTERVALS.  UNDER THE LEAST SQUARES METHOD, THE CON-
C        FIDENCE INTERVALS WILL NOT BE COMPUTED.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS  SUBPROGRAM  CONTAINS  CODE WHICH DOES NOT COMPLY WITH THE
C        PROGRAMMING STANDARDS; GROUP LEADER APPROVAL HAS BEEN OBTAINED.
C       -SPECIFICALLY:
C        THE ARGUMENT  FC IN THE ACCESS TO THE ROUTINE ZERO, IS ACTUALLY
C        INITIALIZED IN THE ACCESSED ROUTINE. WHEN ICOUNT HAS A VALUE OF
C        MORE THAN ONE, THE ARGUMENT BECOMES A BOTH (AS MARKED).  HENCE,
C        FC APPEARS UNDEFINED IN THIS ROUTINE'S COMPILATION LISTING.
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             NS     (I) = SIZE OF DAT
C        YIELDED
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = RESULTS ARRAY
C   LOCAL GLOSSARY
C             A      (R) = LOWER BOUND OF PROPORTIONALITY CONSTANT
C             B      (R) = UPPER BOUND OF PROPORTIONALITY CONSTANT
C             COUNT  (I) = ITERATION COUNTER
C             FA     (R) = VALUE OF FUNCTION AT A
C             FB     (R) = VALUE OF FUNCTION AT B
C             FC     (R) = VALUE OF FUNCTION AT C
C             ICOUNT (I) = STEP COUNTER
C             IFLAG  (I) = CONVERGENCE FLAG
C             RN     (R) = REAL VARIABLE STORAGE OF NS
C             S1     (R) = SUMMATION VARIABLE
C             S2     (R) = SUMMATION VARIABLE
C             TMP    (R) = INTERMEDIATE CALCULATION
C   ERRORS
C        RFLAG  = 3 : THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
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
         INTEGER              ESF           ,NS            ,RFLAG
         DIMENSION            DAT(NS)       ,STATS(4,3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              COUNT         ,I             ,ICOUNT
     1                       ,IFLAG
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE COUNTER  AND RETURN STATUS FLAG.
         COUNT = 0
         RFLAG = -1
C
C        ZERO ALL LOCATIONS OF THE RESULTS VECTOR.
         DO 1000 I = 1, 4
             STATS(I,1) = 0.0
             STATS(I,2) = 0.0
             STATS(I,3) = 0.0
 1000        CONTINUE
C        ENDDO
C
C        COMPUTE THE VALUE OF THE FUNCTION AT ITS UPPER BOUND.
         B = .999
         A = B / 2.0
         IF (ESF.EQ.1) THEN
             FB = SGEOML
     G                   (B     , DAT   , NS    )
         ELSE
             FB = SGEOLS
     G                   (B     , DAT   , NS    )
         ENDIF
C
C        COMPUTE THE VALUE OF THE FUNCTION AT A POINT WHERE ITS SIGN  IS
C        THE  OPPOSITE  OF  THE  COMPUTED  VALUE AT THE  FUNCTIONS UPPER
C        BOUND.
C
C        DOWHILE (RFLAG .EQ. -1)
 1025        COUNT = COUNT + 1
             IF (ESF.EQ.1) THEN
                 FA = SGEOML
     G                       (A     , DAT   , NS    )
             ELSE
                 FA = SGEOLS
     G                       (A     , DAT   , NS    )
             ENDIF
C
C            SET RFLAG, IF CONVERGENCE POSSIBLE.
             IF ((FA*FB).LE.0.0) THEN
                 RFLAG = 0
             ENDIF
C
C            SET RFLAG, IF THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
             IF (COUNT.GE.5) THEN
                 RFLAG = 3
             ENDIF
             IF (RFLAG.EQ.-1) THEN
                 A = A / 2.0
             ENDIF
             IF (RFLAG.EQ.-1) GO TO 1025
C        ENDWHILE
C
         IF (RFLAG.EQ.0) THEN
C            COMPUTE THE  ESTIMATE OF THE  PROPORTIONALITY CONSTANT  AND
C            STORE THE VALUE IN STATS(1,1).
             IFLAG  = -1
             ICOUNT = 1
C
C            DOWHILE (IFLAG .EQ. -1)
C                COMPUTE THE ZERO OF THE FUNCTION IN THE INTERVAL  A AND
C                B; NOTE THE ARGUMENT FC IS INITIALIZED BY THE ZERO ROU-
C                TINE WHEN ICOUNT IS SET TO ONE.   <<SEE RESTRICTIONS <<
 1050            CALL ZERO
     B                     (A     , B     , FA    , FB    , FC
     B                     ,ICOUNT
     Y                     ,IFLAG )
C
                 IF (IFLAG.EQ.-1) THEN
C                    COMPUTE THE VALUE OF THE FUNCTION AT ITS NEW  UPPER
C                    BOUND.
                     IF (ESF.EQ.1) THEN
                         FB = SGEOML
     G                               (B     , DAT   , NS    )
                     ELSE
                         FB = SGEOLS
     G                               (B     , DAT   , NS    )
                     ENDIF
                 ENDIF
                 IF (IFLAG.EQ.-1) GO TO 1050
C            ENDWHILE
             STATS(1,1) = B
         ENDIF
C
C        THE STATISTICS  CALCULATIONS  WILL ONLY BE MADE FOR RFLAG INDI-
C        CATING SUCCESSFUL CONVERGENCE.
         IF (RFLAG.EQ.0) THEN
             IF (ESF.EQ.1) THEN
C                COMPUTE THE STATISTICS  FOR THE  MAXIMUM LIKELIHOOD ME-
C                THOD, STARTING BY STORING  THE SIZE IN A REAL VARIABLE.
                 RN = NS
C
C                USING THE PHI ESTIMATE, COMPUTE ITS 95% CI.
                 TMP        = 1.95 * SQRT(((12.0 * STATS(1,1))**2)
     1                      / (RN * (RN - 1.0) * (RN + 1.0)))
                 STATS(1,2) = STATS(1,1) - TMP
                 STATS(1,3) = STATS(1,1) + TMP
                 IF (STATS(1,2).LT.0.0) THEN
                     STATS(1,2) = 0.0
                 ENDIF
                 IF (STATS(1,3).GT.1.0) THEN
                     STATS(1,3) = 1.0
                 ENDIF
C
C                COMPUTE THE HAZARD RATE AND ITS 95% CI.
                 TMP = 0.0
                 DO 1075 I = 1, NS
                     TMP = TMP + ((STATS(1,1)**I) * DAT(I))
 1075                CONTINUE
C                ENDDO
                 STATS(2,1) = (STATS(1,1) * RN) / TMP
                 TMP        = 1.95 * SQRT(((2.0 * STATS(2,1))
     1                      * ((2.0 * RN) - 1.0)) / (RN * (RN + 1.0)))
                 STATS(2,2) = STATS(2,1) - TMP
                 IF (STATS(2,2).LT.0.0) THEN
                     STATS(2,2) = 0.0
                 ENDIF
                 STATS(2,3) = STATS(2,1) + TMP
C
C                COMPUTE THE MTBNF AND ITS 95% CI.
                 STATS(3,1) = 1.0 / (STATS(2,1) * (STATS(1,1)**NS))
                 TMP        = 1.95 * SQRT((2.0 * ((2.0 * RN) + 1.0))
     1                      / (STATS(2,1)**2 * (STATS(1,1)**(2 * NS))
     2                      * RN * (RN - 1.0)))
                 STATS(3,2) = STATS(3,1) - TMP
                 IF (STATS(3,2).LT.0.0) THEN
                     STATS(3,2) = 0.0
                 ENDIF
                 STATS(3,3) = STATS(3,1) + TMP
C
C                COMPUTE THE PURIFICATION LEVEL AND ITS 95% CI.
                 STATS(4,1) = 1.0 - (STATS(1,1)**NS)
                 TMP        = 1.95 * SQRT(((12.0 * RN)
     1                      * (STATS(1,1)**(2 * NS))) / ((RN - 1.0)
     2                      * (RN + 1.0)))
                 STATS(4,2) = STATS(4,1) - TMP
                 IF (STATS(4,2).LT.0.0) THEN
                     STATS(4,2) = 0.0
                 ENDIF
                 STATS(4,3) = STATS(4,1) + TMP
                 IF (STATS(4,3).GT.1.0) THEN
                     STATS(4,3) = 1.0
                 ENDIF
             ELSE
C                COMPUTE THE  STATISTICS FOR  THE  LEAST SQUARES METHOD,
C                STARTING WITH THE INITIAL HAZARD RATE.
                 S1 = 0.0
                 S2 = 0.0
                 DO 1100 I = 1, NS
                     S1 = S1 + (1.0 / (STATS(1,1)**(2 * (I - 1))))
                     S2 = S2 + (DAT(I) / (STATS(1,1)**(I - 1)))
 1100                CONTINUE
C                ENDDO
                 STATS(2,1) = S1 / S2
C
C                COMPUTE THE MTBNF.
                 STATS(3,1) = 1.0 / (STATS(2,1) * STATS(1,1)**NS)
C
C                COMPUTE THE PURIFICATION LEVEL.
                 STATS(4,1) = 1.0 - STATS(1,1)**NS
             ENDIF
         ENDIF
         RETURN
         END
         FUNCTION SGEOML
     G                   (AB    , DAT   , NS    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION SGEOML
C    G                   (AB    , DAT   , NS    )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   DEKKER-BRENT IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   DEKKER-BRENT CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE VALUE OF THE FUNCTION FOR THE  GEOMETRIC MODEL
C        (REF: NSWCDD TR 82-171) USING THE MAXIMUM LIKELIHOOD ESTIMATION
C        METHOD.
C   DESCRIPTION
C        THIS ROUTINE CALCULATES THE  VALUE OF THE FUNCTION AT THE POINT
C        AB (ESTIMATE OF THE PROPORTIONALITY CONSTANT).
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
C             AB     (R) = ESTIMATE OF THE PROPORTIONALITY CONSTANT
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             NS     (I) = SIZE OF DAT
C        YIELDED
C             SGEOML (R) = FUNCTION VALUE AT POINT AB
C   LOCAL GLOSSARY
C             SM1    (R) = TEMPORARY SUMMATION 1
C             SM2    (R) = TEMPORARY SUMMATION 2
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
         INTEGER              NS
         DIMENSION            DAT(NS)
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
C        INITIALIZE THE SUMMATION VARIABLES.
         SM1 = 0.0
         SM2 = 0.0
C
C        PERFORM THE TWO SUMMATION TYPES OVER ALL ENTRIES.
         DO 1000 I = 1, NS
             TMP = AB**I *  DAT(I)
             SM1 = SM1   + (TMP    * REAL(I))
             SM2 = SM2   +  TMP
 1000        CONTINUE
C        ENDDO
C
C        CALCULATE THE FUNCTION VALUE.
         SGEOML = SM1 / SM2 - REAL(NS + 1) / 2.0
         RETURN
         END
         SUBROUTINE SGEOPR
     G                     (D     , PCON  , NS    , DAT
     Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SGEOPR
C    G                     (D     , PCON  , NS    , DAT
C    Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO CREATE THE PREDICTED DATA VECTOR AND THE KOLMOGOROV DISTANCE
C        FOR THE GEOMETRIC MODEL.
C   DESCRIPTION
C        TO CREATE THE  PREDICTED DATA VECTOR, SGEOPR USES THE ARGUMENTS
C        D AND PCON, AND ASSIGNS NS ELEMENTS OF THE DATA VECTOR PDAT.
C
C        THE CALCULATION FOR THE  KOLMOGOROV STATISTIC  WAS ADDED DURING
C        THE UPDATE THE REVISION 5. FOR THAT CALCULATION, IN ADDITION TO
C        THE MODEL ESTIMATES, THE ACTUAL DATA VECTOR IS NEEDED. ONCE THE
C        NS ELEMENTS ARE COMPUTED, USING THE SAME EQUATION AS FOR A BIAS
C        ANALYSIS OF THIS MODEL, THE MAXIMUM DIFFERENCE IS DETERMINE VIA
C        THE CMPMAX ROUTINE, AND THE SIGNIFICANCE OF THE VALUE IS DETER-
C        MINED VIA THE CHKGOF ROUTINE (BOTH OF WHICH ARE RESIDENT IN THE
C        SMERFS5 LIBRARY OF SUPPORT ROUTINES).
C
C        NOTE, IN AN EFFORT TO RETAIN THE  MODULARITY OF THE PROCESSING,
C        NO ATTEMPT WAS MADE TO REUSE INTERMEDIATE CALCULATIONS FROM THE
C        ORIGINAL PREDICTION ROUTINE (I.E., DUPLICATE PROCESSING FOR IN-
C        TERMEDIATE VALUES MAY APPEAR IN THIS ROUTINE).
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
C             D      (R) = ESTIMATE OF THE INITIAL HAZARD RATE
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             NS     (I) = NUMBER OF TBF ENTRIES TO BE CREATED
C             PCON   (R) = ESTIMATE OF THE PROPORTIONALITY CONSTANT
C        YIELDED
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL, WHERE:
C                          1 - THE VALUE OF D IS LARGER  THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY NOT PROVIDE AN
C                              ADEQUATE FIT.
C                          2 - THE VALUE OF D IS SMALLER THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY PROVIDE AN AD-
C                              EQUATE FIT.
C             DVAL   (R) = CALCULATED KOLMOGOROV DISTANCE
C             PDAT   (R) = VECTOR OF PREDICTED DATA VALUES
C             V      (R) = VECTOR TO CONTAIN THE U-PLOT DATA SET
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
         INTEGER              DFLG          ,NS
         DIMENSION            DAT(NS)       ,PDAT(NS)      ,V(NS)
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
C        INITIALIZE THE KOLMOGOROV DISTANCE TO ZERO.
         DVAL  = 0.0
C
         DO 1000 I = 1, NS
             IF (I.EQ.1) THEN
                 PDAT(I) = 1.0 /  D
             ELSE
                 PDAT(I) = 1.0 / (D * (PCON**(I - 1)))
             ENDIF
C
C            COMPUTE THE CONTRIBUTION FOR THIS ELEMENT.
             V(I)  = 1.0 - EXP(-D * PCON**(I-1) * DAT(I))
 1000        CONTINUE
C        ENDDO
C
C        COMPLETE THE COMPUTATION FOR THE U-PLOT VECTOR;  NOTE, THE VEC-
C        TOR OF U-PLOT DATA IS SORTED UPON RETURN.
         CALL CMPMAX
     G               (NS    , 1
     B               ,DVAL  , V     )
C
C        DETERMINE IF THE CALCULATED DISTANCE IS SIGNIFICANT.
         CALL CHKGOF
     G               (NS    , DVAL
     Y               ,DFLG  )
         RETURN
         END
         FUNCTION SJAMLS
     G                   (AB    , DAT   , NS    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION SJAMLS
C    G                   (AB    , DAT   , NS    )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO CALCULATE THE VALUE OF THE FUNCTION FOR THE JELINSKI/MORANDA
C        DE-EUTROPHICATION MODEL (REF: NSWCDD TR 82-171) USING THE LEAST
C        SQUARES ESTIMATION METHOD.
C   DESCRIPTION
C        THIS ROUTINE CALCULATES THE VALUE OF THE  FUNCTION AT THE POINT
C        AB (ESTIMATE OF THE TOTAL NUMBER OF FAULTS).
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
C             AB     (R) = ESTIMATE OF THE TOTAL NUMBER OF FAULTS
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             NS     (I) = SIZE OF DAT
C        YIELDED
C             SJAMLS (R) = FUNCTION VALUE AT POINT AB
C   LOCAL GLOSSARY
C             SM1    (R) = TEMPORARY SUMMATION 1
C             SM2    (R) = TEMPORARY SUMMATION 2
C             SM3    (R) = TEMPORARY SUMMATION 3
C             SM4    (R) = TEMPORARY SUMMATION 4
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
         INTEGER              NS
         DIMENSION            DAT(NS)
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
C        INITIALIZE THE SUMMATION VARIABLES.
         SM1 = 0.0
         SM2 = 0.0
         SM3 = 0.0
         SM4 = 0.0
C
C        PERFORM THE SUMMATIONS OVER ALL ENTRIES.
         DO 1000 I = 1, NS
             TMP = AB  - REAL(I) + 1.0
             SM1 = SM1 + DAT(I)  / TMP**2
             SM2 = SM2 + 1.0     / TMP**2
             SM3 = SM3 + DAT(I)  / TMP
             SM4 = SM4 + 1.0     / TMP**3
 1000        CONTINUE
C        ENDDO
C
C        CALCULATE THE FUNCTION VALUE.
         SJAMLS = SM1 * SM2 - SM3 * SM4
         RETURN
         END
         SUBROUTINE SJAMMA
     G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
     Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SJAMMA
C    G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
C    Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM FOUR TYPES OF  MODEL APPLICABILITY ANALYSIS FOR  THE
C        JELINSKI/MORANDA DE-EUTROPHICATION MODEL.
C
C        THESE INCLUDE:
C             1) THE MODEL ACCURACY;
C             2) THE MODEL BIAS;
C             3) THE MODEL NOISE; AND
C             4) THE MODEL TREND
C   DESCRIPTION
C        THE ROUTINE  CONTROLS THE EXECUTIONS  OF THE  APPLICABLE SMFLIB
C        ROUTINE, SJAMMD.  THE PROCESSING, AS DEFINED  IN THE LITTLEWOOD
C        PAPER (SEE REFERENCES IN THE MAIN PROGRAM) BASICALLY INVOLVES A
C        SERIES OF EXECUTIONS WHERE  NSB TO NSE DATA POINTS ARE UTILIZED
C        IN THE MODEL.
C
C        FOR EACH ITERATION,  THE MODEL PARAMETERS (ESTIMATES)  FROM THE
C        CURRENT ITERATION ARE USED IN CONJUNCTION WITH THE OBSERVED DA-
C        TA FROM THE NEXT ITERATION POINT  TO DETERMINE AN OVER-ALL PIC-
C        TURE OF THE APPLICABILITY OF THE MODEL  FOR THE PARTICULAR DATA
C        SET.
C
C        THIS ONE ROUTINE IS USED TO OBTAIN ALL FOUR ANALYSES; THE SIXTH
C        ARGUMENT, TYP, IS USED TO INDICATE HOW THE MODEL PARAMETERS ARE
C        TO BE USED.
C
C        NOTE, THE VECTOR, V, IS NOT USED IN THE NOISE ANALYSIS  AND THE
C        VECTOR, VPRE, IS ONLY USED IN THE BIAS ANALYSIS;  HOWEVER, THEY
C        SHOULD BE AVAILABLE TO THE ROUTINE FOR ALL ACCESSES.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED TBF DATA
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C             TYP    (I) = ANALYSIS TYPE TO BE PERFORMED (SEE PURPOSE)
C        YIELDED
C             INDX   (I) = ANALYSIS ITERATION COUNTER (AT RETURN)
C             RFLAG  (I) = MODEL RESULTS ERROR FLAG
C             STAT   (R) = ANALYSIS RESULTS STATISTIC
C             V      (R) = ANALYSIS RESULTS ARRAY OF SIZE NSR
C             VPRE   (R) = VECTOR OF BIAS (U-PLOT) DATA PRIOR TO SORTING
C   LOCAL GLOSSARY
C             ESF    (I) = MODEL ESTIMATION FLAG (ML ONLY)
C             MCUR   (R) = VALUE OF THE FUNCTION FOR THE CURRENT POINT
C             MNXT   (R) = VALUE OF THE FUNCTION FOR THE NEXT    POINT
C             STATS  (R) = MODEL STATISTICS RESULTS ARRAY
C             SUMY   (R) = SUM OF THE PREVIOUS Y'S
C             TMP    (R) = INTERMEDIATE CALCULATION
C             TOTY   (R) = SUM OF ALL THE Y'S
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
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              INDX          ,NS            ,NSB
     1                       ,NSE           ,NSR           ,RFLAG
     2                       ,TYP
         DIMENSION            DAT(NS)       ,V(NSR)        ,VPRE(NSR)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              ESF           ,I
         DIMENSION            STATS(6)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE  THE ITERATION COUNTER,  STORAGE INDEX,  MODEL ERROR
C        FLAG, RESULTS STATISTIC, AND SUM OF THE Y-PLOT VALUES LOCATION.
         I     = NSB - 1
         INDX  = 0
         RFLAG = 0
         STAT  = 0.0
         TOTY  = 0.0
C
C        INITIALIZE THE MODEL SELECTION FLAG TO INDICATE MAXIMUM LIKELI-
C        HOOD ANALYSIS (LEAST SQUARES IS NOT ALLOWED).
         ESF   = 1
C
         IF (TYP.EQ.3) THEN
C            INITIALIZE THE CONSTANT PORTION OF THE NOISE CALCULATION.
             TMP = -LOG(0.5)
         ENDIF
C
C        DOWHILE (PROCESSING LEFT AND NO ERRORS)
 1000        I = I + 1
             IF ((I.GT.NSE) .OR. (RFLAG.NE.0)) GO TO 1025
C
C            COMPUTE MODEL ESTIMATES.
             CALL SJAMMD
     G                   (ESF   , I     ,  DAT
     Y                   ,STATS , RFLAG )
C
C            INCREMENT THE ITERATION COUNTER FOR STORAGE.
             INDX = INDX + 1
C
             IF (RFLAG.EQ.0) THEN
                 IF     (TYP.EQ.1) THEN
C                    ACCURACY ANALYSIS IS DESIRED;  APPEND THE CONTRIBU-
C                    TION OF THE  CURRENT  ITERATION TO  THE PREQUENTIAL
C                    LIKELIHOOD STATISTIC.
                     TMP     = STATS(4) - REAL(I)
                     V(INDX) = 0.0 - (LOG(STATS(1)) + LOG(TMP)
     1                       - STATS(1) * TMP * DAT(I+1))
                     STAT    = STAT + V(INDX)
C
                 ELSEIF ((TYP.EQ.2) .OR. (TYP.EQ.4)) THEN
C                    BIAS OR TREND ANALYSIS IS DESIRED;  STORE THE FUNC-
C                    TIONAL VALUE FOR THIS ELEMENT OF THE U-PLOT.
                     V(INDX) = 1.0
     1                       - EXP(-STATS(1) * (STATS(4) - REAL(I))
     2                       * DAT(I+1))
                     IF (TYP.EQ.4) THEN
C                        TREND ANALYSIS IS DESIRED; TRANSFORM THE U-PLOT
C                        VALUE TO THE Y-PLOT REPRESENTATION.
                         IF (V(INDX).LT.1.0) THEN
                             V(INDX) = -LOG(1.0 - V(INDX))
                             TOTY    = TOTY + V(INDX)
                         ELSE
                             RFLAG   = 5
                         ENDIF
                     ENDIF
C
                 ELSE
C                    NOISE ANALYSIS IS DESIRED; COMPUTE THE FUNCTION FOR
C                    THE NEXT POINT.
                     MNXT = TMP / (STATS(1) * (STATS(4) - REAL(I)))
C
C                    PERFORM THE ADDITION OF THE NOISE ON ALL ITERATIONS
C                    EXCEPT THE FIRST; AND THEN PREPARE FOR THE NEXT.
                     IF (I.GT.NSB) THEN
                         STAT = STAT + ABS((MNXT - MCUR) / MCUR)
                     ENDIF
                     MCUR = MNXT
C
                 ENDIF
             ENDIF
             GO TO 1000
 1025        CONTINUE
C        ENDWHILE
C
         IF ((TYP.EQ.2) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE U-PLOT VECTOR  AND COMPUTE
C            THE KOLMOGOROV STATISTIC. FIRST STORE THE UNSORTED DATA.
             DO 1050 I = 1, NSR
                 VPRE(I) = V(I)
 1050            CONTINUE
C            ENDDO
             CALL CMPMAX
     G                   (NSR   , 1
     B                   ,STAT  , V     )
         ENDIF
C
         IF ((TYP.EQ.4) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE Y-PLOT VECTOR.
             SUMY = 0.0
             DO 1075 I = 1, NSR
                 SUMY = SUMY + V(I)
                 V(I) = SUMY / TOTY
 1075            CONTINUE
C            ENDDO
C
C            COMPUTE THE KOLMOGOROV STATISTIC;  NOTE, SORTING IS NOT RE-
C            QUIRED FOR THE Y-PLOT DATA.
             CALL CMPMAX
     G                   (NSR   , 0
     B                   ,STAT  , V     )
         ENDIF
         RETURN
         END
         SUBROUTINE SJAMMD
     G                     (ESF   , NS    , DAT
     Y                     ,STATS , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SJAMMD
C    G                     (ESF   , NS    , DAT
C    Y                     ,STATS , RFLAG )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO CALCULATE  THE ESTIMATES  USING THE  MAXIMUM  LIKELIHOOD AND
C        LEAST SQUARES METHODS OF THE JELINSKI/MORANDA DE-EUTROPHICATION
C        MODEL.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE CALL LINE (ESF)  WILL BE USED BY THE
C        ROUTINE TO DETERMINE  WHICH OF THE TWO METHODS  OF EXECUTION IS
C        DESIRED.
C
C        ONCE THE ITERATIVE EXECUTION IS INITIATED, PROCESSING WILL CON-
C        TINUE UNTIL  ONE OF THREE POSSIBLE TERMINATION STATES IS REACH-
C        ED.  THE LAST ARGUMENT OF THE  CALL LINE (RFLAG) WILL BE SET TO
C        INDICATE TO THE CALLING  ROUTINE THE REASON FOR EXIT.  THE POS-
C        SIBLE REASONS FOR SJAMMD EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS THAN THE  OBSERVED
C                 FAULTS TO DATE
C
C        THE RESULTANT STATISTICS WILL BE RETURNED IN THE STATS ARGUMENT
C        FOR A RFLAG VALUE OF 0 ONLY.  FOR  BOTH THE  MAXIMUM LIKELIHOOD
C        AND LEAST SQUARES METHODS, NO 95% CONFIDENCE INTERVALS ARE COM-
C        PUTED.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS  SUBPROGRAM  CONTAINS  CODE WHICH DOES NOT COMPLY WITH THE
C        PROGRAMMING STANDARDS; GROUP LEADER APPROVAL HAS BEEN OBTAINED.
C       -SPECIFICALLY:
C        THE ARGUMENT  FC IN THE ACCESS TO THE ROUTINE ZERO, IS ACTUALLY
C        INITIALIZED IN THE ACCESSED ROUTINE. WHEN ICOUNT HAS A VALUE OF
C        MORE THAN ONE, THE ARGUMENT BECOMES A BOTH (AS MARKED).  HENCE,
C        FC APPEARS UNDEFINED IN THIS ROUTINE'S COMPILATION LISTING.
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             NS     (I) = SIZE OF DAT
C        YIELDED
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = RESULTS ARRAY
C   LOCAL GLOSSARY
C             A      (R) = LOWER BOUND OF THE TOTAL NUMBER OF FAULTS
C             B      (R) = UPPER BOUND OF THE TOTAL NUMBER OF FAULTS
C             COUNT  (I) = ITERATION COUNTER
C             FA     (R) = VALUE OF FUNCTION AT A
C             FB     (R) = VALUE OF FUNCTION AT B
C             FC     (R) = VALUE OF FUNCTION AT C
C             ICOUNT (I) = STEP COUNTER
C             IFLAG  (I) = CONVERGENCE FLAG
C             RN     (R) = REAL VARIABLE STORAGE OF NS
C             S1     (R) = SUMMATION VARIABLE
C             S2     (R) = SUMMATION VARIABLE
C             TMP    (R) = INTERMEDIATE CALCULATION
C   ERRORS
C        RFLAG  = 3 : THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
C        RFLAG  = 4 : THE  TOTAL  FAULTS  ESTIMATE  WAS  LESS  THAN  THE
C                     OBSERVED FAULTS TO DATE
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
         INTEGER              ESF           ,NS            ,RFLAG
         DIMENSION            DAT(NS)       ,STATS(6)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              COUNT         ,I             ,ICOUNT
     1                       ,IFLAG
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE COUNTER  AND RETURN STATUS FLAG.
         COUNT = 0
         RFLAG = -1
C
C        ZERO ALL LOCATIONS OF THE RESULTS VECTOR.
         DO 1000 I = 1, 6
             STATS(I) = 0.0
 1000        CONTINUE
C        ENDDO
C
C        COMPUTE THE VALUE OF THE FUNCTION AT ITS LOWER BOUND.
         A = REAL(NS) + 0.001
         B = A * 2.0
C
         IF (ESF.EQ.1) THEN
C            COMPUTE THE 2 SUMMATIONS USED LOCALLY AND IN ML FUNCTION.
             S1 = 0.0
             S2 = 0.0
             DO 1005 I = 1, NS
                 S1 = S1 + DAT(I)
                 S2 = S2 + DAT(I) * REAL(I-1)
 1005            CONTINUE
C            ENDDO
             FA = SJAMML
     G                   (A     , NS    , S1    , S2    )
         ELSE
             FA = SJAMLS
     G                   (A     , DAT   , NS    )
         ENDIF
C
C        COMPUTE THE VALUE OF THE FUNCTION AT A POINT WHERE ITS SIGN  IS
C        THE  OPPOSITE  OF  THE  COMPUTED  VALUE AT THE  FUNCTIONS LOWER
C        BOUND.
C
C        DOWHILE (RFLAG .EQ. -1)
 1025        COUNT = COUNT + 1
             IF (ESF.EQ.1) THEN
                 FB = SJAMML
     G                       (B     , NS    , S1    , S2    )
             ELSE
                 FB = SJAMLS
     G                       (B     , DAT   , NS    )
             ENDIF
C
C            SET RFLAG, IF CONVERGENCE POSSIBLE.
             IF ((FA*FB).LE.0.0) THEN
                 RFLAG = 0
             ENDIF
C
C            SET RFLAG, IF THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
             IF (COUNT.GE.5) THEN
                 RFLAG = 3
             ENDIF
             IF (RFLAG.EQ.-1) THEN
                 B = B * 2.0
             ENDIF
             IF (RFLAG.EQ.-1) GO TO 1025
C        ENDWHILE
C
         IF (RFLAG.EQ.0) THEN
C            COMPUTE  THE ESTIMATE  OF THE  TOTAL NUMBER OF FAULTS   AND
C            STORE THE VALUE IN STATS(4).
             IFLAG  = -1
             ICOUNT = 1
C
C            DOWHILE (IFLAG .EQ. -1)
C                COMPUTE THE ZERO OF THE FUNCTION IN THE INTERVAL  A AND
C                B; NOTE THE ARGUMENT FC IS INITIALIZED BY THE ZERO ROU-
C                TINE WHEN ICOUNT IS SET TO ONE.   <<SEE RESTRICTIONS <<
 1050            CALL ZERO
     B                     (A     , B     , FA    , FB    , FC
     B                     ,ICOUNT
     Y                     ,IFLAG )
C
                 IF (IFLAG.EQ.-1) THEN
C                    COMPUTE THE VALUE OF THE FUNCTION AT ITS NEW  UPPER
C                    BOUND.
                     IF (ESF.EQ.1) THEN
                         FB = SJAMML
     G                               (B     , NS    , S1   , S2)
                     ELSE
                         FB = SJAMLS
     G                               (B     , DAT   , NS    )
                     ENDIF
                 ENDIF
                 IF (IFLAG.EQ.-1) GO TO 1050
C            ENDWHILE
             STATS(4) = B
         ENDIF
C
C        THE STATISTICS  CALCULATIONS  WILL ONLY BE MADE FOR RFLAG INDI-
C        CATING SUCCESSFUL CONVERGENCE.
         IF (RFLAG.EQ.0) THEN
C            COMPUTE THE STATISTICS, STARTING BY STORING THE SIZE IN THE
C            REAL VARIABLE.
             RN = NS
C
             IF (STATS(4).LT.RN) THEN
C                PREDICTED TOTAL NUMBER OF FAULTS LESS THAN OBSERVED.
                 RFLAG = 4
             ELSE
C                COMPUTE THE PROPORTIONALITY CONSTANT OF THE MODEL.
                 IF (ESF.EQ.1) THEN
                     STATS(1) = RN  / (STATS(4) * S1 - S2)
                 ELSE
                     S1 = 0.0
                     S2 = 0.0
                     DO 1075 I = 1, NS
                         S1 = S1 + 1.0    / (STATS(4) - REAL(I)+1.0)**2
                         S2 = S2 + DAT(I) / (STATS(4) - REAL(I)+1.0)
 1075                    CONTINUE
C                    ENDDO
                     STATS(1) = S1 / S2
                 ENDIF
C
C                COMPUTE THE INITIAL HAZARD RATE. THE SAME EQUATION PRO-
C                VIDES THE INITIAL INTENSITY FUNCTION.
                 STATS(2) = STATS(1) *  STATS(4)
C
C                COMPUTE THE NUMBER OF FAULTS REMAINING.
                 STATS(5) = STATS(4) -  RN
C
C                COMPUTE THE CURRENT HAZARD RATE.
                 STATS(3) = STATS(1) *  STATS(5)
C
C                COMPUTE THE MTBNF.
                 STATS(6) = 1.0      / (STATS(1) * STATS(5))
             ENDIF
         ENDIF
         RETURN
         END
         FUNCTION SJAMML
     G                   (AB    , NS    , S1    , S2    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION SJAMML
C    G                   (AB    , NS    , S1    , S2    )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO CALCULATE THE VALUE OF THE FUNCTION FOR THE JELINSKI/MORANDA
C        DE-EUTROPHICATION MODEL (REF: NSWCDD TR 82-171) USING THE MAXI-
C        MUM LIKELIHOOD ESTIMATION METHOD.
C   DESCRIPTION
C        THIS ROUTINE CALCULATES THE  VALUE OF THE FUNCTION AT THE POINT
C        AB (ESTIMATE OF THE TOTAL NUMBER OF FAULTS).
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
C             AB     (R) = ESTIMATE OF THE TOTAL NUMBER OF FAULTS
C             NS     (I) = SIZE OF DAT
C             S1     (R) = SUM OF DAT(I)
C             S2     (R) = SUM OF DAT(I)*(I-1)
C        YIELDED
C             SJAMML (R) = FUNCTION VALUE AT POINT AB
C   LOCAL GLOSSARY
C             SM     (R) = TEMPORARY SUMMATION
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
         INTEGER              NS
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
C        INITIALIZE THE SUMMATION VARIABLE.
         SM = 0.0
C
C        PERFORM THE SUMMATION TYPE OVER ALL ENTRIES.
         DO 1000 I = 1, NS
             SM = SM + (1.0 / (AB - REAL(I - 1)))
 1000        CONTINUE
C        ENDDO
C
C        CALCULATE THE FUNCTION VALUE.
         SJAMML = SM - (REAL(NS) / (AB - (1.0 / S1) * S2))
         RETURN
         END
         SUBROUTINE SJAMPR
     G                     (NPV   , NS    , PCON  , TNOF  , EXE   , DAT
     Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SJAMPR
C    G                     (NPV   , NS    , PCON  , TNOF  , EXE   , DAT
C    Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO COMPUTE THE MTBNF TO THE NEXT EXE FAILURES AND TO CREATE THE
C        PREDICTED DATA VECTOR FOR THE  JELINSKI/MORANDA  DE-EUTROPHICA-
C        TION MODEL.  DURING THE PREDICTED VECTOR CREATION, THIS ROUTINE
C        WILL ALSO COMPUTE THE KOLMOGOROV DISTANCE.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE SJAMPR  CALL LINE  INDICATES WHETHER
C        THE PREDICTED MTBNF OR THE ENTIRE TBF DATA VECTOR IS TO BE COM-
C        PUTED.  NPV IS ACTUALLY THE  DIMENSION  STATEMENT FOR  THE PDAT
C        VECTOR;  AND IT CAN ONLY TAKE ON THE VALUES OF ONE AND NS.  THE
C        VALUE OF 1 INDICATES  THAT THE  PREDICTED MTBNF IS DESIRED, AND
C        THE ARGUMENT  EXE IS USED.  WHEN NPV IS SET TO NS,  THE ROUTINE
C        PROCESSES THE ENTIRE PREDICTED TBF DATA VECTOR.
C
C        THE CALCULATION FOR THE  KOLMOGOROV STATISTIC  WAS ADDED DURING
C        THE UPDATE THE REVISION 5. FOR THAT CALCULATION, IN ADDITION TO
C        THE MODEL ESTIMATES, THE ACTUAL DATA VECTOR IS NEEDED. ONCE THE
C        NS ELEMENTS ARE COMPUTED, USING THE SAME EQUATION AS FOR A BIAS
C        ANALYSIS OF THIS MODEL, THE MAXIMUM DIFFERENCE IS DETERMINE VIA
C        THE CMPMAX ROUTINE, AND THE SIGNIFICANCE OF THE VALUE IS DETER-
C        MINED VIA THE CHKGOF ROUTINE (BOTH OF WHICH ARE RESIDENT IN THE
C        SMERFS5 LIBRARY OF SUPPORT ROUTINES).
C
C        NOTE, IN AN EFFORT TO RETAIN THE  MODULARITY OF THE PROCESSING,
C        NO ATTEMPT WAS MADE TO REUSE INTERMEDIATE CALCULATIONS FROM THE
C        ORIGINAL PREDICTION ROUTINE (I.E., DUPLICATE PROCESSING FOR IN-
C        TERMEDIATE VALUES MAY APPEAR IN THIS ROUTINE).
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
C             EXE    (I) = EXTRA FAILURES TO BE DETECTED
C             NPV    (I) = NUMBER OF PREDICTED VALUES DESIRED
C             NS     (I) = NUMBER OF TBF ENTRIES TO BE CREATED
C             PCON   (R) = ESTIMATE OF THE PROPORTIONALITY CONSTANT
C             TNOF   (R) = ESTIMATE OF THE TOTAL NUMBER OF FAULTS
C        YIELDED
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL, WHERE:
C                          1 - THE VALUE OF D IS LARGER  THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY NOT PROVIDE AN
C                              ADEQUATE FIT.
C                          2 - THE VALUE OF D IS SMALLER THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY PROVIDE AN AD-
C                              EQUATE FIT.
C             DVAL   (R) = CALCULATED KOLMOGOROV DISTANCE
C             PDAT   (R) = VECTOR OF PREDICTED DATA VALUES
C             V      (R) = VECTOR TO CONTAIN THE U-PLOT DATA SET
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
         INTEGER              DFLG          ,EXE           ,NPV
     1                       ,NS
         DIMENSION            DAT(NS)       ,PDAT(NS)      ,V(NS)
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
         IF (NPV.EQ.1) THEN
C            THE MTBNF PREDICTION IS DESIRED.
             PDAT(1) = 0.0
             J       = NS + EXE - 1
             DO 1000 I = NS, J
                 PDAT(1) = PDAT(1) + 1.0 / (PCON * (TNOF - REAL(I)))
 1000            CONTINUE
C            ENDDO
C
         ELSE
C            INITIALIZE THE KOLMOGOROV DISTANCE TO ZERO.
             DVAL  = 0.0
C
C            THE ENTIRE VECTOR IS TO BE GENERATED.
             DO 1025 I = 1, NS
                 PDAT(I) = 1.0 / (PCON * (TNOF - REAL(I) + 1.0))
C
C                COMPUTE THE CONTRIBUTION FOR THIS ELEMENT.
                 V(I)  = 1.0 - EXP(-PCON * (TNOF - REAL(I-1)) * DAT(I))
 1025            CONTINUE
C            ENDDO
C
C            COMPLETE THE COMPUTATION  FOR THE U-PLOT VECTOR;  NOTE, THE
C            VECTOR OF U-PLOT DATA IS SORTED UPON RETURN.
             CALL CMPMAX
     G                   (NS    , 1
     B                   ,DVAL  , V     )
C
C            DETERMINE IF THE CALCULATED DISTANCE IS SIGNIFICANT.
             CALL CHKGOF
     G                   (NS    , DVAL
     Y                   ,DFLG  )
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
         SUBROUTINE SLAVMA
     G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
     G                     ,MAXIC , PHIIND
     Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SLAVMA
C    G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
C    G                     ,MAXIC , PHIIND
C    Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM FOUR TYPES OF  MODEL APPLICABILITY ANALYSIS FOR  THE
C        LITTLEWOOD AND VERRALL BAYESIAN RELIABILITY GROWTH MODEL.
C
C        THESE INCLUDE:
C             1) THE MODEL ACCURACY;
C             2) THE MODEL BIAS;
C             3) THE MODEL NOISE; AND
C             4) THE MODEL TREND
C   DESCRIPTION
C        THE ROUTINE  CONTROLS THE EXECUTIONS  OF THE  APPLICABLE SMFLIB
C        ROUTINE, SLAVMD.  THE PROCESSING, AS DEFINED  IN THE LITTLEWOOD
C        PAPER (SEE REFERENCES IN THE MAIN PROGRAM) BASICALLY INVOLVES A
C        SERIES OF EXECUTIONS WHERE  NSB TO NSE DATA POINTS ARE UTILIZED
C        IN THE MODEL.
C
C        THIS ROUTINE ACTUALLY RUNS THE SLAVMD ROUTINE TWICE FOR EACH OF
C        THE ITERATIONS.  THE LEAST SQUARES EXECUTION IS FIRST PERFORMED
C        TO OBTAIN THE  INITIAL ESTIMATES FOR BETA(0) AND BETA(1)  TO BE
C        USED IN THE MAXIMUM LIKELIHOOD EXECUTION.
C
C        FOR EACH ITERATION,  THE MODEL PARAMETERS (ESTIMATES)  FROM THE
C        CURRENT ITERATION ARE USED IN CONJUNCTION WITH THE OBSERVED DA-
C        TA FROM THE NEXT ITERATION POINT  TO DETERMINE AN OVER-ALL PIC-
C        TURE OF THE APPLICABILITY OF THE MODEL  FOR THE PARTICULAR DATA
C        SET.
C
C        THIS ONE ROUTINE IS USED TO OBTAIN ALL FOUR ANALYSES; THE SIXTH
C        ARGUMENT, TYP, IS USED TO INDICATE HOW THE MODEL PARAMETERS ARE
C        TO BE USED.
C
C        NOTE, THE VECTOR, V, IS NOT USED IN THE NOISE ANALYSIS  AND THE
C        VECTOR, VPRE, IS ONLY USED IN THE BIAS ANALYSIS;  HOWEVER, THEY
C        SHOULD BE AVAILABLE TO THE ROUTINE FOR ALL ACCESSES.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED TBF DATA
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C             PHIIND (I) = PHI INDICATOR FLAG (LINEAR OR QUADRATIC)
C             TYP    (I) = ANALYSIS TYPE TO BE PERFORMED (SEE PURPOSE)
C        YIELDED
C             INDX   (I) = ANALYSIS ITERATION COUNTER (AT RETURN)
C             RFLAG  (I) = MODEL RESULTS ERROR FLAG
C             STAT   (R) = ANALYSIS RESULTS STATISTIC
C             V      (R) = ANALYSIS RESULTS ARRAY OF SIZE NSR
C             VPRE   (R) = VECTOR OF BIAS (U-PLOT) DATA PRIOR TO SORTING
C   LOCAL GLOSSARY
C             ALPHA  (R) = ALPHA ESTIMATE FROM SLAVMD
C             BETA   (R) = INITIAL BETA ESTIMATES FOR SLAVMD
C             COUNT  (I) = ITERATION COUNTER
C             ESF    (I) = MODEL ESTIMATION FLAG (ML ONLY)
C             ESFTMP (I) = ESTIMATION SELECTION FLAG (LS) FOR INITIALS
C             MCUR   (R) = VALUE OF THE FUNCTION FOR THE CURRENT POINT
C             MNXT   (R) = VALUE OF THE FUNCTION FOR THE NEXT    POINT
C             SUMY   (R) = SUM OF THE PREVIOUS Y'S
C             TMP    (R) = INTERMEDIATE CALCULATION
C             TOTY   (R) = SUM OF ALL THE Y'S
C             X      (R) = BETA ESTIMATES FROM SLAVMD
C             Z      (R) = MINIMIZED FUNCTION VALUE
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
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              INDX          ,MAXIC         ,NS
     1                       ,NSB           ,NSE           ,NSR
     2                       ,PHIIND        ,RFLAG         ,TYP
         DIMENSION            DAT(NS)       ,V(NSR)        ,VPRE(NSR)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              COUNT         ,ESF           ,ESFTMP
     1                       ,I
         DIMENSION            BETA(2)       ,X(2)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE  THE ITERATION COUNTER,  STORAGE INDEX,  MODEL ERROR
C        FLAG, RESULTS STATISTIC, AND SUM OF THE Y-PLOT VALUES LOCATION.
         I      = NSB - 1
         INDX   = 0
         RFLAG  = 0
         STAT   = 0.0
         TOTY   = 0.0
C
C        INITIALIZE THE MODEL SELECTION FLAG TO INDICATE MAXIMUM LIKELI-
C        HOOD ANALYSIS (LEAST SQUARES IS NOT ALLOWED).
         ESF    = 1
C
C        INITIALIZE THE  SPECIAL  MODEL SELECTION FLAG FOR LEAST SQUARES
C        STARTING POINTS.
         ESFTMP = 3
C
C        DOWHILE (PROCESSING LEFT AND NO ERRORS)
 1000        I = I + 1
             IF ((I.GT.NSE) .OR. (RFLAG.NE.0)) GO TO 1025
C
C            SET THE INITIAL ESTIMATES FOR BETA(0) AND BETA(1) FOR  MAX-
C            IMUM LIKELIHOOD ESTIMATION FROM SPECIAL LEAST SQUARES.
             CALL SLAVMD
     G                   (DAT   , ESFTMP, MAXIC , I     , 3
     G                   ,PHIIND, BETA
     Y                   ,ALPHA , COUNT , RFLAG , X     , Z     )
             BETA(1) = X(1)
             BETA(2) = X(2)
             IF (BETA(1).LT.BETA(2)) THEN
                 BETA(1) = 2.0 * REAL(PHIIND) * BETA(2)
             ENDIF
C
C            COMPUTE MODEL ESTIMATES.
             CALL SLAVMD
     G                   (DAT   , ESF   , MAXIC , I     , 3
     G                   ,PHIIND, BETA
     Y                   ,ALPHA , COUNT , RFLAG , X     , Z     )
C
C            INCREMENT THE ITERATION COUNTER FOR STORAGE.
             INDX = INDX + 1
C
             IF (RFLAG.EQ.0) THEN
C                COMPUTE THE INTERMEDIATE VALUE.
                 TMP = X(1) + X(2) * REAL(I+1)**PHIIND
C
                 IF     (TYP.EQ.1) THEN
C                    ACCURACY ANALYSIS IS DESIRED;  APPEND THE CONTRIBU-
C                    TION OF THE  CURRENT  ITERATION TO  THE PREQUENTIAL
C                    LIKELIHOOD STATISTIC.
                     V(INDX) = 0.0 - (LOG(ALPHA) + ALPHA * LOG(TMP)
     1                       - (ALPHA + 1.0) * LOG(DAT(I+1) + TMP))
                     STAT    = STAT + V(INDX)
C
                 ELSEIF ((TYP.EQ.2) .OR. (TYP.EQ.4)) THEN
C                    BIAS OR TREND ANALYSIS IS DESIRED;  STORE THE FUNC-
C                    TIONAL VALUE FOR THIS ELEMENT OF THE U-PLOT.
                     V(INDX) = 1.0
     1                       - (TMP / (DAT(I+1) + TMP))**ALPHA
                     IF (TYP.EQ.4) THEN
C                        TREND ANALYSIS IS DESIRED; TRANSFORM THE U-PLOT
C                        VALUE TO THE Y-PLOT REPRESENTATION.
                         IF (V(INDX).LT.1.0) THEN
                             V(INDX) = -LOG(1.0 - V(INDX))
                             TOTY    = TOTY + V(INDX)
                         ELSE
                             RFLAG   = 5
                         ENDIF
                     ENDIF
C
                 ELSE
C                    NOISE ANALYSIS IS DESIRED; COMPUTE THE FUNCTION FOR
C                    THE NEXT POINT.
                     MNXT = (TMP / (0.5**(1.0 / ALPHA))) - TMP
C
C                    PERFORM THE ADDITION OF THE NOISE ON ALL ITERATIONS
C                    EXCEPT THE FIRST; AND THEN PREPARE FOR THE NEXT.
                     IF (I.GT.NSB) THEN
                         STAT = STAT + ABS((MNXT - MCUR) / MCUR)
                     ENDIF
                     MCUR = MNXT
C
                 ENDIF
             ENDIF
             GO TO 1000
 1025        CONTINUE
C        ENDWHILE
C
         IF ((TYP.EQ.2) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE U-PLOT VECTOR  AND COMPUTE
C            THE KOLMOGOROV STATISTIC. FIRST STORE THE UNSORTED DATA.
             DO 1050 I = 1, NSR
                 VPRE(I) = V(I)
 1050            CONTINUE
C            ENDDO
             CALL CMPMAX
     G                   (NSR   , 1
     B                   ,STAT  , V     )
         ENDIF
C
         IF ((TYP.EQ.4) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE Y-PLOT VECTOR.
             SUMY = 0.0
             DO 1075 I = 1, NSR
                 SUMY = SUMY + V(I)
                 V(I) = SUMY / TOTY
 1075            CONTINUE
C            ENDDO
C
C            COMPUTE THE KOLMOGOROV STATISTIC;  NOTE, SORTING IS NOT RE-
C            QUIRED FOR THE Y-PLOT DATA.
             CALL CMPMAX
     G                   (NSR   , 0
     B                   ,STAT  , V     )
         ENDIF
         RETURN
         END
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
         SUBROUTINE SLAVPR
     G                     (NPV   , NS    , ALPHA , BETA0 , BETA1 , PHI
     G                     ,DAT
     Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SLAVPR
C    G                     (NPV   , NS    , ALPHA , BETA0 , BETA1 , PHI
C    G                     ,DAT
C    Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO COMPUTE THE PREDICTED MTBNF AND TO CREATE THE PREDICTED DATA
C        VECTOR FOR  LITTLEWOOD AND VERRALL BAYESIAN  RELIABILITY GROWTH
C        MODEL.  DURING THE PREDICTED VECTOR CREATION, THIS ROUTINE WILL
C        ALSO COMPUTE THE KOLMOGOROV DISTANCE.
C   DESCRIPTION
C        THE FIRST ARGUMENT OF THE  SLAVPR  CALL LINE  INDICATES WHETHER
C        THE PREDICTED MTBNF OR THE ENTIRE TBF VECTOR IS TO BE COMPUTED.
C        IF NPV IS SET TO 1, THE ROUTINE WILL COMPUTE THE ONE  PREDICTED
C        VALUE USING THE ARGUMENTS NS, ALPHA, BETA0, BETA1, AND PHI.  IF
C        THE VALUE IS NOT 1,  THE ROUTINE WILL  COMPUTE THE  ENTIRE DATA
C        VECTOR OF PREDICTED TBFS, USING THE SAME ARGUMENTS.
C
C        THE CALCULATION FOR THE  KOLMOGOROV STATISTIC  WAS ADDED DURING
C        THE UPDATE THE REVISION 5. FOR THAT CALCULATION, IN ADDITION TO
C        THE MODEL ESTIMATES, THE ACTUAL DATA VECTOR IS NEEDED. ONCE THE
C        NS ELEMENTS ARE COMPUTED, USING THE SAME EQUATION AS FOR A BIAS
C        ANALYSIS OF THIS MODEL, THE MAXIMUM DIFFERENCE IS DETERMINE VIA
C        THE CMPMAX ROUTINE, AND THE SIGNIFICANCE OF THE VALUE IS DETER-
C        MINED VIA THE CHKGOF ROUTINE (BOTH OF WHICH ARE RESIDENT IN THE
C        SMERFS5 LIBRARY OF SUPPORT ROUTINES).
C
C        NOTE, IN AN EFFORT TO RETAIN THE  MODULARITY OF THE PROCESSING,
C        NO ATTEMPT WAS MADE TO REUSE INTERMEDIATE CALCULATIONS FROM THE
C        ORIGINAL PREDICTION ROUTINE (I.E., DUPLICATE PROCESSING FOR IN-
C        TERMEDIATE VALUES MAY APPEAR IN THIS ROUTINE).
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
C             ALPHA  (R) = ESTIMATE OF ALPHA FROM SLAVMD
C             BETA0  (R) = ESTIMATE OF BETA(0) FROM SLAVMD
C             BETA1  (R) = ESTIMATE OF BETA(1) FROM SLAVMD
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             NPV    (I) = NUMBER OF PREDICTED VALUES DESIRED
C             NS     (I) = NUMBER OF FAULT DETECTIONS TO DATE
C             PHI    (I) = PHI INDICATOR (LINEAR OR QUADRATIC)
C        YIELDED
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL, WHERE:
C                          1 - THE VALUE OF D IS LARGER  THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY NOT PROVIDE AN
C                              ADEQUATE FIT.
C                          2 - THE VALUE OF D IS SMALLER THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY PROVIDE AN AD-
C                              EQUATE FIT.
C             DVAL   (R) = CALCULATED KOLMOGOROV DISTANCE
C             PDAT   (R) = VECTOR OF PREDICTED DATA VALUE(S)
C             V      (R) = VECTOR TO CONTAIN THE U-PLOT DATA SET
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
         INTEGER              DFLG          ,NPV           ,NS
     1                       ,PHI
         DIMENSION            DAT(NS)       ,PDAT(NPV)     ,V(NS)
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
         IF (NPV.EQ.1) THEN
C            COMPUTE THE ESTIMATE OF THE MTBNF.
             PDAT(1) = (BETA0 + BETA1 * REAL(NS + 1)**PHI)
     1               / (ALPHA - 1.0)
         ELSE
C            INITIALIZE THE KOLMOGOROV DISTANCE TO ZERO.
             DVAL  = 0.0
C
C            COMPUTE THE ESTIMATES FOR THE ENTIRE TBF DATA VECTOR.
             DO 1000 I = 1, NPV
                 PDAT(I) = (BETA0 + BETA1 * REAL(I)**PHI)
     1                   / (ALPHA - 1.0)
C
C                COMPUTE THE CONTRIBUTION FOR THIS ELEMENT.
                 TMP = BETA0 + BETA1 * REAL(I)**PHI
                 V(I) = 1.0 - (TMP / (DAT(I) + TMP))**ALPHA
 1000            CONTINUE
C            ENDDO
C
C            COMPLETE THE COMPUTATION  FOR THE U-PLOT VECTOR;  NOTE, THE
C            VECTOR OF U-PLOT DATA IS SORTED UPON RETURN.
             CALL CMPMAX
     G                   (NS    , 1
     B                   ,DVAL  , V     )
C
C            DETERMINE IF THE CALCULATED DISTANCE IS SIGNIFICANT.
             CALL CHKGOF
     G                   (NS    , DVAL
     Y                   ,DFLG  )
         ENDIF
         RETURN
         END
         SUBROUTINE SMSAMA
     G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
     Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMSAMA
C    G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
C    Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM FOUR TYPES OF  MODEL APPLICABILITY ANALYSIS FOR  THE
C        MUSA LOGARITHMIC POISSON EXECUTION TIME MODEL.
C
C        THESE INCLUDE:
C             1) THE MODEL ACCURACY;
C             2) THE MODEL BIAS;
C             3) THE MODEL NOISE; AND
C             4) THE MODEL TREND
C   DESCRIPTION
C        THE ROUTINE  CONTROLS THE EXECUTIONS  OF THE  APPLICABLE SMFLIB
C        ROUTINE, SMSAMD.  THE PROCESSING, AS DEFINED  IN THE LITTLEWOOD
C        PAPER (SEE REFERENCES IN THE MAIN PROGRAM) BASICALLY INVOLVES A
C        SERIES OF EXECUTIONS WHERE  NSB TO NSE DATA POINTS ARE UTILIZED
C        IN THE MODEL.
C
C        THE SMSAMD ARGUMENT, SUMTBF, IS SET TO REFLECT NO  FAILURE-FREE
C        TESTING TIME FOR ALL EXECUTIONS.
C
C        FOR EACH ITERATION,  THE MODEL PARAMETERS (ESTIMATES)  FROM THE
C        CURRENT ITERATION ARE USED IN CONJUNCTION WITH THE OBSERVED DA-
C        TA FROM THE NEXT ITERATION POINT  TO DETERMINE AN OVER-ALL PIC-
C        TURE OF THE APPLICABILITY OF THE MODEL  FOR THE PARTICULAR DATA
C        SET.
C
C        THIS ONE ROUTINE IS USED TO OBTAIN ALL FOUR ANALYSES; THE SIXTH
C        ARGUMENT, TYP, IS USED TO INDICATE HOW THE MODEL PARAMETERS ARE
C        TO BE USED.
C
C        NOTE, THE VECTOR, V, IS NOT USED IN THE NOISE ANALYSIS  AND THE
C        VECTOR, VPRE, IS ONLY USED IN THE BIAS ANALYSIS;  HOWEVER, THEY
C        SHOULD BE AVAILABLE TO THE ROUTINE FOR ALL ACCESSES.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED TBF DATA
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C             TYP    (I) = ANALYSIS TYPE TO BE PERFORMED (SEE PURPOSE)
C        YIELDED
C             INDX   (I) = ANALYSIS ITERATION COUNTER (AT RETURN)
C             RFLAG  (I) = MODEL RESULTS ERROR FLAG
C             STAT   (R) = ANALYSIS RESULTS STATISTIC
C             V      (R) = ANALYSIS RESULTS ARRAY OF SIZE NSR
C             VPRE   (R) = VECTOR OF BIAS (U-PLOT) DATA PRIOR TO SORTING
C   LOCAL GLOSSARY
C             BETA0  (R) = BETA(0) CONSTANT OF THE MODEL
C             BETA1  (R) = BETA(1) CONSTANT OF THE MODEL
C             CIF    (R) = CURRENT INTENSITY FUNCTION
C             IIF    (R) = INITIAL INTENSITY FUNCTION
C             MCUR   (R) = VALUE OF THE FUNCTION FOR THE CURRENT POINT
C             MNXT   (R) = VALUE OF THE FUNCTION FOR THE NEXT    POINT
C             SUMTBF (R) = SUM OF THE TBF DATA
C             SUMY   (R) = SUM OF THE PREVIOUS Y'S
C             TOTY   (R) = SUM OF ALL THE Y'S
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
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              INDX          ,NS            ,NSB
     1                       ,NSE           ,NSR           ,RFLAG
     2                       ,TYP
         DIMENSION            DAT(NS)       ,V(NSR)        ,VPRE(NSR)
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
C        INITIALIZE  THE ITERATION COUNTER,  STORAGE INDEX,  MODEL ERROR
C        FLAG, RESULTS STATISTIC, AND SUM OF THE Y-PLOT VALUES LOCATION.
         I     = NSB - 1
         INDX  = 0
         RFLAG = 0
         STAT  = 0.0
         TOTY  = 0.0
C
C        CALCULATE THE SUM OF THE TBF DATA THROUGH THE PRIOR STEP.
         SUMTBF = 0.0
         DO 1000 J = 1, I
             SUMTBF = SUMTBF + DAT(J)
 1000        CONTINUE
C        ENDDO
C
C        DOWHILE (PROCESSING LEFT AND NO ERRORS)
 1025        I = I + 1
             IF ((I.GT.NSE) .OR. (RFLAG.NE.0)) GO TO 1050
C
C            ADD THE CONTRIBUTION OF THIS STEP TO THE SUM OF TBF DATA.
             SUMTBF = SUMTBF + DAT(I)
C
C            COMPUTE MODEL ESTIMATES.
             CALL SMSAMD
     G                   (I     , DAT   , SUMTBF
     Y                   ,BETA0 , BETA1 , CIF   , IIF
     Y                   ,RFLAG )
C
C            INCREMENT THE ITERATION COUNTER FOR STORAGE.
             INDX = INDX + 1
C
             IF (RFLAG.EQ.0) THEN
                 IF     (TYP.EQ.1) THEN
C                    ACCURACY ANALYSIS IS DESIRED;  APPEND THE CONTRIBU-
C                    TION OF THE  CURRENT  ITERATION TO  THE PREQUENTIAL
C                    LIKELIHOOD STATISTIC.
                     V(INDX) = 0.0 - (LOG(BETA0) + LOG(BETA1)
     1                       - LOG(1.0 + BETA1 * (SUMTBF + DAT(I+1)))
     2                       + BETA0 * (LOG(1.0 + BETA1  * SUMTBF)
     3                       - LOG(1.0 + BETA1 * (SUMTBF + DAT(I+1)))))
                     STAT    = STAT + V(INDX)
C
                 ELSEIF ((TYP.EQ.2) .OR. (TYP.EQ.4)) THEN
C                    BIAS OR TREND ANALYSIS IS DESIRED;  STORE THE FUNC-
C                    TIONAL VALUE FOR THIS ELEMENT OF THE U-PLOT.
                     V(INDX) = 1.0 - ((BETA1 * SUMTBF + 1.0) / (BETA1
     1                       * (SUMTBF + DAT(I+1)) + 1.0))**BETA0
C
                     IF (TYP.EQ.4) THEN
C                        TREND ANALYSIS IS DESIRED; TRANSFORM THE U-PLOT
C                        VALUE TO THE Y-PLOT REPRESENTATION.
                         IF (V(INDX).LT.1.0) THEN
                             V(INDX) = -LOG(1.0 - V(INDX))
                             TOTY    = TOTY + V(INDX)
                         ELSE
                             RFLAG   = 5
                         ENDIF
                     ENDIF
C
                 ELSE
C                    NOISE ANALYSIS IS DESIRED; COMPUTE THE FUNCTION FOR
C                    THE NEXT POINT.
                     MNXT = (BETA1 * SUMTBF + 1.0)
     1                    / (BETA1 * 0.5**(1.0 / BETA0))
     2                    - (1.0 / BETA1) - SUMTBF
C
C                    PERFORM THE ADDITION OF THE NOISE ON ALL ITERATIONS
C                    EXCEPT THE FIRST; AND THEN PREPARE FOR THE NEXT.
                     IF (I.GT.NSB) THEN
                         STAT = STAT + ABS((MNXT - MCUR) / MCUR)
                     ENDIF
                     MCUR = MNXT
C
                 ENDIF
             ENDIF
             GO TO 1025
 1050        CONTINUE
C        ENDWHILE
C
         IF ((TYP.EQ.2) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE U-PLOT VECTOR  AND COMPUTE
C            THE KOLMOGOROV STATISTIC. FIRST STORE THE UNSORTED DATA.
             DO 1075 I = 1, NSR
                 VPRE(I) = V(I)
 1075            CONTINUE
C            ENDDO
             CALL CMPMAX
     G                   (NSR   , 1
     B                   ,STAT  , V     )
         ENDIF
C
         IF ((TYP.EQ.4) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE Y-PLOT VECTOR.
             SUMY = 0.0
             DO 1100 I = 1, NSR
                 SUMY = SUMY + V(I)
                 V(I) = SUMY / TOTY
 1100            CONTINUE
C            ENDDO
C
C            COMPUTE THE KOLMOGOROV STATISTIC;  NOTE, SORTING IS NOT RE-
C            QUIRED FOR THE Y-PLOT DATA.
             CALL CMPMAX
     G                   (NSR   , 0
     B                   ,STAT  , V     )
         ENDIF
         RETURN
         END
         SUBROUTINE SMSAMD
     G                     (NS    , DAT   , SUMTBF
     Y                     ,BETA0 , BETA1 , CIF   , IIF
     Y                     ,RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMSAMD
C    G                     (NS    , DAT   , SUMTBF
C    Y                     ,BETA0 , BETA1 , CIF   , IIF
C    Y                     ,RFLAG )
C
C   AUTHOR
C        PAUL J. KRAUS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) AND OLIVER D. SMITH (EG&G, WASC)
C   PURPOSE
C        TO  CALCULATE THE  ESTIMATES  IN THE  MUSA LOGARITHMIC  POISSON
C        EXECUTION TIME  MODEL USING THE DEKKER-BRENT MINIMIZATION ALGO-
C        RITHM.
C   DESCRIPTION
C        ONCE THE ITERATIVE EXECUTION IS INITIATED, PROCESSING WILL CON-
C        TINUE UNTIL ONE OF TWO  POSSIBLE  TERMINATION STATES IS REACHED
C        IN THE EXECUTION.  THE LAST ARGUMENT  OF THE CALL LINE  (RFLAG)
C        WILL BE SET TO INDICATE  TO THE CALLING ROUTINE  THE REASON FOR
C        EXIT. THE POSSIBLE REASONS FOR SMSAMD EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C
C        THE RESULTANT STATISTICS  WILL BE RETURNED  IN THE 4-TH THROUGH
C        THE 7-TH ARGUMENTS OF THE CALL LINE.  THE ARGUMENTS IIF AND CIF
C        WILL CONTAIN THE INITIAL AND CURRENT INTENSITY FUNCTIONS.   THE
C        ARGUMENTS  BETA0 AND BETA1 WILL CONTAIN THE BETA(0) AND BETA(1)
C        PARAMETERS OF THE MODEL.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS  SUBPROGRAM  CONTAINS  CODE WHICH DOES NOT COMPLY WITH THE
C        PROGRAMMING STANDARDS; GROUP LEADER APPROVAL HAS BEEN OBTAINED.
C       -SPECIFICALLY:
C        THE ARGUMENT  FC IN THE ACCESS TO THE ROUTINE ZERO, IS ACTUALLY
C        INITIALIZED IN THE ACCESSED ROUTINE. WHEN ICOUNT HAS A VALUE OF
C        MORE THAN ONE, THE ARGUMENT BECOMES A BOTH (AS MARKED).  HENCE,
C        FC APPEARS UNDEFINED IN THIS ROUTINE'S COMPILATION LISTING.
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             NS     (I) = SIZE OF DAT
C             SUMTBF (R) = SUM OF THE TBF DATA, INCLUDING  ANY  FAILURE-
C                          FREE TEST TIME
C        YIELDED
C             BETA0  (R) = BETA(0) CONSTANT OF THE MODEL
C             BETA1  (R) = BETA(1) CONSTANT OF THE MODEL
C             CIF    (R) = CURRENT INTENSITY FUNCTION
C                          (FAILURES / CPU TIME)
C             IIF    (R) = INITIAL INTENSITY FUNCTION
C                          (FAILURES / CPU TIME)
C             RFLAG  (I) = RETURN STATUS FLAG
C   LOCAL GLOSSARY
C             A      (R) = LOWER BOUND OF BETA(1) CONSTANT
C             B      (R) = UPPER BOUND OF BETA(1) CONSTANT
C             COUNT  (I) = ITERATION COUNTER
C             FA     (R) = VALUE OF FUNCTION AT A
C             FB     (R) = VALUE OF FUNCTION AT B
C             FC     (R) = VALUE OF FUNCTION AT C
C             ICOUNT (I) = STEP COUNTER
C             IFLAG  (I) = CONVERGENCE FLAG
C             RN     (R) = REAL VARIABLE STORAGE OF NS
C             TMPTBF (R) = SUM OF THE TBF (W/O THE FAILURE-FREE PART)
C   ERRORS
C        RFLAG  = 3 : THE DATA ARE NOT APPROPRIATE FOR THE MODEL
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
         INTEGER              NS            ,RFLAG
         DIMENSION            DAT(NS)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              COUNT         ,I             ,ICOUNT
     1                       ,IFLAG
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE COUNTER AND RETURN STATUS FLAG.
         COUNT    = 0
         RFLAG    = -1
C
C        ZERO ALL RETURN STATISTICS ARGUMENTS.
         CIF   = 0.0
         IIF   = 0.0
         BETA0 = 0.0
         BETA1 = 0.0
C
C        COMPUTE THE VALUE OF THE FUNCTION AT ITS LOWER BOUND.
         A =  0.1 / SUMTBF
         B = 10.0 / SUMTBF
         FA = SMSAML
     G               (A     , DAT   , NS    , SUMTBF)
C
C        COMPUTE THE VALUE OF THE FUNCTION AT A POINT WHERE ITS SIGN  IS
C        THE  OPPOSITE  OF  THE  COMPUTED  VALUE AT THE  FUNCTIONS LOWER
C        BOUND.
C
C        DOWHILE (RFLAG .EQ. -1)
 1000        COUNT = COUNT + 1
             FB = SMSAML
     G                   (B     , DAT   , NS    , SUMTBF)
C
C            SET RFLAG, IF CONVERGENCE POSSIBLE.
             IF ((FA*FB).LE.0.0) THEN
                 RFLAG = 0
             ENDIF
C
C            SET RFLAG, IF THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
             IF (COUNT.GE.5) THEN
                 RFLAG = 3
             ENDIF
             IF (RFLAG.EQ.-1) THEN
                 B = B * 10.0
             ENDIF
             IF (RFLAG.EQ.-1) GO TO 1000
C        ENDWHILE
C
         IF (RFLAG.EQ.0) THEN
C            COMPUTE THE ESTIMATE OF THE BETA(1) CONSTANT.
             IFLAG  = -1
             ICOUNT = 1
C
C            DOWHILE (IFLAG .EQ. -1)
C                COMPUTE THE ZERO OF THE FUNCTION IN THE INTERVAL A  AND
C                B; NOTE THE ARGUMENT FC IS INITIALIZED BY THE ZERO ROU-
C                TINE WHEN ICOUNT IS SET TO ONE.    <<SEE RESTRICTIONS<<
 1025            CALL ZERO
     B                     (A     , B     , FA    , FB    , FC
     B                     ,ICOUNT, IFLAG )
C
                 IF (IFLAG.EQ.-1) THEN
C                    COMPUTE THE VALUE OF THE FUNCTION AT ITS NEW  UPPER
C                    BOUND.
                     FB = SMSAML
     G                           (B     , DAT   , NS    , SUMTBF)
                 ENDIF
                 IF (IFLAG.EQ.-1) GO TO 1025
C            ENDWHILE
C
C            STORE THE VALUE OF THE  BETA(1)  CONSTANT IN BETA1 AND COM-
C            PUTE THE VALUE OF THE BETA(0) CONSTANT.
             RN    = NS
             BETA1 = B
             BETA0 = RN / LOG(1.0 + BETA1 * SUMTBF)
             IF (BETA0.LE.0.0) THEN
                 RFLAG = 3
             ENDIF
         ENDIF
C
         IF (RFLAG.EQ.0) THEN
C            CONVERGENCE WAS THE REASON FOR THE ABOVE LOOP EXIT; COMPUTE
C            TIME TO THE LAST FAILURE. THE VALUE MUST BE GOTTEN FROM THE
C            DATA VECTOR, SINCE THE MODEL GETS  SUMTBF WITH FAILURE-FREE
C            IN THE VALUE (I.E., THERE IS NO WAY TO DETERMINE "XLTM").
             TMPTBF = 0.0
             DO 1050 I = 1, NS
                 TMPTBF = TMPTBF + DAT(I)
 1050            CONTINUE
C            ENDDO
C
C            THE INITIAL AND CURRENT INTENSITY FUNCTIONS.
             IIF = BETA0 *  BETA1
             CIF = IIF   / (BETA1 * TMPTBF + 1.0)
         ENDIF
         RETURN
         END
         FUNCTION SMSAML
     G                   (AB    , DAT   , NS    , SUMTBF)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION SMSAML
C    G                   (AB    , DAT   , NS    , SUMTBF)
C
C   AUTHOR
C        PAUL J. KRAUS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) AND OLIVER D. SMITH (EG&G, WASC)
C   PURPOSE
C        TO CALCULATE THE VALUE OF THE FUNCTION FOR THE MUSA LOGARITHMIC
C        POISSON  EXECUTION  TIME  MODEL  USING  THE  MAXIMUM LIKELIHOOD
C        ESTIMATION METHOD.
C   DESCRIPTION
C        THIS ROUTINE CALCULATES THE VALUE OF THE FUNCTION AT THE  POINT
C        AB (BETA(1) CONSTANT).
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
C             AB     (R) = BETA(1) CONSTANT VALUE
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             NS     (I) = NUMBER OF FAULTS FOUND TO DATE
C             SUMTBF (R) = SUM OF THE TBF DATA, INCLUDING  ANY  FAILURE-
C                          FREE TEST TIME
C        YIELDED
C             SMSAML (R) = FUNCTION VALUE AT POINT AB
C   LOCAL GLOSSARY
C             RN     (R) = REAL VARIABLE STORAGE OF NS
C             TMP1   (R) = INTERMEDIATE CALCULATION 1
C             TMP2   (R) = INTERMEDIATE CALCULATION 2
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
         INTEGER              NS
         DIMENSION            DAT(NS)
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
C        CALCULATE THE FUNCTION VALUE.
         RN = NS
         TMP1  = 0.0
         TMP2  = 0.0
         DO 1000 I = 1, NS
             TMP1  = TMP1  + DAT(I)
             TMP2  = TMP2  + 1.0 / (1.0 + AB * TMP1)
 1000        CONTINUE
C        ENDDO
         SMSAML = TMP2 / AB - (RN * SUMTBF) / (LOG(1.0 + AB * SUMTBF)
     1          * (1.0 + AB * SUMTBF))
         RETURN
         END
         SUBROUTINE SMSAPR
     G                     (NPV   , NS    , CIF   , DIF   , BETA0
     G                     ,BETA1 , EXTM  , DAT
     Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMSAPR
C    G                     (NPV   , NS    , CIF   , DIF   , BETA0
C    G                     ,BETA1 , EXTM  , DAT
C    Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C   AUTHOR
C        PAUL J. KRAUS (COMPUTATIONAL ENGINEERING, INC)
C   UPDATED NPV=1 EQUATION BY
C        OLIVER D. SMITH (EG&G, WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) AND OLIVER D. SMITH (EG&G, WASC)
C   PURPOSE
C        TO COMPUTE THE ESTIMATE OF THE  NUMBER OF FAILURES FOR AN ADDI-
C        TIONAL EXECUTION TIME; THE ESTIMATES OF THE  FUTURE RELIABILITY
C        AND  ADDITIONAL TESTING TIME  REQUIRED  TO ACHIEVE  THE DESIRED
C        INTENSITY FUNCTION; AND TO CREATE THE PREDICTED DATA VECTOR FOR
C        THE MUSA LOGARITHMIC POISSON  EXECUTION TIME MODEL.  DURING THE
C        PREDICTED VECTOR  CREATION,  THIS ROUTINE WILL ALSO COMPUTE THE
C        KOLMOGOROV DISTANCE.
C   DESCRIPTION
C        THE FIRST  ARGUMENT OF THE SMSAPR  CALL LINE  INDICATES WHETHER
C        THE ESTIMATES OF THE NUMBER OF FAILURES EXPECTED, THE ESTIMATES
C        OF THE FUTURE  RELIABILITY AND  ADDITIONAL TESTING  TIME OR THE
C        ENTIRE TBF  VECTOR IS  TO BE COMPUTED.  IF NPV IS SET TO 1, THE
C        ROUTINE  WILL  COMPUTE THE  PREDICTED  VALUE FOR  THE NUMBER OF
C        FAILURES USING THE ARGUMENTS BETA0, BETA1 & EXTM.  THE COMPUTED
C        VALUE WILL BE RETURNED IN THE FIRST ELEMENT OF PDAT.  IF NPV IS
C        SET TO 2, THE  ROUTINE  WILL COMPUTE THE  TWO  PREDICTED VALUES
C        USING THE ARGUMENTS BETA0, CIF, AND DIF.  THE 2 COMPUTED VALUES
C        WILL BE RETURNED IN THE FIRST TWO ELEMENTS OF PDAT.  OTHERWISE,
C        THE ROUTINE WILL  COMPUTE THE  ENTIRE DATA  VECTOR OF PREDICTED
C        TBFS, USING THE ARGUMENTS  BETA0, BETA1, AND NS.  THE PREDICTED
C        TBFS WILL BE RETURNED IN THE NS ELEMENTS OF PDAT.
C
C        THE CALCULATION FOR THE  KOLMOGOROV STATISTIC  WAS ADDED DURING
C        THE UPDATE THE REVISION 5. FOR THAT CALCULATION, IN ADDITION TO
C        THE MODEL ESTIMATES, THE ACTUAL DATA VECTOR IS NEEDED. ONCE THE
C        NS ELEMENTS ARE COMPUTED, USING THE SAME EQUATION AS FOR A BIAS
C        ANALYSIS OF THIS MODEL, THE MAXIMUM DIFFERENCE IS DETERMINE VIA
C        THE CMPMAX ROUTINE, AND THE SIGNIFICANCE OF THE VALUE IS DETER-
C        MINED VIA THE CHKGOF ROUTINE (BOTH OF WHICH ARE RESIDENT IN THE
C        SMERFS5 LIBRARY OF SUPPORT ROUTINES).
C
C        NOTE, IN AN EFFORT TO RETAIN THE  MODULARITY OF THE PROCESSING,
C        NO ATTEMPT WAS MADE TO REUSE INTERMEDIATE CALCULATIONS FROM THE
C        ORIGINAL PREDICTION ROUTINE (I.E., DUPLICATE PROCESSING FOR IN-
C        TERMEDIATE VALUES MAY APPEAR IN THIS ROUTINE).
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
C             BETA0  (R) = BETA(0) CONSTANT OF THE MODEL
C             BETA1  (R) = BETA(1) CONSTANT OF THE MODEL
C             CIF    (R) = CURRENT INTENSITY FUNCTION
C                          (FAILURES / CPU TIME)
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             DIF    (R) = DESIRED INTENSITY FUNCTION
C                          (FAILURES / CPU TIME)
C             EXTM   (R) = ADDITIONAL EXECUTION TIME (INCLUDES PAST)
C             NPV    (I) = NUMBER OF PREDICTED VALUES DESIRED
C             NS     (I) = SIZE OF DAT
C        YIELDED
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL, WHERE:
C                          1 - THE VALUE OF D IS LARGER  THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY NOT PROVIDE AN
C                              ADEQUATE FIT.
C                          2 - THE VALUE OF D IS SMALLER THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY PROVIDE AN AD-
C                              EQUATE FIT.
C             DVAL   (R) = CALCULATED KOLMOGOROV DISTANCE
C             PDAT   (R) = VECTOR OF PREDICTED DATA VALUE(S)
C             V      (R) = VECTOR TO CONTAIN THE U-PLOT DATA SET
C   LOCAL GLOSSARY
C             SUMTBF (R) = SUM OF THE TBF DATA
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
         INTEGER              DFLG          ,NPV           ,NS
         DIMENSION            DAT(NS)       ,PDAT(NPV)     ,V(NS)
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
         IF (NPV.EQ.1) THEN
C            COMPUTE THE  NUMBER OF FAILURES  EXPECTED FOR AN ADDITIONAL
C            EXECUTION TIME. START BY RETRIEVING THE SUM OF THE TBF FROM
C            EXTM (WHICH IS TOTAL TESTING PLUS ADDITIONAL TIME).
             SUMTBF = 0.0
             DO 1000 I = 1, NS
                 SUMTBF = SUMTBF + DAT(I)
 1000            CONTINUE
C            ENDDO
             PDAT(1) = LOG((BETA1 * EXTM   + 1.0)
     1               /     (BETA1 * SUMTBF + 1.0))
         ELSE
             IF (NPV.EQ.2) THEN
C                COMPUTE THE  FUTURE RELIABILITY  AND ADDITIONAL TESTING
C                TIME ESTIMATES.
                 PDAT(1) = BETA0 * LOG(CIF / DIF)
                 PDAT(2) = BETA0 * (1.0 / DIF - 1.0 / CIF)
             ELSE
C                INITIALIZE THE KOLMOGOROV DISTANCE  AND SUM OF FAILURES
C                TO ZERO.
                 DVAL   = 0.0
                 SUMTBF = 0.0
C
C                COMPUTE THE ESTIMATES FOR THE ENTIRE TBF DATA VECTOR.
                 DO 1025 I = 1, NS
                     PDAT(I) = (1.0 / BETA1)
     1                       *(((BETA0 / (BETA0 - 1.0))** I   ) - 1.0)
     2                       - (1.0 / BETA1)
     3                       *(((BETA0 / (BETA0 - 1.0))**(I-1)) - 1.0)
C
C                    COMPUTE THE CONTRIBUTION FOR THIS ELEMENT.
                     V(I)  = 1.0 - ((BETA1 * SUMTBF + 1.0) / (BETA1
     1                     * (SUMTBF + DAT(I)) + 1.0))**BETA0
                     SUMTBF = SUMTBF + DAT(I)
 1025                CONTINUE
C                ENDDO
C
C                COMPLETE THE COMPUTATION  FOR THE U-PLOT VECTOR;  NOTE,
C                THE VECTOR OF U-PLOT DATA IS SORTED UPON RETURN.
                 CALL CMPMAX
     G                       (NS    , 1
     B                       ,DVAL  , V     )
C
C                DETERMINE IF THE CALCULATED DISTANCE IS SIGNIFICANT.
                 CALL CHKGOF
     G                       (NS    , DVAL
     Y                       ,DFLG  )
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE SMUSCT
     G                     (TNOF  , CMTBFH, DMTBFH, IMTBFH, NS    , PC
     G                     ,PF    , PI    , PMQ   , RHOC  , TC    , TI
     G                     ,XMC   , XMF   , XMI   , XMQ
     Y                     ,XINT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMUSCT
C    G                     (TNOF  , CMTBFH, DMTBFH, IMTBFH, NS    , PC
C    G                     ,PF    , PI    , PMQ   , RHOC  , TC    , TI
C    G                     ,XMC   , XMF   , XMI   , XMQ
C    Y                     ,XINT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   UPDATED MODEL IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   UPDATED MODEL CODE STRUCTURING BY
C        PAUL J. KRAUS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO COMPUTE THE  ADDITIONAL WC TIME  NEEDED TO  MEET A SPECIFIED
C        (DESIRED) MTBNF FOR THE MUSA CALENDAR TIME MODEL COMPONENT.
C   DESCRIPTION
C        THE ADDITIONAL WALL CLOCK TIME NEEDED TO ACHIEVE  DESIRED MTBNF
C        (DMTBFH), GIVEN THE CURRENT MTBNF (CMTBFH), IS CALCULATED AS AN
C        INTEGRAL OVER THIS TIME PERIOD. THE INTEGRAND IS EXPRESSED AS A
C        FUNCTION INVOLVING EACH OF THE THREE RESOURCES (FAILURE IDENTI-
C        FICATION,  FAILURE CORRECTION, AND COMPUTER TIME),  AND HOW THE
C        WALL CLOCK TIME  VARIES PER UNIT OF CPU TIME FOR EACH RESOURCE.
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
C             CMTBFH (R) = CURRENT MTBNF (HRS)
C             DMTBFH (R) = DESIRED MTBNF (HRS)
C             IMTBFH (R) = INITIAL MTBNF (HRS)
C             NS     (I) = NUMBER OF FAULTS FOUND TO DATE
C             PC     (R) = NUMBER OF COMPUTER SHIFTS
C             PF     (R) = NUMBER OF FAILURE CORRECTION PERSONNEL
C             PI     (R) = NUMBER OF FAILURE IDENTIFICATION PERS.
C             PMQ    (R) = PROB. THAT QUEUE LENGTH .LE. MAX. QUEUE
C             RHOC   (R) = COMPUTER UTILIZATION FACTOR
C             TC     (R) = COMPUTER WC EXPENDED PER UNIT OF CP
C             TI     (R) = IDENTIFICATION TIME PER CPU TIME
C             TNOF   (R) = TOTAL FAULTS ESTIMATE FROM SMUSMD
C             XMC    (R) = COMPUTER TIME EXPENDED PER FAILURE
C             XMF    (R) = CORRECTION TIME PER FAILURE
C             XMI    (R) = IDENTIFICATION TIME PER FAILURE
C             XMQ    (R) = MAXIMUM QUEUE LENGTH PER DEBUGGER
C        YIELDED
C             XINT   (R) = ADDITIONAL WC TIME TO MEET SPECIFIED MTBNF
C   LOCAL GLOSSARY
C             DEL    (R) = TIME INCREMENT
C             DELT   (R) = SCALE TIME FACTOR
C             IM1    (I) = DATA INDEX POINTER
C             IM2    (I) = DATA INDEX POINTER
C             IV     (I) = DATA INDEX POINTER
C             P      (R) = VECTOR OF AVAILABLE RESOURCES
C             RHO    (R) = VECTOR OF UTILIZATION FACTORS
C             RHOF   (R) = CALCULATED   UTILIZATION FACTOR   FOR FAILURE
C                          CORRECTION PERSONNEL
C             T      (T) = VECTOR OF THE  CMTBF, DMTBF, AND  THE TRANSI-
C                          TION POINTS BETWEEN
C             TCNT   (I) = NUMBER OF POINTS IN T
C             TH     (R) = VECTOR OF RESOURCE   EXPENDITURE PER  UNIT OF
C                          CPU TEST TIME
C             TM     (R) = MIDPOINT OF TIME INCREMENT
C             V      (R) = DERIVATIVE OF RESOURCE  EXPENDITURES PER UNIT
C                          OF CPU TIME
C             X      (R) = VECTOR OF TRANSITION POINTS BETWEEN RESOURCES
C             XM     (R) = VECTOR OF RESOURCE EXPENDITURE  TIME (WC) RE-
C                          QUIRED PER FAILURE
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
         INTEGER              NS
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,IM1           ,IM2
     1                       ,IV            ,J             ,K
     2                       ,L             ,TCNT
         DIMENSION            P(3)          ,RHO(3)        ,T(5)
     1                       ,TH(3)         ,V(3)          ,X(3)
     2                       ,XM(3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        CALCULATE THE RHOF.
         RHOF = (1.0 - PMQ**(1.0 / PF))**(1.0 / XMQ)
C
C        STORE THE CURRENT MTBNF IN THE VECTOR T.
         TCNT    = 1
         T(TCNT) = CMTBFH
C
C        COMPUTE THE TRANSITION POINTS (STORING THEM IN X).
         X(1) = (1.0 / TI)  * ((PI * XMF) / (PF  * RHOF) - XMI)
         X(2) = (1.0 / TC)  * ((PC * RHOC * XMF) / (PF   * RHOF) - XMC)
         X(3) = (PC  * RHOC *  XMI - PI   * XMC) / (PI * TC - PC * RHOC
     1        * TI)
C
C        STORE THE X(I)'S  THAT FALL WITHIN  THE RANGE SPECIFIED BETWEEN
C        THE CURRENT MTBNF AND THE DESIRED MTBNF.
         DO 1000 I = 1, 3
             IF ((X(I).GT.CMTBFH) .AND. (X(I).LT.DMTBFH)) THEN
                 TCNT    = TCNT + 1
                 T(TCNT) = X(I)
             ENDIF
 1000        CONTINUE
C        ENDDO
C
C        STORE THE DESIRED MTBNF IN THE VECTOR T.
         TCNT    = TCNT + 1
         T(TCNT) = DMTBFH
C
C        SORT THE CENTRAL PORTION  OF THE VECTOR T, IF MORE THAN ONE VA-
C        LUE WAS TRANSFERRED IN FROM X.
         IF (TCNT.GT.3) THEN
             IM2 = TCNT - 2
             IM1 = TCNT - 1
             DO 1050 I = 2, IM2
                 L = I + 1
                 DO 1025 J = L, IM1
                     IF (T(I).GT.T(J)) THEN
                         TM   = T(I)
                         T(I) = T(J)
                         T(J) = TM
                     ENDIF
 1025                CONTINUE
C                ENDDO
 1050            CONTINUE
C            ENDDO
         ENDIF
C
C        PERFORM DECLARATIONS  AND INITIALIZATIONS OF  THE CONSTANTS AND
C        VARIABLES.
         P(1)   = PI
         P(2)   = PF
         P(3)   = PC
C
         RHO(1) = 1.0
         RHO(2) = RHOF
         RHO(3) = RHOC
C
         TH(1)  = TI
         TH(2)  = 0.0
         TH(3)  = TC
C
         XM(1)  = XMI
         XM(2)  = XMF
         XM(3)  = XMC
C
         XINT   = 0.0
         DELT   = (TNOF - REAL(NS)) / (TNOF * IMTBFH)
C
         DO 1100 I = 2, TCNT
             J  = I - 1
             TM = T(J) + (T(I) - T(J)) / 2.0
             DO 1075 K = 1, 3
                 V(K) = (TH(K) * TM + XM(K)) / (P(K) * RHO(K) * TM)
 1075            CONTINUE
C            ENDDO
C
C            DETERMINE THE ELEMENT OF V THAT CONTAINS THE LARGEST VALUE.
C            (NOTE IF MULTIPLE ELEMENTS  CONTAIN THE SAME LARGEST VALUE,
C            THE FIRST OCCURRENCE WILL BE USED).
             IV = 3
             IF ((V(2).GE.V(3)) .AND. (V(2).GE.V(1))) THEN
                 IV = 2
             ENDIF
             IF ((V(1).GE.V(2)) .AND. (V(1).GE.V(3))) THEN
                 IV = 1
             ENDIF
C
C            COMPUTE THE VALUE OF DEL.
             DEL = (1.0 / T(J)) - (1.0 / T(I))
             DEL = MIN(DEL, DELT)
C
C            PERFORM XINT UPDATE BASED ON THE INDEX OF THE LARGEST V.
             IF (IV.EQ.1) THEN
                 XINT = XINT + ((TNOF * IMTBFH) / PI)
     1                * (XMI *   DEL  + TI      * LOG(T(I) / T(J)))
             ELSEIF (IV.EQ.2) THEN
                 XINT = XINT + ((TNOF * IMTBFH) /      (PF * RHOF))
     1                * (DEL * XMF)
             ELSE
                 XINT = XINT + ((TNOF * IMTBFH) /      (PC * RHOC))
     1                * (XMC *   DEL  + TC      * LOG(T(I) / T(J)))
             ENDIF
             DELT = DELT - DEL
 1100        CONTINUE
C        ENDDO
         RETURN
         END
         FUNCTION SMUSFN
     G                   (AB    , NS    , SUMTBF, SUMTTF)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION SMUSFN
C    G                   (AB    , NS    , SUMTBF, SUMTTF)
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   DEKKER-BRENT AND UPDATED MODEL IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   DEKKER-BRENT AND UPDATED MODEL CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE VALUE OF THE FUNCTION FOR THE MUSA BASIC  EXE-
C        CUTION TIME  MODEL  USING  THE  MAXIMUM  LIKELIHOOD  ESTIMATION
C        METHOD.
C   DESCRIPTION
C        THIS ROUTINE CALCULATES THE VALUE OF THE FUNCTION AT THE  POINT
C        AB (BETA(1) CONSTANT).
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
C             AB     (R) = BETA(1) CONSTANT VALUE
C             NS     (I) = NUMBER OF FAULTS FOUND TO DATE
C             SUMTBF (R) = SUM OF THE TBF DATA, INCLUDING  ANY  FAILURE-
C                          FREE TEST TIME
C             SUMTTF (R) = SUM OF THE TTF
C        YIELDED
C             SMUSFN (R) = FUNCTION VALUE AT POINT AB
C   LOCAL GLOSSARY
C             RN     (R) = REAL VARIABLE STORAGE OF NS
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
         INTEGER              NS
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
C        CALCULATE THE FUNCTION VALUE; FIRST ENSURING THAT THE EXP FUNC-
C        TION WILL NOT RESULT IN A FLOATING OVERFLOW.
         RN   = NS
C
         TMP1 = AB * SUMTBF
         TMP2 = 0.0
         IF (TMP1 .LT. 85.0) THEN
             TMP2 = RN * SUMTBF / (EXP(TMP1) - 1.0)
         ENDIF
C
         SMUSFN = - RN / AB + SUMTTF + TMP2
         RETURN
         END
         SUBROUTINE SMUSMA
     G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
     Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMUSMA
C    G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
C    Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM FOUR TYPES OF  MODEL APPLICABILITY ANALYSIS FOR  THE
C        BASIC MUSA EXECUTION TIME MODEL.
C
C        THESE INCLUDE:
C             1) THE MODEL ACCURACY;
C             2) THE MODEL BIAS;
C             3) THE MODEL NOISE; AND
C             4) THE MODEL TREND
C   DESCRIPTION
C        THE ROUTINE  CONTROLS THE EXECUTIONS  OF THE  APPLICABLE SMFLIB
C        ROUTINE, SMUSMD.  THE PROCESSING, AS DEFINED  IN THE LITTLEWOOD
C        PAPER (SEE REFERENCES IN THE MAIN PROGRAM) BASICALLY INVOLVES A
C        SERIES OF EXECUTIONS WHERE  NSB TO NSE DATA POINTS ARE UTILIZED
C        IN THE MODEL.
C
C        THE SMUSMD ARGUMENT, XLTM, IS SET TO ZERO ON ALL EXECUTIONS TO
C        INDICATE NO FAILURE-FREE TESTING TIME.
C
C        FOR EACH ITERATION,  THE MODEL PARAMETERS (ESTIMATES)  FROM THE
C        CURRENT ITERATION ARE USED IN CONJUNCTION WITH THE OBSERVED DA-
C        TA FROM THE NEXT ITERATION POINT  TO DETERMINE AN OVER-ALL PIC-
C        TURE OF THE APPLICABILITY OF THE MODEL  FOR THE PARTICULAR DATA
C        SET.
C
C        THIS ONE ROUTINE IS USED TO OBTAIN ALL FOUR ANALYSES; THE SIXTH
C        ARGUMENT, TYP, IS USED TO INDICATE HOW THE MODEL PARAMETERS ARE
C        TO BE USED.
C
C        NOTE, THE VECTOR, V, IS NOT USED IN THE NOISE ANALYSIS  AND THE
C        VECTOR, VPRE, IS ONLY USED IN THE BIAS ANALYSIS;  HOWEVER, THEY
C        SHOULD BE AVAILABLE TO THE ROUTINE FOR ALL ACCESSES.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED TBF DATA
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C             TYP    (I) = ANALYSIS TYPE TO BE PERFORMED (SEE PURPOSE)
C        YIELDED
C             INDX   (I) = ANALYSIS ITERATION COUNTER (AT RETURN)
C             RFLAG  (I) = MODEL RESULTS ERROR FLAG
C             STAT   (R) = ANALYSIS RESULTS STATISTIC
C             V      (R) = ANALYSIS RESULTS ARRAY OF SIZE NSR
C             VPRE   (R) = VECTOR OF BIAS (U-PLOT) DATA PRIOR TO SORTING
C   LOCAL GLOSSARY
C             BETA0  (R) = BETA(0) CONSTANT OF THE MODEL
C             BETA1  (R) = BETA(1) CONSTANT OF THE MODEL
C             CIF    (R) = CURRENT INTENSITY FUNCTION
C             CMTBF  (R) = CURRENT MTBNF
C             ECR    (R) = ESTIMATE OF CURRENT RELIABILITY
C             IFLG   (I) = FLAG SIGNALING C.I. AVAILABILITY IN IMTBF
C             IIF    (R) = INITIAL INTENSITY FUNCTION
C             IMTBF  (R) = ARRAY OF INITIAL MTBNF AND 95% C.I.
C             MCUR   (R) = VALUE OF THE FUNCTION FOR THE CURRENT POINT
C             MNXT   (R) = VALUE OF THE FUNCTION FOR THE NEXT    POINT
C             SUMTBF (R) = SUM OF THE TBF DATA
C             SUMY   (R) = SUM OF THE PREVIOUS Y'S
C             TMP    (R) = INTERMEDIATE CALCULATION
C             TNOF   (R) = ARRAY FOR TOTAL # OF FAULTS AND 95% C.I.
C             TOTY   (R) = SUM OF ALL THE Y'S
C             XLTM   (R) = FAILURE-FREE TEST TIME (SET TO ZERO)
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
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              INDX          ,NS            ,NSB
     1                       ,NSE           ,NSR           ,RFLAG
     2                       ,TYP
         DIMENSION            DAT(NS)       ,V(NSR)        ,VPRE(NSR)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,IFLG          ,J
         DIMENSION            IMTBF(3)      ,TNOF(3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE  THE ITERATION COUNTER,  STORAGE INDEX,  MODEL ERROR
C        FLAG, RESULTS STATISTIC, AND SUM OF THE Y-PLOT VALUES LOCATION.
         I     = NSB - 1
         INDX  = 0
         RFLAG = 0
         STAT  = 0.0
         TOTY  = 0.0
C
C        INITIALIZE THE FAILURE-FREE TESTING TIME LOCATION TO ZERO.
         XLTM  = 0.0
C
         IF (TYP.EQ.3) THEN
C            INITIALIZE THE CONSTANT PORTION OF THE NOISE CALCULATION.
             TMP = LOG(0.5)
         ENDIF
C
C        CALCULATE THE SUM OF THE TBF DATA THROUGH THE PRIOR STEP.
         SUMTBF = 0.0
         DO 1000 J = 1, I
             SUMTBF = SUMTBF + DAT(J)
 1000        CONTINUE
C        ENDDO
C
C        DOWHILE (PROCESSING LEFT AND NO ERRORS)
 1025        I = I + 1
             IF ((I.GT.NSE) .OR. (RFLAG.NE.0)) GO TO 1050
C
C            ADD THE CONTRIBUTION OF THIS STEP TO THE SUM OF TBF DATA.
             SUMTBF = SUMTBF + DAT(I)
C
C            COMPUTE MODEL ESTIMATES.
             CALL SMUSMD
     G                   (I     , DAT   , XLTM
     Y                   ,CMTBF , ECR   , IMTBF , IFLG  , TNOF
     Y                   ,IIF   , CIF   , RFLAG )
C
C            INCREMENT THE ITERATION COUNTER FOR STORAGE.
             INDX = INDX + 1
C
             IF (RFLAG.EQ.0) THEN
C                RECLAIM THE BETA0 AND BETA1 PARAMETERS.
                 BETA0 = TNOF(1)
                 BETA1 = IIF    / BETA0
C
                 IF     (TYP.EQ.1) THEN
C                    ACCURACY ANALYSIS IS DESIRED;  APPEND THE CONTRIBU-
C                    TION OF THE  CURRENT  ITERATION TO  THE PREQUENTIAL
C                    LIKELIHOOD STATISTIC.
                     V(INDX) = 0.0 - (LOG(BETA0) + LOG(BETA1) - BETA1
     1                       * (SUMTBF + DAT(I+1))
     2                       + BETA0 * (EXP(-BETA1 * (SUMTBF+DAT(I+1)))
     3                       - EXP(-BETA1 * SUMTBF)))
                     STAT    = STAT + V(INDX)
C
                 ELSEIF ((TYP.EQ.2) .OR. (TYP.EQ.4)) THEN
C                    BIAS OR TREND ANALYSIS IS DESIRED;  STORE THE FUNC-
C                    TIONAL VALUE FOR THIS ELEMENT OF THE U-PLOT.
                     V(INDX) = 1.0 - EXP(-(BETA0 * EXP(-BETA1 * SUMTBF))
     1                       * (1.0 - EXP(-BETA1 * DAT(I+1))))
C
                     IF (TYP.EQ.4) THEN
C                        TREND ANALYSIS IS DESIRED; TRANSFORM THE U-PLOT
C                        VALUE TO THE Y-PLOT REPRESENTATION.
                         IF (V(INDX).LT.1.0) THEN
                             V(INDX) = -LOG(1.0 - V(INDX))
                             TOTY    = TOTY + V(INDX)
                         ELSE
                             RFLAG   = 5
                         ENDIF
                     ENDIF
C
                 ELSE
C                    NOISE ANALYSIS IS DESIRED; COMPUTE THE FUNCTION FOR
C                    THE NEXT POINT.
                     MNXT = -(1.0 / BETA1) * LOG(1.0
     1                    + (TMP / (BETA0 * EXP(-BETA1 * SUMTBF))))
C
C                    PERFORM THE ADDITION OF THE NOISE ON ALL ITERATIONS
C                    EXCEPT THE FIRST; AND THEN PREPARE FOR THE NEXT.
                     IF (I.GT.NSB) THEN
                         STAT = STAT + ABS((MNXT - MCUR) / MCUR)
                     ENDIF
                     MCUR = MNXT
C
                 ENDIF
             ENDIF
             GO TO 1025
 1050        CONTINUE
C        ENDWHILE
C
         IF ((TYP.EQ.2) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE U-PLOT VECTOR  AND COMPUTE
C            THE KOLMOGOROV STATISTIC. FIRST STORE THE UNSORTED DATA.
             DO 1075 I = 1, NSR
                 VPRE(I) = V(I)
 1075            CONTINUE
C            ENDDO
             CALL CMPMAX
     G                   (NSR   , 1
     B                   ,STAT  , V     )
         ENDIF
C
         IF ((TYP.EQ.4) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE Y-PLOT VECTOR.
             SUMY = 0.0
             DO 1100 I = 1, NSR
                 SUMY = SUMY + V(I)
                 V(I) = SUMY / TOTY
 1100            CONTINUE
C            ENDDO
C
C            COMPUTE THE KOLMOGOROV STATISTIC;  NOTE, SORTING IS NOT RE-
C            QUIRED FOR THE Y-PLOT DATA.
             CALL CMPMAX
     G                   (NSR   , 0
     B                   ,STAT  , V     )
         ENDIF
         RETURN
         END
         SUBROUTINE SMUSMD
     G                     (NS    , DAT   , XLTM
     Y                     ,CMTBF , ECR   , IMTBF , IFLG  , TNOF
     Y                     ,IIF   , CIF   , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMUSMD
C    G                     (NS    , DAT   , XLTM
C    Y                     ,CMTBF , ECR   , IMTBF , IFLG  , TNOF
C    Y                     ,IIF   , CIF   , RFLAG )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   DEKKER-BRENT AND UPDATED MODEL IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   DEKKER-BRENT AND UPDATED MODEL CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE  ESTIMATES IN THE MUSA  BASIC  EXECUTION  TIME
C        MODEL USING THE DEKKER-BRENT MINIMIZATION ALGORITHM.
C   DESCRIPTION
C        ONCE THE ITERATIVE EXECUTION IS INITIATED, PROCESSING WILL CON-
C        TINUE UNTIL ONE OF TWO  POSSIBLE  TERMINATION STATES IS REACHED
C        IN THE EXECUTION.  THE LAST ARGUMENT  OF THE CALL LINE  (RFLAG)
C        WILL BE SET TO INDICATE  TO THE CALLING ROUTINE  THE REASON FOR
C        EXIT. THE POSSIBLE REASONS FOR SMUSMD EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS THAN THE  OBSERVED
C                 FAULTS TO DATE
C
C        THE RESULTANT STATISTICS  WILL BE RETURNED  IN THE 4-TH THROUGH
C        THE 10-TH ARGUMENTS  OF THE CALL LINE.  THE ARGUMENT CMTBF WILL
C        CONTAIN THE ESTIMATE OF THE CURRENT MTBNF, ECR WILL CONTAIN THE
C        ESTIMATE OF THE CURRENT RELIABILITY, IMTBF WILL CONTAIN THE ES-
C        TIMATE OF THE INITIAL MTBNF AND ITS 95% CI,  AND TNOF WILL CON-
C        TAIN THE ESTIMATE OF THE TOTAL NUMBER OF FAULTS AND ITS 95% CI.
C
C        THE ARGUMENT IFLG WILL SIGNAL WHETHER THE 95% C.I. OF THE IMTBF
C        COULD BE COMPUTED.  IFLG WILL BE SET TO ONE TO INDICATE THE VA-
C        LUES WERE COMPUTABLE OR TO ZERO TO INDICATE THE VALUES WERE NOT
C        COMPUTABLE.
C
C        THE ARGUMENTS IIF AND CIF  WILL CONTAIN THE INITIAL AND CURRENT
C        INTENSITY FUNCTION, RESPECTIVELY.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS  SUBPROGRAM  CONTAINS  CODE WHICH DOES NOT COMPLY WITH THE
C        PROGRAMMING STANDARDS; GROUP LEADER APPROVAL HAS BEEN OBTAINED.
C       -SPECIFICALLY:
C        THE ARGUMENT  FC IN THE ACCESS TO THE ROUTINE ZERO, IS ACTUALLY
C        INITIALIZED IN THE ACCESSED ROUTINE. WHEN ICOUNT HAS A VALUE OF
C        MORE THAN ONE, THE ARGUMENT BECOMES A BOTH (AS MARKED).  HENCE,
C        FC APPEARS UNDEFINED IN THIS ROUTINE'S COMPILATION LISTING.
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             NS     (I) = SIZE OF DAT
C             XLTM   (R) = LAST FAILURE-FREE TEST TIME
C        YIELDED
C             CIF    (R) = CURRENT INTENSITY FUNCTION
C                          (FAILURES / CPU TIME)
C             CMTBF  (R) = CURRENT MTBNF
C             ECR    (R) = ESTIMATE OF THE CURRENT RELIABILITY
C             IFLG   (I) = FLAG SIGNALING C.I. AVAILABILITY IN IMTBF
C             IIF    (R) = INITIAL INTENSITY FUNCTION
C                          (FAILURES / CPU TIME)
C             IMTBF  (R) = INITIAL MTBNF AND 95% CI
C             RFLAG  (I) = RETURN STATUS FLAG
C             TNOF   (R) = TOTAL # OF FAULTS ESTIMATE AND 95% CI
C   LOCAL GLOSSARY
C             A      (R) = LOWER BOUND OF BETA(1) CONSTANT
C             B      (R) = UPPER BOUND OF BETA(1) CONSTANT
C             BETA0  (R) = BETA(0) CONSTANT OF THE MODEL
C             BETA1  (R) = BETA(1) CONSTANT OF THE MODEL
C             COUNT  (I) = ITERATION COUNTER
C             FA     (R) = VALUE OF FUNCTION AT A
C             FB     (R) = VALUE OF FUNCTION AT B
C             FC     (R) = VALUE OF FUNCTION AT C
C             ICOUNT (I) = STEP COUNTER
C             IFLAG  (I) = CONVERGENCE FLAG
C             RN     (R) = REAL VARIABLE STORAGE OF NS
C             SUMTBF (R) = SUM OF THE TBF DATA, INCLUDING  ANY  FAILURE-
C                          FREE TEST TIME
C             SUMTTF (R) = SUM OF THE TTF
C             TMP    (R) = INTERMEDIATE CALCULATION
C             VR0    (R) = INTERMEDIATE VALUE IN THE  COMPUTATION OF THE
C                          95% CI VALUES
C             VR1    (R) = INTERMEDIATE VALUE IN THE  COMPUTATION OF THE
C                          95% CI VALUES
C   ERRORS
C        RFLAG  = 3 : THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C        RFLAG  = 4 : THE  TOTAL  FAULTS  ESTIMATE  WAS  LESS  THAN  THE
C                     OBSERVED FAULTS TO DATE
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
         INTEGER              IFLG          ,NS            ,RFLAG
         DIMENSION            DAT(NS)       ,IMTBF(3)      ,TNOF(3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              COUNT         ,I             ,ICOUNT
     1                       ,IFLAG
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        CALCULATE THE  SUM OF THE TBF  INCLUDING THE  LAST FAILURE-FREE
C        TEST TIME, AND THE SUM OF THE TTF DATA.
         RN = NS
         SUMTBF = 0.0
         SUMTTF = 0.0
         DO 1000 I = 1, NS
             SUMTBF = SUMTBF + DAT(I)
             SUMTTF = SUMTTF + SUMTBF
 1000        CONTINUE
C        ENDDO
         SUMTBF = SUMTBF + XLTM
C
C        SET THE COUNTER AND RETURN STATUS FLAG.
         COUNT    = 0
         RFLAG    = -1
C
C        ZERO ALL RETURN STATISTICS ARGUMENTS.
         CMTBF    = 0.0
         ECR      = 0.0
         IMTBF(1) = 0.0
         IMTBF(2) = 0.0
         IMTBF(3) = 0.0
         TNOF(1)  = 0.0
         TNOF(2)  = 0.0
         TNOF(3)  = 0.0
         IFLG     = 0
C
C        COMPUTE THE VALUE OF THE FUNCTION AT ITS LOWER BOUND.
         A =  0.1 / SUMTBF
         B = 10.0 / SUMTBF
         FA = SMUSFN
     G               (A     , NS    , SUMTBF, SUMTTF)
C
C        COMPUTE THE VALUE OF THE FUNCTION AT A POINT WHERE ITS SIGN  IS
C        THE  OPPOSITE  OF  THE  COMPUTED  VALUE AT THE  FUNCTIONS LOWER
C        BOUND.
C
C        DOWHILE (RFLAG .EQ. -1)
 1025        COUNT = COUNT + 1
             FB = SMUSFN
     G                   (B     , NS    , SUMTBF, SUMTTF)
C
C            SET RFLAG, IF CONVERGENCE POSSIBLE.
             IF ((FA*FB).LE.0.0) THEN
                 RFLAG = 0
             ENDIF
C
C            SET RFLAG, IF THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
             IF (COUNT.GE.5) THEN
                 RFLAG = 3
             ENDIF
             IF (RFLAG.EQ.-1) THEN
                 B = B * 10.0
             ENDIF
             IF (RFLAG.EQ.-1) GO TO 1025
C        ENDWHILE
C
         IF (RFLAG.EQ.0) THEN
C            COMPUTE THE ESTIMATE OF THE BETA(1) CONSTANT.
             IFLAG  = -1
             ICOUNT = 1
C
C            DOWHILE (IFLAG .EQ. -1)
C                COMPUTE THE ZERO OF THE FUNCTION IN THE INTERVAL  A AND
C                B; NOTE THE ARGUMENT FC IS INITIALIZED BY THE ZERO ROU-
C                TINE WHEN ICOUNT IS SET TO ONE.   <<SEE RESTRICTIONS <<
 1050            CALL ZERO
     B                     (A     , B     , FA    , FB    , FC
     B                     ,ICOUNT
     Y                     ,IFLAG )
C
                 IF (IFLAG.EQ.-1) THEN
C                    COMPUTE THE VALUE OF THE FUNCTION AT ITS NEW  UPPER
C                    BOUND.
                     FB = SMUSFN
     G                           (B     , NS    , SUMTBF, SUMTTF)
                 ENDIF
                 IF (IFLAG.EQ.-1) GO TO 1050
C            ENDWHILE
C
C            STORE THE VALUE OF THE  BETA(1)  CONSTANT IN BETA1 AND COM-
C            PUTE THE VALUE OF THE BETA(0) CONSTANT.
             BETA1 = B
             BETA0 = RN / (1.0 - EXP(-BETA1 * SUMTBF))
         ENDIF
C
         IF (RFLAG.EQ.0) THEN
           IF (BETA0.LT.RN) THEN
C            PREDICTED TOTAL NUMBER OF FAULTS LESS THAN OBSERVED.
             RFLAG = 4
           ELSE
C            CONVERGENCE WAS THE REASON FOR THE ABOVE LOOP EXIT; SET THE
C            TOTAL NUMBER OF FAULTS,  AND COMPUTE THE  INITIAL INTENSITY
C            FUNCTION AND MTBNF.
             TNOF(1)  = BETA0
             IIF      = BETA0 * BETA1
             IMTBF(1) = 1.0   / IIF
C
C            REMOVE THE FAILURE-FREE TESTING TIME FROM SUMTBF.
             SUMTBF = SUMTBF - XLTM
C
C            COMPUTE  THE 95%  CI ABOUT THE ESTIMATE OF THE TOTAL NUMBER
C            OF FAULTS.   IF THE LOWER LIMIT IS LESS  THAN THE  OBSERVED
C            NUMBER OF FAULTS, SET THE LOWER  LIMIT TO THE OBSERVED NUM-
C            BER OF FAULTS.
             VR0     = 1.95    * BETA0 / SQRT(RN)
             TNOF(2) = TNOF(1) - VR0
             IF (TNOF(2).LT.RN) THEN
                 TNOF(2) = RN
             ENDIF
             TNOF(3) = TNOF(1) + VR0
C
C            IF THE 95%  CI  ABOUT  THE ESTIMATE OF THE INITIAL MTBNF IS
C            COMPUTABLE, SET IFLG TO ONE AND COMPUTE THE 95% CI.  IF THE
C            LOWER LIMIT IS LESS THAN ZERO, SET THE LOWER LIMIT TO ZERO.
             TMP = (EXP(BETA1  * SUMTBF) - 1.0)**2
             VR1 =      (1.95  / SQRT(RN))
     1           *  SQRT((TMP  * BETA1**2)
     2           /       (TMP  - EXP(BETA1 * SUMTBF)
     3           *      (BETA1 * SUMTBF)**2))
             IF ((BETA1 - VR1).GT.0.0) THEN
                 IFLG     = 1
                 IMTBF(2) = 1.0 / ((BETA0 + VR0) * (BETA1 + VR1))
                 IF (IMTBF(2).LT.0.0) THEN
                     IMTBF(2) = 0.0
                 ENDIF
                 IMTBF(3) = 1.0 / ((BETA0 - VR0) * (BETA1 - VR1))
             ENDIF
C
C            COMPUTE THE ESTIMATE OF THE  CURRENT INTENSITY FUNCTION AND
C            MTBNF.
             CIF   = IIF * EXP((-BETA1 * SUMTBF))
             CMTBF = 1.0 / CIF
C
C            COMPUTE THE ESTIMATE OF THE CURRENT RELIABILITY.
             ECR = EXP(-SUMTBF / CMTBF)
           ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE SMUSPR
     G                     (NPV   , NS    , DAT   , IMTBF , CMTBF
     G                     ,DMTBF , TNOF  , IIF
     Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMUSPR
C    G                     (NPV   , NS    , DAT   , IMTBF , CMTBF
C    G                     ,DMTBF , TNOF  , IIF
C    Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   UPDATED MODEL IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   UPDATED MODEL CODE STRUCTURING BY
C        PAUL J. KRAUS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO COMPUTE THE  ESTIMATES OF THE  FUTURE RELIABILITY  AND ADDI-
C        TIONAL TESTING TIME REQUIRED  TO ACHIEVE THE DESIRED MTBNF; AND
C        TO CREATE THE  PREDICTED DATA  VECTOR FOR THE MUSA BASIC EXECU-
C        TION TIME MODEL.  DURING THE  PREDICTED VECTOR  CREATION,  THIS
C        ROUTINE WILL ALSO COMPUTE THE KOLMOGOROV DISTANCE.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE SMUSPR  CALL LINE  INDICATES WHETHER
C        THE ESTIMATES OF THE  FUTURE RELIABILITY AND ADDITIONAL TESTING
C        TIME OR THE ENTIRE TBF VECTOR IS TO BE COMPUTED.  IF NPV IS SET
C        TO 2, THE ROUTINE  WILL COMPUTE THE  TWO PREDICTED VALUES USING
C        THE ARGUMENTS  NS, IMTBF, CMTBF, DMTBF, AND TNOF.  THE TWO COM-
C        PUTED  VALUES WILL  BE  RETURNED IN THE FIRST  TWO  ELEMENTS OF
C        PDAT.  OTHERWISE, THE ROUTINE WILL COMPUTE THE ENTIRE DATA VEC-
C        TOR OF  PREDICTED TBFS, USING THE ARGUMENTS NS, DAT, IMTBF, AND
C        TNOF.  THE PREDICTED TBFS WILL BE RETURNED IN THE FIRST NS ELE-
C        MENTS OF PDAT.
C
C        THE CALCULATION FOR THE  KOLMOGOROV STATISTIC  WAS ADDED DURING
C        THE UPDATE THE REVISION 5. FOR THAT CALCULATION, IN ADDITION TO
C        THE MODEL ESTIMATES, THE ACTUAL DATA VECTOR IS NEEDED. ONCE THE
C        NS ELEMENTS ARE COMPUTED, USING THE SAME EQUATION AS FOR A BIAS
C        ANALYSIS OF THIS MODEL, THE MAXIMUM DIFFERENCE IS DETERMINE VIA
C        THE CMPMAX ROUTINE, AND THE SIGNIFICANCE OF THE VALUE IS DETER-
C        MINED VIA THE CHKGOF ROUTINE (BOTH OF WHICH ARE RESIDENT IN THE
C        SMERFS5 LIBRARY OF SUPPORT ROUTINES).
C
C        NOTE, IN AN EFFORT TO RETAIN THE  MODULARITY OF THE PROCESSING,
C        NO ATTEMPT WAS MADE TO REUSE INTERMEDIATE CALCULATIONS FROM THE
C        ORIGINAL PREDICTION ROUTINE (I.E., DUPLICATE PROCESSING FOR IN-
C        TERMEDIATE VALUES MAY APPEAR IN THIS ROUTINE).
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
C             CMTBF  (R) = CURRENT MTBNF
C             DAT    (R) = TBF VECTOR (WC OR CPU)
C             DMTBF  (R) = DESIRED MTBNF
C             IIF    (R) = INITIAL INTENSITY FUNCTION
C             IMTBF  (R) = INITIAL MTBNF
C             NPV    (I) = NUMBER OF PREDICTED VALUES DESIRED
C             NS     (I) = SIZE OF DAT
C             TNOF   (R) = ESTIMATE OF THE TOTAL NUMBER OF FAULTS
C        YIELDED
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL, WHERE:
C                          1 - THE VALUE OF D IS LARGER  THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY NOT PROVIDE AN
C                              ADEQUATE FIT.
C                          2 - THE VALUE OF D IS SMALLER THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY PROVIDE AN AD-
C                              EQUATE FIT.
C             DVAL   (R) = CALCULATED KOLMOGOROV DISTANCE
C             PDAT   (R) = VECTOR OF PREDICTED DATA VALUE(S)
C             V      (R) = VECTOR TO CONTAIN THE U-PLOT DATA SET
C   LOCAL GLOSSARY
C             BETA0  (R) = BETA(0) CONSTANT OF THE MODEL
C             BETA1  (R) = BETA(1) CONSTANT OF THE MODEL
C             SUMTBF (R) = SUM OF THE TBF DATA
C             TM1    (R) = INTERMEDIATE CALCULATION
C             TM2    (R) = INTERMEDIATE CALCULATION
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
         INTEGER              DFLG          ,NPV           ,NS
         DIMENSION            DAT(NS)       ,PDAT(NPV)     ,V(NS)
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
         IF (NPV.EQ.2) THEN
C            COMPUTE THE FUTURE RELIABILITY  AND ADDITIONAL TESTING TIME
C            ESTIMATES.
             TM1     = TNOF - REAL(NS)
             TM2     = TNOF * IMTBF *    (1.0   / CMTBF - 1.0 / DMTBF)
             PDAT(1) = MIN(TM1, TM2)
             PDAT(2) = TNOF * IMTBF * LOG(DMTBF / CMTBF)
         ELSE
C            INITIALIZE THE KOLMOGOROV DISTANCE  AND SUM OF FAILURES TO
C            ZERO.
             DVAL   = 0.0
             SUMTBF = 0.0
C
C            RECLAIM THE BETA0 AND BETA1 PARAMETERS.
             BETA0 = TNOF
             BETA1 = IIF  / BETA0
C
C            COMPUTE THE ESTIMATES FOR THE ENTIRE TBF DATA VECTOR.
             TM1 = 0.0
             TM2 = TNOF * IMTBF
             DO 1000 I = 1, NS
                 PDAT(I) = IMTBF * EXP(TM1 / TM2)
                 TM1     = TM1   + DAT(I)
C
C                COMPUTE THE CONTRIBUTION FOR THIS ELEMENT.
                 V(I)   = 1.0 - EXP(-(BETA0 * EXP(-BETA1 * SUMTBF))
     1                  * (1.0 - EXP(-BETA1 * DAT(I))))
                 SUMTBF = SUMTBF + DAT(I)
 1000            CONTINUE
C            ENDDO
C
C            COMPLETE THE COMPUTATION  FOR THE U-PLOT VECTOR;  NOTE, THE
C            VECTOR OF U-PLOT DATA IS SORTED UPON RETURN.
             CALL CMPMAX
     G                   (NS    , 1
     B                   ,DVAL  , V     )
C
C            DETERMINE IF THE CALCULATED DISTANCE IS SIGNIFICANT.
             CALL CHKGOF
     G                   (NS    , DVAL
     Y                   ,DFLG  )
         ENDIF
         RETURN
         END
         SUBROUTINE SNPTMA
     G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
     Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SNPTMA
C    G                     (DAT   , NS    , NSB   , NSE   , NSR   , TYP
C    Y                     ,STAT  , RFLAG , INDX  , V     , VPRE  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM FOUR TYPES OF  MODEL APPLICABILITY ANALYSIS FOR  THE
C        NON-HOMOGENEOUS POISSON MODEL FOR EXECUTION TIME DATA.
C
C        THESE INCLUDE:
C             1) THE MODEL ACCURACY;
C             2) THE MODEL BIAS;
C             3) THE MODEL NOISE; AND
C             4) THE MODEL TREND
C   DESCRIPTION
C        THE ROUTINE  CONTROLS THE EXECUTIONS  OF THE  APPLICABLE SMFLIB
C        ROUTINE, SNPTMD.  THE PROCESSING, AS DEFINED  IN THE LITTLEWOOD
C        PAPER (SEE REFERENCES IN THE MAIN PROGRAM) BASICALLY INVOLVES A
C        SERIES OF EXECUTIONS WHERE  NSB TO NSE DATA POINTS ARE UTILIZED
C        IN THE MODEL.
C
C        FOR EACH ITERATION,  THE MODEL PARAMETERS (ESTIMATES)  FROM THE
C        CURRENT ITERATION ARE USED IN CONJUNCTION WITH THE OBSERVED DA-
C        TA FROM THE NEXT ITERATION POINT  TO DETERMINE AN OVER-ALL PIC-
C        TURE OF THE APPLICABILITY OF THE MODEL  FOR THE PARTICULAR DATA
C        SET.
C
C        THIS ONE ROUTINE IS USED TO OBTAIN ALL FOUR ANALYSES; THE SIXTH
C        ARGUMENT, TYP, IS USED TO INDICATE HOW THE MODEL PARAMETERS ARE
C        TO BE USED.
C
C        NOTE, THE VECTOR, V, IS NOT USED IN THE NOISE ANALYSIS  AND THE
C        VECTOR, VPRE, IS ONLY USED IN THE BIAS ANALYSIS;  HOWEVER, THEY
C        SHOULD BE AVAILABLE TO THE ROUTINE FOR ALL ACCESSES.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED TTF DATA
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C             TYP    (I) = ANALYSIS TYPE TO BE PERFORMED (SEE PURPOSE)
C        YIELDED
C             INDX   (I) = ANALYSIS ITERATION COUNTER (AT RETURN)
C             RFLAG  (I) = MODEL RESULTS ERROR FLAG
C             STAT   (R) = ANALYSIS RESULTS STATISTIC
C             V      (R) = ANALYSIS RESULTS ARRAY OF SIZE NSR
C             VPRE   (R) = VECTOR OF BIAS (U-PLOT) DATA PRIOR TO SORTING
C   LOCAL GLOSSARY
C             STATS  (R) = MODEL STATISTICS RESULTS ARRAY
C             SUMY   (R) = SUM OF THE PREVIOUS Y'S
C             TMP    (R) = INTERMEDIATE CALCULATION
C             TOTY   (R) = SUM OF ALL THE Y'S
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
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              INDX          ,NS            ,NSB
     1                       ,NSE           ,NSR           ,RFLAG
     2                       ,TYP
         DIMENSION            DAT(NS)       ,V(NSR)        ,VPRE(NSR)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I
         DIMENSION            STATS(2)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE  THE ITERATION COUNTER,  STORAGE INDEX,  MODEL ERROR
C        FLAG, RESULTS STATISTIC, AND SUM OF THE Y-PLOT VALUES LOCATION.
         I     = NSB - 1
         INDX  = 0
         RFLAG = 0
         STAT  = 0.0
         TOTY  = 0.0
C
         IF (TYP.EQ.3) THEN
C            INITIALIZE THE CONSTANT PORTION OF THE NOISE CALCULATION.
             TMP = LOG(0.5)
         ENDIF
C
C        DOWHILE (PROCESSING LEFT AND NO ERRORS)
 1000        I = I + 1
             IF ((I.GT.NSE) .OR. (RFLAG.NE.0)) GO TO 1025
C
C            COMPUTE MODEL ESTIMATES.
             CALL SNPTMD
     G                   (DAT   , I
     Y                   ,STATS , RFLAG )
C
C            INCREMENT THE ITERATION COUNTER FOR STORAGE.
             INDX = INDX + 1
C
             IF (RFLAG.EQ.0) THEN
                 IF     (TYP.EQ.1) THEN
C                    ACCURACY ANALYSIS IS DESIRED;  APPEND THE CONTRIBU-
C                    TION OF THE  CURRENT  ITERATION TO  THE PREQUENTIAL
C                    LIKELIHOOD STATISTIC.
                     V(INDX) = 0.0 - (LOG(STATS(2)) + LOG(STATS(1))
     1                       - STATS(2)
     2                       * (EXP(-STATS(1) * DAT(I))
     3                       -  EXP(-STATS(1) * DAT(I+1)))
     4                       - STATS(1) * DAT(I+1))
                     STAT    = STAT + V(INDX)
C
                 ELSEIF ((TYP.EQ.2) .OR. (TYP.EQ.4)) THEN
C                    BIAS OR TREND ANALYSIS IS DESIRED;  STORE THE FUNC-
C                    TIONAL VALUE FOR THIS ELEMENT OF THE U-PLOT.
                     V(INDX) = 1.0 - EXP(-STATS(2)
     1                       * (EXP(-STATS(1) * DAT(I))
     2                       -  EXP(-STATS(1) * DAT(I+1))))
C
                     IF (TYP.EQ.4) THEN
C                        TREND ANALYSIS IS DESIRED; TRANSFORM THE U-PLOT
C                        VALUE TO THE Y-PLOT REPRESENTATION.
                         IF (V(INDX).LT.1.0) THEN
                             V(INDX) = -LOG(1.0 - V(INDX))
                             TOTY    = TOTY + V(INDX)
                         ELSE
                             RFLAG   = 5
                         ENDIF
                     ENDIF
C
                 ELSE
C                    NOISE ANALYSIS IS DESIRED; COMPUTE THE FUNCTION FOR
C                    THE NEXT POINT.
                     MNXT = (LOG(STATS(2)) - STATS(1) * DAT(I)
     1                    - LOG(TMP + STATS(2)
     2                    * EXP(-STATS(1) * DAT(I))))
     3                    / STATS(1)
C
C
C                    PERFORM THE ADDITION OF THE NOISE ON ALL ITERATIONS
C                    EXCEPT THE FIRST; AND THEN PREPARE FOR THE NEXT.
                     IF (I.GT.NSB) THEN
                         STAT = STAT + ABS((MNXT - MCUR) / MCUR)
                     ENDIF
                     MCUR = MNXT
C
                 ENDIF
             ENDIF
             GO TO 1000
 1025        CONTINUE
C        ENDWHILE
C
         IF ((TYP.EQ.2) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE U-PLOT VECTOR  AND COMPUTE
C            THE KOLMOGOROV STATISTIC. FIRST STORE THE UNSORTED DATA.
             DO 1050 I = 1, NSR
                 VPRE(I) = V(I)
 1050            CONTINUE
C            ENDDO
             CALL CMPMAX
     G                   (NSR   , 1
     B                   ,STAT  , V     )
         ENDIF
C
         IF ((TYP.EQ.4) .AND. (RFLAG.EQ.0)) THEN
C            COMPLETE THE COMPUTATION FOR THE Y-PLOT VECTOR.
             SUMY = 0.0
             DO 1075 I = 1, NSR
                 SUMY = SUMY + V(I)
                 V(I) = SUMY / TOTY
 1075            CONTINUE
C            ENDDO
C
C            COMPUTE THE KOLMOGOROV STATISTIC;  NOTE, SORTING IS NOT RE-
C            QUIRED FOR THE Y-PLOT DATA.
             CALL CMPMAX
     G                   (NSR   , 0
     B                   ,STAT  , V     )
         ENDIF
         RETURN
         END
         SUBROUTINE SNPTMD
     G                     (DAT   , NS
     Y                     ,STATS , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SNPTMD
C    G                     (DAT   , NS
C    Y                     ,STATS , RFLAG )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   DEKKER-BRENT IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   DEKKER-BRENT CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE  ESTIMATE IN THE NON-HOMOGENEOUS POISSON MODEL
C        FOR EXECUTION TIME DATA.
C   DESCRIPTION
C        THE ROUTINE  UTILIZES AN ITERATIVE  EXECUTION TO  COMPUTE RELI-
C        ABILITY STATISTICS.
C
C        ONCE THE ITERATIVE EXECUTION IS INITIATED, PROCESSING WILL CON-
C        TINUE UNTIL ONE OF TWO  POSSIBLE  TERMINATION STATES IS REACHED
C        IN THE EXECUTION.  THE LAST ARGUMENT  OF THE CALL LINE  (RFLAG)
C        WILL BE SET TO INDICATE  TO THE CALLING ROUTINE  THE REASON FOR
C        EXIT. THE POSSIBLE REASONS FOR SNPTMD EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS THAN THE  OBSERVED
C                 FAULTS TO DATE
C
C        THE RESULTANT STATISTICS WILL BE RETURNED IN THE STATS ARGUMENT
C        FOR A RFLAG VALUE OF 0 ONLY. THOSE STATISTICS INCLUDE THE ESTI-
C        MATES FOR THE PROPORTIONALITY CONSTANT AND THE TOTAL NUMBER  OF
C        FAULTS.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS  SUBPROGRAM  CONTAINS  CODE WHICH DOES NOT COMPLY WITH THE
C        PROGRAMMING STANDARDS; GROUP LEADER APPROVAL HAS BEEN OBTAINED.
C       -SPECIFICALLY:
C        THE ARGUMENT  FC IN THE ACCESS TO THE ROUTINE ZERO, IS ACTUALLY
C        INITIALIZED IN THE ACCESSED ROUTINE. WHEN ICOUNT HAS A VALUE OF
C        MORE THAN ONE, THE ARGUMENT BECOMES A BOTH (AS MARKED).  HENCE,
C        FC APPEARS UNDEFINED IN THIS ROUTINE'S COMPILATION LISTING.
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             DAT    (R) = TTF DATA VECTOR (WC OR CPU)
C             NS     (I) = SIZE OF DAT
C        YIELDED
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = STATISTICS RESULTS VECTOR
C   LOCAL GLOSSARY
C             A      (R) = LOWER BOUND OF PROPORTIONALITY CONSTANT
C             B      (R) = UPPER BOUND OF PROPORTIONALITY CONSTANT
C             COUNT  (I) = ITERATION COUNTER
C             FA     (R) = VALUE OF FUNCTION AT A
C             FB     (R) = VALUE OF FUNCTION AT B
C             FC     (R) = VALUE OF FUNCTION AT C
C             ICOUNT (I) = STEP COUNTER
C             IFLAG  (I) = CONVERGENCE FLAG
C             SUMTTF (R) = SUM OF TTF ENTRIES
C   ERRORS
C        RFLAG  = 3 : THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C        RFLAG  = 4 : THE  TOTAL  FAULTS  ESTIMATE  WAS  LESS  THAN  THE
C                     OBSERVED FAULTS TO DATE
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
         INTEGER              NS            ,RFLAG
         DIMENSION            DAT(NS)       ,STATS(2)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              COUNT         ,I             ,ICOUNT
     1                       ,IFLAG
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET COUNTER AND THE RETURN STATUS FLAG.
         RFLAG = -1
         COUNT =  0
C
C        ZERO ALL LOCATIONS OF THE RESULTS VECTOR.
         STATS(1) = 0.0
         STATS(2) = 0.0
C
C        CALCULATE THE SUM OF THE TTF DATA.
         SUMTTF = 0.0
         DO 1000 I = 1, NS
             SUMTTF = SUMTTF + DAT(I)
 1000        CONTINUE
C        ENDDO
C
C        COMPUTE THE VALUE OF THE FUNCTION AT ITS LOWER BOUND.
         A  = 1.0 / DAT(NS)
         FA = SNPTML
     G               (A    , NS    , SUMTTF, DAT(NS))
         B  = 2.0 * A
C
C        COMPUTE THE VALUE OF THE FUNCTION AT A POINT WHERE ITS SIGN  IS
C        THE  OPPOSITE  OF THE  COMPUTED  VALUE  OF THE  FUNCTIONS LOWER
C        BOUND.
C
C        DOWHILE (RFLAG .EQ. -1)
 1025        COUNT = COUNT + 1
             FB    = SNPTML
     G                      (B    , NS    , SUMTTF, DAT(NS))
C
C            SET RFLAG, IF CONVERGENCE POSSIBLE.
             IF (FA * FB.LE.0.0) THEN
                 RFLAG = 0
             ENDIF
C
C            SET RFLAG, IF THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
             IF (COUNT.GE.5) THEN
                 RFLAG = 3
             ENDIF
             IF (RFLAG.EQ.-1) THEN
                 B = 2.0 * B
             ENDIF
             IF (RFLAG.EQ.-1) GO TO 1025
C        ENDWHILE
C
         IF (RFLAG.EQ.0) THEN
C            COMPUTE THE  ESTIMATE  OF THE  PROPORTIONALITY CONSTANT AND
C            STORE ITS VALUE IN STATS(1).
             IFLAG  = -1
             ICOUNT =  1
C
C            DOWHILE (IFLAG .EQ. -1)
C                COMPUTE THE ZERO OF THE FUNCTION IN THE INTERVAL  A AND
C                B; NOTE THE ARGUMENT FC IS INITIALIZED BY THE ZERO ROU-
C                TINE WHEN ICOUNT IS SET TO ONE.   <<SEE RESTRICTIONS <<
 1050            CALL ZERO
     B                     (A     , B     , FA    , FB    , FC
     B                     ,ICOUNT
     Y                     ,IFLAG )
C
                 IF (IFLAG.EQ.-1) THEN
C                    COMPUTE THE VALUE OF THE FUNCTION AT ITS NEW  UPPER
C                    BOUND.
                     FB    = SNPTML
     G                              (B    , NS    , SUMTTF, DAT(NS))
                 ENDIF
                 IF (IFLAG.EQ.-1) GO TO 1050
C            ENDWHILE
             STATS(1) = B
C
C            COMPUTE THE ESTIMATE FOR THE TOTAL NUMBER OF FAULTS.
             STATS(2) = REAL(NS) / (1.0 - EXP(-STATS(1) * DAT(NS)))
             IF (STATS(2).LT.REAL(NS)) THEN
C                PREDICTED TOTAL NUMBER OF FAULTS LESS THAN OBSERVED.
                 RFLAG = 4
             ENDIF
         ENDIF
         RETURN
         END
         FUNCTION SNPTML
     G                   (AB    , NS    , SUMTTF, LTTF  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION SNPTML
C    G                   (AB    , NS    , SUMTTF, LTTF  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   DEKKER-BRENT IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   DEKKER-BRENT CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO EVALUATE THE FUNCTION USED BY  THE  MODIFIED  NON-HOMOGENOUS
C        POISSON MODEL, AS ADAPTED FOR THE TIME-BETWEEN-FAILURES DATA.
C   DESCRIPTION
C        THIS ROUTINE CALCULATES THE  VALUE OF THE FUNCTION AT THE POINT
C        AB (ESTIMATE OF THE PROPORTIONALITY CONSTANT).
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
C             AB     (R) = ESTIMATE OF THE PROPORTIONALITY CONSTANT
C             LTTF   (R) = LAST ENTRY IN THE VECTOR
C             NS     (I) = NUMBER OF TTF ENTRIES
C             SUMTTF (R) = SUM OF TTF ENTRIES
C        YIELDED
C             SNPTML (R) = FUNCTION VALUE AT POINT AB
C   LOCAL GLOSSARY
C             EXPI   (R) = INTERMEDIATE EXPONENT CALCULATION
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
         INTEGER              NS
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
C        COMPUTE THE FUNCTION VALUE.
         EXPI   = EXP(-AB * LTTF)
         SNPTML = (REAL(NS) / AB) - SUMTTF - ((REAL(NS) * LTTF * EXPI)
     1          / (1.0 - EXPI))
         RETURN
         END
         SUBROUTINE SNPTPR
     G                     (NPV   , NS    , DAT   , PCON  , TNOF
     G                     ,SOT   , SR
     Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SNPTPR
C    G                     (NPV   , NS    , DAT   , PCON  , TNOF
C    G                     ,SOT   , SR
C    Y                     ,PDAT  , DVAL  , DFLG  , V     )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO COMPUTE THE ESTIMATE OF THE RELIABILITY OF THE PROGRAM FOR A
C        SPECIFIED OPERATIONAL TIME,  THE ESTIMATE OF THE REQUIRED TEST-
C        ING TIME TO ACHIEVE A SPECIFIED RELIABILITY FOR A SPECIFIED OP-
C        ERATIONAL TIME, AND TO CREATE THE PREDICTED DATA VECTOR FOR THE
C        NON-HOMOGENEOUS POISSON  EXECUTION TIME MODEL.  DURING THE PRE-
C        DICTED VECTOR CREATION, THIS ROUTINE WILL ALSO COMPUTE THE KOL-
C        MOGOROV DISTANCE.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE SNPTPR  CALL LINE  INDICATES WHETHER
C        ONE OF THE  FUTURE ESTIMATES OR THE  ENTIRE TBF VECTOR IS TO BE
C        COMPUTED. IF NPV IS SET TO NS THEN THE ROUTINE WILL COMPUTE THE
C        ENTIRE DATA VECTOR OF  PREDICTED  TBFS, USING THE ARGUMENTS NS,
C        DAT, PCON, AND TNOF. IF NPV IS SET TO A ONE, ONE OF THE TWO FU-
C        TURE ESTIMATES IS DESIRED;  AND THE ROUTINE USES A SECOND ARGU-
C        MENT (SR) TO DETERMINE WHETHER THE RELIABILITY OR TIME ESTIMATE
C        IS DESIRED. IF SR HAS BEEN SET TO CONTAIN A REALISTIC SPECIFIED
C        RELIABILITY, THEN THE ROUTINE WILL COMPUTE THE REQUIRED TESTING
C        TIME ESTIMATE, USING THE ARGUMENTS NS, DAT, PCON, TNOF, SR, AND
C        SOT.  OTHERWISE,  IF SR IS SET  TO AN  UNREALISTIC  RELIABILITY
C        (I.E., -1), THEN THE RELIABILITY  OF THE PROGRAM FOR THE SPECI-
C        FIED  OPERATIONAL  TIME WILL  BE MADE,  USING THE ARGUMENTS NS,
C        DAT, PCON, TNOF, AND SOT.
C
C        THE FUTURE ESTIMATES  (EITHER RELIABILITY OR TESTING TIME) WILL
C        BE RETURNED IN THE FIRST  ELEMENT OF THE PDAT ARGUMENT.  IF THE
C        PREDICTED TBFS  WERE PROCESSED  THEY  WILL BE  RETURNED  IN THE
C        FIRST NS LOCATIONS OF PDAT.
C
C        THE CALCULATION FOR THE  KOLMOGOROV STATISTIC  WAS ADDED DURING
C        THE UPDATE THE REVISION 5. FOR THAT CALCULATION, IN ADDITION TO
C        THE MODEL ESTIMATES, THE ACTUAL DATA VECTOR IS NEEDED. ONCE THE
C        NS ELEMENTS ARE COMPUTED, USING THE SAME EQUATION AS FOR A BIAS
C        ANALYSIS OF THIS MODEL, THE MAXIMUM DIFFERENCE IS DETERMINE VIA
C        THE CMPMAX ROUTINE, AND THE SIGNIFICANCE OF THE VALUE IS DETER-
C        MINED VIA THE CHKGOF ROUTINE (BOTH OF WHICH ARE RESIDENT IN THE
C        SMERFS5 LIBRARY OF SUPPORT ROUTINES).
C
C        NOTE, IN AN EFFORT TO RETAIN THE  MODULARITY OF THE PROCESSING,
C        NO ATTEMPT WAS MADE TO REUSE INTERMEDIATE CALCULATIONS FROM THE
C        ORIGINAL PREDICTION ROUTINE (I.E., DUPLICATE PROCESSING FOR IN-
C        TERMEDIATE VALUES MAY APPEAR IN THIS ROUTINE).
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
C             DAT    (R) = TTF VECTOR (WC OR CPU)
C             NPV    (I) = NUMBER OF PREDICTED VALUES DESIRED
C             NS     (I) = SIZE OF DAT
C             PCON   (R) = ESTIMATE OF THE PROPORTIONALITY CONSTANT
C             SOT    (R) = SPECIFIED OPERATIONAL TIME
C             SR     (R) = SPECIFIED RELIABILITY
C             TNOF   (R) = ESTIMATE OF THE TOTAL NUMBER OF FAULTS
C        YIELDED
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL, WHERE:
C                          1 - THE VALUE OF D IS LARGER  THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY NOT PROVIDE AN
C                              ADEQUATE FIT.
C                          2 - THE VALUE OF D IS SMALLER THAN THE ALLOW-
C                              ED DISTANCE. THE MODEL MAY PROVIDE AN AD-
C                              EQUATE FIT.
C             DVAL   (R) = CALCULATED KOLMOGOROV DISTANCE
C             PDAT   (R) = VECTOR OF PREDICTED DATA VALUE(S)
C             V      (R) = VECTOR TO CONTAIN THE U-PLOT DATA SET
C   LOCAL GLOSSARY
C             TMP    (R) = INTERMEDIATE CALCULATION
C             YPREV  (R) = INTERMEDIATE VALUE  IN PREDICTED  TBF  VECTOR
C                          PROCESSING
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
         INTEGER              DFLG          ,NPV           ,NS
         DIMENSION            DAT(NS)       ,PDAT(NPV)     ,V(NS)
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
         IF (NPV.EQ.1) THEN
             IF (SR.EQ.-1.0) THEN
C                COMPUTE THE  RELIABILITY OF  THE PROGRAM FOR THE SPECI-
C                FIED OPERATIONAL TIME.
                 TMP     = EXP(-PCON * DAT(NS))
     1                   - EXP(-PCON * (DAT(NS) + SOT))
                 PDAT(1) = EXP(-TNOF * TMP)
             ELSE
C                COMPUTE THE  REQUIRED TESTING  TIME TO REACH  THE INPUT
C                RELIABILITY FOR THE SPECIFIED OPERATIONAL TIME.
                 TMP     = TNOF * (1.0 - EXP(-PCON * SOT))
                 PDAT(1) = (LOG(TMP) - LOG(LOG(1.0 / SR))) / PCON
             ENDIF
         ELSE
C            INITIALIZE THE KOLMOGOROV DISTANCE TO ZERO.
             DVAL  = 0.0
C
C            COMPUTE THE ESTIMATES FOR THE ENTIRE TBF DATA VECTOR.
             YPREV = 0.0
             DO 1000 I = 1, NS
                 TMP     = DAT(I) * REAL(I)
     1                   / (TNOF * (1.0 - EXP(-PCON * DAT(I))))
                 PDAT(I) = TMP - YPREV
                 YPREV   = TMP
C
C                COMPUTE THE CONTRIBUTION FOR THIS ELEMENT.
                 IF (I.EQ.1) THEN
                     V(I) = 1.0 - EXP(-TNOF
     1                    * (1.0
     2                    -  EXP(-PCON * DAT(I))))
                 ELSE
                     V(I) = 1.0 - EXP(-TNOF
     1                    * (EXP(-PCON * DAT(I-1))
     2                    -  EXP(-PCON * DAT(I))))
                 ENDIF
 1000            CONTINUE
C            ENDDO
C
C            COMPLETE THE COMPUTATION  FOR THE U-PLOT VECTOR;  NOTE, THE
C            VECTOR OF U-PLOT DATA IS SORTED UPON RETURN.
             CALL CMPMAX
     G                   (NS    , 1
     B                   ,DVAL  , V     )
C
C            DETERMINE IF THE CALCULATED DISTANCE IS SIGNIFICANT.
             CALL CHKGOF
     G                   (NS    , DVAL
     Y                   ,DFLG  )
         ENDIF
         RETURN
         END
