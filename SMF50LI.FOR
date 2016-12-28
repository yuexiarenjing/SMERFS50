         SUBROUTINE SBAMFN
     G                     (ESF   , CDAT  , FDAT  , LDAT  , MDAT  , NS
     G                     ,X
     Y                     ,FX    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SBAMFN
C    G                     (ESF   , CDAT  , FDAT  , LDAT  , MDAT  , NS
C    G                     ,X
C    Y                     ,FX    )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE  THE  LIKELIHOOD  FUNCTION  FOR X  FOR  EITHER THE
C        BROOKS AND MOTLEY BINOMIAL OR POISSON MODEL.
C   DESCRIPTION
C        IF  ESF IS SET TO 1,  THE LIKELIHOOD  FUNCTION  FOR  BROOKS AND
C        MOTLEY BINOMIAL  MODEL IS CALCULATED.  IF ESF IS SET TO 2,  THE
C        LIKELIHOOD  FUNCTION  FOR  BROOKS AND  MOTLEY POISSON  MODEL IS
C        CALCULATED.  FOR EITHER MODEL, THE RESULT IS STORED IN FX.
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             FDAT   (R) = FRACTIONAL PART OF CODE TEST DATA VECTOR
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             MDAT   (R) = PREVIOUS CUMULATIVE FAULT DATA VECTOR
C             NS     (I) = SIZE OF CDAT, FDAT, LDAT AND MDAT
C             X      (R) = PARAMETER ESTIMATE VALUES
C        YIELDED
C             FX     (R) = MINIMIZED FUNCTION VALUE
C   LOCAL GLOSSARY
C             SM     (R) = TEMPORARY SUMMATION
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
         INTEGER              ESF           ,NS
         DIMENSION            CDAT(NS)      ,FDAT(NS)      ,LDAT(NS)
     1                       ,MDAT(NS)      ,X(3)
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
C        INITIALIZE THE MINIMIZED FUNCTION VALUE.
         FX = 0.0
C
C        CALCULATE THE MINIMIZED FUNCTION VALUE FOR THE  BROOKS AND MOT-
C        LEY BINOMIAL MODEL OR THE BROOKS AND MOTLEY POISSON MODEL.
         DO 1025 I = 1, NS
             TMP1 =     1.0 - (1.0 - X(2))**LDAT(I)
             TMP2 = FDAT(I) * X(1) - X(3) * MDAT(I)
             IF (ESF.EQ.1) THEN
                 SM = 0.0
                 DO 1000 J = 1, INT(CDAT(I))
                     SM = SM + LOG(TMP2 - REAL(J) + 1.0)
 1000                CONTINUE
C                ENDDO
                 FX = FX + SM + CDAT(I) * LOG(TMP1) + (TMP2 - CDAT(I))
     1              * LDAT(I) * LOG(1.0 - X(2))
             ELSE
                 FX = FX - TMP2  * TMP1 + CDAT(I)
     1              * (LOG(TMP2) + LOG(TMP1))
             ENDIF
 1025        CONTINUE
C        ENDDO
         FX = -FX
         RETURN
         END
         SUBROUTINE SBAMGD
     G                     (ESF   , CDAT  , FDAT  , LDAT  , MDAT
     G                     ,NS    , X
     Y                     ,G     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SBAMGD
C    G                     (ESF   , CDAT  , FDAT  , LDAT  , MDAT
C    G                     ,NS    , X
C    Y                     ,G     )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE GRADIENT OF THE LIKELIHOOD FUNCTION FOR X  FOR
C        EITHER THE BROOKS AND MOTLEY BINOMIAL OR POISSON MODEL.
C   DESCRIPTION
C        IF ESF IS SET TO 1, THE GRADIENT OF THE LIKELIHOOD FUNCTION FOR
C        BROOKS  AND  MOTLEY BINOMIAL MODEL IS  CALCULATED AND IF ESF IS
C        SET TO 2, THE  GRADIENT OF THE  LIKELIHOOD  FUNCTION FOR BROOKS
C        AND MOTLEY  POISSON MODEL IS CALCULATED.  FOR EITHER MODEL, THE
C        RESULTING GRADIENT VALUES ARE STORED IN G.
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             FDAT   (R) = FRACTIONAL PART OF CODE TEST DATA VECTOR
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             MDAT   (R) = PREVIOUS CUMULATIVE FAULT DATA VECTOR
C             NS     (I) = SIZE OF CDAT, FDAT, LDAT AND MDAT
C             X      (R) = PARAMETER ESTIMATE VALUES
C        YIELDED
C             G      (R) = GRADIENT VALUES FOR THE FUNCTION
C   LOCAL GLOSSARY
C             SM     (R) = TEMPORARY SUMMATION
C             TMP1   (R) = INTERMEDIATE CALCULATION 1
C             TMP2   (R) = INTERMEDIATE CALCULATION 2
C             TMP3   (R) = INTERMEDIATE CALCULATION 3
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
         INTEGER              ESF           ,NS
         DIMENSION            CDAT(NS)      ,FDAT(NS)      ,G(3)
     1                       ,LDAT(NS)      ,MDAT(NS)      ,X(3)
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
C        INITIALIZE THE GRADIENT VALUES.
         G(1) = 0.0
         G(2) = 0.0
C
C        CALCULATE THE  GRADIENT  VALUES OF THE  LIKELIHOOD FUNCTION FOR
C        BROOKS AND MOTLEY BINOMIAL OR POISSON MODEL.
         DO 1025 I = 1, NS
             TMP1 =     1.0 - (1.0 - X(2))**LDAT(I)
             TMP2 = FDAT(I) * X(1) - X(3) * MDAT(I)
             IF (ESF.EQ.1) THEN
                 SM = 0.0
                 DO 1000 J = 1, INT(CDAT(I))
                     SM = SM + 1.0 / (TMP2 - REAL(J) + 1.0)
 1000                CONTINUE
C                ENDDO
                 G(1) = G(1) + FDAT(I)
     1                * (SM  + LDAT(I) * LOG(1.0 - X(2)))
                 G(2) = G(2) + CDAT(I) * LDAT(I) / TMP1 - TMP2
     1                * LDAT(I)
             ELSE
                 TMP3 = CDAT(I) / TMP2 - TMP1
                 G(1) = G(1) + FDAT(I) * TMP3
                 G(2) = G(2) + (TMP2   - CDAT(I) / TMP1) * LDAT(I)
     1                * (1.0 - X(2))**(LDAT(I)   - 1.0)
             ENDIF
 1025        CONTINUE
C        ENDDO
C
         G(1) = -G(1)
         IF (ESF.EQ.1) THEN
             G(2) = -G(2) / (1.0 - X(2))
         ENDIF
         RETURN
         END
         SUBROUTINE SBAMHS
     G                     (ESF   , CDAT  , FDAT  , LDAT  , MDAT
     G                     ,N     , NS    , X
     Y                     ,H     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SBAMHS
C    G                     (ESF   , CDAT  , FDAT  , LDAT  , MDAT
C    G                     ,N     , NS    , X
C    Y                     ,H     )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE  HESSIAN  OF THE LIKELIHOOD FUNCTION FOR X FOR
C        EITHER THE BROOKS AND MOTLEY BINOMIAL OR POISSON MODEL.
C   DESCRIPTION
C        IF ESF IS SET TO 1, THE HESSIAN  OF THE LIKELIHOOD FUNCTION FOR
C        BROOKS  AND  MOTLEY BINOMIAL MODEL IS  CALCULATED AND IF ESF IS
C        SET TO 2, THE  HESSIAN  OF THE  LIKELIHOOD  FUNCTION FOR BROOKS
C        AND MOTLEY POISSON MODEL IS  CALCULATED.  FOR EITHER MODEL, THE
C        RESULTING HESSIAN VALUES ARE STORED IN H.
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             FDAT   (R) = FRACTIONAL PART OF CODE TEST DATA VECTOR
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             MDAT   (R) = PREVIOUS CUMULATIVE FAULT DATA VECTOR
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C             NS     (I) = SIZE OF CDAT, FDAT, LDAT AND MDAT
C             X      (R) = PARAMETER ESTIMATE VALUES
C        YIELDED
C             H      (R) = HESSIAN VALUES FOR THE FUNCTION
C   LOCAL GLOSSARY
C             D      (R) = EIGENVALUES OF THE HESSIAN
C             DUM1   (R) = DUMMY STORAGE ARRAY 1 FOR EGNVAL CALL
C             DUM2   (R) = DUMMY STORAGE ARRAY 2 FOR EGNVAL CALL
C             EMIN   (R) = VALUE TO BE ADDED TO THE DIAGONAL ELEMENTS OF
C                          THE HESSIAN WHEN THE HESSIAN IS  NOT POSITIVE
C                          DEFINITE.
C             HTMP   (R) = INTERMEDIATE HESSIAN STORAGE FOR EGNVAL CALL
C             SM     (R) = TEMPORARY SUMMATION
C             TMP1   (R) = INTERMEDIATE CALCULATION 1
C             TMP2   (R) = INTERMEDIATE CALCULATION 2
C             TMP3   (R) = INTERMEDIATE CALCULATION 3
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
         DIMENSION            CDAT(NS)      ,FDAT(NS)      ,H(3,3)
     1                       ,LDAT(NS)      ,MDAT(NS)      ,X(3)
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
         DO 1025 I = 1, N
             DO 1000 J = 1, N
                 H(I,J) = 0.0
 1000            CONTINUE
C            ENDDO
 1025        CONTINUE
C        ENDDO
C
C        CALCULATE THE  HESSIAN VALUES OF  THE  LIKELIHOOD  FUNCTION FOR
C        BROOKS AND MOTLEY BINOMIAL OR POISSON MODEL.
         DO 1075 I = 1, NS
             TMP1 = 1.0     - (1.0 - X(2))**LDAT(I)
             TMP2 = FDAT(I) * X(1) - X(3) * MDAT(I)
             IF (ESF.EQ.1) THEN
                 SM = 0.0
                 DO 1050 J = 1, INT(CDAT(I))
                     SM = SM + 1.0 / (TMP2 - REAL(J) + 1.0)**2
 1050                CONTINUE
C                ENDDO
                 H(1,1) = H(1,1) + SM      * FDAT(I)**2
                 H(1,2) = H(1,2) + LDAT(I) * FDAT(I)
                 H(2,2) = H(2,2) + TMP2    * LDAT(I) + CDAT(I)
     1                  * LDAT(I)**2       * (1.0    - X(2))**LDAT(I)
     2                  / TMP1   **2       - CDAT(I) * LDAT(I)
     3                  / TMP1
             ELSE
                 TMP3   = LDAT(I) *    (1.0 - X(2))**(LDAT(I) - 1.0)
                 H(1,1) = H(1,1)  + CDAT(I) * (FDAT(I) / TMP2)**2
                 H(1,2) = H(1,2)  + FDAT(I) * TMP3
                 H(2,2) = H(2,2)  + CDAT(I) * (TMP3 / TMP1)**2
     1                  - (TMP2   - CDAT(I) / TMP1) * (LDAT(I) - 1.0)
     2                  *  TMP3   /    (1.0 - X(2))
             ENDIF
 1075        CONTINUE
C        ENDDO
C
         IF (ESF.EQ.1) THEN
             H(1,2) = H(1,2) / (1.0 - X(2))
             H(2,2) = H(2,2) / (1.0 - X(2))**2
         ENDIF
C
C        STORE THE SYMMETRIC HESSIAN VALUES.
         H(2,1) = H(1,2)
C
C        COMPUTE THE EIGENVALUES OF THE HESSIAN.
         DO 1125 I = 1, N
             DO 1100 J = 1, N
                 HTMP(I,J) = H(I,J)
 1100            CONTINUE
C            ENDDO
 1125        CONTINUE
C        ENDDO
         CALL EGNVAL
     G           (3      , N     , HTMP  , DUM1  , DUM2
     Y           ,D      )
C
C        WHEN THE HESSIAN IS NOT POSITIVE DEFINITE, REPLACE THE DIAGONAL
C        ELEMENTS OF THE HESSIAN WITH THE HESSIAN VALUE PLUS EMIN.  THIS
C        FORCES THE HESSIAN TO BE POSITIVE DEFINITE.
         IF (D(1).LE.0.0) THEN
             EMIN = ABS(D(1)) + D(N) * 0.0001
             DO 1150 I = 1, N
                 H(I,I) = H(I,I) + EMIN
 1150            CONTINUE
C            ENDDO
         ENDIF
         RETURN
         END
         SUBROUTINE SBAMMA
     G                     (CDAT  , LDAT  , FDAT  , MDAT  , NS    , NSB
     G                     ,NSE   , NSR   , BOPFLG, MAXIC , PEC
     Y                     ,STAT  , RFLAG , INDX  , V     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SBAMMA
C    G                     (CDAT  , LDAT  , FDAT  , MDAT  , NS    , NSB
C    G                     ,NSE   , NSR   , BOPFLG, MAXIC , PEC
C    Y                     ,STAT  , RFLAG , INDX  , V     )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM THE  PREQUENTIAL LIKELIHOOD ANALYSIS  FOR THE BROOKS
C        AND MOTLEY DISCRETE SOFTWARE  RELIABILITY MODELS FOR A SOFTWARE
C        SYSTEM.
C   DESCRIPTION
C        THE ROUTINE  CONTROLS THE EXECUTIONS  OF THE  APPLICABLE SMFLIB
C        ROUTINE, SBAMMD.  THE PROCESSING, AS DEFINED  IN THE LITTLEWOOD
C        PAPER (SEE REFERENCES IN THE MAIN PROGRAM) BASICALLY INVOLVES A
C        SERIES OF EXECUTIONS WHERE  NSB TO NSE DATA POINTS ARE UTILIZED
C        IN THE MODEL.
C
C        FOR EACH ITERATION,  THE MODEL PARAMETERS (ESTIMATES)  FROM THE
C        CURRENT ITERATION ARE USED IN CONJUNCTION WITH THE OBSERVED DA-
C        TA FROM THE NEXT ITERATION POINT  TO DETERMINE AN OVER-ALL PIC-
C        TURE OF THE APPLICABILITY OF THE MODEL  FOR THE PARTICULAR DATA
C        SET.
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
C             BOPFLG (I) = BINOMIAL OR POISSON MODEL
C             CDAT   (R) = ARRAY TO HOLD OBSERVED FAULT COUNT DATA
C             FDAT   (R) = ARRAY TO HOLD FRACTION OF PROGRAM TESTED
C             LDAT   (R) = ARRAY TO HOLD INTERVAL TESTING LENGTHS
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS
C             MDAT   (R) = ARRAY TO HOLD CUMULATIVE FAULT CORRECTIONS
C             NS     (I) = THE NUMBER OF ENTRIES
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C             PEC    (R) = PROBABILITY OF CORRECTING FAULTS  WITHOUT IN-
C                          TRODUCING NEW FAULTS
C        YIELDED
C             INDX   (I) = ANALYSIS ITERATION COUNTER (AT RETURN)
C             RFLAG  (I) = MODEL RESULTS ERROR FLAG
C             STAT   (R) = PREQUENTIAL LIKELIHOOD STATISTIC
C             V      (R) = ANALYSIS RESULTS ARRAY OF SIZE NSR
C   LOCAL GLOSSARY
C             COUNT  (I) = ITERATION COUNTER
C             LOGFN  (R) = LOG(N(I))!
C             LOGFNX (R) = LOG(N(I)-X(I))!
C             LOGFX  (R) = LOG(X(I))!
C             PED    (R) = PROB. OF DETECTING FAULTS INITIAL ESTIMATE
C             STATS  (R) = STATISTICS RESULTS VECTOR
C             TMP1   (R) = INTERMEDIATE CALCULATION 1
C             TMP2   (R) = INTERMEDIATE CALCULATION 2
C             TNOF   (R) = TOTAL NUMBER OF FAULTS INITIAL ESTIMATE
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
         INTEGER              BOPFLG        ,INDX          ,NS
     1                       ,NSB           ,NSE           ,NSR
     2                       ,MAXIC         ,RFLAG
         DIMENSION            CDAT(NS)      ,FDAT(NS)      ,LDAT(NS)
     1                       ,MDAT(NS)      ,V(NSR)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              COUNT         ,I             ,J
     1                       ,K
         DIMENSION            STATS(3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE THE P.L. ITERATION COUNTER, STORAGE INDEX, THE MODEL
C        ERROR FLAG, AND THE P.L. STATISTIC.
         I      = NSB - 1
         INDX   = 0
         RFLAG  = 0
         STAT   = 0.0
C
C        DOWHILE (PROCESSING LEFT AND NO ERRORS)
 1000        I  = I + 1
             IF ((I.GT.NSE) .OR. (RFLAG.NE.0)) GO TO 1150
C
C            COMPUTE THE INITIAL ESTIMATES OF THE PROBABILITY OF DETECT-
C            ING FAULTS AND THE TOTAL NUMBER OF FAULTS.
             PED  = 0.0
             TNOF = 0.0
             DO 1025 J = 1, I
                 PED = PED + CDAT(J) / ((CDAT(I) + MDAT(I))
     2               * LDAT(J) * FDAT(J))
                 IF (TNOF.LT.(MDAT(J) + CDAT(J)) / FDAT(J)) THEN
                     TNOF =  (MDAT(J) + CDAT(J)) / FDAT(J)
                 ENDIF
 1025            CONTINUE
C            ENDDO
             PED  = PED  / REAL(I)
             TNOF = TNOF + 1.0
C
C            COMPUTE MODEL ESTIMATES.
             CALL SBAMMD
     G                   (BOPFLG, MAXIC , I     , PEC   , PED
     G                   ,TNOF  , FDAT  , MDAT  , CDAT  , LDAT
     Y                   ,STATS , COUNT , RFLAG)
C
C            INCREMENT THE ITERATION COUNTER FOR STORAGE.
             INDX = INDX + 1
C
             IF (RFLAG.EQ.0) THEN
                 TMP1 = STATS(1) - (STATS(3) * MDAT(I))
                 TMP2 = 1.0 - (1.0 - STATS(2))**LDAT(I+1)
C
                 IF (BOPFLG.EQ.1) THEN
C                    CALCULATE THE SUMS FOR THE  LOG(N(I))!, LOG(X(I))!,
C                    AND LOG(N(I)-X(I))!.
                     K      = INT(TMP1)
                     LOGFN  = 0.0
                     DO 1050 J = 1, K
                         LOGFN  = LOGFN  + LOG(REAL(J))
 1050                    CONTINUE
C                    ENDDO
                     K      = INT(CDAT(I+1))
                     LOGFX  = 0.0
                     DO 1075 J = 1, K
                         LOGFX  = LOGFX  + LOG(REAL(J))
 1075                    CONTINUE
C                    ENDDO
                     K      = INT(TMP1 - CDAT(I+1))
                     LOGFNX = 0.0
                     DO 1100 J = 1, K
                         LOGFNX = LOGFNX + LOG(REAL(J))
 1100                    CONTINUE
C                    ENDDO
C
                     V(INDX) = 0.0
     1                       - (LOGFN - LOGFX - LOGFNX
     2                       +  CDAT(I+1) * LOG(TMP2)
     3                       + (TMP1 - CDAT(I+1)) * LOG(1.0 - TMP2))
                     STAT    = STAT + V(INDX)
                 ELSE
C                    CALCULATE THE SUM FOR THE LOG(X(I))!.
                     K      = INT(CDAT(I+1))
                     LOGFX  = 0.0
                     DO 1125 J = 1, K
                         LOGFX  = LOGFX  + LOG(REAL(J))
 1125                    CONTINUE
C                    ENDDO
C
                     V(INDX) = 0.0
     1                       - (CDAT(I+1) * LOG(TMP1)
     2                       +  CDAT(I+1) * LOG(TMP2)
     3                       -  TMP1      * TMP2
     4                       -  LOGFX)
                     STAT    = STAT + V(INDX)
                 ENDIF
             ENDIF
             GO TO 1000
 1150        CONTINUE
C        ENDWHILE
         RETURN
         END
         SUBROUTINE SBAMMD
     G                     (ESF   , MAXIC , NS    , PEC   , PED
     G                     ,TNOF  , FDAT  , MDAT  , CDAT  , LDAT
     Y                     ,STATS , COUNT , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SBAMMD
C    G                     (ESF   , MAXIC , NS    , PEC   , PED
C    G                     ,TNOF  , FDAT  , MDAT  , CDAT  , LDAT
C    Y                     ,STATS , COUNT , RFLAG )
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
C        TO CALCULATE THE  ESTIMATES USING THE BINOMIAL AND POISSON MOD-
C        ELS OF THE BROOKS AND MOTLEY DISCRETE SOFTWARE RELIABILITY MOD-
C        EL FOR A SOFTWARE  SYSTEM  USING THE TRUST  REGION MINIMIZATION
C        ALGORITHM.
C   DESCRIPTION
C        THE SBAMMD ROUTINE  CAN BE EXECUTED UNDER TWO  DIFFERENT MODES.
C        THE FIRST ARGUMENT OF THE CALL LINE  (ESF) WILL BE  USED BY THE
C        ROUTINE TO DETERMINE WHETHER THE BROOKS AND MOTLEY  BINOMIAL OR
C        POISSON MODEL IS DESIRED.
C
C        ONCE THE ITERATIVE EXECUTION IS INITIATED, PROCESSING WILL CON-
C        TINUE UNTIL ONE OF THREE  POSSIBLE TERMINATION STATES IS REACH-
C        ED.  THE LAST ARGUMENT OF THE CALL LINE  (RFLAG) WILL BE SET TO
C        INDICATE TO THE CALLING ROUTINE  THE REASON FOR EXIT.  THE POS-
C        SIBLE REASONS FOR SBAMMD EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             1 - THE MAXIMUM ITERATION COUNT WAS REACHED
C             2 - THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS  THAN THE OBSERVED
C                 FAULTS TO DATE
C
C        THE NUMBER OF ITERATIONS  NEEDED FOR THE SUCCESSFUL CONVERGENCE
C        WILL BE RETURNED IN THE NEXT-TO-LAST ARGUMENT (COUNT).
C
C        THE RESULTANT STATISTICS WILL BE RETURNED IN THE STATS ARGUMENT
C        FOR RFLAG VALUES  OF 0 AND 1 ONLY.
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             FDAT   (R) = FRACTIONAL PART OF CODE TEST DATA VECTOR
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS
C             MDAT   (R) = PREVIOUS CUMULATIVE FAULT DATA VECTOR
C             NS     (I) = SIZE OF CDAT, FDAT, LDAT, AND MDAT
C             PEC    (R) = PROBABILITY  OF CORRECTING FAULTS WITHOUT IN-
C                          TRODUCING NEW FAULTS (SPECIFIED VALUE)
C             PED    (R) = PROBABILITY  OF DETECTING  FAULTS INITIAL ES-
C                          TIMATE
C             TNOF   (R) = TOTAL NUMBER OF FAULTS INITIAL ESTIMATE
C        YIELDED
C             COUNT  (I) = ITERATION COUNTER
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = RESULTS VECTOR
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
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C             NDIFF  (R) = FUNCTION VALUE AT DIFFERENCE
C             NEWSTP (R) = NEWTON STEP VALUE
C             NGRAD  (R) = GRADIENT VALUE AT DIFFERENCE
C             R      (R) = TRUST REGION RADIUS
C             RNEW   (R) = TRUST REGION RADIUS FACTOR FOR 1ST REDUCTION
C             RTMP   (R) = INTERMEDIATE TRUST REGION RADIUS
C             S      (R) = DIAGONALS OF THE HESSIAN INVERSE
C             TAU    (R) = TRUST REGION RADIUS TAU VALUE
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
C        RFLAG  = 1 : THE MAXIMUM ITERATION COUNT WAS REACHED
C        RFLAG  = 2 : THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY
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
         INTEGER              COUNT         ,ESF           ,MAXIC
     1                       ,NS            ,RFLAG
         DIMENSION            CDAT(NS)      ,FDAT(NS)      ,LDAT(NS)
     1                       ,MDAT(NS)      ,STATS(3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,IRED          ,J
     1                       ,N
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
C        SET THE COUNTER, RETURN STATUS FLAG, FUNCTION VALUE DIFFERENCE,
C        GRADIENT VALUE DIFFERENCE, AND NUMBER OF UNKNOWN PARAMETERS.
         COUNT =  1
         RFLAG = -1
         EPSD  = 1.0E-4
         EPSG  = 1.0E-4
         N     = 2
C
C        SET THE UPPER AND LOWER BOUNDS FOR THE SOLUTION.
         XMAX(1) = 1.0E10
         XMAX(2) = 0.999
         XMIN(1) = 0.0
         DO 1000 I = 1, NS
             IF (XMIN(1).LT.(MDAT(I) + CDAT(I)) / FDAT(I)) THEN
                 XMIN(1) =  (MDAT(I) + CDAT(I)) / FDAT(I)
             ENDIF
 1000        CONTINUE
C        ENDDO
         XMIN(1) = XMIN(1) + 0.001
         XMIN(2) = 1.0E-10
C
C        SET THE INITIAL GUESSES FOR THE PARAMETER ESTIMATES.
         XP(1) = TNOF
         XP(2) = PED
         IF (XP(1).LT.XMIN(1)) THEN
             XP(1) = XMIN(1)
         ELSE
             IF (XP(1).GT.XMAX(1)) THEN
                 XP(1) = XMAX(1)
             ENDIF
         ENDIF
         IF (XP(2).LT.XMIN(2)) THEN
             XP(2) = XMIN(2)
         ELSE
             IF (XP(2).GT.XMAX(2)) THEN
                 XP(2) = XMAX(2)
             ENDIF
         ENDIF
C
C        SET THE SPECIFIED VALUE FOR  PROBABILITY OF  CORRECTING  FAULTS
C        WITHOUT INTRODUCING NEW FAULTS.
         XP(3) = PEC
         Y(3)  = PEC
C
C        INITIALIZE THE TRUST REGION.
         CALL SBAMFN
     G               (ESF   , CDAT  , FDAT  , LDAT  , MDAT  , NS    , XP
     Y               ,FX    )
         CALL SBAMGD
     G               (ESF   , CDAT  , FDAT  , LDAT  , MDAT  , NS    , XP
     Y               ,G     )
         CALL SBAMHS
     G               (ESF   , CDAT  , FDAT  , LDAT  , MDAT  , N     , NS
     G               ,XP
     Y               ,H     )
C
C        COMPUTE THE NEWTON STEP.
         CALL FACTOR
     G               (H     , N
     Y               ,U     )
         CALL SOLVE
     G               (N     , U     , G
     Y               ,NEWSTP)
C
C        COMPUTE THE DIAGONALS OF H INVERSE.
         DO 1050 I = 1, N
             DO 1025 J = 1, N
                 Y(J) = 0.0
 1025            CONTINUE
C            ENDDO
             Y(I) = 1.0
             CALL SOLVE
     G                  (N     , U     , Y
     Y                  ,Y     )
             S(I) = SQRT(Y(I))
 1050        CONTINUE
C        ENDDO
C
C        COMPUTE THE TRUST REGION RADIUS.
         R = 0.0
         DO 1075 I = 1, N
             RTMP = ABS(NEWSTP(I) / S(I))
             R    = MAX(R,RTMP)
 1075        CONTINUE
C        ENDDO
         R = 0.5 * R
C
C        DOWHILE (RFLAG .EQ. -1)
 1100        IRED = 0
             IF (COUNT.NE.1) THEN
                 CALL SBAMHS
     G                       (ESF   , CDAT  , FDAT  , LDAT  , MDAT
     G                       ,N     , NS    , XP
     Y                       ,H     )
             ENDIF
C
C            COMPUTE THE MINIMUM WITHIN THE TRUST REGION OF THE QUADRAT-
C            IC MODEL.
C
C            DOWHILE ((IRED .LE. 10) .AND. (FY .GT. FUNUP))
 1125            CONTINUE
C
C                SET UP THE TRUST REGION BOUNDARIES.
                 DO 1150 I = 1, N
                     TMPMI   = XP(I) - R * S(I)
                     TMPMX   = XP(I) + R * S(I)
                     TMIN(I) = MAX(XMIN(I),TMPMI)
                     TMAX(I) = MIN(XMAX(I),TMPMX)
 1150                CONTINUE
C                ENDDO
C
C                COMPUTE THE MINIMUM WITHIN THE TRUST REGION.
                 CALL CONMIN
     G                       (G     , H     , N    , TMAX  , TMIN  , XP
     Y                       ,Y     )
                 CALL SBAMFN
     G                       (ESF   , CDAT  , FDAT , LDAT  , MDAT  , NS
     G                       ,Y
     Y                       ,FY    )
C
C                CHECK WHETHER THE FUNCTION HAS DECREASED SUFFICIENTLY.
                 GPROD = 0.0
                 DO 1175 I = 1, N
                     GPROD = GPROD + G(I) * (Y(I) - XP(I))
 1175                CONTINUE
C                ENDDO
                 FUNUP = FX + 0.0001 * GPROD
C
                 IF (FY.GT.FUNUP) THEN
C                    REDUCE THE TRUST REGION AND TRY AGAIN.
                     IRED = IRED + 1
C
                     IF (IRED.EQ.1) THEN
C                        MAKE  SURE  THE  FIRST  REDUCTION IS ENOUGH  TO
C                        CAUSE A CHANGE IN THE CONSTRAINED MINIMUM.
                         RNEW = R
                         DO 1200 I = 1, N
                             FACT = ABS(XP(I) - Y(I))
                             IF (FACT.NE.0.0) THEN
                                 IF (FACT.LT.(RNEW * S(I))) THEN
                                     RNEW = FACT / S(I)
                                 ENDIF
                             ENDIF
 1200                        CONTINUE
C                        ENDDO
                         RTMP = R
                         IF (RNEW.LT.R) THEN
                             R = 0.9 * RNEW
                         ENDIF
                     ENDIF
C
C                    REDUCE THE TRUST REGION RADIUS.
                     IF (((IRED.EQ.1) .AND. (RNEW.GE.RTMP)) .OR.
     1                   ((IRED.GT.1) .AND. (IRED.LE.10)))  THEN
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
                 IF ((IRED.LE.10) .AND. (FY.GT.FUNUP)) GO TO 1125
C            ENDWHILE
C
C            SET RFLAG IF TRUST REGION COULD NOT BE ADJUSTED PROPERLY.
             IF (IRED.GT.10) THEN
                 RFLAG = 2
             ELSE
C                AN  ACCEPTABLE  POINT,  Y HAS BEEN FOUND.  SET RFLAG IF
C                MAXIMUM ITERATIONS WAS REACHED.
                 COUNT = COUNT + 1
                 IF (COUNT.GT.MAXIC) THEN
                     RFLAG = 1
                 ELSE
C                    EVALUATE THE GRADIENT AT THE NEW POINT.
                     CALL SBAMGD
     G                           (ESF   , CDAT  , FDAT  , LDAT  , MDAT
     G                           ,NS    , Y
     Y                           ,G     )
C
C                    TO TEST FOR  CONVERGENCE,  COMPUTE THE  NORM OF THE
C                    GRADIENT  AND THE  NORM OF THE  DIFFERENCE  BETWEEN
C                    THIS POINT AND THE PREVIOUS POINT.
                     NDIFF = 0.0
                     NGRAD = 0.0
                     DO 1225 I = 1, N
                         NDIFF = NDIFF + (XP(I) - Y(I))**2
                         NGRAD = NGRAD + G(I)**2
 1225                    CONTINUE
C                    ENDDO
                     NDIFF = SQRT(NDIFF)
                     NGRAD = SQRT(NGRAD)
C
C                    SET RFLAG IF CONVERGENCE OCCURRED.
                     IF ((NDIFF.LT.EPSD) .OR. (NGRAD.LT.EPSG)) THEN
                         RFLAG = 0
                     ELSE
C                        ADJUST THE TRUST REGION.
                         HPROD = 0.0
                         DO 1275 I = 1, N
                             DO 1250 J  = 1, N
                                 HPROD = HPROD + H(I,J)
     1                                 * (Y(I) - XP(I))
     2                                 * (Y(J) - XP(J))
 1250                            CONTINUE
C                            ENDDO
 1275                        CONTINUE
C                        ENDDO
                         ERR = ABS(FX + GPROD + 0.5 * HPROD - FY)
                         IF (ERR.LE.(0.1 * ABS(FY))) THEN
                             R = 2.0 * R
                         ENDIF
                         IF (ERR.GE.(0.75 * ABS(FY))) THEN
                             R = 0.5 * R
                         ENDIF
C
C                        SWAP POINTS AND FUNCTION VALUES.
                         DO 1300 I = 1, N
                             XP(I) = Y(I)
 1300                        CONTINUE
C                        ENDDO
                         FX = FY
                     ENDIF
                 ENDIF
             ENDIF
             IF (RFLAG.EQ.-1) GO TO 1100
C        ENDWHILE
C
C        SET THE RESULTANT STATISTICS.
         STATS(1) = XP(1)
         STATS(2) = XP(2)
         STATS(3) = XP(3)
C
C        REDEFINE THE ERROR FLAG IF THE ESTIMATE FOR THE TOTAL NUMBER OF
C        FAULTS IS LESS THAN THE  OBSERVED NUMBER.  NOTE,  THE TEST WILL
C        BE CONSERVATIVE  IF ALL FAULTS IDENTIFIED IN CDAT  FOR THE LAST
C        PERIOD WERE NOT LOCATED  IN THE FRACTIONAL PORTION  OF THE PRO-
C        GRAM BEING TESTED.
         IF (STATS(1).LT.(MDAT(NS)+CDAT(NS))) THEN
             RFLAG = 4
         ENDIF
         RETURN
         END
         SUBROUTINE SBAMPR
     G                     (NPV   , NS    , TNOF  , PED   , PEC   , EXF
     G                     ,EXL   , ERM   , FDAT  , LDAT  , MDAT
     Y                     ,PDAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SBAMPR
C    G                     (NPV   , NS    , TNOF  , PED   , PEC   , EXF
C    G                     ,EXL   , ERM   , FDAT  , LDAT  , MDAT
C    Y                     ,PDAT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO COMPUTE THE  PREDICTED NUMBER  OF FAULTS IN THE NEXT TESTING
C        PERIOD AND TO CREATE THE  PREDICTED FAULT COUNT DATA VECTOR FOR
C        THE BROOKS AND MOTLEY DISCRETE  SOFTWARE RELIABILITY MODELS FOR
C        A SOFTWARE SYSTEM.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE SBAMPR  CALL LINE  INDICATES WHETHER
C        THE PREDICTED  NUMBER OF FAULTS  IN THE NEXT TESTING  PERIOD OR
C        THE ENTIRE FAULT COUNT VECTOR IS TO BE GENERATED. IF NPV IS SET
C        TO 1, THE ROUTINE  WILL COMPUTE THE  ONE PREDICTED  VALUE USING
C        THE ARGUMENTS TNOF, PED, PEC, EXL, EXF, AND ERM. OTHERWISE, THE
C        ROUTINE WILL COMPUTE THE ENTIRE DATA VECTOR OF FAULT COUNTS US-
C        ING THE ARGUMENTS  TNOF, PED,  PEC, AND ALL  NS ELEMENTS OF THE
C        FDAT, LDAT, AND MDAT VECTORS.
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
C             ERM    (R) = FAULTS FOUND TO DATE IN FRACTIONAL PART
C             EXF    (R) = EXPECTED  FRACTIONAL  PART OF CODE  TESTED IN
C                          THE NEXT PERIOD
C             EXL    (R) = EXPECTED TESTING LENGTH OF NEXT PERIOD
C             FDAT   (R) = FRACTIONAL PART OF CODE TESTED VECTOR
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             MDAT   (R) = PREVIOUS CUMULATIVE FAULT DATA VECTOR
C             NPV    (I) = NUMBER OF PREDICTED VALUES DESIRED
C             NS     (I) = SIZE OF FDAT, LDAT, AND MDAT
C             PEC    (R) = PROB. OF CORRECTING FAULTS  WITHOUT INTRODUC-
C                          ING NEW FAULTS.
C             PED    (R) = PROB. OF DETECTING FAULTS
C             TNOF   (R) = ESTIMATE OF THE TOTAL NUMBER OF FAULTS
C        YIELDED
C             PDAT   (R) = VECTOR OF PREDICTED DATA VALUE(S)
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
         INTEGER              NPV           ,NS
         DIMENSION            FDAT(NS)      ,LDAT(NS)      ,MDAT(NS)
     1                       ,PDAT(NPV)
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
C            COMPUTE THE  ESTIMATE OF THE  NUMBER OF FAULTS  IN THE NEXT
C            TESTING PERIOD.
             PDAT(1) = (1.0 - (1.0 - PED)**EXL)
     1               * (TNOF * EXF - PEC * ERM)
         ELSE
C            COMPUTE THE ESTIMATES FOR THE ENTIRE FAULT COUNT VECTOR.
             DO 1000 I = 1, NS
                 PDAT(I) = (1.0 - (1.0 - PED)**LDAT(I))
     1                   * (TNOF * FDAT(I) - PEC * MDAT(I))
 1000            CONTINUE
C            ENDDO
         ENDIF
         RETURN
         END
         SUBROUTINE SESHMA
     G                     (CDAT  , LDAT  , NS    , NSB   , NSE   , NSR
     Y                     ,STAT  , RFLAG , INDX  , V     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SESHMA
C    G                     (CDAT  , LDAT  , NS    , NSB   , NSE   , NSR
C    Y                     ,STAT  , RFLAG , INDX  , V     )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM THE PREQUENTIAL LIKELIHOOD ANALYSIS FOR THE S-SHAPED
C        RELIABILITY GROWTH MODEL.
C   DESCRIPTION
C        THE ROUTINE  CONTROLS THE EXECUTIONS  OF THE  APPLICABLE SMFLIB
C        ROUTINE, SESHMD.  THE PROCESSING, AS DEFINED  IN THE LITTLEWOOD
C        PAPER (SEE REFERENCES IN THE MAIN PROGRAM) BASICALLY INVOLVES A
C        SERIES OF EXECUTIONS WHERE  NSB TO NSE DATA POINTS ARE UTILIZED
C        IN THE MODEL.
C
C        FOR EACH ITERATION,  THE MODEL PARAMETERS (ESTIMATES)  FROM THE
C        CURRENT ITERATION ARE USED IN CONJUNCTION WITH THE OBSERVED DA-
C        TA FROM THE NEXT ITERATION POINT  TO DETERMINE AN OVER-ALL PIC-
C        TURE OF THE APPLICABILITY OF THE MODEL  FOR THE PARTICULAR DATA
C        SET.
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
C             CDAT   (R) = ARRAY TO HOLD OBSERVED FAULT COUNT DATA
C             LDAT   (R) = ARRAY TO HOLD INTERVAL TESTING LENGTHS (CUM)
C             NS     (I) = THE NUMBER OF ENTRIES
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C        YIELDED
C             INDX   (I) = ANALYSIS ITERATION COUNTER (AT RETURN)
C             RFLAG  (I) = MODEL RESULTS ERROR FLAG
C             STAT   (R) = PREQUENTIAL LIKELIHOOD STATISTIC
C             V      (R) = ANALYSIS RESULTS ARRAY OF SIZE NSR
C   LOCAL GLOSSARY
C             LOGFX  (R) = LOG(X(I))!
C             STATS  (R) = STATISTICS RESULTS ARRAY
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
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              INDX          ,NS            ,NSB
     1                       ,NSE           ,NSR           ,RFLAG
         DIMENSION            CDAT(NS)      ,LDAT(NS)      ,V(NSR)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J             ,K
         DIMENSION            STATS(2,3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE THE P.L. ITERATION COUNTER, STORAGE INDEX, THE MODEL
C        ERROR FLAG, AND THE P.L. STATISTIC.
         I      = NSB - 1
         INDX   = 0
         RFLAG  = 0
         STAT   = 0.0
C
C        DOWHILE (PROCESSING LEFT AND NO ERRORS)
 1000        I  = I + 1
             IF ((I.GT.NSE) .OR. (RFLAG.NE.0)) GO TO 1050
C
C            COMPUTE MODEL ESTIMATES.
             CALL SESHMD
     G                   (CDAT  , LDAT  , I
     Y                   ,STATS , RFLAG )
C
C            INCREMENT THE ITERATION COUNTER FOR STORAGE.
             INDX = INDX + 1
C
             IF (RFLAG.EQ.0) THEN
                 TMP  = STATS(2,1)
     1                *((1.0 + STATS(1,1) * LDAT(I))
     2                *   EXP(-STATS(1,1) * LDAT(I))
     3                - (1.0 + STATS(1,1) * LDAT(I+1))
     4                *   EXP(-STATS(1,1) * LDAT(I+1)))
C
C                CALCULATE THE SUM FOR THE LOG(X(I))!.
                 K     = INT(CDAT(I+1))
                 LOGFX = 0.0
                 DO 1025 J = 1, K
                     LOGFX = LOGFX + LOG(REAL(J))
 1025                CONTINUE
C                ENDDO
C
                 V(INDX) = 0.0  - (CDAT(I+1) * LOG(TMP) - TMP - LOGFX)
                 STAT    = STAT + V(INDX)
             ENDIF
             GO TO 1000
 1050        CONTINUE
C        ENDWHILE
         RETURN
         END
         SUBROUTINE SESHMD
     G                     (CDAT  , LDAT  , NS
     Y                     ,STATS , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SESHMD
C    G                     (CDAT  , LDAT  , NS
C    Y                     ,STATS , RFLAG )
C
C   AUTHOR
C        PAUL J. KRAUS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE ESTIMATES USING THE MAXIMUM  LIKELIHOOD METHOD
C        OF THE S-SHAPED RELIABILITY GROWTH MODEL.
C   DESCRIPTION
C        ONCE THE ITERATIVE EXECUTION IS INITIATED, PROCESSING WILL CON-
C        TINUE UNTIL ONE OF TWO POSSIBLE TERMINATION STATES IS  REACHED.
C        THE LAST ARGUMENT OF THE CALL LINE (RFLAG) WILL BE SET TO INDI-
C        CATE  TO  THE  CALLING ROUTINE  THE REASON  FOR EXIT.  THE POS-
C        SIBLE REASONS FOR SESHMD EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS  THAN THE OBSERVED
C                 FAULTS TO DATE
C
C        THE RESULTANT STATISTICS WILL BE RETURNED IN THE STATS ARGUMENT
C        FOR A RFLAG VALUE OF 0 ONLY. THIS WILL INCLUDE THE TWO STATIST-
C        ICS AND THEIR  95%  CONFIDENCE INTERVALS (CI).
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             NS     (I) = SIZE OF CDAT AND LDAT
C        YIELDED
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = RESULTS ARRAY
C   LOCAL GLOSSARY
C             A      (R) = LOWER BOUND OF PROPORTIONALITY CONSTANT
C             B      (R) = UPPER BOUND OF PROPORTIONALITY CONSTANT
C             CTOT   (R) = TOTAL FAULTS FOUND TO DATE
C             COUNT  (I) = ITERATION COUNTER
C             D      (R) = DELTA VALUE FOR 95% CI COMPUTATIONS
C             EXPI   (R) = INTERMEDIATE VALUE IN STATS ASSIGNMENT
C             EXPJ   (R) = INTERMEDIATE VALUE IN STATS ASSIGNMENT
C             FA     (R) = VALUE OF FUNCTION AT A
C             FB     (R) = VALUE OF FUNCTION AT B
C             FC     (R) = VALUE OF FUNCTION AT C
C             ICOUNT (I) = STEP COUNTER
C             IFLAG  (I) = CONVERGENCE FLAG
C             SM1    (R) = TEMPORARY SUMMATION 1
C             SM2    (R) = TEMPORARY SUMMATION 2
C             SM3    (R) = TEMPORARY SUMMATION 3
C             TMP1   (R) = INTERMEDIATE CALCULATION 1
C             TMP2   (R) = INTERMEDIATE CALCULATION 2
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
         DIMENSION            CDAT(NS)      ,LDAT(NS)      ,STATS(2,3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              COUNT         ,I             ,ICOUNT
     1                       ,IFLAG         ,J
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE COUNTER AND RETURN STATUS FLAG.
         COUNT = 0
         RFLAG = -1
C
C        ZERO ALL LOCATIONS OF THE RESULTS VECTOR.
         DO 1000 I = 1, 3
             STATS(1,I) = 0.0
             STATS(2,I) = 0.0
 1000        CONTINUE
C        ENDDO
C
C        COMPUTE THE TOTAL NUMBER OF FAULTS FOUND TO DATE.
         CTOT = 0.0
         DO 1025 I = 1, NS
             CTOT = CTOT + CDAT(I)
 1025        CONTINUE
C        ENDDO
C
C        COMPUTE THE VALUE OF THE FUNCTION AT ITS UPPER BOUND.
         B = REAL(NS) / LDAT(NS)
         IF (B.GT.1.0) THEN
             B  = 1.0
         ENDIF
         FB = SESHML
     G               (B     , CDAT  , CTOT  , LDAT  , NS    )
         A = B * 0.1
C
C        COMPUTE THE VALUE OF THE FUNCTION AT A POINT WHERE ITS SIGN  IS
C        THE OPPOSITE OF THE  COMPUTED  VALUE  OF  THE  FUNCTIONS  UPPER
C        BOUND.
C
C        DOWHILE (RFLAG .EQ. -1)
 1050        COUNT = COUNT + 1
             FA    = SESHML
     G                      (A     , CDAT  , CTOT  , LDAT  , NS    )
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
                 A = A * 0.1
             ENDIF
             IF (RFLAG.EQ.-1) GO TO 1050
C        ENDWHILE
C
         IF (RFLAG.EQ.0) THEN
C            COMPUTE THE ESTIMATE  OF  THE  PROPORTIONALITY CONSTANT AND
C            STORE THE VALUE IN STATS(1,1).
             ICOUNT = 1
             IFLAG  = -1
C
C            DOWHILE (IFLAG .EQ. -1)
C                COMPUTE THE ZERO OF THE FUNCTION IN THE INTERVAL  A AND
C                B; NOTE THE ARGUMENT FC IS INITIALIZED BY THE ZERO ROU-
C                TINE WHEN ICOUNT IS SET TO ONE.   <<SEE RESTRICTIONS <<
 1075            CALL ZERO
     B                     (A     , B     , FA    , FB    , FC
     B                     ,ICOUNT
     Y                     ,IFLAG )
C
                 IF (IFLAG.EQ.-1) THEN
C                    COMPUTE THE VALUE OF THE FUNCTION AT ITS NEW  UPPER
C                    BOUND.
                     FB = SESHML
     G                           (B     , CDAT  , CTOT  , LDAT
     G                           ,NS    )
                 ENDIF
                 IF (IFLAG.EQ.-1) GO TO 1075
C            ENDWHILE
             STATS(1,1) = B
         ENDIF
C
C        THE STATISTICS CALCULATIONS  WILL ONLY BE MADE FOR RFLAGS INDI-
C        CATING SUCCESSFUL CONVERGENCE.
         IF (RFLAG.EQ.0) THEN
C
C          COMPUTE THE TOTAL NUMBER OF FAULTS.
           STATS(2,1) = CTOT
     1                / (1.0 - (1.0 + STATS(1,1) * LDAT(NS))
     2                *          EXP(-STATS(1,1) * LDAT(NS)))
C
C          REDEFINE THE ERROR FLAG IF THE ESTIMATE FOR  THE TOTAL NUMBER
C          OF FAULTS IS LESS THAN THE OBSERVED NUMBER.
           IF (STATS(2,1).LT.CTOT) THEN
             RFLAG = 4
           ELSE
C            CALCULATE THE  SUMMATIONS REQUIRED  FOR THE 95%  CONFIDENCE
C            INTERVAL COMPUTATIONS.
             EXPI = EXP(-STATS(1,1) * LDAT(NS))
             SM1  = (1.0 - (1.0 + STATS(1,1) * LDAT(NS)) * EXPI)
     1            / STATS(2,1)
             SM2  = STATS(1,1) * EXPI * LDAT(NS)**2
             EXPI = EXP(-STATS(1,1) * LDAT(1))
             SM3  = ((EXPI * LDAT(1)**2)**2)
     1            /  (1.0  - (1.0 + STATS(1,1) * LDAT(1)) * EXPI)
             DO 1100 I = 2, NS
                 J    = I - 1
                 EXPI = EXP(-STATS(1,1) * LDAT(I))
                 EXPJ = EXP(-STATS(1,1) * LDAT(J))
                 SM3  = SM3
     1                + ((EXPI * LDAT(I)**2 - EXPJ * LDAT(J)**2)**2)
     2                / ((1.0  + STATS(1,1) * LDAT(J)) * EXPJ
     3                -  (1.0  + STATS(1,1) * LDAT(I)) * EXPI)
 1100            CONTINUE
C            ENDDO
             SM3  = SM3 * STATS(2,1) * STATS(1,1)**2
             TMP1 = ABS(SM1 * SM3 -  SM2 * SM2)
C
C            COMPUTE THE 95% CI ABOUT THE PROPORTIONALITY CONSTANT.
             TMP2       = 1.95 * SQRT(SM1 / TMP1)
             STATS(1,2) = STATS(1,1) - TMP2
             STATS(1,3) = STATS(1,1) + TMP2
             IF (STATS(1,2).LT.0.0) THEN
                 STATS(1,2) = 0.0
             ENDIF
C
C            COMPUTE THE 95% CI ABOUT THE TOTAL NUMBER OF FAULTS.
             TMP2       = 1.95 * SQRT(SM3 / TMP1)
             STATS(2,2) = STATS(2,1) - TMP2
             STATS(2,3) = STATS(2,1) + TMP2
             IF (STATS(2,2).LT.CTOT) THEN
                 STATS(2,2) = CTOT
             ENDIF
           ENDIF
         ENDIF
         RETURN
         END
         FUNCTION SESHML
     G                   (AB    , CDAT  , CTOT  , LDAT  , NS    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION SESHML
C    G                   (AB    , CDAT  , CTOT  , LDAT  , NS    )
C
C   AUTHOR
C        PAUL J. KRAUS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO  EVALUATE THE  FUNCTION  USED BY  THE  S-SHAPED  RELIABILITY
C        GROWTH  MODEL USING  THE MAXIMUM  LIKELIHOOD ESTIMATION METHOD.
C   DESCRIPTION
C        THIS ROUTINE CALCULATES THE VALUE OF THE FUNCTION AT THE  POINT
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             CTOT   (R) = TOTAL FAULTS FOUND TO DATE
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             NS     (I) = SIZE OF CDAT AND LDAT
C        YIELDED
C             SESHML (R) = FUNCTION VALUE AT POINT AB
C   LOCAL GLOSSARY
C             EXPI   (R) = INTERMEDIATE EXPONENT CALCULATION 1
C             EXPJ   (R) = INTERMEDIATE EXPONENT CALCULATION 2
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
         DIMENSION            CDAT(NS)      ,LDAT(NS)
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
C        INITIALIZE THE SUMMATION VARIABLE.
         EXPI = EXP(-AB  * LDAT(1))
         SM   = (CDAT(1) * EXPI * LDAT(1)**2)
     1        / (1.0     - (1.0 + AB * LDAT(1)) * EXPI)
C
C        PERFORM THE SUMMATION FOR THE MAXIMUM LIKELIHOOD FUNCTION.
         DO 1000 I = 2, NS
             J    = I - 1
             EXPI = EXP(-AB * LDAT(I))
             EXPJ = EXP(-AB * LDAT(J))
             SM   =  SM   + (CDAT(I)
     1            * (EXPI *  LDAT(I)**2    - EXPJ * LDAT(J)**2))
     2            / ((1.0 +  AB * LDAT(J)) * EXPJ
     3            -  (1.0 +  AB * LDAT(I)) * EXPI)
 1000        CONTINUE
C        ENDDO
C
C        CALCULATE THE FUNCTION VALUE.
         EXPI   = EXP(-AB * LDAT(NS))
         SESHML =   SM - (CTOT * EXPI * LDAT(NS)**2)
     1          / (1.0 - (1.0  +  AB  * LDAT(NS)) * EXPI)
         RETURN
         END
         SUBROUTINE SESHPR
     G                     (NPV   , NS    , TNOF  , PCON
     G                     ,LDAT  , EXL
     Y                     ,PDAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SESHPR
C    G                     (NPV   , NS    , TNOF  , PCON
C    G                     ,LDAT  , EXL
C    Y                     ,PDAT  )
C
C   AUTHOR
C        PAUL J. KRAUS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO COMPUTE AN ESTIMATE OF THE SOFTWARE RELIABILITY IN THE  NEXT
C        TESTING  PERIOD, TO COMPUTE THE  PREDICTED NUMBER  OF FAULTS IN
C        THE  NEXT  TESTING  PERIOD,  AND TO CREATE THE  PREDICTED FAULT
C        COUNT DATA VECTOR FOR THE S-SHAPED RELIABILITY GROWTH MODEL.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE SESHPR  CALL LINE  INDICATES WHETHER
C        THE  SOFTWARE RELIABILITY IN  THE NEXT TESTING PERIOD, THE PRE-
C        DICTED  NUMBER OF  FAULTS  IN  THE  NEXT TESTING  PERIOD OR THE
C        ENTIRE FAULT COUNT VECTOR IS TO BE COMPUTED.   IF NPV IS SET TO
C        1, THE ROUTINE  WILL COMPUTE THE ONE PREDICTED  VALUE USING THE
C        ARGUMENTS  TNOF,  PCON,  EXL, AND THE NS  ELEMENT  OF LDAT.  IF
C        NPV IS SET TO 2, THE ROUTINE WILL  COMPUTE THE  SOFTWARE RELIA-
C        BILITY  VALUE USING THE  ARGUMENTS TNOF, PCON, EXL, AND THE  NS
C        ELEMENT OF LDAT. OTHERWISE, THE ROUTINE WILL COMPUTE THE ENTIRE
C        DATA VECTOR OF FAULT COUNTS USING THE ARGUMENTS TNOF, PCON, AND
C        ALL NS ELEMENTS OF LDAT.
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
C             EXL    (R) = EXPECTED TESTING LENGTH OF NEXT PERIOD
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             NPV    (I) = NUMBER OF PREDICTED VALUES DESIRED
C             NS     (I) = SIZE OF LDAT
C             PCON   (R) = ESTIMATE OF THE PROPORTIONALITY CONSTANT
C             TNOF   (R) = ESTIMATE OF THE TOTAL NUMBER OF FAULTS
C        YIELDED
C             PDAT   (R) = VECTOR OF PREDICTED DATA VALUE(S)
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
         INTEGER              NPV           ,NS
         DIMENSION            LDAT(NS)      ,PDAT(NPV)
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
         IF (NPV.LE.2) THEN
C            COMPUTE THE  ESTIMATE OF THE  NUMBER OF FAULTS  IN THE NEXT
C            TESTING PERIOD OR THE ESTIMATE OF THE  SOFTWARE RELIABILITY
C            IN THE NEXT TESTING PERIOD.
             PDAT(1) = TNOF * ((1.0 +  PCON * LDAT(NS))
     1               * EXP(-PCON    *  LDAT(NS))
     2               - (1.0 + PCON  * (LDAT(NS) + EXL))
     3               * EXP(-PCON    * (LDAT(NS) + EXL)))
             IF (NPV.EQ.2) THEN
                 PDAT(1) = EXP(-PDAT(1))
             ENDIF
         ELSE
C            COMPUTE THE ESTIMATES FOR THE ENTIRE FAULT COUNT VECTOR.
             PDAT(1) = TNOF      * (1.0 - (1.0 + PCON * LDAT(1))
     1               * EXP(-PCON * LDAT(1)))
             DO 1000 I = 2, NS
                 PDAT(I) = TNOF
     1                   * ((1.0 + PCON * LDAT(I-1))
     2                   *   EXP(-PCON  * LDAT(I-1))
     3                   -  (1.0 + PCON * LDAT(I))
     4                   *   EXP(-PCON  * LDAT(I)))
 1000            CONTINUE
C            ENDDO
         ENDIF
         RETURN
         END
         FUNCTION SGPOFN
     G                   (AB    , CDAT  , ESF   , LDAT  , MDAT  , NS
     G                   ,SUMTMP)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION SGPOFN
C    G                   (AB    , CDAT  , ESF   , LDAT  , MDAT  , NS
C    G                   ,SUMTMP)
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
C        TO  CALCULATE  THE  VALUE OF THE  FUNCTION FOR THE  GENERALIZED
C        POISSON MODEL USING THE  MAXIMUM  LIKELIHOOD  ESTIMATION METHOD
C        IF ESF=1 AND THE  LEAST  SQUARES  ESTIMATION  METHOD  IF ESF=2.
C   DESCRIPTION
C        THIS ROUTINE  CALCULATES THE VALUE OF THE FUNCTION AT THE POINT
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             MDAT   (R) = CUMULATIVE FAULT CORRECTIONS DATA VECTOR
C             NS     (I) = NUMBER OF ENTRIES
C             SUMTMP (R) = STORAGE VARIABLE FROM SGPOMD
C        YIELDED
C             SGPOFN (R) = FUNCTION VALUE AT POINT AB
C   LOCAL GLOSSARY
C             SM1    (R) = TEMPORARY SUMMATION 1
C             SM2    (R) = TEMPORARY SUMMATION 2
C             SM3    (R) = TEMPORARY SUMMATION 3
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
         INTEGER              ESF           ,NS
         DIMENSION            CDAT(NS)      ,LDAT(NS)      ,MDAT(NS)
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
C
C        PERFORM THE SUMMATIONS FOR THE  MAXIMUM LIKELIHOOD  FUNCTION OR
C        THE LEAST SQUARES FUNCTION.
         DO 1000 I = 1, NS
             TMP = AB - MDAT(I)
             IF (ESF.EQ.1) THEN
                 SM1 = SM1 + LDAT(I) * TMP
                 SM2 = SM2 + CDAT(I) / TMP
             ELSE
                 SM1 = SM1 + CDAT(I)    * LDAT(I) * TMP
                 SM2 = SM2 + LDAT(I)**2 * TMP
                 SM3 = SM3 + LDAT(I)**2 * TMP**2
             ENDIF
 1000        CONTINUE
C        ENDDO
C
C        CALCULATE THE FUNCTION VALUE.
         IF (ESF.EQ.1) THEN
             SGPOFN = SUMTMP + SM1 * SM2
         ELSE
             SGPOFN = SM1    * SM2 - SUMTMP * SM3
         ENDIF
         RETURN
         END
         SUBROUTINE SGPOGD
     G                     (XP    , CDAT  , LDAT  , MDAT  , NS    , SC
     G                     ,SCAL
     Y                     ,G     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SGPOGD
C    G                     (XP    , CDAT  , LDAT  , MDAT  , NS    , SC
C    G                     ,SCAL
C    Y                     ,G     )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE GRADIENT OF THE LIKELIHOOD  FUNCTION  FOR  THE
C        GENERALIZED POISSON MODEL.
C   DESCRIPTION
C        THE GRADIENT OF THE LIKELIHOOD FUNCTION IS  CALCULATED  AND THE
C        RESULT IS STORED IN G.
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             MDAT   (R) = CUMULATIVE FAULT CORRECTIONS DATA VECTOR
C             NS     (I) = NUMBER OF ENTRIES
C             SC     (R) = SUM (CDAT(I))
C             SCAL   (R) = SUM (CDAT(I) * LOG(LDAT(I)))
C             XP     (R) = PARAMETER ESTIMATE VALUES
C        YIELDED
C             G      (R) = GRADIENT VALUES FOR THE FUNCTION
C   LOCAL GLOSSARY
C             SM1    (R) = TEMPORARY SUMMATION 1
C             SM2    (R) = TEMPORARY SUMMATION 2
C             SM3    (R) = TEMPORARY SUMMATION 3
C             SM4    (R) = TEMPORARY SUMMATION 4
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
         DIMENSION            CDAT(NS)      ,G(3)          ,LDAT(NS)
     1                       ,MDAT(NS)      ,XP(3)
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
C        CALCULATE THE GRADIENT VALUES FOR THE MAXIMUM LIKELIHOOD  FUNC-
C        TION.
         DO 1000 I = 1, NS
             TMP1 = XP(1)  - MDAT(I)
             TMP2 = LDAT(I)**XP(2)
             SM1 = SM1   + TMP1    * TMP2
             SM2 = SM2   + TMP2
             SM3 = SM3   + CDAT(I) / TMP1
             SM4 = SM4   + TMP2    * TMP1 * LOG(LDAT(I))
 1000        CONTINUE
C        ENDDO
C
C        STORE THE GRADIENT VALUES.
         G(1) = SC * SM2 / SM1 - SM3
         G(2) = SC * SM4 / SM1 - SCAL
         RETURN
         END
         SUBROUTINE SGPOHS
     G                     (XP    , CDAT  , LDAT  , MDAT  , NS    , SC
     Y                     ,H     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SGPOHS
C    G                     (XP    , CDAT  , LDAT  , MDAT  , NS    , SC
C    Y                     ,H     )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE  HESSIAN OF THE  LIKELIHOOD  FUNCTION  FOR THE
C        GENERALIZED POISSON MODEL.
C   DESCRIPTION
C        THE HESSIAN OF THE LIKELIHOOD FUNCTION IS  CALCULATED  AND  THE
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             MDAT   (R) = CUMULATIVE FAULT CORRECTIONS DATA VECTOR
C             NS     (I) = NUMBER OF ENTRIES
C             SC     (R) = SUM (CDAT(I))
C             XP     (R) = PARAMETER ESTIMATE VALUES
C        YIELDED
C             H      (R) = HESSIAN VALUES FOR THE FUNCTION
C   LOCAL GLOSSARY
C             D      (R) = EIGENVALUES OF THE HESSIAN
C             DUM1   (R) = DUMMY STORAGE ARRAY 1 FOR EGNVAL CALL
C             DUM2   (R) = DUMMY STORAGE ARRAY 2 FOR EGNVAL CALL
C             EMIN   (R) = VALUE TO BE ADDED TO THE DIAGONAL ELEMENTS OF
C                          THE HESSIAN WHEN THE HESSIAN IS  NOT POSITIVE
C                          DEFINITE.
C             HTMP   (R) = INTERMEDIATE HESSIAN STORAGE FOR EGNVAL CALL
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C             SM1    (R) = TEMPORARY SUMMATION 1
C             SM2    (R) = TEMPORARY SUMMATION 2
C             SM3    (R) = TEMPORARY SUMMATION 3
C             SM4    (R) = TEMPORARY SUMMATION 4
C             SM5    (R) = TEMPORARY SUMMATION 5
C             SM6    (R) = TEMPORARY SUMMATION 6
C             TMP1   (R) = INTERMEDIATE CALCULATION 1
C             TMP2   (R) = INTERMEDIATE CALCULATION 2
C             TMP3   (R) = INTERMEDIATE CALCULATION 3
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
         DIMENSION            CDAT(NS)      ,H(3,3)        ,LDAT(NS)
     1                       ,MDAT(NS)      ,XP(3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J             ,N
         DIMENSION            D(3)          ,DUM1(3,3)     ,DUM2(3,3)
     1                       ,HTMP(3,3)
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
         SM5 = 0.0
         SM6 = 0.0
C
C        CALCULATE THE HESSIAN VALUES FOR THE  MAXIMUM  LIKELIHOOD FUNC-
C        TION.
         DO 1000 I = 1, NS
             TMP1 = XP(1) - MDAT(I)
             TMP2 = LDAT(I)**XP(2)
             SM1  = SM1   + TMP2
             SM2  = SM2   + TMP1    * TMP2
             SM3  = SM3   + CDAT(I) / TMP1**2
             SM4  = SM4   + TMP2    * LOG(LDAT(I))
             TMP3 = TMP2  * TMP1    * LOG(LDAT(I))
             SM5  = SM5   + TMP3
             SM6  = SM6   + TMP3    * LOG(LDAT(I))
 1000        CONTINUE
C        ENDDO
C
C        STORE THE HESSIAN VALUES.
         TMP3   =  SM2**2
         H(1,1) =  SM3 - SC * SM1**2   / TMP3
         H(1,2) = (SM2 * SC * SM4 - SC * SM1 * SM5) / TMP3
         H(2,1) =  H(1,2)
         H(2,2) = (SM2 * SC * SM6 - SC * SM5**2)    / TMP3
C
C        COMPUTE THE EIGENVALUES OF THE HESSIAN.
         N = 2
         DO 1050 I = 1, N
             DO 1025 J = 1, N
                 HTMP(I,J) = H(I,J)
 1025            CONTINUE
C            ENDDO
 1050        CONTINUE
C        ENDDO
C
         CALL EGNVAL
     G               (3     , N     , HTMP  , DUM1  , DUM2
     Y               ,D     )
C
C        WHEN THE HESSIAN IS NOT POSITIVE DEFINITE, REPLACE THE DIAGONAL
C        ELEMENTS OF THE HESSIAN WITH THE HESSIAN VALUE PLUS EMIN.  THIS
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
         SUBROUTINE SGPOMA
     G                     (CDAT  , LDAT  , MDAT  , NS    , NSB   , NSE
     G                     ,NSR   , ALPHA
     Y                     ,STAT  , RFLAG , INDX  , V     , SDAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SGPOMA
C    G                     (CDAT  , LDAT  , MDAT  , NS    , NSB   , NSE
C    G                     ,NSR   , ALPHA
C    Y                     ,STAT  , RFLAG , INDX  , V     , SDAT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM THE PREQUENTIAL LIKELIHOOD ANALYSIS FOR THE GENERAL-
C        IZED POISSON MODEL.
C   DESCRIPTION
C        THE ROUTINE  CONTROLS THE EXECUTIONS  OF THE  APPLICABLE SMFLIB
C        ROUTINE, SGPOMD.  THE PROCESSING, AS DEFINED  IN THE LITTLEWOOD
C        PAPER (SEE REFERENCES IN THE MAIN PROGRAM) BASICALLY INVOLVES A
C        SERIES OF EXECUTIONS WHERE  NSB TO NSE DATA POINTS ARE UTILIZED
C        IN THE MODEL.
C
C        FOR EACH ITERATION,  THE MODEL PARAMETERS (ESTIMATES)  FROM THE
C        CURRENT ITERATION ARE USED IN CONJUNCTION WITH THE OBSERVED DA-
C        TA FROM THE NEXT ITERATION POINT  TO DETERMINE AN OVER-ALL PIC-
C        TURE OF THE APPLICABILITY OF THE MODEL  FOR THE PARTICULAR DATA
C        SET.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS ROUTINE IS CURRENTLY SET FOR ONLY ML EXECUTIONS WITH
C        WEIGHTING FUNCTIONS OF 2 (ALPHA INPUT).
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             ALPHA  (R) = USER INPUT FOR ALPHA
C             CDAT   (R) = ARRAY TO HOLD OBSERVED FAULT COUNT DATA
C             LDAT   (R) = ARRAY TO HOLD INTERVAL TESTING LENGTHS
C             MDAT   (R) = ARRAY TO HOLD CUMULATIVE FAULT CORRECTIONS
C             NS     (I) = THE NUMBER OF ENTRIES
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C        YIELDED
C             INDX   (I) = ANALYSIS ITERATION COUNTER (AT RETURN)
C             RFLAG  (I) = MODEL RESULTS ERROR FLAG
C             SDAT   (R) = SCRATCH DATA ARRAY TO APPLY THE ALPHA WEIGHT
C             STAT   (R) = PREQUENTIAL LIKELIHOOD STATISTIC
C             V      (R) = ANALYSIS RESULTS ARRAY OF SIZE NSR
C   LOCAL GLOSSARY
C             AN     (R) = INIT. EST. OF TOTAL NUMBER OF FAULTS
C             COUNT  (I) = ITERATION COUNTER
C             ESF    (I) = ESTIMATION SELECTION FLAG (ML ONLY)
C             LOGFX  (R) = LOG(X(I))!
C             MAXIC  (I) = USER-SPECIFIED MAX. NUMBER OF ITERATIONS
C             STATS  (R) = STATISTICS RESULTS ARRAY
C             TMP1   (R) = INTERMEDIATE CALCULATION 1
C             TMP2   (R) = INTERMEDIATE CALCULATION 2
C             WFSF   (I) = WEIGHTING FUNCTION SELECTION FLAG (INPUT)
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
         DIMENSION            CDAT(NS)      ,LDAT(NS)      ,MDAT(NS)
     1                       ,SDAT(NS)      ,V(NSR)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              COUNT         ,ESF           ,I
     1                       ,J             ,K             ,MAXIC
     2                       ,WFSF
         DIMENSION            STATS(4,3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE THE P.L. ITERATION COUNTER, STORAGE INDEX, THE MODEL
C        ERROR FLAG, AND THE P.L. STATISTIC.
         I      = NSB - 1
         INDX   = 0
         RFLAG  = 0
         STAT   = 0.0
C
C        INITIALIZE THE MODEL AND WEIGHTING SELECTION  FLAGS TO INDICATE
C        MAXIMUM LIKELIHOOD ANALYSIS WITH ALPHA INPUT.
         ESF    = 1
         WFSF   = 2
C
C        DOWHILE (PROCESSING LEFT AND NO ERRORS)
 1000        I  = I + 1
             IF ((I.GT.NSE) .OR. (RFLAG.NE.0)) GO TO 1075
C
C            COPY THE LENGTH DATA TO THE SCRATCH ARRAY SO THE WEIGHT CAN
C            BE APPLIED.
             DO 1025 J = 1, I
                 SDAT(J) = LDAT(J)
 1025            CONTINUE
C            ENDDO
C
C            COMPUTE MODEL ESTIMATES.
             CALL SGPOMD
     G                   (ESF   , WFSF  , ALPHA , AN
     G                   ,MAXIC , CDAT  , MDAT  , I
     B                   ,SDAT
     Y                   ,STATS , COUNT , RFLAG )
C
C            INCREMENT THE ITERATION COUNTER FOR STORAGE.
             INDX = INDX + 1
C
             IF (RFLAG.EQ.0) THEN
                 TMP1 = STATS(2,1) - MDAT(I)
                 TMP2 = LDAT(I+1)**ALPHA
C
C                CALCULATE THE SUM FOR THE LOG(X(I))!.
                 K     = INT(CDAT(I+1))
                 LOGFX = 0.0
                 DO 1050 J = 1, K
                     LOGFX = LOGFX + LOG(REAL(J))
 1050                CONTINUE
C                ENDDO
C
                 V(INDX) = 0.0
     1                   - (CDAT(I+1)  * LOG(STATS(1,1))
     2                   +  CDAT(I+1)  * LOG(TMP1)
     3                   +  CDAT(I+1)  * LOG(TMP2)
     4                   -  STATS(1,1) * TMP1      * TMP2
     5                   -  LOGFX)
                 STAT    = STAT + V(INDX)
             ENDIF
             GO TO 1000
 1075        CONTINUE
C        ENDWHILE
         RETURN
         END
         SUBROUTINE SGPOMD
     G                     (ESF   , WFSF  , ALPHA , AN
     G                     ,MAXIC , CDAT  , MDAT  , NS
     B                     ,LDAT
     Y                     ,STATS , COUNT , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SGPOMD
C    G                     (ESF   , WFSF  , ALPHA , AN
C    G                     ,MAXIC , CDAT  , MDAT  , NS
C    B                     ,LDAT
C    Y                     ,STATS , COUNT , RFLAG )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   DEKKER-BRENT AND TRUST REGION IMPLEMENTATION BY
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   DEKKER-BRENT AND TRUST REGION CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE  THE ESTIMATES  USING THE  MAXIMUM LIKELIHOOD  AND
C        LEAST SQUARES METHODS OF THE GENERALIZED POISSON MODEL.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE CALL  LINE (ESF) WILL BE USED BY THE
C        ROUTINE TO DETERMINE  WHICH OF THE  TWO METHODS OF EXECUTION IS
C        DESIRED.  THE SECOND ARGUMENT (WFSF)  WILL BE USED TO DETERMINE
C        WHICH OF THE THREE POSSIBLE INTERVAL LENGTH WEIGHTING FUNCTIONS
C        IS TO BE APPLIED. THE THREE POSSIBLE WEIGHTING FUNCTIONS ARE:
C             1 X(I) ** 2 / 2
C             2 X(I) ** ALPHA (ALPHA INPUT)
C             3 X(I) ** ALPHA (ALPHA ESTIMATED)
C
C        NOTE THAT THE ORIGINAL DATA OF THE LDAT VECTOR WILL BE DESTROY-
C        ED FOR SGPOMD EXECUTIONS  WHERE WFSF HAS  BEEN SET TO THE VALUE
C        OF 1 OR 2.  IT IS FURTHER EMPHASIZED THAT THE WFSF OF 3 IS ONLY
C        ALLOWED IN CONJUNCTION  WITH AN ESF OF 1 (I.E., MAXIMUM LIKELI-
C        HOOD) WHEN THE INTERVALS OF THE INTERVAL LENGTH DATA HAVE VARI-
C        ABLE LENGTHS.
C
C        ONCE THE ITERATIVE EXECUTION IS INITIATED, PROCESSING WILL CON-
C        TINUE UNTIL ONE OF FOUR POSSIBLE TERMINATION STATES IS REACHED.
C        THE LAST ARGUMENT OF THE CALL LINE (RFLAG) WILL BE SET TO INDI-
C        CATE TO THE CALLING ROUTINE  THE REASON FOR EXIT.  THE POSSIBLE
C        REASONS FOR SGPOMD EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             1 - MAXIMUM ITERATIONS
C             2 - THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS  THAN THE OBSERVED
C                 FAULTS TO DATE
C
C        THE NUMBER OF ITERATIONS  NEEDED FOR THE SUCCESSFUL CONVERGENCE
C        WILL BE RETURNED IN THE NEXT-TO-LAST ARGUMENT (COUNT).
C
C        THE RESULTANT STATISTICS WILL BE RETURNED IN THE STATS ARGUMENT
C        FOR RFLAG VALUES  OF 0 AND 1 ONLY.  THE AMOUNT OF RETURNED DATA
C        DATA WILL DEPEND ON THE SETTING  OF THE ESF AND WFSF ARGUMENTS.
C        THE  FOLLOWING CHARTS  WILL SHOW THE THREE  POSSIBLE AMOUNTS OF
C        OUTPUT AS CONTROLLED BY THOSE TWO ARGUMENTS.
C
C        ESF.EQ.1 .AND. WFSF.NE.3
C             1,1 PCON            1,2 LOWER 95% CI    1,3 UPPER 95% CI
C             2,1 TNOF            2,2 LOWER 95% CI    2,3 UPPER 95% CI
C             3,1 FAULTS LEFT     3,2 LOWER 95% CI    3,3 UPPER 95% CI
C             4,1 (EMPTY)         4,2 (EMPTY)         4,3 (EMPTY)
C
C
C        ESF.EQ.2 .AND. WFSF.NE.3
C             1,1 PCON            1,2 (EMPTY)         1,3 (EMPTY)
C             2,1 TNOF            2,2 (EMPTY)         2,3 (EMPTY)
C             3,1 FAULTS LEFT     3,2 (EMPTY)         3,3 (EMPTY)
C             4,1 (EMPTY)         4,2 (EMPTY)         4,3 (EMPTY)
C
C        ESF.EQ.1 .AND. WFSF.EQ.3
C             1,1 PCON            1,2 (EMPTY)         1,3 (EMPTY)
C             2,1 TNOF            2,2 (EMPTY)         2,3 (EMPTY)
C             3,1 FAULTS LEFT     3,2 (EMPTY)         3,3 (EMPTY)
C             4,1 ALPHA           4,2 (EMPTY)         4,3 (EMPTY)
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
C             ALPHA  (R) = INPUT OR INIT. EST. OF ALPHA
C             AN     (R) = INIT. ESTIMATE OF TOTAL NUMBER OF FAULTS
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS
C             MDAT   (R) = CUMULATIVE FAULT CORRECTIONS DATA VECTOR
C             NS     (I) = SIZE OF CDAT, LDAT, AND MDAT
C             WFSF   (I) = WEIGHTING FUNCTION SELECTION FLAG
C        BOTH
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR (WEIGHTED
C                          ON RETURN IF WFSF.NE.3)
C        YIELDED
C             COUNT  (I) = ITERATION COUNTER
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = RESULTS ARRAY
C   LOCAL GLOSSARY
C             A      (R) = LOWER BOUND OF TOTAL NUMBER OF FAULTS
C             AMAX   (R) = INTERMEDIATE CALCULATION
C             B      (R) = UPPER BOUND OF TOTAL NUMBER OF FAULTS
C             CTOT   (R) = TOTAL FAULTS FOUND TO DATE
C             D      (R) = INTERMEDIATE VALUE IN 95% CI CALCULATIONS
C             FA     (R) = VALUE OF FUNCTION AT A
C             FB     (R) = VALUE OF FUNCTION AT B
C             FC     (R) = VALUE OF FUNCTION AT C
C             ICOUNT (I) = STEP COUNTER
C             IFLAG  (I) = CONVERGENCE FLAG
C             SC     (R) = SUM(CDAT(I))
C             SCAL   (R) = SUM(CDAT(I) * LOG(LDAT(I)))
C             SCL    (R) = SUM(CDAT(I) * LDAT(I))
C             SL     (R) = SUM(LDAT(I))
C             SUMTMP (R) = STORAGE VARIABLE FOR SGPOFN ACCESS
C             S1     (R) = INTERMEDIATE SUM FOR STATISTICS
C             S2     (R) = INTERMEDIATE SUM FOR STATISTICS
C             TMP    (R) = INTERMEDIATE CALCULATION
C             XMAX   (R) = UPPER BOUNDS OF SOLUTION
C             XMIN   (R) = LOWER BOUNDS OF SOLUTION
C             XP     (R) = PARAMETER ESTIMATE VALUES
C   ERRORS
C        RFLAG  = 1 : MAXIMUM ITERATIONS
C        RFLAG  = 2 : THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY
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
         INTEGER              COUNT         ,ESF           ,MAXIC
     1                       ,NS            ,RFLAG         ,WFSF
         DIMENSION            CDAT(NS)      ,LDAT(NS)      ,MDAT(NS)
     1                       ,STATS(4,3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,ICOUNT        ,IFLAG
         DIMENSION            XMAX(3)       ,XMIN(3)       ,XP(3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE COUNTER AND RETURN STATUS FLAG.
         COUNT  = 0
         RFLAG  = -1
C
C        ZERO ALL LOCATIONS OF THE RESULTS VECTOR.
         DO 1000 I = 1, 4
             STATS(I,1) = 0.0
             STATS(I,2) = 0.0
             STATS(I,3) = 0.0
 1000        CONTINUE
C        ENDDO
C
C        ZERO ALL POSSIBLE SUMMATION VARIABLES.
         SC   = 0.0
         SCAL = 0.0
         SCL  = 0.0
         SL   = 0.0
         S1   = 0.0
         S2   = 0.0
C
C        ZERO THE EXTRA SUMMATION FOR TOTAL FAULTS.  THIS LOOKS LIKE THE
C        SAME CALCULATION AS MADE FOR "SC"; HOWEVER, THE SUMMATION ISN'T
C        PERFORMED FOR WEIGHTING FUNCTION TWO.  AS THERE WAS NOT TIME TO
C        TRACE FOR THE IMPACT OF PERFORMING THE SUMMATION, I ADDED CTOT.
         CTOT = 0.0
C
         DO 1025 I = 1, NS
             CTOT = CTOT + CDAT(I)
             IF (WFSF.NE.3) THEN
C                PERFORM THE INTERVAL TESTING LENGTH DATA WEIGHTING.
                 IF (WFSF.EQ.1) THEN
                     LDAT(I) = LDAT(I)**2 / 2.0
                 ELSE
                     LDAT(I) = LDAT(I)**ALPHA
                 ENDIF
C
C                PERFORM THE NECESSARY DATA VECTOR SUMMATIONS.
                 IF (ESF.EQ.1) THEN
                     SC  = SC + CDAT(I)
                     SL  = SL + LDAT(I)
                 ELSE
                     SCL = SCL + CDAT(I) * LDAT(I)
                 ENDIF
             ELSE
C                PERFORM THE DATA SUMMATIONS  FOR THE MAXIMUM LIKELIHOOD
C                METHOD WITH ALPHA ESTIMATED.
                 SC   = SC + CDAT(I)
                 SCAL = SCAL + CDAT(I) * LOG(LDAT(I))
             ENDIF
 1025        CONTINUE
C        ENDDO
C
         IF (WFSF.NE.3) THEN
C            USE THE DEKKER-BRENT ALGORITHM TO FIND A  SINGLE  PARAMETER
C            (ALPHA NOT ESTIMATED).
             SUMTMP = SCL
             IF (ESF.EQ.1) THEN
                 SUMTMP = -SC * SL
             ENDIF
C
C            COMPUTE THE VALUE OF THE FUNCTION AT ITS LOWER BOUND.
             A  = MDAT(NS) + 0.001
             FA = SGPOFN
     G                   (A     , CDAT  , ESF   , LDAT  , MDAT  , NS
     G                   ,SUMTMP)
             B = 2.0 * A
C
C            COMPUTE THE VALUE OF THE FUNCTION AT A POINT WHERE ITS SIGN
C            IS THE OPPOSITE OF THE COMPUTED VALUE OF THE FUNCTIONS LOW-
C            ER BOUND.
C
C            DOWHILE (RFLAG .EQ. -1)
 1050            COUNT = COUNT + 1
                 FB = SGPOFN
     G                       (B     , CDAT  , ESF   , LDAT  , MDAT  , NS
     G                       ,SUMTMP)
C
C                SET RFLAG, IF CONVERGENCE IS POSSIBLE.
                 IF (FA * FB.LE.0.0) THEN
                     RFLAG = 0
                 ENDIF
C
C                SET RFLAG, IF THE DATA ARE NOT APPROPRIATE FOR MODEL.
                 IF (COUNT.GE.5) THEN
                    RFLAG = 3
                 ENDIF
                 IF (RFLAG.EQ.-1) THEN
                     B = 2.0 * B
                 ENDIF
                 IF (RFLAG.EQ.-1) GO TO 1050
C            ENDWHILE
C
             IF (RFLAG.EQ.0) THEN
C                COMPUTE THE ESTIMATE OF THE TOTAL NUMBER OF FAULTS  AND
C                STORE THE VALUE IN STATS(2,1).
                 IFLAG  = -1
                 ICOUNT = 1
C
C                DOWHILE (IFLAG .EQ. -1)
C                    COMPUTE THE ZERO OF THE FUNCTION IN THE INTERVAL  A
C                    AND B;  NOTE THE  ARGUMENT FC IS INITIALIZED BY THE
C                    ZERO ROUTINE WHEN ICOUNT IS SET TO ONE.
C                                                  <<SEE RESTRICTIONS <<
 1075                CALL ZERO
     B                         (A     , B     , FA    , FB    , FC
     B                         ,ICOUNT
     Y                         ,IFLAG )
C
                     IF (IFLAG.EQ.-1) THEN
C                        COMPUTE THE  VALUE OF THE  FUNCTION AT ITS  NEW
C                        UPPER BOUND.
                         FB = SGPOFN
     G                               (B     , CDAT  , ESF   , LDAT
     G                               ,MDAT  , NS    , SUMTMP)
                     ENDIF
                     IF (IFLAG.EQ.-1) GO TO 1075
C                ENDWHILE
                 STATS(2,1) = B
             ENDIF
         ELSE
C
C            USE THE TRUST REGION ALGORITHM TO FIND ALPHA ESTIMATED. SET
C            THE UPPER AND LOWER BOUNDS FOR THE SOLUTION.
             XMIN(1) = MDAT(NS) + 0.001
             XMIN(2) = 0.0
             XMAX(1) = 1.0E10
             XMAX(2) = 1.0E10
             DO 1100 I = 1, NS
                 IF (LDAT(I).NE.1.0) THEN
                     AMAX = ABS(10.0 / LOG(LDAT(I)))
                     IF (AMAX.LT.XMAX(2)) THEN
                         XMAX(2) = AMAX
                     ENDIF
                 ENDIF
 1100            CONTINUE
C            ENDDO
C
C            SET THE INITIAL PARAMETER ESTIMATES.
             XP(1) = AN
             IF (XP(1).LT.XMIN(1)) THEN
                 XP(1)  = XMIN(1)
             ELSE
                 IF (XP(1).GT.XMAX(1)) THEN
                     XP(1)  = XMAX(1)
                 ENDIF
             ENDIF
             XP(2) = ALPHA
             IF (XP(2).LT.XMIN(2)) THEN
                 XP(2)  = XMIN(2)
             ELSE
                 IF (XP(2).GT.XMAX(2)) THEN
                     XP(2)  = XMAX(2)
                 ENDIF
             ENDIF
C
C            COMPUTE THE PARAMETER ESTIMATES.
             CALL SGPOTR
     G                   (CDAT  , LDAT  , MAXIC , MDAT  , NS    , SC
     G                   ,SCAL  , XMAX  , XMIN
     B                   ,XP
     Y                   ,COUNT , RFLAG )
             STATS(2,1) = XP(1)
             STATS(4,1) = XP(2)
         ENDIF
C
C        THE STATISTICS CALCULATIONS  WILL ONLY BE MADE FOR RFLAGS INDI-
C        CATING SUCCESSFUL CONVERGENCE OR MAXIMUM ITERATIONS.
         IF (RFLAG.LE.1) THEN
C
C          REDEFINE THE ERROR FLAG IF THE  ESTIMATE FOR THE TOTAL NUMBER
C          OF FAULTS IS LESS THAN THE OBSERVED NUMBER.
           IF (STATS(2,1).LT.CTOT) THEN
             RFLAG = 4
           ELSE
C            COMPUTE THE NUMBER OF FAULTS REMAINING IN THE CODE; NOTE, A
C            FAULT WHICH IS FOUND BUT NOT CORRECTED IS STILL IN THE CODE
C            ACCORDING TO THIS MODEL; HENCE MDAT(NS) RATHER THAN CTOT IS
C            USED IN THIS CALCULATION.
             STATS(3,1) = STATS(2,1) - MDAT(NS)
             IF ((ESF.EQ.1) .AND. (WFSF.NE.3)) THEN
C
C                COMPUTE THE MAXIMUM LIKELIHOOD STATISTICS.
                 DO 1125 I = 1, NS
                     TMP = STATS(2,1) - MDAT(I)
                     S1  = S1 + LDAT(I) * TMP
                     S2  = S2 + LDAT(I) / TMP
 1125                CONTINUE
C                ENDDO
C
C                COMPUTE THE PROPORTIONALITY CONSTANT.
                 STATS(1,1) = SC / S1
C
C                COMPUTE THE INTERMEDIATE STATISTIC D.
                 D          = 1.0 / STATS(1,1) * SC * S2 - SL**2
C
C                COMPUTE THE 95% CI FOR THE PROPORTIONALITY CONSTANT
                 TMP        = 1.95 * SQRT(STATS(1,1) / D * S2)
                 STATS(1,2) = STATS(1,1) - TMP
                 IF (STATS(1,2).LT.0.0) THEN
                     STATS(1,2) = 0.0
                 ENDIF
                 STATS(1,3) = STATS(1,1) + TMP
C
C                COMPUTE THE 95% CI  FOR THE  ESTIMATED  TOTAL NUMBER OF
C                FAULTS.
                 TMP        = 1.95 * SQRT(SC / (STATS(1,1) * D))
                 STATS(2,2) = STATS(2,1) - TMP
                 IF (STATS(2,2).LT.MDAT(NS)) THEN
                     STATS(2,2) = MDAT(NS)
                 ENDIF
                 STATS(2,3) = STATS(2,1) + TMP
C
C                COMPUTE THE 95% CI FOR THE  NUMBER OF FAULTS  REMAINING
C                IN THE CODE ESTIMATE.
                 STATS(3,2) = STATS(2,2) - MDAT(NS)
                 IF (STATS(3,2).LT.0.0) THEN
                     STATS(3,2) = 0.0
                 ENDIF
                 STATS(3,3) = STATS(2,3) - MDAT(NS)
             ELSEIF ((ESF.EQ.2) .AND. (WFSF.NE.3)) THEN
C                COMPUTE THE LEAST SQUARES STATISTIC FOR THE PROPORTION-
C                ALITY CONSTANT.
                 DO 1150 I = 1, NS
                     S1 = S1 + LDAT(I)**2 * (STATS(2,1) - MDAT(I))
 1150                CONTINUE
C                ENDDO
                 STATS(1,1) = SCL / S1
             ELSE
C                ASSUME MAXIMUM LIKELIHOOD WITH ALPHA ESTIMATED.
                 DO 1175 I = 1, NS
                     S1 = S1 + LDAT(I)**STATS(4,1)
     1                  * (STATS(2,1) - MDAT(I))
 1175                CONTINUE
C                ENDDO
                 STATS(1,1) = SC / S1
             ENDIF
           ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE SGPOML
     G                     (XP    , CDAT  , LDAT  ,  MDAT  , NS    , SC
     Y                     ,FX    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SGPOML
C    G                     (XP    , CDAT  , LDAT  ,  MDAT  , NS    , SC
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
C        TO  CALCULATE  THE  LIKELIHOOD  FUNCTION  FOR  THE  GENERALIZED
C        POISSON MODEL.
C   DESCRIPTION
C        THE LIKELIHOOD FUNCTION IS CALCULATED AND THE RESULT IS  STORED
C        IN FX.
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             MDAT   (R) = CUMULATIVE FAULT CORRECTIONS DATA VECTOR
C             NS     (I) = NUMBER OF ENTRIES
C             SC     (R) = SUM (CDAT(I))
C             XP     (R) = PARAMETER ESTIMATE VALUES
C        YIELDED
C             FX     (R) = MINIMIZED FUNCTION VALUE
C   LOCAL GLOSSARY
C             TMP1   (R) = INTERMEDIATE CALCULATION 1
C             TMP2   (R) = INTERMEDIATE CALCULATION 2
C             TMP3   (R) = INTERMEDIATE CALCULATION 3
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
         DIMENSION            CDAT(NS)      ,LDAT(NS)      ,MDAT(NS)
     1                       ,XP(3)
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
C        INITIALIZE THE  INTERMEDIATE  VARIABLE AND  MINIMIZED  FUNCTION
C        VALUE.
         TMP1 = 0.0
         FX   = 0.0
C
C        CALCULATE THE MINIMIZED FUNCTION VALUE FOR THE  MAXIMUM LIKELI-
C        HOOD FUNCTION.
         DO 1000 I = 1, NS
             TMP1 = TMP1 + (XP(1) - MDAT(I)) * LDAT(I)**XP(2)
 1000        CONTINUE
C        ENDDO
         TMP3 = SC / TMP1
C
         DO 1025 I = 1, NS
             TMP1 = XP(1) - MDAT(I)
             TMP2 = LDAT(I)**XP(2)
             FX   = FX + TMP3 * TMP1 * TMP2 - CDAT(I) * LOG(TMP3)
     1            - CDAT(I)   * LOG(TMP1)   - CDAT(I) * LOG(TMP2)
 1025        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE SGPOPR
     G                     (NPV   , NS    , WFSF  , CDAT  , LDAT  , MDAT
     G                     ,EXL   , LCOR  , PCON  , TNOF  , ALPHA
     Y                     ,PDAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SGPOPR
C    G                     (NPV   , NS    , WFSF  , CDAT  , LDAT  , MDAT
C    G                     ,EXL   , LCOR  , PCON  , TNOF  , ALPHA
C    Y                     ,PDAT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO COMPUTE THE  PREDICTED NUMBER OF FAULTS  IN THE NEXT TESTING
C        PERIOD AND TO CREATE THE  PREDICTED FAULT COUNT DATA VECTOR FOR
C        THE GENERALIZED POISSON MODEL.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE SGPOPR  CALL LINE  INDICATES WHETHER
C        THE PREDICTED  NUMBER OF FAULTS  IN THE NEXT  TESTING PERIOD OR
C        THE ENTIRE FAULT  COUNT DATA VECTOR  IS TO BE COMPUTED.  NPV IS
C        ACTUALLY THE  DIMENSION STATEMENT  FOR THE PDAT VECTOR;  AND IT
C        CAN TAKE ON  THREE POSSIBLE  VALUES (1, 3, OR NS).  VALUES OF 1
C        SHOULD BE USED FOR LEAST SQUARES  EXECUTIONS (WHERE WFSF IS SET
C        TO 1 OR 2) AND FOR MAXIMUM LIKELIHOOD EXECUTIONS (WHERE WFSF IS
C        SET 3); VALUES OF 3 SHOULD ONLY BE  USED FOR MAXIMUM LIKELIHOOD
C        EXECUTIONS  (WHERE WFSF IS SET TO 1 OR 2).  THE EXTRA TWO LOCA-
C        TIONS CONTAIN THE 95% CONFIDENCE INTERVAL ABOUT THE PREDICTION.
C
C        WHEN NPV IS SET  TO A VALUE  OTHER THAN  1 OR 3 (I.E., NS), THE
C        ROUTINE PROCESSES THE ENTIRE PREDICTED FAULT COUNT DATA VECTOR.
C
C        BECAUSE THE ONE ROUTINE  PROCESSES UNDER THREE DIFFERENT MODES,
C        DEPENDING ON THE VALUE OF NPV, THE FOLLOWING LISTS ARE SUPPLIED
C        TO SHOW WHICH ARGUMENTS OF THE CALL LINE ARE NEEDED:
C
C        NPV=1  : NS    , WFSF  , MDAT  , EXL   , LCOR  , PCON  , TNOF
C                ,ALPHA
C        NPV=3  : NS    , WFSF  , CDAT  , LDAT  , MDAT  , EXL   , LCOR
C                ,PCON  , TNOF  , ALPHA
C        NPV=NS : NS    , WFSF  , LDAT  , MDAT  , PCON  , TNOF  , ALPHA
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
C             ALPHA  (R) = INPUT OR ESTIMATED ALPHA
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             EXL    (R) = EXPECTED TESTING LENGTH OF NEXT PERIOD
C             LCOR   (R) = FAULT CORRECTIONS AFTER LAST PERIOD
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             MDAT   (R) = CUMULATIVE FAULT CORRECTIONS DATA VECTOR
C             NPV    (I) = NUMBER OF PREDICTED VALUES DESIRED
C             NS     (I) = SIZE OF CDAT, LDAT, AND MDAT
C             PCON   (R) = ESTIMATE OF THE PROPORTIONALITY CONSTANT
C             TNOF   (R) = ESTIMATE OF THE TOTAL NUMBER OF FAULTS
C             WFSF   (I) = WEIGHTING FUNCTION SELECTION FLAG
C        YIELDED
C             PDAT   (R) = VECTOR OF PREDICTED DATA VALUE(S)
C   LOCAL GLOSSARY
C             D      (R) = INTERMEDIATE CALCULATION IN 95% CI
C             EXLW   (R) = EXPECTED TESTING LENGTH (WEIGHTED)
C             LDW    (R) = INTERVAL TESTING LENGTH (WEIGHTED)
C             SC     (R) = SUM(CDAT(I))
C             SL     (R) = SUM(LDAT(I))
C             SLM    (R) = INTERMEDIATE SUMMATION FOR 95% CI
C             TMP    (R) = INTERMEDIATE CALCULATION
C             VAR    (R) = INTERMEDIATE CALCULATION
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
         INTEGER              NPV           ,NS            ,WFSF
         DIMENSION            CDAT(NS)      ,LDAT(NS)      ,MDAT(NS)
     1                       ,PDAT(NPV)
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
         IF ((NPV.EQ.1) .OR. (NPV.EQ.3)) THEN
C            COMPUTE THE  ESTIMATE OF THE  NUMBER OF FAULTS  ON THE NEXT
C            TESTING PERIOD.  THE EXPECTED TESTING  LENGTH MUST FIRST BE
C            WEIGHTED IN ACCORDANCE TO THE WEIGHTING FUNCTION SEL. FLAG.
             IF (WFSF.EQ.1) THEN
                 EXLW = EXL**2 / 2.0
             ELSE
                 EXLW = EXL**ALPHA
             ENDIF
             PDAT(1) = PCON * (TNOF - (MDAT(NS) + LCOR)) * EXLW
C
             IF (NPV.EQ.3) THEN
C                THE MAXIMUM LIKELIHOOD METHOD  WITH ALPHA NOT ESTIMATED
C                WAS PERFORMED; COMPUTE THE ADDITIONAL 95% CI.
                 SC  = 0.0
                 SL  = 0.0
                 SLM = 0.0
                 DO 1000 I = 1, NS
                     IF (WFSF.EQ.1) THEN
                         LDW = LDAT(I)**2 / 2.0
                     ELSE
                         LDW = LDAT(I)**ALPHA
                     ENDIF
                     SC  = SC  + CDAT(I)
                     SL  = SL  + LDW
                     SLM = SLM + LDW / (TNOF - MDAT(I))
 1000                CONTINUE
C                ENDDO
C
C                COMPUTE D.
                 D = (1.0 / PCON) * SC * SLM - SL**2
C
C                COMPUTE THE CI OF THE  PREDICTION FOR  THE NEXT TESTING
C                PERIOD.
                 TMP     = TNOF - (MDAT(NS) + LCOR)
                 EXLW    = EXLW**2
                 VAR     = (TMP**2 / D * EXLW * PCON * SLM)
     1                   - (2.0 * PCON * TMP / D * EXLW * SL)
     2                   + (EXLW * SC / D)
                 PDAT(2) = PDAT(1) - 1.95 * SQRT(VAR)
                 IF (PDAT(2).LT.0.0) THEN
                     PDAT(2) = 0.0
                 ENDIF
                 PDAT(3) = PDAT(1) + 1.95 * SQRT(VAR)
             ENDIF
         ELSE
C            COMPUTE THE ESTIMATES FOR THE ENTIRE FAULT COUNT VECTOR.
             DO 1025 I = 1, NS
                 IF (WFSF.EQ.1) THEN
                     LDW = LDAT(I)**2 / 2.0
                 ELSE
                     LDW = LDAT(I)**ALPHA
                 ENDIF
                 PDAT(I) = PCON * (TNOF - MDAT(I)) * LDW
 1025            CONTINUE
C            ENDDO
         ENDIF
         RETURN
         END
         SUBROUTINE SGPOTR
     G                     (CDAT  , LDAT  , MAXIC , MDAT  , NS    , SC
     G                     ,SCAL  , XMAX  , XMIN
     B                     ,XP
     Y                     ,COUNT , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SGPOTR
C    G                     (CDAT  , LDAT  , MAXIC , MDAT  , NS    , SC
C    G                     ,SCAL  , XMAX  , XMIN
C    B                     ,XP
C    Y                     ,COUNT , RFLAG )
C
C   AUTHOR
C        JAMES S. VANDERGRAFT (COMPUTATIONAL ENGINEERING, INC)
C   CODE STRUCTURING BY
C        PAUL J. KRAUS & CINDY A. WELLS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO CALCULATE THE ESTIMATES IN  THE  GENERALIZED  POISSON  MODEL
C        USING THE TRUST REGION MINIMIZATION ALGORITHM.
C   DESCRIPTION
C        THIS  ROUTINE IS  USED  BY  THE  MAXIMUM  LIKELIHOOD  FUNCTION.
C        ONCE  THE  ITERATIVE  EXECUTION  IS  INITIATED, PROCESSING WILL
C        CONTINUE UNTIL ONE OF THE THREE POSSIBLE TERMINATION  STATES IS
C        REACHED.  THE LAST ARGUMENT OF THE CALL LINE  (RFLAG)  WILL  BE
C        SET TO INDICATE TO THE CALLING ROUTINE THE REASON FOR EXIT. THE
C        POSSIBLE REASONS FOR SGPOTR EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             1 - MAXIMUM ITERATIONS
C             2 - THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY
C        THE NUMBER OF ITERATIONS NEEDED FOR SUCCESSFUL CONVERGENCE WILL
C        BE RETURNED IN THE NEXT TO LAST ARGUMENT (COUNT).
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS
C             MDAT   (R) = CUMULATIVE FAULT CORRECTIONS DATA VECTOR
C             NS     (I) = NUMBER OF ENTRIES
C             SC     (R) = SUM (CDAT(I))
C             SCAL   (R) = SUM (CDAT(I) * LOG(LDAT(I)))
C             XMAX   (R) = UPPER BOUNDS OF THE SOLUTION
C             XMIN   (R) = LOWER BOUNDS OF THE SOLUTION
C        BOTH
C             XP     (R) = PARAMETER ESTIMATE VALUES
C        YIELDED
C             COUNT  (I) = ITERATION COUNTER
C             RFLAG  (I) = RETURN STATUS FLAG
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
C             N      (I) = NUMBER OF UNKNOWN PARAMETERS
C             NDIFF  (R) = FUNCTION VALUE AT DIFFERENCE
C             NEWSTP (R) = NEWTON STEP VALUE
C             NGRAD  (R) = GRADIENT VALUE AT DIFFERENCE
C             R      (R) = TRUST REGION RADIUS
C             RNEW   (R) = TRUST REGION RADIUS FACTOR FOR 1ST REDUCTION
C             RTMP   (R) = INTERMEDIATE TRUST REGION RADIUS
C             S      (R) = DIAGONALS OF THE HESSIAN INVERSE
C             TAU    (R) = TRUST REGION RADIUS TAU VALUE
C             TMPMI  (R) = INTERMEDIATE TRUST REGION MINIMUM
C             TMPMX  (R) = INTERMEDIATE TRUST REGION MAXIMUM
C             TMAX   (R) = TRUST REGION MAXIMUM
C             TMIN   (R) = TRUST REGION MINIMUM
C             U      (R) = UPPER TRIANGULAR FACTORS OF HESSIAN
C             Y      (R) = UPDATED PARAMETER ESTIMATE VALUES
C   ERRORS
C        RFLAG  = 1 : MAXIMUM ITERATIONS
C        RFLAG  = 2 : THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY
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
         INTEGER              COUNT         ,MAXIC         ,NS
     1                       ,RFLAG
         DIMENSION            CDAT(NS)      ,LDAT(NS)      ,MDAT(NS)
     1                       ,XMAX(3)       ,XMIN(3)       ,XP(3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,IRED          ,J
     1                       ,N
         DIMENSION            G(3)          ,H(3,3)        ,NEWSTP(3)
     1                       ,S(3)          ,TMAX(3)       ,TMIN(3)
     2                       ,U(3,3)        ,Y(3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE COUNTER, RETURN STATUS FLAG, FUNCTION VALUE  DIFFERENCE
C        AND GRADIENT VALUE DIFFERENCE.
         COUNT =  1
         RFLAG = -1
         EPSD  = 1.E-4
         EPSG  = 1.E-8
         N     = 2
C
C        INITIALIZE THE TRUST REGION.
         CALL SGPOML
     G               (XP    , CDAT  , LDAT  , MDAT  , NS    , SC
     Y               ,FX    )
         CALL SGPOGD
     G               (XP    , CDAT  , LDAT  , MDAT  , NS    , SC
     G               ,SCAL
     Y               ,G     )
         CALL SGPOHS
     G               (XP    , CDAT  , LDAT  , MDAT  , NS    , SC
     Y               ,H     )
C
C        COMPUTE THE NEWTON STEP.
         CALL FACTOR
     G               (H     , N
     Y               ,U     )
         CALL SOLVE
     G               (N     , U     , G
     Y               ,NEWSTP)
C
C        COMPUTE THE DIAGONALS OF H INVERSE.
         DO 1050 I = 1, N
             DO 1025 J = 1, N
                 Y(J) = 0.0
 1025            CONTINUE
C            ENDDO
             Y(I) = 1.0
             CALL SOLVE
     G                  (N     , U     , Y
     Y                  ,Y     )
             S(I) = SQRT(Y(I))
 1050        CONTINUE
C        ENDDO
C
C        COMPUTE THE TRUST REGION RADIUS.
         R = 0.0
         DO 1075 I = 1, N
             RTMP = ABS(NEWSTP(I) / S(I))
             R    = MAX(R,RTMP)
 1075        CONTINUE
C        ENDDO
         R = 0.5 * R
C
C        DOWHILE (RFLAG .EQ. -1)
 1100        IRED = 0
             IF (COUNT.NE.1) THEN
                 CALL SGPOHS
     G                       (XP    , CDAT  , LDAT  , MDAT  , NS    , SC
     Y                       ,H     )
             ENDIF
C
C            COMPUTE THE MINIMUM WITHIN THE TRUST REGION OF THE QUADRAT-
C            IC MODEL.
C            DOWHILE ((IRED .LE. 10) .AND. (FY .GT. FUNUP))
 1125            CONTINUE
C
C                SET UP THE TRUST REGION BOUNDARIES.
                 DO 1150 I = 1, N
                     TMPMI   = XP(I) - R * S(I)
                     TMPMX   = XP(I) + R * S(I)
                     TMIN(I) = MAX(XMIN(I),TMPMI)
                     TMAX(I) = MIN(XMAX(I),TMPMX)
 1150                CONTINUE
C                ENDDO
C
C                COMPUTE THE MINIMUM WITHIN THE TRUST REGION.
                 CALL CONMIN
     G                       (G     , H     , N     , TMAX  , TMIN  , XP
     Y                       ,Y     )
                 CALL SGPOML
     G                       (Y     , CDAT  , LDAT  , MDAT  , NS    , SC
     Y                       ,FY    )
C
C                CHECK WHETHER THE FUNCTION HAS DECREASED SUFFICIENTLY.
                 GPROD = 0.0
                 DO 1175 I = 1, N
                     GPROD = GPROD + G(I) * (Y(I) - XP(I))
 1175                CONTINUE
C                ENDDO
                 FUNUP = FX + 0.0001 * GPROD
C
                 IF (FY.GT.FUNUP) THEN
C                    REDUCE THE TRUST REGION AND TRY AGAIN.
                     IRED = IRED + 1
C
                     IF (IRED.EQ.1) THEN
C                        MAKE  SURE  THE  FIRST  REDUCTION  IS ENOUGH TO
C                        CAUSE A CHANGE IN THE CONSTRAINED MINIMUM.
                         RNEW = R
                         DO 1200 I = 1, N
                             FACT = ABS(XP(I) - Y(I))
                             IF (FACT.NE.0.0) THEN
                                 IF (FACT.LT.(RNEW * S(I))) THEN
                                     RNEW = FACT / S(I)
                                 ENDIF
                             ENDIF
 1200                        CONTINUE
C                        ENDDO
                         RTMP = R
                         IF (RNEW.LT.R) THEN
                             R = 0.9 * RNEW
                         ENDIF
                     ENDIF
C
C                    REDUCE THE TRUST REGION RADIUS.
                     IF (((IRED.EQ.1) .AND. (RNEW.GE.RTMP)) .OR.
     1                   ((IRED.GT.1) .AND. (IRED.LE.10)))  THEN
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
                 IF ((IRED.LE.10) .AND. (FY.GT.FUNUP)) GO TO 1125
C            ENDWHILE
C
C            SET RFLAG IF TRUST REGION COULD NOT BE ADJUSTED PROPERLY.
             IF (IRED.GT.10) THEN
                 RFLAG = 2
             ELSE
C                AN ACCEPTABLE POINT, Y HAS  BEEN  FOUND.  SET  RFLAG IF
C                MAXIMUM ITERATIONS WAS REACHED.
                 COUNT = COUNT + 1
                 IF (COUNT.GT.MAXIC) THEN
                     RFLAG = 1
                 ELSE
C                    EVALUATE THE GRADIENT AT THE NEW POINT.
                     CALL SGPOGD
     G                           (Y     , CDAT  , LDAT  , MDAT  , NS
     G                           ,SC    , SCAL
     Y                           ,G     )
C
C                    TO TEST FOR CONVERGENCE, COMPUTE THE  NORM  OF  THE
C                    GRADIENT AND  THE  NORM  OF THE  DIFFERENCE BETWEEN
C                    THIS POINT AND THE PREVIOUS POINT.
                     NDIFF = 0.0
                     NGRAD = 0.0
                     DO 1225 I = 1, N
                         NDIFF = NDIFF + (XP(I) - Y(I))**2
                         NGRAD = NGRAD + G(I)**2
 1225                    CONTINUE
C                    ENDDO
                     NDIFF = SQRT(NDIFF)
                     NGRAD = SQRT(NGRAD)
C
C                    SET RFLAG IF CONVERGENCE OCCURRED.
                     IF ((NDIFF.LT.EPSD) .OR. (NGRAD.LT.EPSG)) THEN
                         RFLAG = 0
                     ELSE
C                        ADJUST THE TRUST REGION.
                         HPROD = 0.0
                         DO 1275 I = 1, N
                             DO 1250 J  = 1, N
                                 HPROD = HPROD + H(I,J)
     1                                 * (Y(I) - XP(I))
     2                                 * (Y(J) - XP(J))
 1250                            CONTINUE
C                            ENDDO
 1275                        CONTINUE
C                        ENDDO
                         ERR = ABS(FX + GPROD + 0.5 * HPROD - FY)
                         IF (ERR.LE.(0.1 * ABS(FY))) THEN
                             R = 2.0 * R
                         ENDIF
                         IF (ERR.GE.(0.75 * ABS(FY))) THEN
                             R = 0.5 * R
                         ENDIF
C
C                        SWAP POINTS AND FUNCTION VALUES.
                         DO 1300 I = 1, N
                             XP(I) = Y(I)
 1300                        CONTINUE
C                        ENDDO
                         FX = FY
                     ENDIF
                 ENDIF
             ENDIF
             IF (RFLAG.EQ.-1) GO TO 1100
C        ENDWHILE
         RETURN
         END
         FUNCTION SNPIFN
     G                   (AB    , CDAT  , CTOT  , ESF   , LDAT
     G                   ,NS    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION SNPIFN
C    G                   (AB    , CDAT  , CTOT  , ESF   , LDAT
C    G                   ,NS    )
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
C        TO EVALUATE THE  FUNCTION  USED BY THE  NON-HOMOGENOUS  POISSON
C        MODEL, FOR THE  FAULT COUNT DATA.  IF ESF=1, THE  MAXIMUM LIKE-
C        LIHOOD  ESTIMATION  METHOD  IS  USED  AND  IF  ESF=2, THE LEAST
C        SQUARES ESTIMATION METHOD IS USED.
C   DESCRIPTION
C        THIS ROUTINE CALCULATES THE VALUE OF THE FUNCTION AT THE  POINT
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             CTOT   (R) = TOTAL FAULTS FOUND TO DATE
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             NS     (I) = SIZE OF CDAT AND LDAT
C        YIELDED
C             SNPIFN (R) = FUNCTION VALUE AT POINT AB
C   LOCAL GLOSSARY
C             EXPI   (R) = INTERMEDIATE EXPONENT CALCULATION 1
C             EXPJ   (R) = INTERMEDIATE EXPONENT CALCULATION 2
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
         INTEGER              ESF           ,NS
         DIMENSION            CDAT(NS)      ,LDAT(NS)
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
         EXPI = EXP(-AB * LDAT(1))
C
C        INITIALIZE THE SUMMATION VARIABLE(S) FOR THE MAXIMUM LIKELIHOOD
C        FUNCTION OR THE LEAST SQUARES FUNCTION.
         IF (ESF.EQ.1) THEN
             TMP = CDAT(1) * LDAT(1) * EXPI / (1.0 - EXPI)
         ELSE
             SM1 = CDAT(1) * LDAT(1) * EXPI
             SM2 = CDAT(1) * (1.0    - EXPI)
             SM3 = (1.0    - EXPI)**2
             SM4 = (1.0    - EXPI)   * LDAT(1) * EXPI
         ENDIF
C
C        PERFORM THE SUMMATIONS FOR THE  MAXIMUM LIKELIHOOD  FUNCTION OR
C        THE LEAST SQUARES FUNCTION.
         DO 1000 I = 2, NS
             J    = I - 1
             EXPI = EXP(-AB * LDAT(I))
             EXPJ = EXP(-AB * LDAT(J))
             IF (ESF.EQ.1) THEN
                 TMP = TMP     +  CDAT(I) * (LDAT(I) * EXPI
     1               - LDAT(J) *  EXPJ)   / (EXPJ    - EXPI)
             ELSE
                 SM1 = SM1     +  CDAT(I) * (LDAT(I) * EXPI
     1               - LDAT(J) *  EXPJ)
                 SM2 = SM2     +  CDAT(I) * (EXPJ    - EXPI)
                 SM3 = SM3     + (EXPJ    - EXPI)**2
                 SM4 = SM4     + (LDAT(I) * EXPI     - LDAT(J) * EXPJ)
     1               * (EXPJ   -  EXPI)
             ENDIF
 1000        CONTINUE
C        ENDDO
C
C        CALCULATE THE FUNCTION VALUE.
         IF (ESF.EQ.1) THEN
             EXPI   = EXP(-AB * LDAT(NS))
             SNPIFN = TMP - LDAT(NS)  * EXPI * CTOT / (1.0 - EXPI)
         ELSE
             SNPIFN = SM1 * SM3 - SM2 * SM4
         ENDIF
         RETURN
         END
         SUBROUTINE SNPIMA
     G                     (CDAT  , LDAT  , NS    , NSB   , NSE   , NSR
     Y                     ,STAT  , RFLAG , INDX  , V     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SNPIMA
C    G                     (CDAT  , LDAT  , NS    , NSB   , NSE   , NSR
C    Y                     ,STAT  , RFLAG , INDX  , V     )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM THE PREQUENTIAL LIKELIHOOD ANALYSIS  FOR THE NON-HO-
C        MOGENEOUS POISSON MODEL.
C   DESCRIPTION
C        THE ROUTINE  CONTROLS THE EXECUTIONS  OF THE  APPLICABLE SMFLIB
C        ROUTINE, SNPIMD.  THE PROCESSING, AS DEFINED  IN THE LITTLEWOOD
C        PAPER (SEE REFERENCES IN THE MAIN PROGRAM) BASICALLY INVOLVES A
C        SERIES OF EXECUTIONS WHERE  NSB TO NSE DATA POINTS ARE UTILIZED
C        IN THE MODEL.
C
C        FOR EACH ITERATION,  THE MODEL PARAMETERS (ESTIMATES)  FROM THE
C        CURRENT ITERATION ARE USED IN CONJUNCTION WITH THE OBSERVED DA-
C        TA FROM THE NEXT ITERATION POINT  TO DETERMINE AN OVER-ALL PIC-
C        TURE OF THE APPLICABILITY OF THE MODEL  FOR THE PARTICULAR DATA
C        SET.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS ROUTINE IS CURRENTLY SET FOR ONLY ML EXECUTIONS.
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             CDAT   (R) = ARRAY TO HOLD OBSERVED FAULT COUNT DATA
C             LDAT   (R) = ARRAY TO HOLD INTERVAL TESTING LENGTHS (CUM)
C             NS     (I) = THE NUMBER OF ENTRIES
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C        YIELDED
C             INDX   (I) = ANALYSIS ITERATION COUNTER (AT RETURN)
C             RFLAG  (I) = MODEL RESULTS ERROR FLAG
C             STAT   (R) = PREQUENTIAL LIKELIHOOD STATISTIC
C             V      (R) = ANALYSIS RESULTS ARRAY OF SIZE NSR
C   LOCAL GLOSSARY
C             ESF    (I) = ESTIMATION SELECTION FLAG (ML ONLY)
C             LOGFX  (R) = LOG(X(I))!
C             STATS  (R) = STATISTICS RESULTS ARRAY
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
C                             GLOBAL SPECIFICATIONS
C        (NONE)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              INDX          ,NS            ,NSB
     1                       ,NSE           ,NSR           ,RFLAG
         DIMENSION            CDAT(NS)      ,LDAT(NS)      ,V(NSR)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              ESF           ,I             ,J
     1                       ,K
         DIMENSION            STATS(2,3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE THE P.L. ITERATION COUNTER, STORAGE INDEX, THE MODEL
C        ERROR FLAG, AND THE P.L. STATISTIC.
         I      = NSB - 1
         INDX   = 0
         RFLAG  = 0
         STAT   = 0.0
C
C        INITIALIZE THE MODEL SELECTION FLAG TO INDICATE MAXIMUM LIKELI-
C        HOOD ANALYSIS.
         ESF    = 1
C
C        DOWHILE (PROCESSING LEFT AND NO ERRORS)
 1000        I  = I + 1
             IF ((I.GT.NSE) .OR. (RFLAG.NE.0)) GO TO 1050
C
C            COMPUTE MODEL ESTIMATES.
             CALL SNPIMD
     G                   (ESF   , CDAT  , LDAT  , I
     Y                   ,STATS , RFLAG )
C
C            INCREMENT THE ITERATION COUNTER FOR STORAGE.
             INDX = INDX + 1
C
             IF (RFLAG.EQ.0) THEN
                 TMP1 = EXP(-STATS(1,1) * LDAT(I  ))
                 TMP2 = EXP(-STATS(1,1) * LDAT(I+1))
C
C                CALCULATE THE SUM FOR THE LOG(DAT(I))!.
                 K     = INT(CDAT(I+1))
                 LOGFX = 0.0
                 DO 1025 J = 1, K
                     LOGFX = LOGFX + LOG(REAL(J))
 1025                CONTINUE
C                ENDDO
C
                 V(INDX) = 0.0
     1                   - (CDAT(I+1)  * LOG(STATS(2,1))
     2                   +  CDAT(I+1)  * LOG(TMP1 - TMP2)
     3                   +  STATS(2,1) *    (TMP2 - TMP1)
     4                   -  LOGFX)
                 STAT    = STAT + V(INDX)
             ENDIF
             GO TO 1000
 1050        CONTINUE
C        ENDWHILE
         RETURN
         END
         SUBROUTINE SNPIMD
     G                     (ESF   , CDAT  , LDAT  , NS
     Y                     ,STATS , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SNPIMD
C    G                     (ESF   , CDAT  , LDAT  , NS
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
C        LEAST SQUARES METHODS OF THE NON-HOMOGENEOUS POISSON MODEL.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE CALL LINE (ESF)  WILL BE USED BY THE
C        ROUTINE TO DETERMINE  WHICH OF THE TWO METHODS  OF EXECUTION IS
C        DESIRED.
C
C        ONCE THE ITERATIVE EXECUTION IS INITIATED, PROCESSING WILL CON-
C        TINUE UNTIL ONE OF THREE POSSIBLE TERMINATION STATES IS  REACH-
C        ED. THE LAST ARGUMENT OF THE CALL LINE   (RFLAG) WILL BE SET TO
C        INDICATE TO THE CALLING ROUTINE  THE REASON FOR EXIT.  THE POS-
C        SIBLE REASONS FOR SNPIMD EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS  THAN THE OBSERVED
C                 FAULTS TO DATE
C
C        THE RESULTANT STATISTICS WILL BE RETURNED IN THE STATS ARGUMENT
C        FOR A RFLAG VALUE OF 0 ONLY.  FOR THE  MAXIMUM LIKELIHOOD METH-
C        OD THIS WILL INCLUDE THE TWO  STATISTICS AND THEIR  95%  CONFI-
C        DENCE INTERVALS  (CI).   UNDER THE LEAST SQUARES METHOD, THE CI
C        WILL NOT BE COMPUTED.
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             NS     (I) = SIZE OF CDAT AND LDAT
C        YIELDED
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = RESULTS ARRAY
C   LOCAL GLOSSARY
C             A      (R) = LOWER BOUND OF PROPORTIONALITY CONSTANT
C             B      (R) = UPPER BOUND OF PROPORTIONALITY CONSTANT
C             CTOT   (R) = TOTAL FAULTS FOUND TO DATE
C             COUNT  (I) = ITERATION COUNTER
C             D      (R) = DELTA VALUE FOR 95% CI COMPUTATIONS
C             EXPI   (R) = INTERMEDIATE VALUE IN STATS ASSIGNMENT
C             EXPJ   (R) = INTERMEDIATE VALUE IN STATS ASSIGNMENT
C             FA     (R) = VALUE OF FUNCTION AT A
C             FB     (R) = VALUE OF FUNCTION AT B
C             FC     (R) = VALUE OF FUNCTION AT C
C             ICOUNT (I) = STEP COUNTER
C             IFLAG  (I) = CONVERGENCE FLAG
C             S1     (R) = SUMMATION VARIABLE OVER TESTING PERIODS
C             S2     (R) = SUMMATION VARIABLE OVER TESTING PERIODS
C             TMP    (R) = INTERMEDIATE CALCULATION
C             XAN    (R) = TEMPORARY "AN" HOLDER
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
         INTEGER              ESF           ,NS            ,RFLAG
         DIMENSION            CDAT(NS)      ,LDAT(NS)      ,STATS(2,3)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              COUNT         ,I             ,ICOUNT
     1                       ,IFLAG         ,J
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE COUNTER AND RETURN STATUS FLAG.
         COUNT = 0
         RFLAG = -1
C
C        ZERO ALL LOCATIONS OF THE RESULTS VECTOR.
         DO 1000 I = 1, 3
             STATS(1,I) = 0.0
             STATS(2,I) = 0.0
 1000        CONTINUE
C        ENDDO
C
C        COMPUTE THE TOTAL NUMBER OF FAULTS FOUND TO DATE.
         CTOT = 0.0
         DO 1025 I = 1, NS
             CTOT = CTOT + CDAT(I)
 1025        CONTINUE
C        ENDDO
C
C        COMPUTE THE VALUE OF THE FUNCTION AT ITS UPPER BOUND.
         B = REAL(NS) / LDAT(NS)
         IF (B.GT.1.0) THEN
             B  = 1.0
         ENDIF
         FB = SNPIFN
     G               (B     , CDAT  , CTOT  , ESF   , LDAT  , NS    )
         A = B * 0.1
C
C        COMPUTE THE VALUE OF THE FUNCTION AT A POINT WHERE ITS SIGN  IS
C        THE OPPOSITE OF THE  COMPUTED  VALUE  OF  THE  FUNCTIONS  UPPER
C        BOUND.
C
C        DOWHILE (RFLAG .EQ. -1)
 1050        COUNT = COUNT + 1
             FA    = SNPIFN
     G                      (A     , CDAT  , CTOT  , ESF   , LDAT
     G                      ,NS    )
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
                 A = A * 0.1
             ENDIF
             IF (RFLAG.EQ.-1) GO TO 1050
C        ENDWHILE
C
         IF (RFLAG.EQ.0) THEN
C            COMPUTE THE ESTIMATE  OF  THE  PROPORTIONALITY CONSTANT AND
C            STORE THE VALUE IN STATS(1,1).
             ICOUNT = 1
             IFLAG  = -1
C
C            DOWHILE (IFLAG .EQ. -1)
C                COMPUTE THE ZERO OF THE FUNCTION IN THE INTERVAL  A AND
C                B; NOTE THE ARGUMENT FC IS INITIALIZED BY THE ZERO ROU-
C                TINE WHEN ICOUNT IS SET TO ONE.   <<SEE RESTRICTIONS <<
 1075            CALL ZERO
     B                     (A     , B     , FA    , FB    , FC
     B                     ,ICOUNT
     Y                     ,IFLAG )
C
                 IF (IFLAG.EQ.-1) THEN
C                    COMPUTE THE VALUE OF THE FUNCTION AT ITS NEW  UPPER
C                    BOUND.
                     FB = SNPIFN
     G                           (B     , CDAT  , CTOT  , ESF   , LDAT
     G                           ,NS    )
                 ENDIF
                 IF (IFLAG.EQ.-1) GO TO 1075
C            ENDWHILE
             STATS(1,1) = B
         ENDIF
C
C        THE STATISTICS CALCULATIONS  WILL ONLY BE MADE FOR RFLAGS INDI-
C        CATING SUCCESSFUL CONVERGENCE.
         IF (RFLAG.EQ.0) THEN
             IF (ESF.EQ.1) THEN
C                COMPUTE THE  STATISTICS FOR  THE MAXIMUM LIKELIHOOD ME-
C                THOD, STARTING WITH THE TOTAL NUMBER OF FAULTS.
                 STATS(2,1) = CTOT / (1.0 - EXP(-STATS(1,1) * LDAT(NS)))
C
C                CALCULATE THE SUMMATION NEEDED FOR THE 95%  CI COMPUTA-
C                TIONS.
                 EXPI = EXP(-STATS(1,1) * LDAT(1))
                 S1   = LDAT(1)**2 * EXPI / (1.0 - EXPI)
                 DO 1100 I = 2, NS
                     J    = I - 1
                     EXPI = EXP(-STATS(1,1) * LDAT(I))
                     EXPJ = EXP(-STATS(1,1) * LDAT(J))
                     S1   = S1 + (LDAT(I)   - LDAT(J))**2
     1                    * EXP(-STATS(1,1) * (LDAT(I) + LDAT(J)))
     2                    / (EXPJ - EXPI)
 1100                CONTINUE
C                ENDDO
C
C                COMPUTE THE DELTA VALUE.
                 D = (1.0 - EXPI) * (S1 - LDAT(NS)**2 * EXPI)
     1             - (LDAT(NS)**2 * EXPI**2)
C
C                COMPUTE THE 95% CI ABOUT THE PROPORTIONALITY CONSTANT.
                 TMP        = 1.95 * SQRT((1.0 - EXPI) / STATS(2,1)
     1                      * (1.0 / D))
                 STATS(1,2) = STATS(1,1) - TMP
                 STATS(1,3) = STATS(1,1) + TMP
                 IF (STATS(1,2).LT.0.0) THEN
                     STATS(1,2) = 0.0
                 ENDIF
C
C                COMPUTE THE 95% CI ABOUT THE TOTAL NUMBER OF FAULTS.
                 TMP        = 1.95 * SQRT(STATS(2,1) / D
     1                      * (S1 - LDAT(NS)**2 * EXPI))
                 STATS(2,2) = STATS(2,1) - TMP
                 STATS(2,3) = STATS(2,1) + TMP
                 IF (STATS(2,2).LT.CTOT) THEN
                     STATS(2,2) = CTOT
                 ENDIF
             ELSE
C                COMPUTE THE ESTIMATE OF THE  TOTAL NUMBER OF FAULTS US-
C                ING THE  LEAST SQUARES METHOD OF  EXECUTION; AND VERIFY
C                THE ESTIMATE TO BE VALID.
                 TMP = (1.0 - EXP(-STATS(1,1) * LDAT(1)))
                 S1  = CDAT(1) * TMP
                 S2  = TMP**2
                 DO 1125 I = 2, NS
                     J    = I - 1
                     EXPI = EXP(-STATS(1,1) * LDAT(I))
                     EXPJ = EXP(-STATS(1,1) * LDAT(J))
                     S1   = S1 + (CDAT(I) * (EXPJ - EXPI))
                     S2   = S2 + (EXPJ - EXPI)**2
 1125                CONTINUE
C                ENDDO
                 STATS(2,1) = S1 / S2
             ENDIF
             IF (STATS(2,1).LT.CTOT) THEN
                 RFLAG = 4
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE SNPIPR
     G                     (NPV   , NS    , TNOF  , PCON
     G                     ,LDAT  , EXL
     Y                     ,PDAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SNPIPR
C    G                     (NPV   , NS    , TNOF  , PCON
C    G                     ,LDAT  , EXL
C    Y                     ,PDAT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO COMPUTE THE  PREDICTED NUMBER  OF FAULTS IN THE NEXT TESTING
C        PERIOD AND TO CREATE THE  PREDICTED FAULT COUNT DATA VECTOR FOR
C        THE NON-HOMOGENEOUS POISSON MODEL.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE SNPIPR  CALL LINE  INDICATES WHETHER
C        THE PREDICTED  NUMBER OF FAULTS  IN THE NEXT TESTING  PERIOD OR
C        THE ENTIRE FAULT COUNT VECTOR IS TO BE COMPUTED.  IF NPV IS SET
C        TO 1, THE ROUTINE  WILL COMPUTE  THE ONE PREDICTED  VALUE USING
C        THE ARGUMENTS  TNOF,  PCON,  EXL, AND THE NS  ELEMENT  OF LDAT.
C        OTHERWISE, THE  ROUTINE WILL COMPUTE THE  ENTIRE DATA VECTOR OF
C        FAULT COUNTS USING THE  ARGUMENTS TNOF,  PCON, AND  ALL NS ELE-
C        MENTS OF LDAT.
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
C             EXL    (R) = EXPECTED TESTING LENGTH OF NEXT PERIOD
C             LDAT   (R) = INTERVAL TESTING LENGTH DATA VECTOR
C             NPV    (I) = NUMBER OF PREDICTED VALUES DESIRED
C             NS     (I) = SIZE OF LDAT
C             PCON   (R) = ESTIMATE OF THE PROPORTIONALITY CONSTANT
C             TNOF   (R) = ESTIMATE OF THE TOTAL NUMBER OF FAULTS
C        YIELDED
C             PDAT   (R) = VECTOR OF PREDICTED DATA VALUE(S)
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
         INTEGER              NPV           ,NS
         DIMENSION            LDAT(NS)      ,PDAT(NPV)
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
C            COMPUTE THE  ESTIMATE OF THE  NUMBER OF FAULTS  IN THE NEXT
C            TESTING PERIOD.
             PDAT(1) = TNOF * (EXP(-PCON * LDAT(NS))
     1               - EXP(-PCON * (LDAT(NS) + EXL)))
         ELSE
C            COMPUTE THE ESTIMATES FOR THE ENTIRE FAULT COUNT VECTOR.
             PDAT(1) = TNOF * (1.0 - EXP(-PCON * LDAT(1)))
             DO 1000 I = 2, NS
                 PDAT(I) = TNOF * (EXP(-PCON * LDAT(I-1))
     1                   - EXP(-PCON * LDAT(I)))
 1000            CONTINUE
C            ENDDO
         ENDIF
         RETURN
         END
         SUBROUTINE SSDWMA
     G                     (CDAT  , NS    , NSB   , NSE   , NSR
     Y                     ,STAT  , RFLAG , INDX  , V     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SSDWMA
C    G                     (CDAT  , NS    , NSB   , NSE   , NSR
C    Y                     ,STAT  , RFLAG , INDX  , V     )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM THE PREQUENTIAL LIKELIHOOD ANALYSIS  FOR THE SCHNEI-
C        DEWIND MODEL FOR INTERVAL DATA.
C   DESCRIPTION
C        THE ROUTINE  CONTROLS THE EXECUTIONS  OF THE  APPLICABLE SMFLIB
C        ROUTINE, SSDWMD.  THE PROCESSING, AS DEFINED  IN THE LITTLEWOOD
C        PAPER (SEE REFERENCES IN THE MAIN PROGRAM) BASICALLY INVOLVES A
C        SERIES OF EXECUTIONS WHERE  NSB TO NSE DATA POINTS ARE UTILIZED
C        IN THE MODEL.
C
C        FOR EACH ITERATION,  THE MODEL PARAMETERS (ESTIMATES)  FROM THE
C        CURRENT ITERATION ARE USED IN CONJUNCTION WITH THE OBSERVED DA-
C        TA FROM THE NEXT ITERATION POINT  TO DETERMINE AN OVER-ALL PIC-
C        TURE OF THE APPLICABILITY OF THE MODEL  FOR THE PARTICULAR DATA
C        SET.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS ROUTINE IS  CURRENTLY SET FOR ONLY TREATMENT TYPE 1 EXECU-
C        TIONS (USE ALL DATA).
C   PARAMETER GLOSSARY
C             (NONE)
C   ARGUMENT LIST
C        GIVEN
C             CDAT   (R) = ARRAY TO HOLD OBSERVED FAULT COUNT DATA
C             NS     (I) = THE NUMBER OF ENTRIES
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C        YIELDED
C             INDX   (I) = ANALYSIS ITERATION COUNTER (AT RETURN)
C             RFLAG  (I) = MODEL RESULTS ERROR FLAG
C             STAT   (R) = PREQUENTIAL LIKELIHOOD STATISTIC
C             V      (R) = ANALYSIS RESULTS ARRAY OF SIZE NSR
C   LOCAL GLOSSARY
C             LOGFX  (R) = LOG(X(I))
C             STATS  (R) = STATISTICAL RESULTS VECTOR
C             TMP    (R) = INTERMEDIATE CALCULATION
C             TSF    (I) = TREATMENT SELECTION FLAG (ONLY 1)
C             TSS    (I) = TREATMENT SELECTION STARTING POINT (ONLY 1)
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
         DIMENSION            CDAT(NS)      ,V(NSR)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J             ,K
     1                       ,TSF           ,TSS
         DIMENSION            STATS(6)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE THE P.L. ITERATION COUNTER, STORAGE INDEX, THE MODEL
C        ERROR FLAG, AND THE P.L. STATISTIC.
         I      = NSB - 1
         INDX   = 0
         RFLAG  = 0
         STAT   = 0.0
C
C        INITIALIZE THE TREATMENT SELECTION FLAG  AND THE STARTING INDEX
C        TO INDICATE ALL DATA POINTS ARE TO BE USED INDIVIDUALLY.
         TSF    = 1
         TSS    = 1
C
C        DOWHILE (PROCESSING LEFT AND NO ERRORS)
 1000        I  = I + 1
             IF ((I.GT.NSE) .OR. (RFLAG.NE.0)) GO TO 1050
C
C            COMPUTE MODEL ESTIMATES.
             CALL SSDWMD
     G                   (CDAT  , I     , TSF  , TSS
     Y                   ,STATS , RFLAG )
C
C            INCREMENT THE ITERATION COUNTER FOR STORAGE.
             INDX = INDX + 1
C
             IF (RFLAG.EQ.0) THEN
                 TMP  = STATS(2) / STATS(1)
     1                * (EXP(-STATS(1) * REAL(I  ))
     2                -  EXP(-STATS(1) * REAL(I+1)))
C
C                CALCULATE THE SUM FOR THE LOG(DAT(I))!.
                 K     = INT(CDAT(I+1))
                 LOGFX = 0.0
                 DO 1025 J = 1, K
                     LOGFX = LOGFX + LOG(REAL(J))
 1025                CONTINUE
C                ENDDO
C
                 V(INDX) = 0.0  - (CDAT(I+1) * LOG(TMP) - TMP - LOGFX)
                 STAT    = STAT + V(INDX)
             ENDIF
             GO TO 1000
 1050        CONTINUE
C        ENDWHILE
         RETURN
         END
         SUBROUTINE SSDWMD
     G                     (CDAT  , NS    , TSF   , TSS
     Y                     ,STATS , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SSDWMD
C    G                     (CDAT  , NS    , TSF   , TSS
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
C        TO COMPUTE THE ESTIMATES USING THE SCHNEIDEWIND MAXIMUM LIKELI-
C        HOOD MODEL USING THREE DIFFERENT POSSIBLE TREATMENT TYPES.
C   DESCRIPTION
C        THE THIRD ARGUMENT  OF THE CALL LINE (TSF)  WILL BE USED BY THE
C        ROUTINE (IN  CONJUNCTION WITH THE FOURTH ARGUMENT (TSS)) TO DE-
C        TERMINE WHICH OF THE THREE DATA TREATMENT TYPES IS DESIRED. THE
C        POSSIBLE DATA TREATMENT TYPES ARE AS FOLLOWS:
C             1 - ALL FAULT COUNTS ARE USED (TSS SHOULD BE SET TO 1).
C             2 - IGNORE THE FIRST TSS-1 FAULT COUNTS.
C             3 - USE THE SUM OF THE FIRST TSS-1 FAULT COUNTS AS THE 1ST
C                 POINT AND  USE THE INDIVIDUAL  FAULT COUNTS AS THE TSS
C                 THROUGH NS POINTS.
C
C        ONCE THE ITERATIVE EXECUTION IS INITIATED, PROCESSING WILL CON-
C        TINUE UNTIL  ONE OF TWO POSSIBLE TERMINATION STATES IS REACHED.
C        THE LAST ARGUMENT OF THE CALL LINE (RFLAG) WILL BE SET TO INDI-
C        CATE TO THE  CALLING ROUTINE THE REASON FOR EXIT.  THE POSSIBLE
C        REASONS FOR SSDWMD EXIT ARE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS  THAN THE OBSERVED
C                 FAULTS TO DATE
C
C        THE RESULTANT STATISTICS WILL BE RETURNED IN THE STATS ARGUMENT
C        FOR RFLAG VALUE OF 0 ONLY.  THE TNOF ELEMENT OF THE STATS ARRAY
C        CONTAINS AN EXTRA FLAG (OF -1.0) TO  INDICATE THAT THE TNOF ES-
C        TIMATE COULD NOT BE COMPUTED BECAUSE OF A ZERO BETA VALUE.  THE
C        ELEMENT (3) SHOULD ALWAYS BE CHECKED  BEFORE OUTPUT.  THE SIXTH
C        ELEMENT USES THE SAME FLAG TO INDICATE THAT ITS VALUE FOR MSE-T
C        COULD NOT BE COMPUTED BECAUSE OF A NON-POSITIVE NUMBER.
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             NS     (I) = SIZE OF CDAT
C             TSF    (I) = TREATMENT SELECTION FLAG
C             TSS    (I) = TREATMENT SELECTION STARTING POINT
C        YIELDED
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = RESULTS VECTOR
C   LOCAL GLOSSARY
C             A      (R) = SUMMATION OF SCALED CDAT ELEMENTS ABOVE STAR-
C                          TING POINT
C             ABTFLG (I) = MSE-T CALCULATION ABORT FLAG (LOG OF NON-POS-
C                          ITIVE NUMBER)
C             ADB    (R) = LOWER BOUND OF BETA CONSTANT
C             BDB    (R) = UPPER BOUND OF BETA CONSTANT
C             COUNT  (I) = ITERATION COUNTER
C             CTOT   (R) = TOTAL FAULTS FOUND TO DATE
C             F      (R) = SUMMATION OF CDAT ELEMENTS ABOVE STARTING PT.
C             FA     (R) = VALUE OF FUNCTION AT A
C             FB     (R) = VALUE OF FUNCTION AT B
C             FC     (R) = VALUE OF FUNCTION AT C
C             ICOUNT (I) = STEP COUNTER
C             IDXLST (I) = INDEX TO THE LAST NON-ZERO INTERVAL
C             IFLAG  (I) = CONVERGENCE FLAG
C             R      (R) = SUMMATION OF CDAT ELEMENTS BELOW STARTING PT.
C                          FOR DATA TREATMENT TYPE NUMBER 3
C             SPCS   (I) = SPECIAL "S" FOR SUMMATIONS AND DIVISIONS OVER
C                          THE THREE TREATMENT TYPES, WHERE:
C                          1) FOR TREATMENT 1, SPCS = 0
C                          2) FOR TREATMENT 2, SPCS = S-1
C                          3) FOR TREATMENT 3, SPCS = S-2
C             SUMF   (R) = SUM OF THE FAULT COUNTS FOR MSE-T CALCULATION
C             TMP    (R) = INTERMEDIATE CALCULATION
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
         INTEGER              NS            ,RFLAG         ,TSF
     1                       ,TSS
         DIMENSION            CDAT(NS)      ,STATS(6)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              ABTFLG        ,COUNT         ,I
     1                       ,ICOUNT        ,IDXLST        ,IFLAG
     2                       ,J             ,SPCS
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE COUNTER AND RETURN STATUS FLAG.
         COUNT = 0
         RFLAG = -1
C
C        INITIALIZE THE RESULTS VECTOR (SEE PROLOGUE).
         STATS(1) =  0.0
         STATS(2) =  0.0
         STATS(3) = -1.0
         STATS(4) =  0.0
         STATS(5) =  0.0
         STATS(6) =  0.0
C
C        DETERMINE THE TOTAL FAULTS FOUND TO DATE (ALL PERIODS).
         CTOT = 0.0
         DO 1000 I = 1, NS
             CTOT = CTOT + CDAT(I)
 1000        CONTINUE
C        ENDDO
C
C        INITIALIZE THE VARIABLES USED FOR DATA SUMMATIONS.
         A = 0.0
         F = 0.0
         R = 0.0
C
C        COMPUTE THE SUM OF THE CDAT ELEMENTS ABOVE THE STARTING POINT.
         DO 1005 I = TSS, NS
            F = F + CDAT(I)
 1005       CONTINUE
C        ENDDO
C
         IF (TSF.EQ.2) THEN
C            COMPUTE THE SCALED SUM OF ELEMENTS ABOVE STARTING POINT.
             DO 1025 I = TSS, NS
                 A = A + REAL(I - TSS) * CDAT(I)
 1025            CONTINUE
C            ENDDO
         ELSE
C            COMPUTE THE SCALED SUM AS ABOVE, BUT SCALED DIFFERENTLY.
             DO 1050 I = TSS, NS
                 A = A + REAL(I - 1) * CDAT(I)
 1050            CONTINUE
C            ENDDO
C
C            COMPUTE THE  EXTRA TYPE 3  SUMMATION OF ELEMENTS BELOW  THE
C            STARTING POINT.
             DO 1075 I = 1, TSS-1
                 R = R + CDAT(I)
 1075            CONTINUE
C            ENDDO
         ENDIF
C
C        SET RFLAG, IF THE DATA ARE NOT  APPROPRIATE FOR THE  MODEL WHEN
C        DATA TREATMENT TYPE 2 OR 3 IS DESIRED.
         IF (((TSF.EQ.2) .AND. (TSS.EQ.NS)) .OR.
     1       ((TSF.EQ.3) .AND. (TSS.EQ.1))) THEN
             RFLAG = 3
         ELSE
C            COMPUTE THE VALUE OF THE FUNCTION AT ITS LOWER BOUND.
             ADB = 1.0000001
             FA = SSDWML
     G                   (ADB   , NS    , A     , F     , R     , TSF
     G                   ,TSS   )
             BDB = ADB + 0.5
C
C            COMPUTE THE VALUE OF THE FUNCTION AT A POINT WHERE ITS SIGN
C            IS THE OPPOSITE OF THE COMPUTED VALUE OF THE FUNCTIONS LOW-
C            ER BOUND.
C
C            DOWHILE (RFLAG .EQ. -1)
 1100            COUNT = COUNT + 1
                 FB = SSDWML
     G                       (BDB   , NS    , A     , F     , R
     G                       ,TSF   , TSS   )
C
C                SET RFLAG, IF CONVERGENCE POSSIBLE.
                 IF (FA * FB.LE.0.0) THEN
                     RFLAG = 0
                 ENDIF
C
C                SET RFLAG, IF THE DATA ARE NOT APPROPRIATE FOR MODEL.
                 IF (COUNT.GE.5) THEN
                     RFLAG = 3
                 ENDIF
                 IF (RFLAG.EQ.-1) THEN
                     BDB = BDB + 0.5
                 ENDIF
                 IF (RFLAG.EQ.-1) GO TO 1100
C            ENDWHILE
         ENDIF
C
         IF (RFLAG.EQ.0) THEN
C            COMPUTE  THE ESTIMATE OF THE  BETA CONSTANT AND  STORE  THE
C            VALUE IN STATS(1).
             IFLAG  = -1
             ICOUNT =  1
C
C            DOWHILE (IFLAG .EQ. -1)
C                COMPUTE THE ZERO OF THE FUNCTION IN THE INTERVAL  ADB &
C                BDB;  NOTE  THE ARGUMENT  FC IS INITIALIZED BY THE ZERO
C                ROUTINE WHEN ICOUNT IS SET TO ONE.<<SEE RESTRICTIONS <<
 1125            CALL ZERO
     B                     (ADB   , BDB   , FA    , FB    , FC
     B                     ,ICOUNT
     Y                     ,IFLAG )
C
                 IF (IFLAG.EQ.-1) THEN
C                    COMPUTE THE VALUE OF THE  FUNCTION AT ITS NEW UPPER
C                    BOUND.
                     FB = SSDWML
     G                           (BDB   , NS    , A     , F     , R
     G                           ,TSF   , TSS   )
                 ENDIF
                 IF (IFLAG.EQ.-1) GO TO 1125
C            ENDWHILE
             STATS(1) = LOG(BDB)
C
C            ALTER THE SUMMATION F FOR TYPE THREE PROCESSING.
             IF (TSF.EQ.3) THEN
                 F = F + R
             ENDIF
C
C            DEFINE THE SPECIAL "S" AS DEFINED IN LOCAL GLOSSARY.
             IF (TSF.EQ.1) THEN
                 SPCS = 0
             ELSEIF (TSF.EQ.2) THEN
                 SPCS = TSS - 1
             ELSEIF (TSF.EQ.3) THEN
                 SPCS = TSS - 2
             ENDIF
C
C            COMPUTE THE DENOMINATOR FOR THE ESTIMATE OF ALPHA.
             STATS(2) = F * STATS(1)
     1                / (1.0 - EXP(-STATS(1) * REAL(NS-SPCS)))
C
C            COMPUTE THE TNOF ESTIMATE, IF THE BETA PARAMETER ALLOWS.
             IF (STATS(1).GT.0.000001) THEN
                 STATS(3) = STATS(2) / STATS(1)
             ENDIF
C
             IF (STATS(3).LT.CTOT) THEN
                 RFLAG = 4
             ELSE
C                COMPUTE THE WEIGHTED SUMS-OF-SQUARES (WLS).
                 STATS(4) = 0.0
                 IF (TSF.EQ.3) THEN
C                    INITIALIZE SUM TO THOSE MERGED.
                     STATS(4) = EXP(STATS(1))
     1                        * (STATS(2) / STATS(1)
     2                        * (EXP(-STATS(1)))
     3                        * (EXP(STATS(1)) - 1.0) - R)**2
                 ENDIF
                 DO 1150 I = TSS, NS
                     STATS(4) = STATS(4)
     1                        + (EXP(STATS(1) * REAL(I-SPCS))
     2                        * (STATS(2) / STATS(1)
     3                        * (EXP(-STATS(1) * REAL(I-SPCS)))
     4                        * (EXP(STATS(1)) - 1.0) - CDAT(I))**2)
 1150                CONTINUE
C                ENDDO
                 STATS(4) = STATS(4) / REAL(NS-SPCS)
C
C                COMPUTE THE MEAN SQUARE ERROR (MSE-F).
                 STATS(5) = 0.0
                 SUMF     = 0.0
                 IF (TSF.EQ.3) THEN
C                    INITIALIZE SUM TO THOSE MERGED.
                     SUMF     = R
                     STATS(5) = (STATS(2) / STATS(1)
     1                        * (1.0 - EXP(-STATS(1)))
     2                        - SUMF)**2
                 ENDIF
                 DO 1175 I = TSS, NS
                     SUMF     = SUMF + CDAT(I)
                     STATS(5) = STATS(5)
     1                        + (STATS(2) / STATS(1)
     2                        * (1.0 - EXP(-STATS(1) * REAL(I-SPCS)))
     3                        - SUMF)**2
 1175                CONTINUE
C                ENDDO
                 STATS(5) = STATS(5) / REAL(NS-SPCS)
C
C                COMPUTE THE MEAN SQUARE ERROR (MSE-T); START BY GETTING
C                THE INDEX OF LAST INTERVAL WITH A NON-ZERO FAULT COUNT.
                 IDXLST = NS + 1
C                DOWHILE (CDAT(IDXLST) .EQ. 0.0)
 1225                IDXLST = IDXLST - 1
                     IF (CDAT(IDXLST).EQ.0.0) GO TO 1225
C                ENDWHILE
C
                 STATS(6) = 0.0
                 SUMF     = 0.0
                 ABTFLG   = 0
                 IF (TSF.EQ.3) THEN
C                    INITIALIZE SUM TO THOSE MERGED.
                     SUMF = R
C
C                    SET J TO THE INDEX OF THE NEXT NON-ZERO INTERVAL.
                     J = TSS - 1
C                    DOWHILE (CDAT(J) .EQ. 0.0)
 1275                    J = J + 1
                         IF (CDAT(J).EQ.0.0) GO TO 1275
C                    ENDWHILE
C
C                    BREAKOUT THE INTERMEDIATE VALUE  TO TRAP FOR A NON-
C                    POSITIVE VALUE.
                     TMP = STATS(2)
     1                   / (STATS(2) - STATS(1) * (SUMF + CDAT(J)))
C
C                    SET THE ABORT FLAG OR FINISH THE CALCULATION.
                     IF (TMP.LE.0.0) THEN
                         ABTFLG = 1
                     ELSE
                         STATS(6) = ((LOG(TMP) / STATS(1)
     1                            - 1.0) - REAL(J))**2
                     ENDIF
                 ENDIF
C
                 IF (ABTFLG.EQ.0) THEN
C                    THIS SHOULD CHANGE  TO A  "DOWHILE (NOT DONE AND NO
C                    ABORT)" IN THE NEXT SMERFS RELEASE.
                     DO 1325 I = TSS, IDXLST-1
                         SUMF = SUMF + CDAT(I)
C
C                        SET J TO THE INDEX OF THE NEXT NON-ZERO INTERVA
                         J = I
C                        DOWHILE (CDAT(J) .EQ. 0.0)
 1300                        J = J + 1
                             IF (CDAT(J).EQ.0.0) GO TO 1300
C                        ENDWHILE
C
C                        BREAKOUT THE  INTERMEDIATE VALUE  TO TRAP FOR A
C                        POSITIVE VALUE.
                         TMP = STATS(2)
     1                       / (STATS(2) - STATS(1) * (SUMF + CDAT(J)))
C
C                        SET THE ABORT FLAG OR FINISH THE CALCULATION.
                         IF (TMP.LE.0.0) THEN
                             ABTFLG = 1
                         ELSE
                             STATS(6) = STATS(6)
     1                                + ((LOG(TMP) / STATS(1)
     2                                - REAL(I-SPCS)) - REAL(J-I))**2
                         ENDIF
 1325                    CONTINUE
C                    ENDDO
                 ENDIF
C
                 IF (ABTFLG.EQ.1) THEN
                     STATS(6) = -1.0
                 ELSE
                     STATS(6) = STATS(6) / REAL(IDXLST-SPCS-1)
                 ENDIF
             ENDIF
         ENDIF
         RETURN
         END
         FUNCTION SSDWML
     G                   (XAN   , NS    , SA    , SF    , SR    , TSF
     G                   ,TSS   )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION SSDWML
C    G                   (XAN   , NS    , SA    , SF    , SR    , TSF
C    G                   ,TSS   )
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
C        TO EVALUATE THE FUNCTION USED BY SCHNEIDEWINDS MODEL.
C   DESCRIPTION
C        THIS ROUTINE CALCULATES THE VALUE OF THE FUNCTION AT THE  POINT
C        XAN (ESTIMATE OF THE BETA CONSTANT).
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
C             NS     (I) = NUMBER OF ENTRIES
C             SA     (R) = SUMMATION OF SCALED CDAT ELEMENTS ABOVE STAR-
C                          TING POINT
C             SF     (R) = SUMMATION OF CDAT ELEMENTS ABOVE STARTING PT.
C             SR     (R) = SUMMATION OF CDAT ELEMENTS BELOW STARTING PT.
C                          FOR DATA TREATMENT TYPE NUMBER 3
C             TSF    (I) = TREATMENT SELECTION FLAG
C             TSS    (I) = TREATMENT SELECTION STARTING POINT
C             XAN    (R) = ESTIMATE OF THE BETA CONSTANT
C        YIELDED
C             SSDWML (R) = FUNCTION VALUE AT POINT XAN
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
         INTEGER              NS            ,TSF           ,TSS
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
         IF (TSF.NE.3) THEN
C            COMPUTE THE FUNCTION VALUE FOR TREATMENT TYPES 1 AND 2.
             SSDWML =  SF / (XAN - 1.0) - SA
     1              - (REAL (NS  - TSS  + 1) * SF)
     2              / (XAN**(NS  - TSS  + 1) - 1.0)
         ELSE
C            COMPUTE THE FUNCTION VALUE FOR TREATMENT TYPE 3.
             SSDWML = (REAL (TSS - 1)   * SR)
     1              / (XAN**(TSS - 1)   - 1.0)
     2              +  SF / (XAN - 1.0) - SA  - REAL(NS) * (SF + SR)
     3              / (XAN**NS   - 1.0)
         ENDIF
         RETURN
         END
         SUBROUTINE SSDWPR
     G                     (NPV   , NS    , DNOF  , DNOP  , BETA
     G                     ,ALPHA , CDAT  , TSF   , TSS
     Y                     ,PDAT  , RFLAG )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SSDWPR
C    G                     (NPV   , NS    , DNOF  , DNOP  , BETA
C    G                     ,ALPHA , CDAT  , TSF   , TSS
C    Y                     ,PDAT  , RFLAG )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO COMPUTE  THE PREDICTED NUMBER OF FAULTS  IN THE NEXT TESTING
C        PERIOD, TO COMPUTE  THE NUMBER OF PERIODS  REQUIRED TO DETECT A
C        DESIRED  NUMBER OF FAULTS,  AND TO  CREATE THE  PREDICTED FAULT
C        COUNT DATA VECTOR FOR THE SCHNEIDEWIND MODEL.
C   DESCRIPTION
C        THE FIRST ARGUMENT OF THE SSDWPR CALL LINE (NPV) INDICATES WHE-
C        THER A SINGLE QUANTITY IS TO BE  PRODUCED OR IF THE ENTIRE DATA
C        VECTOR IS TO BE GENERATED. IF NPV IS NOT SET TO A ONE, THEN THE
C        ENTIRE DATA VECTOR WILL BE GENERATED, USING THE ARGUMENTS ALPHA
C        AND BETA. IF NPV IS SET TO A ONE  AND DNOF IS SET TO MINUS ONE,
C        THEN THE  PREDICTION OF THE  NUMBER OF FAULTS  IN THE NEXT DNOP
C        TESTING PERIODS WILL  BE MADE.  IF NPV IS SET TO A ONE AND DNOF
C        IS OTHER THAN A MINUS ONE, THEN THE PREDICTION OF THE NUMBER OF
C        PERIODS TO DETECT THE SPECIFIED DNOF IS MADE USING THE ARGUMENT
C        ASSIGNMENTS FOR ALPHA, BETA, DNOF, AND ALL NS ELEMENTS OF CDAT.
C
C        NOTE, THE CREATED DATA VECTOR  (ALWAYS DIMENSIONED AT NPV) DOES
C        NOT CONTAIN THE SAME NUMBER OF ASSIGNED  ELEMENTS FOR THE THREE
C        TREATMENT TYPES.  THAT IS, THE CALCULATION CONSIDERS THE TREAT-
C        MENT TYPE AND THE STARTING POINT.
C
C        THE RETURN STATUS  FLAG (RFLAG)  WILL INITIALLY BE SET TO ZERO.
C        THIS STATUS WILL ONLY BE  CHANGED TO THE ERROR SETTING (THE VA-
C        LUE ONE) FOR THE NUMBER OF PERIODS PREDICTION WHEN THE LOG OF A
C        NONPOSITIVE NUMBER IS ENCOUNTERED.
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
C             ALPHA  (R) = ESTIMATE OF ALPHA FROM SSDWMD
C             BETA   (R) = ESTIMATE OF BETA FROM SSDWMD
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             DNOF   (R) = DESIRED  NUMBER OF FAULTS  TO BE DETECTED FOR
C                          THE NUMBER OF PERIODS PREDICTION
C             DNOP   (R) = DESIRED  NUMBER OF PERIODS TO BE EXAMINED FOR
C                          THE NUMBER OF FAULTS PREDICTION
C             NPV    (I) = NUMBER OF PREDICTED VALUES FLAG
C             NS     (I) = SIZE OF CDAT
C             TSF    (I) = TREATMENT SELECTION FLAG
C             TSS    (I) = TREATMENT SELECTION STARTING POINT
C        YIELDED
C             PDAT   (R) = VECTOR OF PREDICTED DATA VALUE(S)
C             RFLAG  (I) = RETURN STATUS FLAG
C   LOCAL GLOSSARY
C             CNOF   (R) = CURRENT TOTAL NUMBER OF FAULTS
C             RN     (R) = REAL VARIABLE STORAGE OF NS
C             PE     (I) = PERIOD ENDING    NUMBER FOR VECTOR CREATION
C             PS     (I) = PERIOD STARTINGS NUMBER FOR VECTOR CREATION
C                           FOR THE THREE TREATMENT TYPES, THEN:
C                           1) FOR TREATMENT 1, PS = 1 AND PE = NS
C                           2) FOR TREATMENT 2, PS = S AND PE = NS
C                           3) FOR TREATMENT 3, PS = 1 AND PE = NS-S+2
C             SPCS   (I) = SPECIAL "S" FOR SUMMATIONS AND DIVISIONS OVER
C                          THE THREE TREATMENT TYPES, WHERE:
C                          1) FOR TREATMENT 1, SPCS = 1
C                          2) FOR TREATMENT 2, SPCS = S
C                          3) FOR TREATMENT 3, SPCS = 1
C             ZTEST  (R) = ZERO TEST CONSTANT
C   ERRORS
C        RFLAG  = 1 : POTENTIAL LOG OF NONPOSITIVE NUMBER
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
         INTEGER              NPV           ,NS            ,RFLAG
     1                       ,TSF           ,TSS
         DIMENSION            CDAT(NS)      ,PDAT(NPV)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J             ,PE
     1                       ,PS            ,SPCS
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
C        INITIALIZE THE  RETURN STATUS FLAG,  AND SET THE  REAL VARIABLE
C        STORAGE OF NS.
         RFLAG = 0
         RN    = NS
         IF (NPV.EQ.1) THEN
C            DEFINE THE SPECIAL "S" AS DEFINED IN LOCAL GLOSSARY.
             SPCS = 1
             IF (TSF.EQ.2) THEN
                 SPCS = TSS
             ENDIF
C
             IF (DNOF.EQ.-1.0) THEN
C                COMPUTE THE  ESTIMATE OF  THE NUMBER  OF FAULTS  IN THE
C                NEXT DNOP TESTING PERIODS.
                 PDAT(1) = ALPHA / BETA
     1                   * (EXP(-BETA *  REAL(NS-SPCS+1))
     2                   -  EXP(-BETA * (REAL(NS-SPCS+1) + DNOP)))
             ELSE
C                COMPUTE THE ESTIMATE OF THE NUMBER OF PERIODS TO DETECT
C                THE DNOF.
                 CNOF = 0.0
                 DO 1000 I = SPCS, NS
                     CNOF = CNOF + CDAT(I)
 1000                CONTINUE
C                ENDDO
C
                 PDAT(1) = ALPHA / (ALPHA - BETA * (CNOF + DNOF))
                 IF (PDAT(1).LT.ZTEST) THEN
C                    LOG ERROR.
                     RFLAG = 1
                 ELSE
                     PDAT(1) = LOG(PDAT(1)) / BETA
     1                       - (RN - REAL(TSS - 1))
                 ENDIF
             ENDIF
         ELSE
C            DEFINE THE STARTING AND ENDING PERIOD NUMBERS AS DEFINED IN
C            LOCAL GLOSSARY.
             PS = 1
             PE = NS
             IF (TSF.EQ.2) THEN
                 PS = TSS
             ENDIF
             IF (TSF.EQ.3) THEN
                 PE = NS - TSS + 2
             ENDIF
C
C            DEFINE THE ESTIMATES FOR THE  SELECTED PORTION OF THE FAULT
C            COUNT VECTOR.
             J = 0
             DO 1025 I = PS, PE
                 J       = J + 1
                 PDAT(J) = (ALPHA / BETA)
     1                   * (EXP(-BETA * REAL(I-PS  ))
     2                   -  EXP(-BETA * REAL(I-PS+1)))
 1025            CONTINUE
C            ENDDO
         ENDIF
         RETURN
         END
