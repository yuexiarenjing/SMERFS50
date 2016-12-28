         SUBROUTINE MAADRV
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MAADRV
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE  THE MENUS AND  TRANSFERS FOR  THE EXECUTION  OF THE
C        MODEL APPLICABILITY ANALYSES (ACCURACY, BIAS, NOISE, & TREND).
C   DESCRIPTION
C        THE ONLY REAL PROCESSING INVOLVED  IN THIS ROUTINE INVOLVES THE
C        SELECTION OF SETTING  OF THE  PARAMETERS FOR  THE TBF ANALYSES.
C        THE FIRST SIMPLY POINTS  TO THE FIRST (WC)  OR THE SECOND (CPU)
C        DATA SECTION OF THE DATA VECTOR.   THE SECOND PARAMETER HOLDS A
C        MODIFIED SAMPLE SIZE, WHICH REMOVES THE FINAL ENTERED VALUE FOR
C        DATA SETS WHERE TESTING CONTINUED AFTER THE LAST FAILURE.
C
C        THE ROUTINE THEN ACCESSES A ROUTINE, MAASET, TO SET A RANGE FOR
C        THE ITERATION SEQUENCE.  THAT ROUTINE ALSO ALLOWS FOR A DIFFER-
C        ENT RANGE TO BE SPECIFIED.
C
C        ONCE THE  RANGE IS SET, THE ANALYSIS MENU IS ISSUED AND THE AP-
C        PLICABLE ROUTINES ARE ACCESSED UNTIL THE ANALYSIS IS COMPLETED.
C        WHEN THE USER SELECTS TO  RETURN TO THE MAIN PROGRAM, A SUMMARY
C        REPORT WILL BE GENERATED. THE SUMMARY WILL ALSO INCLUDE THE TO-
C        TAL RANKING OF EACH OF THE TBF MODELS.
C
C        A FINAL OPTION OF THE MENU ALLOWS THE USER TO PERFORM MODEL AP-
C        PLICABILITY ANALYSES WITH THE ONLY PRINTOUT BEING THE SUMMARY.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS  SUBPROGRAM  CONTAINS  CODE WHICH DOES NOT COMPLY WITH THE
C        PROGRAMMING STANDARDS; GROUP LEADER APPROVAL HAS BEEN OBTAINED.
C       -SPECIFICALLY:
C        THE DOWHILE LOOPS CONTAIN TWO MEANS OF EXIT.
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             FLAG   (I) = OPTION FLAG
C             FLAGRI (I) = MODEL RESULT ERROR FLAGS (INTERVAL DATA)
C             FLAGRT (I) = MODEL RESULT ERROR FLAGS (TBF      DATA)
C             FLAGSI (I) = MODEL SELECTION FLAGS    (INTERVAL DATA)
C             FLAGST (I) = MODEL SELECTION FLAGS    (TBF      DATA)
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C             PRTFLG (I) = OPTIONAL PRINT NOTHING FLAG FOR INTERVAL DATA
C             SCTFLG (I) = FLAG INDICATING IF SCATTER PLOTS DESIRED
C             STATI  (R) = MODEL RESULT STATISTICS  (INTERVAL DATA)
C             STATT  (R) = MODEL RESULT STATISTICS  (TBF      DATA)
C   ERRORS
C        "DATA TYPE ERROR; TRY AGAIN (AFTER THE PROMPT)."
C        "ANALYSIS CANNOT BE MADE; RANGE MUST BE AT LEAST TWO."
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
         INTEGER              END1
         PARAMETER           (END1   =   1000)
         INTEGER              END2
         PARAMETER           (END2   =   2000)
C
C                             GLOBAL SPECIFICATIONS
C
C               /DATCOM/      DATA AND PROCESSING COMMON
         COMMON /DATCOM/      DAT(END2)     ,DATTYP        ,PRC(3,3)
     1                       ,UPDFLG
         INTEGER              DATTYP        ,PRC           ,UPDFLG
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              FLAG          ,FLAGRI(6)     ,FLAGRT(7,4)
     1                       ,FLAGSI(6)     ,FLAGST(7,4)   ,I
     2                       ,IDX           ,J             ,K
     3                       ,NS            ,NSB           ,NSE
     4                       ,NSR           ,PRTFLG        ,SCTFLG
         DIMENSION            STATI(6)      ,STATT(7,4)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'   ENTER ONE FOR WC TBF OR TWO FOR CPU TBF.')
  305    FORMAT (' **DATA TYPE ERROR; TRY AGAIN (AFTER THE PROMPT).')
  310    FORMAT (' **ANALYSIS CANNOT BE MADE; RANGE MUST BE AT LEAST TW'
     1          ,'O.')
  315    FORMAT (' '
     1        ,/,'   ENTER MODEL APPLICABILITY ANALYSIS OPTION, OR ZERO'
     2          ,' FOR A LIST.')
  320    FORMAT ('   THE AVAILABLE MODEL APPLICABILITY ANALYSIS OPTIONS'
     1          ,' ARE:'
     2        ,/,'    1 THE MODEL ACCURACY STATISTIC'
     3        ,/,'    2 THE MODEL ACCURACY STATISTIC AND SCATTER PLOT'
     4        ,/,'    3 THE MODEL BIAS STATISTIC AND U-PLOT'
     5        ,/,'    4 THE MODEL BIAS STATISTIC, U-PLOT, AND SCATTER P'
     6          ,'LOT'
     7        ,/,'    5 THE MODEL NOISE STATISTIC'
     8        ,/,'    6 THE MODEL TREND STATISTIC AND Y-PLOT'
     9        ,/,'    7 FULL ANALYSIS WITH SUMMARY REPORT ONLY PRINTOUT'
     A        ,/,'    8 RETURN TO THE MAIN PROGRAM'
     B        ,/,'   ENTER MODEL APPLICABILITY ANALYSIS OPTION.')
  325    FORMAT ('   THE AVAILABLE MODEL APPLICABILITY ANALYSIS OPTIONS'
     1          ,' ARE:'
     2        ,/,'    1 THE MODEL ACCURACY STATISTIC'
     3        ,/,'    2 THE MODEL ACCURACY STATISTIC AND SCATTER PLOT'
     4        ,/,'    3 SUMMARY REPORT ONLY PRINTOUT'
     5        ,/,'    4 RETURN TO THE MAIN PROGRAM'
     6        ,/,'   ENTER MODEL APPLICABILITY ANALYSIS OPTION.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
         IF (DATTYP.LE.3) THEN
C            EXECUTION TIME DATA ARE PRESENT;  ASSIGN STARTING INDEX AND
C            SAMPLE SIZE.
             IF (DATTYP.LE.2) THEN
                 I = DATTYP
             ELSE
C                DOWHILE (ERROR ON INPUT)
 1000                WRITE (   *,300)
                     WRITE (HIST,300)
                     READ  (   *,  *) I
                     WRITE (HIST,  *) I
                     IF ((I.EQ.1) .OR. (I.EQ.2)) GO TO 1025
                     WRITE (   *,305)
                     WRITE (HIST,305)
                     GO TO 1000
 1025                CONTINUE
C                ENDWHILE
             ENDIF
             IDX = 1 + (END1 * (I - 1))
             NS  = PRC(I,1) + (PRC(I,2) - 1)
C
C            SET THE STARTING AND ENDING POINTS OF THE ANALYSIS.
             CALL MAASET
     G                   (NS
     Y                   ,NSB   , NSE   , NSR   )
C
C            ENSURE ADEQUATE DATA EXISTS FOR THE ANALYSIS.
             IF (NSR.LT.2) THEN
                 WRITE (   *,310)
                 WRITE (HIST,310)
             ELSE
C
C                THE ANALYSIS IS TO BE PERFORMED, ZERO ALL PREVIOUS DATA
C                BY SETTING THE MODEL SELECTION FLAGS TO ZERO. THIS PRO-
C                CESSING IS NEEDED TO KEEP THE SUMMARY REPORT CURRENT.
                 DO 1075 J = 1, 7
                     DO 1050 K = 1, 4
                         FLAGST(J,K) = 0
 1050                    CONTINUE
C                    ENDDO
 1075                CONTINUE
C                ENDDO
C
C                                                  <<SEE RESTRICTIONS <<
C                DOWHILE (ANALYSIS OPTION SELECTION .NE. RETURN)
 1100                WRITE (   *,315)
                     WRITE (HIST,315)
                     READ  (   *,  *) FLAG
                     WRITE (HIST,  *) FLAG
                     IF ((FLAG.LT.1) .OR. (FLAG.GT.8)) THEN
                         WRITE (   *,320)
                         WRITE (HIST,320)
                         READ  (   *,  *) FLAG
                         WRITE (HIST,  *) FLAG
                     ENDIF
                     IF ((FLAG.LT.1) .OR. (FLAG.GT.7)) GO TO 1125
C
                     IF     ((FLAG.EQ.1) .OR. (FLAG.EQ.2)) THEN
C                        THE ACCURACY ANALYSIS IS DESIRED.
                         SCTFLG = 0
                         IF (FLAG.EQ.2) THEN
                             SCTFLG = 1
                         ENDIF
                         CALL MAATAC
     G                               (IDX   , NS    , NSB   , NSE
     G                               ,NSR   , SCTFLG
     Y                               ,FLAGRT(1,1)   , FLAGST(1,1)
     Y                               ,STATT(1,1)    )
                     ELSEIF ((FLAG.EQ.3) .OR. (FLAG.EQ.4)) THEN
C                        THE BIAS ANALYSIS IS DESIRED.
                         SCTFLG = 0
                         IF (FLAG.EQ.4) THEN
                             SCTFLG = 1
                         ENDIF
                         CALL MAATBS
     G                               (IDX   , NS    , NSB   , NSE
     G                               ,NSR   , SCTFLG
     Y                               ,FLAGRT(1,2)   , FLAGST(1,2)
     Y                               ,STATT(1,2)    )
                     ELSEIF (FLAG.EQ.5) THEN
C                        THE NOISE ANALYSIS IS DESIRED.
                         CALL MAATNS
     G                               (IDX   , NS    , NSB   , NSE
     G                               ,NSR
     Y                               ,FLAGRT(1,3)   , FLAGST(1,3)
     Y                               ,STATT(1,3)    )
                     ELSEIF (FLAG.EQ.6) THEN
C                        THE TREND ANALYSIS IS DESIRED.
                         CALL MAATTD
     G                               (IDX   , NS    , NSB   , NSE
     G                               ,NSR
     Y                               ,FLAGRT(1,4)   , FLAGST(1,4)
     Y                               ,STATT(1,4)    )
                     ELSE
C                        FULL ANALYSIS WITH ONLY SUMMARY PRINTOUT.
                         CALL MAATQK
     G                               (IDX   , NS    , NSB   , NSE
     G                               ,NSR
     Y                               ,FLAGRT, FLAGST, STATT )
                     ENDIF
                     IF (FLAG.NE.7) GO TO 1100
 1125                CONTINUE
C                ENDWHILE
C
C                PRODUCE THE SUMMARY TABLE FOR THE TBF ANALYSIS.
                 CALL MAASMT
     G                       (FLAGRT, FLAGST
     B                       ,STATT )
             ENDIF
         ELSE
C            INTERVAL DATA ARE PRESENT; ASSIGN THE SAMPLE SIZE.
             NS = PRC(3,1)
C
C            SET THE STARTING AND ENDING POINTS OF THE ANALYSIS.
             CALL MAASET
     G                   (NS
     Y                   ,NSB   , NSE   , NSR   )
C
C            ENSURE ADEQUATE DATA EXISTS FOR THE ANALYSIS.
             IF (NSR.LT.2) THEN
                 WRITE (   *,310)
                 WRITE (HIST,310)
             ELSE
C
C                THE ANALYSIS IS TO BE PERFORMED, ZERO ALL PREVIOUS DATA
C                BY SETTING THE MODEL SELECTION FLAGS TO ZERO. THIS PRO-
C                CESSING IS NEEDED TO KEEP THE SUMMARY REPORT CURRENT.
                 DO 1150 J = 1, 6
                     FLAGSI(J) = 0
 1150                CONTINUE
C                ENDDO
C
C                                                  <<SEE RESTRICTIONS <<
C                DOWHILE (ANALYSIS OPTION SELECTION .NE. RETURN)
 1175                WRITE (   *,315)
                     WRITE (HIST,315)
                     READ  (   *,  *) FLAG
                     WRITE (HIST,  *) FLAG
                     IF ((FLAG.LT.1) .OR. (FLAG.GT.4)) THEN
                         WRITE (   *,325)
                         WRITE (HIST,325)
                         READ  (   *,  *) FLAG
                         WRITE (HIST,  *) FLAG
                     ENDIF
                     IF ((FLAG.LT.1) .OR. (FLAG.GT.3)) GO TO 1200
C
C                    THE ACCURACY ANALYSIS IS DESIRED.
                     SCTFLG = 0
                     IF (FLAG.EQ.2) THEN
C                        SCATTER PLOTS ARE ALSO WANTED.
                         SCTFLG = 1
                     ENDIF
C
                     PRTFLG = 1
                     IF (FLAG.EQ.3) THEN
C                        PRINTOUT IS NOT WANTED; ONLY SUMMARY TABLE.
                         PRTFLG = 0
                     ENDIF
C
                     CALL MAAIAC
     G                           (NS    , NSB   , NSE   , NSR   , PRTFLG
     G                           ,SCTFLG
     Y                           ,FLAGRI, FLAGSI, STATI )
                     IF (FLAG.NE.3) GO TO 1175
 1200                CONTINUE
C                ENDWHILE
C
C                PRODUCE THE SUMMARY TABLE FOR THE INTERVAL ANALYSIS.
                 CALL MAASMI
     G                       (FLAGRI, FLAGSI
     B                       ,STATI )
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE MAAIAC
     G                     (NS    , NSB   , NSE   , NSR   , PRTFLG
     G                     ,SCTFLG
     Y                     ,RFLAG , FLAG  , STAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MAAIAC
C    G                     (NS    , NSB   , NSE   , NSR   , PRTFLG
C    G                     ,SCTFLG
C    Y                     ,RFLAG , FLAG  , STAT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE THE MENUS AND TRANSFERS FOR THE EXECUTION OF  AN AC-
C        CURACY ANALYSIS OF THE INTERVAL DATA PRIOR TO THE  EXECUTION OF
C        THE SOFTWARE RELIABILITY MODELS (VIA MODULE 8).
C   DESCRIPTION
C        THE ONLY REAL PROCESSING INVOLVED  IN THIS ROUTINE INVOLVES THE
C        SETTING OF SOME  PRELIMINARY POINTERS FOR THE DATA  AND THE SE-
C        LECTION OF  WHICH OF THE MODELS  ARE TO BE EXECUTED.  A SPECIAL
C        OPTION CAN BE SET TO INDICATE THAT ALL MODELS ARE TO BE EXECUT-
C        ED DURING THE ANALYSIS.
C
C        DATA VECTORS WILL BE  INTERNALLY CONVERTED TO TTF  (RATHER THAN
C        TBF UNITS) FOR THE NON-HOMOGENEOUS POISSON AND S-SHAPED MODELS;
C        AND THEN RETURNED TO THE STANDARD TBF UNITS.
C
C        ADDITIONALLY, THE ROUTINE  WILL PROMPT FOR AN  ALPHA VALUE (THE
C        PROBABILITY OF CORRECTING FAULTS WITHOUT INSERTING FAULTS)  FOR
C        THE BROOKS AND MOTLEY MODEL, AND ANOTHER  ALPHA VALUE (COUNT(I)
C        **ALPHA) FOR THE GENERALIZED POISSON MODEL; IF THE MODEL(S) ARE
C        ARE TO BE EXECUTED.
C
C        ANY SPECIAL VECTORS (EX, THE FRACTION OF THE PROGRAM UNDER TEST
C        IN THE BROOKS AND MOTLEY MODEL)  WILL BE SET TO  DEFAULT VALUES
C        (EX, THE ENTIRE PROGRAM IS UNDER TEST).
C
C        THE REQUIRED COMPUTATIONS WILL BE PERFORMED  BY ROUTINES OF THE
C        SMFLIB LIBRARY.  UPON RETURN  THIS ROUTINE WILL OUTPUT THE COM-
C        PUTED STATISTIC  AND OPTIONALLY ACCESS THE PLTSCT ROUTINE FOR A
C        PLOT OF THE COMPUTED VALUES.
C
C        THE MODEL SELECTION FLAG, THE MODEL ERROR FLAG, AND THE RESULT-
C        ING STATISTICS ARE RETURNED TO THE CALLING ROUTINE  SO THAT THE
C        OVERALL (SUMMARY) MODEL APPLICABILITY RANKING CAN BE COMPUTED.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C             STR2   (I) = INDEX TO BEGINNING OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C                          (BUT RETURNED TO ORIGINAL VALUES)
C        YIELDED
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1
C             TM2C   (R) = TEMPORARY STORAGE AREA OF SIZE END1
C             TM3C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS SCATTER PLOT.
C             TM4C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS SCRATCH ARRAY.
C   ARGUMENT LIST
C        GIVEN
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C             PRTFLG (I) = OPTIONAL PRINT NOTHING FLAG FOR INTERVAL DATA
C             SCTFLG (I) = FLAG INDICATING IF SCATTER PLOTS DESIRED
C        YIELDED
C             FLAG   (I) = MODEL SELECTION FLAGS
C             RFLAG  (I) = MODEL RESULTS ERROR FLAGS
C             STAT   (R) = PREQUENTIAL LIKELIHOOD STATISTICS
C   LOCAL GLOSSARY
C             ALLFLG (I) = OPTION FLAG TO INDICATE ALL MODELS WANTED
C             ALPHA  (R) = USER INPUT GPO ALPHA VALUES
C             BOPFLG (I) = BAM BINOMIAL OR POISSON FLAG
C             CNT    (I) = ANALYSIS ITERATION COUNT (AT RETURN)
C             CNTO   (I) = SAMPLE OBSERVATION COUNT
C             DM     (R) = DUMMY ARGUMENT FOR SMFTRN ACCESS
C             FXFLG  (I) = FIXED FLAG FOR  THE ROUTINES  TO GENERATE THE
C                          EXTRA VECTORS (I.E., NO USER PROMPT OR INPUT)
C             IDXEL  (I) = INDEX FOR LENGTHS ENDING ADDRESS
C             LCOR   (R) = GPO FAULT CORRECTIONS AFTER LAST PERIOD  (NOT
C                          USED)
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS (SET TO 100)
C             PEC    (R) = BAM PROB. OF CORRECTING FAULTS WITHOUT INSER-
C                          TING FAULTS
C             RFLG   (I) = DUMMY ARGUMENT FOR SMFTRN ACCESS
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
         INTEGER              END1
         PARAMETER           (END1   =   1000)
         INTEGER              END2
         PARAMETER           (END2   =   2000)
         INTEGER              STR2
         PARAMETER           (STR2   =   1001)
C
C                             GLOBAL SPECIFICATIONS
C
C               /DATCOM/      DATA AND PROCESSING COMMON
         COMMON /DATCOM/      DAT(END2)     ,DATTYP        ,PRC(3,3)
     1                       ,UPDFLG
         INTEGER              DATTYP        ,PRC           ,UPDFLG
C
C               /MAACOM/      MODEL APPLICABILITY ANALYSIS COMMON
         COMMON /MAACOM/      TM3C(END1)    ,TM4C(END1)
C
C               /TMPCOM/      TEMPORARY STORAGE COMMON
         COMMON /TMPCOM/      TM1C(END1)    ,TM2C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              FLAG(6)       ,NS            ,NSB
     1                       ,NSE           ,NSR           ,PRTFLG
     2                       ,RFLAG(6)      ,SCTFLG
         DIMENSION            STAT(6)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              ALLFLG        ,BOPFLG        ,CNT
     1                       ,CNTO          ,FXFLG         ,I
     2                       ,IDXEL         ,MAXIC         ,RFLG
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'    ENTER ONE TO SELECT THE MODELS, OR ZERO TO RUN AL'
     2          ,'L SIX.')
  305    FORMAT ('    ENTER ONE FOR BROOKS AND MOTLEY BINOMIAL MODEL; E'
     1          ,'LSE ZERO.')
  310    FORMAT ('    ENTER ONE FOR BROOKS AND MOTLEY POISSON  MODEL; E'
     1          ,'LSE ZERO.')
  315    FORMAT ('    ENTER ONE FOR GENERALIZED POISSON MODEL; ELSE ZER'
     1          ,'O.')
  320    FORMAT ('    ENTER ONE FOR NON-HOMOGENEOUS POISSON MODEL; ELSE'
     1          ,' ZERO.')
  325    FORMAT ('    ENTER ONE FOR SCHNEIDEWIND MODEL; ELSE ZERO.')
  330    FORMAT ('    ENTER ONE FOR S-SHAPED RELIABILITY GROWTH MODEL; '
     1          ,'ELSE ZERO.')
  335    FORMAT ('    ENTER ALPHA  (THE PROB. OF CORRECTING FAULTS, WIT'
     1          ,'HOUT INSERTING'
     2        ,/,'    NEW ONES) FOR THE BROOKS AND MOTLEY MODEL.')
  345    FORMAT ('    ENTER ALPHA (COUNT(I)**ALPHA) FOR THE GENERALIZED'
     2          ,' POISSON MODEL.')
  350    FORMAT (' '
     1        ,/,'    THE FOLLOWING WERE DETERMINED BASED ON ',I3,' POI'
     2          ,'NTS')
  352    FORMAT (' '
     1        ,/,'    CURRENTLY WORKING ON THE MODEL ACCURACY ANALYSIS.'
     2          )
  355    FORMAT ('    BROOKS AND MOTLEY-BIN  STATISTIC IS ',E15.8)
  360    FORMAT ('  **BROOKS AND MOTLEY-BIN  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  365    FORMAT ('    BROOKS AND MOTLEY-POI  STATISTIC IS ',E15.8)
  370    FORMAT ('  **BROOKS AND MOTLEY-POI  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  375    FORMAT ('    GENERALIZED POISSON    STATISTIC IS ',E15.8)
  380    FORMAT ('  **GENERALIZED POISSON    FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  385    FORMAT ('    NHPP FOR INT. DATA     STATISTIC IS ',E15.8)
  390    FORMAT ('  **NHPP FOR INT. DATA     FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  395    FORMAT ('    SCHNEIDEWIND MODEL     STATISTIC IS ',E15.8)
  400    FORMAT ('  **SCHNEIDEWIND MODEL     FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  405    FORMAT ('    S-SHAPED REL. GROWTH   STATISTIC IS ',E15.8)
  410    FORMAT ('  **S-SHAPED REL. GROWTH   FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE DATA VALUES TREATED AS CONSTANTS IN THIS ROUTINE.
         FXFLG = 0
         IDXEL = STR2 + NS - 1
         MAXIC = 100
C
C        DETERMINE IF ALL INTERVAL MODELS ARE TO BE EXECUTED.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) ALLFLG
         WRITE (HIST,  *) ALLFLG
C
         IF (ALLFLG.NE.1) THEN
C            ALL MODELS ARE DESIRED IN THIS EXECUTION.
             DO 1000 I = 1, 6
                 FLAG(I) = 1
 1000            CONTINUE
C            ENDDO
         ELSE
C            ONLY CERTAIN MODELS ARE DESIRED IN THIS EXECUTION.
             WRITE (   *,305)
             WRITE (HIST,305)
             READ  (   *,  *) FLAG(1)
             WRITE (HIST,  *) FLAG(1)
C
             WRITE (   *,310)
             WRITE (HIST,310)
             READ  (   *,  *) FLAG(2)
             WRITE (HIST,  *) FLAG(2)
C
             WRITE (   *,315)
             WRITE (HIST,315)
             READ  (   *,  *) FLAG(3)
             WRITE (HIST,  *) FLAG(3)
C
             WRITE (   *,320)
             WRITE (HIST,320)
             READ  (   *,  *) FLAG(4)
             WRITE (HIST,  *) FLAG(4)
C
             WRITE (   *,325)
             WRITE (HIST,325)
             READ  (   *,  *) FLAG(5)
             WRITE (HIST,  *) FLAG(5)
C
             WRITE (   *,330)
             WRITE (HIST,330)
             READ  (   *,  *) FLAG(6)
             WRITE (HIST,  *) FLAG(6)
         ENDIF
C
         IF ((FLAG(1).EQ.1) .OR. (FLAG(2).EQ.1)) THEN
             WRITE (   *,335)
             WRITE (HIST,335)
             READ  (   *,  *) PEC
             WRITE (HIST,  *) PEC
         ENDIF
         IF (FLAG(3).EQ.1) THEN
             WRITE (   *,345)
             WRITE (HIST,345)
             READ  (   *,  *) ALPHA
             WRITE (HIST,  *) ALPHA
         ENDIF
C
         IF (PRTFLG.EQ.1) THEN
             WRITE (   *,350) NSR
             WRITE (HIST,350) NSR
         ELSE
             WRITE (   *,352)
             WRITE (HIST,352)
         ENDIF
C
         IF (FLAG(1).EQ.1) THEN
C            ACCESS THE EXVBAM ROUTINE TO ASSIGN THE TM1C & TM2C VECTORS
C            (TO INDICATE THE ENTIRE PROGRAM IS UNDER TEST  AND THE NUM-
C            BER OF FAULT DETECTIONS IS THE  CUMULATIVE NUMBER FROM PRE-
C            VIOUS TESTING PERIODS).
             CALL EXVBAM
     G                   (FXFLG    , NS        , DAT
     Y                   ,TM2C     , TM1C      )
C
C            SET THE FLAG FOR BINOMIAL MODEL.
             BOPFLG = 1
C
             CALL SBAMMA
     G                   (DAT      , DAT(STR2) , TM1C     , TM2C
     G                   ,NS       , NSB       , NSE      , NSR
     G                   ,BOPFLG   , MAXIC     , PEC
     Y                   ,STAT(1)  , RFLAG(1)  , CNT      , TM3C      )
C
             IF (RFLAG(1).EQ.0) THEN
                 IF (PRTFLG.EQ.1) THEN
                     WRITE (   *,355) STAT(1)
                     WRITE (HIST,355) STAT(1)
                     IF (SCTFLG.EQ.1) THEN
                         CALL PLTSCT
     G                               (NSR   , TM3C  )
                     ENDIF
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,360) RFLAG(1), CNT, CNTO
                 WRITE (HIST,360) RFLAG(1), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(2).EQ.1) THEN
C            ACCESS THE EXVBAM ROUTINE TO ASSIGN THE TM1C & TM2C VECTORS
C            (TO INDICATE THE ENTIRE PROGRAM IS UNDER TEST  AND THE NUM-
C            BER OF FAULT DETECTIONS IS THE  CUMULATIVE NUMBER FROM PRE-
C            VIOUS TESTING PERIODS).
             CALL EXVBAM
     G                   (FXFLG    , NS        , DAT
     Y                   ,TM2C     , TM1C      )
C
C            SET THE FLAG FOR POISSON MODEL.
             BOPFLG = 2
C
             CALL SBAMMA
     G                   (DAT      , DAT(STR2) , TM1C     , TM2C
     G                   ,NS       , NSB       , NSE      , NSR
     G                   ,BOPFLG   , MAXIC     , PEC
     Y                   ,STAT(2)  , RFLAG(2)  , CNT      , TM3C      )
C
             IF (RFLAG(2).EQ.0) THEN
                 IF (PRTFLG.EQ.1) THEN
                     WRITE (   *,365) STAT(2)
                     WRITE (HIST,365) STAT(2)
                     IF (SCTFLG.EQ.1) THEN
                         CALL PLTSCT
     G                               (NSR   , TM3C  )
                     ENDIF
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,370) RFLAG(2), CNT, CNTO
                 WRITE (HIST,370) RFLAG(2), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(3).EQ.1) THEN
C            ACCESS THE EXVGPO ROUTINE TO ASSIGN THE TM1C VECTOR (TO IN-
C            DICATE  PARALLEL FAULT DETECTIONS AND CORRECTIONS)  AND THE
C            LCOR VARIABLE.  NOTE, THE LCOR ARGUMENT IS ONLY USED IN THE
C            PREDICTION PROCESSING (I.E., IN SGPOPR).
             CALL EXVGPO
     G                   (DAT      , NS        , FXFLG
     Y                   ,TM1C     , LCOR      )
C
C            NOTE ONLY WEIGHTING FUNCTION SELECTION FLAG OF  ALPHA INPUT
C            IS ALLOWED.  ALSO, SINCE THE WEIGHTING IS  DONE INTERNAL TO
C            SGPOMD, A SCRATCH ARRAY IS REQUIRED IN THIS ACCESS.
             CALL SGPOMA
     G                   (DAT      , DAT(STR2) , TM1C      , NS
     G                   ,NSB      , NSE       , NSR       , ALPHA
     Y                   ,STAT(3)  , RFLAG(3)  , CNT       , TM3C
     Y                   ,TM2C     )
C
             IF (RFLAG(3).EQ.0) THEN
                 IF (PRTFLG.EQ.1) THEN
                     WRITE (   *,375) STAT(3)
                     WRITE (HIST,375) STAT(3)
                     IF (SCTFLG.EQ.1) THEN
                         CALL PLTSCT
     G                               (NSR   , TM3C  )
                     ENDIF
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,380) RFLAG(3), CNT, CNTO
                 WRITE (HIST,380) RFLAG(3), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(4).EQ.1) THEN
C            STORE THE INTERVAL TESTING LENGTH DATA AND CONVERT THE DATA
C            TO A CUMULATIVE REPRESENTATION.
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = STR2, IDXEL)
             CALL SMFTRN
     G                   (NS       , DM        , DM       , 6
     B                   ,DAT(STR2)
     Y                   ,RFLG     )
C
             CALL SNPIMA
     G                   (DAT      , DAT(STR2) , NS       , NSB
     G                   ,NSE      , NSR
     Y                   ,STAT(4)  , RFLAG(4)  , CNT      , TM3C      )
C
             IF (RFLAG(4).EQ.0) THEN
                 IF (PRTFLG.EQ.1) THEN
                     WRITE (   *,385) STAT(4)
                     WRITE (HIST,385) STAT(4)
                     IF (SCTFLG.EQ.1) THEN
                         CALL PLTSCT
     G                               (NSR   , TM3C  )
                     ENDIF
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,390) RFLAG(4), CNT, CNTO
                 WRITE (HIST,390) RFLAG(4), CNT, CNTO
             ENDIF
C
C            RESTORE THE INTERVAL TESTING LENGTH DATA VECTOR.
             REWIND TMPB
             READ  (TMPB) (DAT(I), I = STR2, IDXEL)
         ENDIF
C
         IF (FLAG(5).EQ.1) THEN
C            NOTE ONLY TREATMENT TYPE SELECTION FLAG OF ALL IS ALLOWED.
             CALL SSDWMA
     G                   (DAT      , NS        , NSB       , NSE
     G                   ,NSR
     Y                   ,STAT(5)  , RFLAG(5)  , CNT       , TM3C      )
C
             IF (RFLAG(5).EQ.0) THEN
                 IF (PRTFLG.EQ.1) THEN
                     WRITE (   *,395) STAT(5)
                     WRITE (HIST,395) STAT(5)
                     IF (SCTFLG.EQ.1) THEN
                         CALL PLTSCT
     G                               (NSR   , TM3C  )
                     ENDIF
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,400) RFLAG(5), CNT, CNTO
                 WRITE (HIST,400) RFLAG(5), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(6).EQ.1) THEN
C            STORE THE INTERVAL TESTING LENGTH DATA AND CONVERT THE DATA
C            TO A CUMULATIVE REPRESENTATION.
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = STR2, IDXEL)
             CALL SMFTRN
     G                   (NS       , DM        , DM       , 6
     B                   ,DAT(STR2)
     Y                   ,RFLG     )
C
             CALL SESHMA
     G                   (DAT      , DAT(STR2) , NS       , NSB
     G                   ,NSE      , NSR
     Y                   ,STAT(6)  , RFLAG(6)  , CNT      , TM3C      )
C
             IF (RFLAG(6).EQ.0) THEN
                 IF (PRTFLG.EQ.1) THEN
                     WRITE (   *,405) STAT(6)
                     WRITE (HIST,405) STAT(6)
                     IF (SCTFLG.EQ.1) THEN
                         CALL PLTSCT
     G                               (NSR   , TM3C  )
                     ENDIF
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,410) RFLAG(6), CNT, CNTO
                 WRITE (HIST,410) RFLAG(6), CNT, CNTO
             ENDIF
C
C            RESTORE THE INTERVAL TESTING LENGTH DATA VECTOR.
             REWIND TMPB
             READ  (TMPB) (DAT(I), I = STR2, IDXEL)
         ENDIF
         RETURN
         END
         SUBROUTINE MAASET
     G                     (NS
     Y                     ,NSB   , NSE   , NSR   )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MAASET
C    G                     (NS
C    Y                     ,NSB   , NSE   , NSR   )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO ASSIGN THE RANGE FOR THE MODEL APPLICABILITY ANALYSIS.
C   DESCRIPTION
C        THIS ROUTINE DEFINES THE STARTING, ENDING, AND SIZE OF THE DATA
C        TO BE PROCESSED FOR ACCURACY, BIAS, NOISE, OR TREND ANALYSES IN
C        ACCORDANCE WITH THE SMERFS5 FORMULATION:
C             STARTING = ONE HALF OF THE SAMPLE SIZE
C             ENDING   = SAMPLE SIZE MINUS ONE
C             RANGE    = ENDING VALUE MINUS STARTING VALUE PLUS ONE
C
C        IT THEN ALLOWS THE USER TO OVER-RIDE THOSE VALUES. THIS WAS PUT
C        IN THE SMERFS5 DRIVER  TO SIMPLIFY THE CHECK OUT OF THE PROGRAM
C        WHEN TESTING BY THE RE-EXECUTION OF PUBLISHED CASES.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C   ARGUMENT LIST
C        GIVEN
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET FOR TBF DATA)
C        YIELDED
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C   LOCAL GLOSSARY
C             FLAG   (I) = OPTION FLAG
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
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              NS            ,NSB           ,NSE
     1                       ,NSR
C
C                             LOCAL SPECIFICATIONS
         INTEGER              FLAG
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'   THE ANALYSIS OF THE DATA SET OF SIZE',I4,' WILL (B'
     2          ,'Y DEFAULT) OCCUR'
     3        ,/,'   BETWEEN ELEMENTS',I4,' AND',I4,'. ENTER ONE TO USE'
     4          ,' A DIFFERENT RANGE'
     5        ,/,'   OR ZERO TO USE THIS RANGE OF',I4,' ELEMENTS.')
  305    FORMAT ('    ENTER STARTING INDEX, GREATER THAN 1.')
  310    FORMAT ('    ENTER ENDING INDEX, LESS THAN',I4,'.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
         NSB = INT(REAL(NS) * 0.5)
         NSE = NS  - 1
         NSR = NSE - NSB + 1
C
         WRITE (   *,300) NS    , NSB   , NSE   , NSR
         WRITE (HIST,300) NS    , NSB   , NSE   , NSR
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
C
         IF (FLAG.EQ.1) THEN
             WRITE (   *,305)
             WRITE (HIST,305)
             READ  (   *,  *) NSB
             WRITE (HIST,  *) NSB
             WRITE (   *,310) NS
             WRITE (HIST,310) NS
             READ  (   *,  *) NSE
             WRITE (HIST,  *) NSE
             NSR = NSE - NSB + 1
         ENDIF
         RETURN
         END
         SUBROUTINE MAASMI
     G                     (FLAGRI, FLAGSI
     B                     ,STATI )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MAASMI
C    G                     (FLAGRI, FLAGSI
C    B                     ,STATI )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PRODUCE A SUMMARY TABLE OF THE  MODEL APPLICABILITY ANALYSES
C        FOR THE INTERVAL DATA TYPE.
C   DESCRIPTION
C        THIS ROUTINE FIRST REPLACES THE STATISTICS FOR ALL MODELS WHICH
C        WERE NOT EXECUTED OR NOT FULLY  SUCCESSFUL WITH A ZERO.  IN THE
C        LOOP THE VALUES ARE STORED LOCALLY, SO THEY MAY BE ALTERED.
C
C        A LOOP OF THE NUMBER OF SUCCESSFUL EXECUTIONS IS THEN INITIATED
C        TO DETERMINE THE MODEL WITH THE WORST PREDICTIONS.  THE RANK IS
C        STORED AND THE VALUE IS RESET TO ZERO. IN THIS MANNER, THE ROU-
C        TINE ALWAYS SEARCHES FOR THE MAXIMUM.
C
C        THE TABLE OF REAL VALUES AND RANKS IS THEN SIMPLY OUTPUT.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C   ARGUMENT LIST
C        GIVEN
C             FLAGRI (I) = MODEL RESULT ERROR FLAGS (INTERVAL DATA)
C             FLAGSI (I) = MODEL SELECTION FLAGS    (INTERVAL DATA)
C        BOTH
C             STATI  (R) = MODEL RESULT STATISTICS  (INTERVAL DATA)
C   LOCAL GLOSSARY
C             ODR    (I) = ORDER OR RANK OF THE MODEL
C             TMP    (R) = LOCAL COPY OF THE STATI ARRAY
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
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              FLAGRI(6)     ,FLAGSI(6)
         DIMENSION            STATI(6)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,ICNT          ,IDX
     1                       ,J             ,ODR(6)
         DIMENSION            TMP(6)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'   THE FOLLOWING TABLE SUMMARIZES THE PERFORMED MODEL'
     2          ,' APPLICABILITY'
     3        ,/,'   ANALYSIS.  ZEROS UNDER THE VALUES PORTION INDICATE'
     4          ,' THE MODEL WAS'
     5        ,/,'   EITHER NOT RUN OR FAILED PRIOR TO COMPLETION. THE '
     7          ,'INTEGER NUMBER'
     7        ,/,'   TO THE RIGHT OF THE VALUE REFLECTS THE RANKING.')
  305    FORMAT (' '
     1        ,/,'   MODEL  ACCURACY/RANK'
     3        ,/,'   =====  ============='
     2       ,//,'   BAM-B ',E12.5,I2
     3        ,/,'   BAM-P ',E12.5,I2
     4        ,/,'   GPN   ',E12.5,I2
     5        ,/,'   NPI   ',E12.5,I2
     6        ,/,'   SDW   ',E12.5,I2
     7        ,/,'   SSH   ',E12.5,I2)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         ICNT = 6
C
         DO 1000 I = 1, 6
             ODR(I) = 0
             IF ((FLAGSI(I).NE.1) .OR. (FLAGRI(I).NE.0)) THEN
C                THE MODEL WAS EITHER NOT RUN OR FAILED PRIOR TO COMPLE-
C                TION; REDEFINE THE RESULTING STATISTIC TO ZERO.
                 STATI(I) = 0.0
                 ICNT     = ICNT - 1
             ENDIF
C
C            SAVE THE STATISTICS, SO THE VALUES CAN BE RESET AS MAXIMUMS
C            ARE FOUND.
             TMP(I) = STATI(I)
 1000        CONTINUE
C        ENDDO
C
         DO 1050 I = ICNT, 1, -1
             MAX = 0.0
C
C            LOCATE THE MODEL WITH THE WORST PREDICTIONS.
             DO 1025 J = 1, 6
                 IF (TMP(J).GT.MAX) THEN
                     MAX = TMP(J)
                     IDX = J
                 ENDIF
 1025            CONTINUE
C            ENDDO
C
C            STORE THE COMPUTED  RANK AND ZERO THE STATISTIC SO THE NEXT
C            CAN BE DETERMINED.
             ODR(IDX) = I
             TMP(IDX) = 0.0
 1050        CONTINUE
C        ENDDO
C
         WRITE (   *,300)
         WRITE (HIST,300)
         WRITE (   *,305) (STATI(I), ODR(I), I = 1, 6)
         WRITE (HIST,305) (STATI(I), ODR(I), I = 1, 6)
         RETURN
         END
         SUBROUTINE MAASMT
     G                     (FLAGRT, FLAGST
     B                     ,STATT )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MAASMT
C    G                     (FLAGRT, FLAGST
C    B                     ,STATT )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PRODUCE A SUMMARY TABLE OF THE  MODEL APPLICABILITY ANALYSES
C        FOR THE TBF DATA TYPE.
C   DESCRIPTION
C        A MAIN LOOP FOR THE NUMBER OF ANALYSES (4) IS INITIATED. DURING
C        THAT LOOP, EACH ANALYSIS WILL BE PROCESSED INDEPENDENTLY (I.E.,
C        NO ATTEMPT TO DETERMINE THE OVERALL RANKING IS MADE, HERE).
C
C        THIS ROUTINE FIRST REPLACES THE STATISTICS FOR ALL MODELS WHICH
C        WERE NOT EXECUTED OR NOT FULLY  SUCCESSFUL WITH A ZERO.  IN THE
C        LOOP THE VALUES ARE STORED LOCALLY, SO THEY MAY BE ALTERED.
C
C        A LOOP OF THE NUMBER OF SUCCESSFUL EXECUTIONS IS THEN INITIATED
C        TO DETERMINE THE MODEL WITH THE WORST PREDICTIONS.  THE RANK IS
C        STORED AND THE VALUE IS RESET TO ZERO. IN THIS MANNER, THE ROU-
C        TINE ALWAYS SEARCHES FOR THE MAXIMUM.
C
C        AFTER ALL FOUR ANALYSES ARE PROCESSED, THE TABLE OF REAL VALUES
C        AND RANKS IS OUTPUT.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C   ARGUMENT LIST
C        GIVEN
C             FLAGRT (I) = MODEL RESULT ERROR FLAGS (TBF DATA)
C             FLAGST (I) = MODEL SELECTION FLAGS    (TBF DATA)
C        BOTH
C             STATT  (R) = MODEL RESULT STATISTICS  (TBF DATA)
C   LOCAL GLOSSARY
C             AVG    (R) = AVERAGE RANK OF THE MODELS
C             CNT    (I) = NUMBER OF ANALYSES PER MODEL
C             ODR    (I) = ORDER OR RANK OF THE MODEL
C             TMP    (R) = LOCAL COPY OF ONE COLUMN OF THE STATT ARRAY
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
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              FLAGRT(7,4)   ,FLAGST(7,4)
         DIMENSION            STATT(7,4)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CNT           ,I             ,ICNT
     1                       ,IDX           ,J             ,K
     2                       ,ODR(7,4)
         DIMENSION            AVG(7)        ,TMP(7)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'   THE FOLLOWING TABLE SUMMARIZES THE PERFORMED MODEL'
     2          ,' APPLICABILITY'
     3        ,/,'   ANALYSIS.  ZEROS UNDER THE VALUES PORTION INDICATE'
     4          ,' THE MODEL WAS'
     5        ,/,'   EITHER NOT RUN OR FAILED PRIOR TO COMPLETION. THE '
     7          ,'INTEGER NUMBER'
     8        ,/,'   TO THE RIGHT OF THE VALUE REFLECTS THE RANKING. TH'
     9          ,'E VALUE TO THE'
     9        ,/,'   FAR RIGHT REPRESENTS THE AVERAGE RANKING OF EACH M'
     A          ,'ODEL.')
  305    FORMAT (' '
     1        ,/,'   MODEL  ACCURACY/RANK   BIAS/RANK    NOISE/RANK    '
     2          ,'TREND/RANK   AVG'
     3        ,/,'   =====  ============= ============= ============= ='
     4          ,'============ ==='
     5       ,//,'   GEO   ',E12.5,I2,E12.5,I2,E12.5,I2,E12.5,I2,F4.1
     6        ,/,'   JAM   ',E12.5,I2,E12.5,I2,E12.5,I2,E12.5,I2,F4.1
     7        ,/,'   LAV-L ',E12.5,I2,E12.5,I2,E12.5,I2,E12.5,I2,F4.1
     8        ,/,'   LAV-Q ',E12.5,I2,E12.5,I2,E12.5,I2,E12.5,I2,F4.1
     9        ,/,'   MUS-B ',E12.5,I2,E12.5,I2,E12.5,I2,E12.5,I2,F4.1
     A        ,/,'   MUS-L ',E12.5,I2,E12.5,I2,E12.5,I2,E12.5,I2,F4.1
     B        ,/,'   NPT   ',E12.5,I2,E12.5,I2,E12.5,I2,E12.5,I2,F4.1)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         DO 1075 K = 1, 4
             ICNT = 7
C
             DO 1000 I = 1, 7
                 ODR(I,K) = 0
                 IF ((FLAGST(I,K).NE.1) .OR. (FLAGRT(I,K).NE.0)) THEN
C                    THE MODEL  WAS EITHER  NOT RUN OR  FAILED  PRIOR TO
C                    COMPLETION; REDEFINE THE RESULTING STATISTIC TO 0.
                     STATT(I,K) = 0.0
                     ICNT       = ICNT - 1
                 ENDIF
C
C                SAVE THE STATISTICS, SO THE VALUES CAN BE  RESET AS THE
C                MAXIMUMS ARE FOUND.
                 TMP(I) = STATT(I,K)
 1000            CONTINUE
C            ENDDO
C
             DO 1050 I = ICNT, 1, -1
                 MAX = 0.0
C
C                LOCATE THE MODEL WITH THE WORST PREDICTIONS.
                 DO 1025 J = 1, 7
                     IF (TMP(J).GT.MAX) THEN
                         MAX = TMP(J)
                         IDX = J
                     ENDIF
 1025                CONTINUE
C                ENDDO
C
C                STORE THE COMPUTED  RANK AND  ZERO THE STATISTIC SO THE
C                NEXT CAN BE DETERMINED.
                 ODR(IDX,K) = I
                 TMP(IDX)   = 0.0
 1050            CONTINUE
C            ENDDO
 1075        CONTINUE
C        ENDDO
C
         DO 1125 I = 1, 7
             AVG(I) = 0.0
             CNT    = 0
             DO 1100 J = 1, 4
                 IF (ODR(I,J).NE.0) THEN
                     CNT    = CNT    + 1
                     AVG(I) = AVG(I) + REAL(ODR(I,J))
                 ENDIF
 1100            CONTINUE
C            ENDDO
             IF (CNT.GT.0) THEN
                 AVG(I) = AVG(I) / REAL(CNT)
             ENDIF
 1125        CONTINUE
C        ENDDO
C
         WRITE (   *,300)
         WRITE (   *,305) (STATT(I,1), ODR(I,1)
     1                    ,STATT(I,2), ODR(I,2)
     2                    ,STATT(I,3), ODR(I,3)
     3                    ,STATT(I,4), ODR(I,4), AVG(I), I = 1, 7)
         WRITE (HIST,300)
         WRITE (HIST,305) (STATT(I,1), ODR(I,1)
     1                    ,STATT(I,2), ODR(I,2)
     2                    ,STATT(I,3), ODR(I,3)
     3                    ,STATT(I,4), ODR(I,4), AVG(I), I = 1, 7)
         RETURN
         END
         SUBROUTINE MAATAC
     G                     (IDX   , NS    , NSB   , NSE   , NSR
     G                     ,SCTFLG
     Y                     ,RFLAG , FLAG  , STAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MAATAC
C    G                     (IDX   , NS    , NSB   , NSE   , NSR
C    G                     ,SCTFLG
C    Y                     ,RFLAG , FLAG  , STAT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE THE MENUS AND TRANSFERS FOR THE EXECUTION OF  AN AC-
C        CURACY ANALYSIS OF THE  TBF DATA PRIOR TO THE  EXECUTION OF THE
C        SOFTWARE RELIABILITY MODELS (VIA MODULE 8).
C   DESCRIPTION
C        THE ONLY REAL PROCESSING INVOLVED  IN THIS ROUTINE INVOLVES THE
C        SETTING OF SOME  PRELIMINARY POINTERS FOR THE DATA  AND THE SE-
C        LECTION OF  WHICH OF THE MODELS  ARE TO BE EXECUTED.  A SPECIAL
C        OPTION CAN BE SET TO INDICATE THAT ALL MODELS ARE TO BE EXECUT-
C        ED DURING THE ANALYSIS.
C
C        THE REQUIRED COMPUTATIONS WILL BE PERFORMED  BY ROUTINES OF THE
C        SMFLIB LIBRARY.  UPON RETURN  THIS ROUTINE WILL OUTPUT THE COM-
C        PUTED STATISTIC  AND OPTIONALLY ACCESS THE PLTSCT ROUTINE FOR A
C        PLOT OF THE COMPUTED VALUES.
C
C        THE MODEL SELECTION FLAG, THE MODEL ERROR FLAG, AND THE RESULT-
C        ING STATISTICS ARE RETURNED TO THE CALLING ROUTINE  SO THAT THE
C        OVERALL (SUMMARY) MODEL APPLICABILITY RANKING CAN BE COMPUTED.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C                          (BUT RETURNED TO ORIGINAL VALUES)
C        YIELDED
C             TM3C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS SCATTER PLOT.
C             TM4C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS SCRATCH ARRAY.
C   ARGUMENT LIST
C        GIVEN
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C             SCTFLG (I) = FLAG INDICATING IF SCATTER PLOTS DESIRED
C        YIELDED
C             FLAG   (I) = MODEL SELECTION FLAGS
C             RFLAG  (I) = MODEL RESULTS ERROR FLAGS
C             STAT   (R) = PREQUENTIAL LIKELIHOOD STATISTICS
C   LOCAL GLOSSARY
C             ALLFLG (I) = OPTION FLAG TO INDICATE ALL MODELS WANTED
C             CNT    (I) = ANALYSIS ITERATION COUNT (AT RETURN)
C             CNTO   (I) = SAMPLE OBSERVATION COUNT
C             DM     (R) = DUMMY ARGUMENT FOR SMFTRN ACCESS
C             IDXE   (I) = INDEX FOR TBF DATA ENDING ADDRESS
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS (SET TO 100)
C             PHIIND (I) = LAV PHI INDICATOR FLAG (LINEAR OR QUADRATIC)
C             RFLG   (I) = DUMMY ARGUMENT FOR SMFTRN ACCESS
C             TYP    (I) = FLAG TO INDICATE ACCURACY ANALYSIS (1)
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
         INTEGER              END1
         PARAMETER           (END1   =   1000)
         INTEGER              END2
         PARAMETER           (END2   =   2000)
C
C                             GLOBAL SPECIFICATIONS
C
C               /DATCOM/      DATA AND PROCESSING COMMON
         COMMON /DATCOM/      DAT(END2)     ,DATTYP        ,PRC(3,3)
     1                       ,UPDFLG
         INTEGER              DATTYP        ,PRC           ,UPDFLG
C
C               /MAACOM/      MODEL APPLICABILITY ANALYSIS COMMON
         COMMON /MAACOM/      TM3C(END1)    ,TM4C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              FLAG(7)       ,IDX           ,NS
     1                       ,NSB           ,NSE           ,NSR
     2                       ,RFLAG(7)      ,SCTFLG
         DIMENSION            STAT(7)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              ALLFLG        ,CNT           ,CNTO
     1                       ,I             ,IDXE          ,MAXIC
     2                       ,PHIIND        ,RFLG          ,TYP
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'    ENTER ONE TO SELECT THE MODELS, OR ZERO TO RUN AL'
     2          ,'L SEVEN.')
  305    FORMAT ('    ENTER ONE FOR GEOMETRIC MODEL; ELSE ZERO.')
  310    FORMAT ('    ENTER ONE FOR JELINSKI/MORANDA MODEL; ELSE ZERO.')
  315    FORMAT ('    ENTER ONE FOR LITTLEWOOD AND VERRALL-LIN  MODEL; '
     1          ,'ELSE ZERO.')
  320    FORMAT ('    ENTER ONE FOR LITTLEWOOD AND VERRALL-QUAD MODEL; '
     1          ,'ELSE ZERO.')
  325    FORMAT ('    ENTER ONE FOR MUSA BASIC EXECUTION TIME MODEL; EL'
     1          ,'SE ZERO.')
  330    FORMAT ('    ENTER ONE FOR MUSA LOG POISSON EXE. TIME MODEL;  '
     1          ,'ELSE ZERO.')
  335    FORMAT ('    ENTER ONE FOR NHPP MODEL FOR TBF DATA; ELSE ZERO.'
     1          )
  340    FORMAT (' '
     1        ,/,'    THE FOLLOWING WERE DETERMINED BASED ON ',I3,' POI'
     2          ,'NTS')
  345    FORMAT ('    GEOMETRIC                   STATISTIC IS ',E15.8)
  350    FORMAT ('  **GEOMETRIC                   FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  355    FORMAT ('    JELINSKI/MORANDA            STATISTIC IS ',E15.8)
  360    FORMAT ('  **JELINSKI/MORANDA            FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  365    FORMAT ('    LITTLEWOOD AND VERRALL-LIN  STATISTIC IS ',E15.8)
  370    FORMAT ('  **LITTLEWOOD AND VERRALL-LIN  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  375    FORMAT ('    LITTLEWOOD AND VERRALL-QUAD STATISTIC IS ',E15.8)
  380    FORMAT ('  **LITTLEWOOD AND VERRALL-QUAD FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  385    FORMAT ('    MUSA BASIC EXECUTION        STATISTIC IS ',E15.8)
  390    FORMAT ('  **MUSA BASIC EXECUTION        FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  395    FORMAT ('    MUSA LOG POISSON EXECUTION  STATISTIC IS ',E15.8)
  400    FORMAT ('  **MUSA LOG POISSON EXECUTION  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  405    FORMAT ('    NHPP FOR TBF DATA           STATISTIC IS ',E15.8)
  410    FORMAT ('  **NHPP FOR TBF DATA           FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE DATA VALUES TREATED AS CONSTANTS IN THIS ROUTINE.
         IDXE  = IDX + NS - 1
         MAXIC = 100
         TYP   = 1
C
C        DETERMINE IF ALL EXECUTION TIME MODELS ARE TO BE EXECUTED.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) ALLFLG
         WRITE (HIST,  *) ALLFLG
C
         IF (ALLFLG.NE.1) THEN
C            ALL MODELS ARE DESIRED IN THIS EXECUTION.
             DO 1000 I = 1, 7
                 FLAG(I) = 1
 1000            CONTINUE
C            ENDDO
         ELSE
C            ONLY CERTAIN MODELS ARE DESIRED IN THIS EXECUTION.
             WRITE (   *,305)
             WRITE (HIST,305)
             READ  (   *,  *) FLAG(1)
             WRITE (HIST,  *) FLAG(1)
C
             WRITE (   *,310)
             WRITE (HIST,310)
             READ  (   *,  *) FLAG(2)
             WRITE (HIST,  *) FLAG(2)
C
             WRITE (   *,315)
             WRITE (HIST,315)
             READ  (   *,  *) FLAG(3)
             WRITE (HIST,  *) FLAG(3)
C
             WRITE (   *,320)
             WRITE (HIST,320)
             READ  (   *,  *) FLAG(4)
             WRITE (HIST,  *) FLAG(4)
C
             WRITE (   *,325)
             WRITE (HIST,325)
             READ  (   *,  *) FLAG(5)
             WRITE (HIST,  *) FLAG(5)
C
             WRITE (   *,330)
             WRITE (HIST,330)
             READ  (   *,  *) FLAG(6)
             WRITE (HIST,  *) FLAG(6)
C
             WRITE (   *,335)
             WRITE (HIST,335)
             READ  (   *,  *) FLAG(7)
             WRITE (HIST,  *) FLAG(7)
         ENDIF
C
         WRITE (   *,340) NSR
         WRITE (HIST,340) NSR
C
         IF (FLAG(1).EQ.1) THEN
             CALL SGEOMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(1) , RFLAG(1), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(1).EQ.0) THEN
                 WRITE (   *,345) STAT(1)
                 WRITE (HIST,345) STAT(1)
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM3C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,350) RFLAG(1), CNT, CNTO
                 WRITE (HIST,350) RFLAG(1), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(2).EQ.1) THEN
             CALL SJAMMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(2) , RFLAG(2), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(2).EQ.0) THEN
                 WRITE (   *,355) STAT(2)
                 WRITE (HIST,355) STAT(2)
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM3C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,360) RFLAG(2), CNT, CNTO
                 WRITE (HIST,360) RFLAG(2), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(3).EQ.1) THEN
C            SET THE FLAG FOR LINEAR MODEL.
             PHIIND = 1
C
             CALL SLAVMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP     , MAXIC   , PHIIND
     Y                   ,STAT(3) , RFLAG(3), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(3).EQ.0) THEN
                 WRITE (   *,365) STAT(3)
                 WRITE (HIST,365) STAT(3)
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM3C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,370) RFLAG(3), CNT, CNTO
                 WRITE (HIST,370) RFLAG(3), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(4).EQ.1) THEN
C            SET THE FLAG FOR QUADRATIC MODEL.
             PHIIND = 2
C
             CALL SLAVMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP     , MAXIC   , PHIIND
     Y                   ,STAT(4) , RFLAG(4), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(4).EQ.0) THEN
                 WRITE (   *,375) STAT(4)
                 WRITE (HIST,375) STAT(4)
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM3C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,380) RFLAG(4), CNT, CNTO
                 WRITE (HIST,380) RFLAG(4), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(5).EQ.1) THEN
             CALL SMUSMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(5) , RFLAG(5), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(5).EQ.0) THEN
                 WRITE (   *,385) STAT(5)
                 WRITE (HIST,385) STAT(5)
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM3C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,390) RFLAG(5), CNT, CNTO
                 WRITE (HIST,390) RFLAG(5), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(6).EQ.1) THEN
             CALL SMSAMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(6) , RFLAG(6), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(6).EQ.0) THEN
                 WRITE (   *,395) STAT(6)
                 WRITE (HIST,395) STAT(6)
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM3C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,400) RFLAG(6), CNT, CNTO
                 WRITE (HIST,400) RFLAG(6), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(7).EQ.1) THEN
C            STORE THE ORIGINAL TBF DATA AND CONVERT DATA TO TTF UNITS.
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = IDX, IDXE)
             CALL SMFTRN
     G                   (NS      , DM      , DM      , 6
     B                   ,DAT(IDX)
     Y                   ,RFLG    )
C
             CALL SNPTMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(7) , RFLAG(7), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(7).EQ.0) THEN
                 WRITE (   *,405) STAT(7)
                 WRITE (HIST,405) STAT(7)
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM3C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,410) RFLAG(7), CNT, CNTO
                 WRITE (HIST,410) RFLAG(7), CNT, CNTO
             ENDIF
C
C            RESTORE THE TBF DATA.
             REWIND TMPB
             READ  (TMPB) (DAT(I), I = IDX, IDXE)
         ENDIF
         RETURN
         END
         SUBROUTINE MAATBS
     G                     (IDX   , NS    , NSB   , NSE   , NSR
     G                     ,SCTFLG
     Y                     ,RFLAG , FLAG  , STAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MAATBS
C    G                     (IDX   , NS    , NSB   , NSE   , NSR
C    G                     ,SCTFLG
C    Y                     ,RFLAG , FLAG  , STAT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE THE MENUS AND TRANSFERS FOR THE  EXECUTION OF A BIAS
C        ANALYSIS OF THE TBF DATA PRIOR TO THE EXECUTION OF THE SOFTWARE
C        RELIABILITY MODELS (VIA MODULE 8).
C   DESCRIPTION
C        THE ONLY REAL PROCESSING INVOLVED  IN THIS ROUTINE INVOLVES THE
C        SETTING OF SOME  PRELIMINARY POINTERS FOR THE DATA  AND THE SE-
C        LECTION OF  WHICH OF THE MODELS  ARE TO BE EXECUTED.  A SPECIAL
C        OPTION CAN BE SET TO INDICATE THAT ALL MODELS ARE TO BE EXECUT-
C        ED DURING THE ANALYSIS.
C
C        THE REQUIRED COMPUTATIONS WILL BE PERFORMED  BY ROUTINES OF THE
C        SMFLIB LIBRARY.  UPON RETURN  THIS ROUTINE WILL OUTPUT THE COM-
C        PUTED STATISTIC, ACCESS THE PLTUAY ROUTINE TO PRODUCE A U-PLOT,
C        AND OPTIONALLY ACCESS THE PLTSCT ROUTINE  FOR A PLOT OF THE UN-
C        SORTED VALUES.
C
C        THE MODEL SELECTION FLAG, THE MODEL ERROR FLAG, AND THE RESULT-
C        ING STATISTICS ARE RETURNED TO THE CALLING ROUTINE  SO THAT THE
C        OVERALL (SUMMARY) MODEL APPLICABILITY RANKING CAN BE COMPUTED.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C                          (BUT RETURNED TO ORIGINAL VALUES)
C        YIELDED
C             TM3C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS U-PLOT.
C             TM4C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS "UNSORTED".
C   ARGUMENT LIST
C        GIVEN
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C             SCTFLG (I) = FLAG INDICATING IF SCATTER PLOTS DESIRED
C        YIELDED
C             FLAG   (I) = MODEL SELECTION FLAGS
C             RFLAG  (I) = MODEL RESULTS ERROR FLAGS
C             STAT   (R) = KOLMOGOROV STATISTICS
C   LOCAL GLOSSARY
C             ALLFLG (I) = OPTION FLAG TO INDICATE ALL MODELS WANTED
C             CNT    (I) = ANALYSIS ITERATION COUNT (AT RETURN)
C             CNTO   (I) = SAMPLE OBSERVATION COUNT
C             DM     (R) = DUMMY ARGUMENT FOR SMFTRN ACCESS
C             IDXE   (I) = INDEX FOR TBF DATA ENDING ADDRESS
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS (SET TO 100)
C             PHIIND (I) = LAV PHI INDICATOR FLAG (LINEAR OR QUADRATIC)
C             RFLG   (I) = DUMMY ARGUMENT FOR SMFTRN ACCESS
C             TYP    (I) = FLAG TO INDICATE BIAS ANALYSIS (2)
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
         INTEGER              END1
         PARAMETER           (END1   =   1000)
         INTEGER              END2
         PARAMETER           (END2   =   2000)
C
C                             GLOBAL SPECIFICATIONS
C
C               /DATCOM/      DATA AND PROCESSING COMMON
         COMMON /DATCOM/      DAT(END2)     ,DATTYP        ,PRC(3,3)
     1                       ,UPDFLG
         INTEGER              DATTYP        ,PRC           ,UPDFLG
C
C               /MAACOM/      MODEL APPLICABILITY ANALYSIS COMMON
         COMMON /MAACOM/      TM3C(END1)    ,TM4C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              FLAG(7)       ,IDX           ,NS
     1                       ,NSB           ,NSE           ,NSR
     2                       ,RFLAG(7)      ,SCTFLG
         DIMENSION            STAT(7)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              ALLFLG        ,CNT           ,CNTO
     1                       ,I             ,IDXE          ,MAXIC
     2                       ,PHIIND        ,RFLG          ,TYP
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'    ENTER ONE TO SELECT THE MODELS, OR ZERO TO RUN AL'
     2          ,'L SEVEN.')
  305    FORMAT ('    ENTER ONE FOR GEOMETRIC MODEL; ELSE ZERO.')
  310    FORMAT ('    ENTER ONE FOR JELINSKI/MORANDA MODEL; ELSE ZERO.')
  315    FORMAT ('    ENTER ONE FOR LITTLEWOOD AND VERRALL-LIN  MODEL; '
     1          ,'ELSE ZERO.')
  320    FORMAT ('    ENTER ONE FOR LITTLEWOOD AND VERRALL-QUAD MODEL; '
     1          ,'ELSE ZERO.')
  325    FORMAT ('    ENTER ONE FOR MUSA BASIC EXECUTION TIME MODEL; EL'
     1          ,'SE ZERO.')
  330    FORMAT ('    ENTER ONE FOR MUSA LOG POISSON EXE. TIME MODEL;  '
     1          ,'ELSE ZERO.')
  335    FORMAT ('    ENTER ONE FOR NHPP MODEL FOR TBF DATA; ELSE ZERO.'
     1          )
  345    FORMAT ('                     U-PLOT FOR GEOMETRIC'
     1        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     2        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  350    FORMAT (' **GEOMETRIC                   FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  355    FORMAT ('                     U-PLOT FOR JELINSKI/MORANDA'
     1        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     2        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  360    FORMAT (' **JELINSKI/MORANDA            FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  365    FORMAT ('                     U-PLOT FOR LITTLEWOOD AND VERRAL'
     1          ,'L-LIN'
     2        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     3        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  370    FORMAT (' **LITTLEWOOD AND VERRALL-LIN  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  375    FORMAT ('                     U-PLOT FOR LITTLEWOOD AND VERRAL'
     1          ,'L-QUAD'
     2        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     3        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  380    FORMAT (' **LITTLEWOOD AND VERRALL-QUAD FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  385    FORMAT ('                     U-PLOT FOR MUSA BASIC EXECUTION'
     1        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     2        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  390    FORMAT (' **MUSA BASIC EXECUTION        FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  395    FORMAT ('                     U-PLOT FOR MUSA LOG POISSON EXEC'
     1          ,'UTION'
     2        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     3        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  400    FORMAT (' **MUSA LOG POISSON EXECUTION  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  405    FORMAT ('                     U-PLOT FOR NHPP FOR TBF DATA'
     1        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     2        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  410    FORMAT (' **NHPP FOR TBF DATA           FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE DATA VALUES TREATED AS CONSTANTS IN THIS ROUTINE.
         IDXE  = IDX + NS - 1
         MAXIC = 100
         TYP   = 2
C
C        DETERMINE IF ALL EXECUTION TIME MODELS ARE TO BE EXECUTED.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) ALLFLG
         WRITE (HIST,  *) ALLFLG
C
         IF (ALLFLG.NE.1) THEN
C            ALL MODELS ARE DESIRED IN THIS EXECUTION.
             DO 1000 I = 1, 7
                 FLAG(I) = 1
 1000            CONTINUE
C            ENDDO
         ELSE
C            ONLY CERTAIN MODELS ARE DESIRED IN THIS EXECUTION.
             WRITE (   *,305)
             WRITE (HIST,305)
             READ  (   *,  *) FLAG(1)
             WRITE (HIST,  *) FLAG(1)
C
             WRITE (   *,310)
             WRITE (HIST,310)
             READ  (   *,  *) FLAG(2)
             WRITE (HIST,  *) FLAG(2)
C
             WRITE (   *,315)
             WRITE (HIST,315)
             READ  (   *,  *) FLAG(3)
             WRITE (HIST,  *) FLAG(3)
C
             WRITE (   *,320)
             WRITE (HIST,320)
             READ  (   *,  *) FLAG(4)
             WRITE (HIST,  *) FLAG(4)
C
             WRITE (   *,325)
             WRITE (HIST,325)
             READ  (   *,  *) FLAG(5)
             WRITE (HIST,  *) FLAG(5)
C
             WRITE (   *,330)
             WRITE (HIST,330)
             READ  (   *,  *) FLAG(6)
             WRITE (HIST,  *) FLAG(6)
C
             WRITE (   *,335)
             WRITE (HIST,335)
             READ  (   *,  *) FLAG(7)
             WRITE (HIST,  *) FLAG(7)
         ENDIF
C
         IF (FLAG(1).EQ.1) THEN
             CALL SGEOMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(1) , RFLAG(1), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(1).EQ.0) THEN
                 WRITE (   *,345) STAT(1), NSR   , NS
                 WRITE (HIST,345) STAT(1), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 1       )
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM4C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,350) RFLAG(1), CNT, CNTO
                 WRITE (HIST,350) RFLAG(1), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(2).EQ.1) THEN
             CALL SJAMMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(2) , RFLAG(2), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(2).EQ.0) THEN
                 WRITE (   *,355) STAT(2), NSR   , NS
                 WRITE (HIST,355) STAT(2), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 1       )
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM4C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,360) RFLAG(2), CNT, CNTO
                 WRITE (HIST,360) RFLAG(2), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(3).EQ.1) THEN
C            SET THE FLAG FOR LINEAR MODEL.
             PHIIND = 1
C
             CALL SLAVMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP     , MAXIC   , PHIIND
     Y                   ,STAT(3) , RFLAG(3), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(3).EQ.0) THEN
                 WRITE (   *,365) STAT(3), NSR   , NS
                 WRITE (HIST,365) STAT(3), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 1       )
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM4C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,370) RFLAG(3), CNT, CNTO
                 WRITE (HIST,370) RFLAG(3), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(4).EQ.1) THEN
C            SET THE FLAG FOR QUADRATIC MODEL.
             PHIIND = 2
C
             CALL SLAVMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP     , MAXIC   , PHIIND
     Y                   ,STAT(4) , RFLAG(4), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(4).EQ.0) THEN
                 WRITE (   *,375) STAT(4), NSR   , NS
                 WRITE (HIST,375) STAT(4), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 1       )
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM4C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,380) RFLAG(4), CNT, CNTO
                 WRITE (HIST,380) RFLAG(4), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(5).EQ.1) THEN
             CALL SMUSMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(5) , RFLAG(5), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(5).EQ.0) THEN
                 WRITE (   *,385) STAT(5), NSR   , NS
                 WRITE (HIST,385) STAT(5), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 1       )
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM4C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,390) RFLAG(5), CNT, CNTO
                 WRITE (HIST,390) RFLAG(5), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(6).EQ.1) THEN
             CALL SMSAMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(6) , RFLAG(6), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(6).EQ.0) THEN
                 WRITE (   *,395) STAT(6), NSR   , NS
                 WRITE (HIST,395) STAT(6), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 1       )
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM4C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,400) RFLAG(6), CNT, CNTO
                 WRITE (HIST,400) RFLAG(6), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(7).EQ.1) THEN
C            STORE THE ORIGINAL TBF DATA AND CONVERT DATA TO TTF UNITS.
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = IDX, IDXE)
             CALL SMFTRN
     G                   (NS      , DM      , DM      , 6
     B                   ,DAT(IDX)
     Y                   ,RFLG    )
C
             CALL SNPTMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(7) , RFLAG(7), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(7).EQ.0) THEN
                 WRITE (   *,405) STAT(7), NSR   , NS
                 WRITE (HIST,405) STAT(7), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 1       )
                 IF (SCTFLG.EQ.1) THEN
                     CALL PLTSCT
     G                           (NSR   , TM4C  )
                 ENDIF
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,410) RFLAG(7), CNT, CNTO
                 WRITE (HIST,410) RFLAG(7), CNT, CNTO
             ENDIF
C
C            RESTORE THE TBF DATA.
             REWIND TMPB
             READ  (TMPB) (DAT(I), I = IDX, IDXE)
         ENDIF
         RETURN
         END
         SUBROUTINE MAATNS
     G                     (IDX   , NS    , NSB   , NSE   , NSR
     Y                     ,RFLAG , FLAG  , STAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MAATNS
C    G                     (IDX   , NS    , NSB   , NSE   , NSR
C    Y                     ,RFLAG , FLAG  , STAT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE THE  MENUS AND TRANSFERS FOR THE  EXECUTION OF NOISE
C        ANALYSIS OF THE TBF DATA PRIOR TO THE EXECUTION OF THE SOFTWARE
C        RELIABILITY MODELS (VIA MODULE 8).
C   DESCRIPTION
C        THE ONLY REAL PROCESSING INVOLVED  IN THIS ROUTINE INVOLVES THE
C        SETTING OF SOME  PRELIMINARY POINTERS FOR THE DATA  AND THE SE-
C        LECTION OF  WHICH OF THE MODELS  ARE TO BE EXECUTED.  A SPECIAL
C        OPTION CAN BE SET TO INDICATE THAT ALL MODELS ARE TO BE EXECUT-
C        ED DURING THE ANALYSIS.
C
C        THE REQUIRED COMPUTATIONS WILL BE PERFORMED  BY ROUTINES OF THE
C        SMFLIB LIBRARY. UPON RETURN THIS ROUTINE WILL SIMPLY OUTPUT THE
C        COMPUTED STATISTIC.
C
C        THE MODEL SELECTION FLAG, THE MODEL ERROR FLAG, AND THE RESULT-
C        ING STATISTICS ARE RETURNED TO THE CALLING ROUTINE  SO THAT THE
C        OVERALL (SUMMARY) MODEL APPLICABILITY RANKING CAN BE COMPUTED.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C                          (BUT RETURNED TO ORIGINAL VALUES)
C        YIELDED
C             TM3C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS SCRATCH ARRAY.
C             TM4C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS SCRATCH ARRAY.
C   ARGUMENT LIST
C        GIVEN
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C        YIELDED
C             FLAG   (I) = MODEL SELECTION FLAGS
C             RFLAG  (I) = MODEL RESULTS ERROR FLAGS
C             STAT   (R) = NOISE STATISTICS
C   LOCAL GLOSSARY
C             ALLFLG (I) = OPTION FLAG TO INDICATE ALL MODELS WANTED
C             CNT    (I) = ANALYSIS ITERATION COUNT (AT RETURN)
C             CNTO   (I) = SAMPLE OBSERVATION COUNT
C             DM     (R) = DUMMY ARGUMENT FOR SMFTRN ACCESS
C             IDXE   (I) = INDEX FOR TBF DATA ENDING ADDRESS
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS (SET TO 100)
C             PHIIND (I) = LAV PHI INDICATOR FLAG (LINEAR OR QUADRATIC)
C             RFLG   (I) = DUMMY ARGUMENT FOR SMFTRN ACCESS
C             TYP    (I) = FLAG TO INDICATE NOISE ANALYSIS (3)
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
         INTEGER              END1
         PARAMETER           (END1   =   1000)
         INTEGER              END2
         PARAMETER           (END2   =   2000)
C
C                             GLOBAL SPECIFICATIONS
C
C               /DATCOM/      DATA AND PROCESSING COMMON
         COMMON /DATCOM/      DAT(END2)     ,DATTYP        ,PRC(3,3)
     1                       ,UPDFLG
         INTEGER              DATTYP        ,PRC           ,UPDFLG
C
C               /MAACOM/      MODEL APPLICABILITY ANALYSIS COMMON
         COMMON /MAACOM/      TM3C(END1)    ,TM4C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              FLAG(7)       ,IDX           ,NS
     1                       ,NSB           ,NSE           ,NSR
     2                       ,RFLAG(7)
         DIMENSION            STAT(7)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              ALLFLG        ,CNT           ,CNTO
     1                       ,I             ,IDXE          ,MAXIC
     2                       ,PHIIND        ,RFLG          ,TYP
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'    ENTER ONE TO SELECT THE MODELS, OR ZERO TO RUN AL'
     2          ,'L SEVEN.')
  305    FORMAT ('    ENTER ONE FOR GEOMETRIC MODEL; ELSE ZERO.')
  310    FORMAT ('    ENTER ONE FOR JELINSKI/MORANDA MODEL; ELSE ZERO.')
  315    FORMAT ('    ENTER ONE FOR LITTLEWOOD AND VERRALL-LIN  MODEL; '
     1          ,'ELSE ZERO.')
  320    FORMAT ('    ENTER ONE FOR LITTLEWOOD AND VERRALL-QUAD MODEL; '
     1          ,'ELSE ZERO.')
  325    FORMAT ('    ENTER ONE FOR MUSA BASIC EXECUTION TIME MODEL; EL'
     1          ,'SE ZERO.')
  330    FORMAT ('    ENTER ONE FOR MUSA LOG POISSON EXE. TIME MODEL;  '
     1          ,'ELSE ZERO.')
  335    FORMAT ('    ENTER ONE FOR NHPP MODEL FOR TBF DATA; ELSE ZERO.'
     1          )
  340    FORMAT (' '
     1        ,/,'    THE FOLLOWING WERE DETERMINED BASED ON ',I3,' POI'
     2          ,'NTS')
  345    FORMAT ('    GEOMETRIC                   STATISTIC IS ',E15.8)
  350    FORMAT ('  **GEOMETRIC                   FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  355    FORMAT ('    JELINSKI/MORANDA            STATISTIC IS ',E15.8)
  360    FORMAT ('  **JELINSKI/MORANDA            FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  365    FORMAT ('    LITTLEWOOD AND VERRALL-LIN  STATISTIC IS ',E15.8)
  370    FORMAT ('  **LITTLEWOOD AND VERRALL-LIN  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  375    FORMAT ('    LITTLEWOOD AND VERRALL-QUAD STATISTIC IS ',E15.8)
  380    FORMAT ('  **LITTLEWOOD AND VERRALL-QUAD FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  385    FORMAT ('    MUSA BASIC EXECUTION        STATISTIC IS ',E15.8)
  390    FORMAT ('  **MUSA BASIC EXECUTION        FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  395    FORMAT ('    MUSA LOG POISSON EXECUTION  STATISTIC IS ',E15.8)
  400    FORMAT ('  **MUSA LOG POISSON EXECUTION  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  405    FORMAT ('    NHPP FOR TBF DATA           STATISTIC IS ',E15.8)
  410    FORMAT ('  **NHPP FOR TBF DATA           FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE DATA VALUES TREATED AS CONSTANTS IN THIS ROUTINE.
         IDXE  = IDX + NS - 1
         MAXIC = 100
         TYP   = 3
C
C        DETERMINE IF ALL EXECUTION TIME MODELS ARE TO BE EXECUTED.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) ALLFLG
         WRITE (HIST,  *) ALLFLG
C
         IF (ALLFLG.NE.1) THEN
C            ALL MODELS ARE DESIRED IN THIS EXECUTION.
             DO 1000 I = 1, 7
                 FLAG(I) = 1
 1000            CONTINUE
C            ENDDO
         ELSE
C            ONLY CERTAIN MODELS ARE DESIRED IN THIS EXECUTION.
             WRITE (   *,305)
             WRITE (HIST,305)
             READ  (   *,  *) FLAG(1)
             WRITE (HIST,  *) FLAG(1)
C
             WRITE (   *,310)
             WRITE (HIST,310)
             READ  (   *,  *) FLAG(2)
             WRITE (HIST,  *) FLAG(2)
C
             WRITE (   *,315)
             WRITE (HIST,315)
             READ  (   *,  *) FLAG(3)
             WRITE (HIST,  *) FLAG(3)
C
             WRITE (   *,320)
             WRITE (HIST,320)
             READ  (   *,  *) FLAG(4)
             WRITE (HIST,  *) FLAG(4)
C
             WRITE (   *,325)
             WRITE (HIST,325)
             READ  (   *,  *) FLAG(5)
             WRITE (HIST,  *) FLAG(5)
C
             WRITE (   *,330)
             WRITE (HIST,330)
             READ  (   *,  *) FLAG(6)
             WRITE (HIST,  *) FLAG(6)
C
             WRITE (   *,335)
             WRITE (HIST,335)
             READ  (   *,  *) FLAG(7)
             WRITE (HIST,  *) FLAG(7)
         ENDIF
C
         WRITE (   *,340) NSR
         WRITE (HIST,340) NSR
C
         IF (FLAG(1).EQ.1) THEN
             CALL SGEOMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(1) , RFLAG(1), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(1).EQ.0) THEN
                 WRITE (   *,345) STAT(1)
                 WRITE (HIST,345) STAT(1)
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,350) RFLAG(1), CNT, CNTO
                 WRITE (HIST,350) RFLAG(1), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(2).EQ.1) THEN
             CALL SJAMMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(2) , RFLAG(2), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(2).EQ.0) THEN
                 WRITE (   *,355) STAT(2)
                 WRITE (HIST,355) STAT(2)
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,360) RFLAG(2), CNT, CNTO
                 WRITE (HIST,360) RFLAG(2), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(3).EQ.1) THEN
C            SET THE FLAG FOR LINEAR MODEL.
             PHIIND = 1
C
             CALL SLAVMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP     , MAXIC   , PHIIND
     Y                   ,STAT(3) , RFLAG(3), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(3).EQ.0) THEN
                 WRITE (   *,365) STAT(3)
                 WRITE (HIST,365) STAT(3)
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,370) RFLAG(3), CNT, CNTO
                 WRITE (HIST,370) RFLAG(3), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(4).EQ.1) THEN
C            SET THE FLAG FOR QUADRATIC MODEL.
             PHIIND = 2
C
             CALL SLAVMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP     , MAXIC   , PHIIND
     Y                   ,STAT(4) , RFLAG(4), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(4).EQ.0) THEN
                 WRITE (   *,375) STAT(4)
                 WRITE (HIST,375) STAT(4)
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,380) RFLAG(4), CNT, CNTO
                 WRITE (HIST,380) RFLAG(4), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(5).EQ.1) THEN
             CALL SMUSMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(5) , RFLAG(5), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(5).EQ.0) THEN
                 WRITE (   *,385) STAT(5)
                 WRITE (HIST,385) STAT(5)
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,390) RFLAG(5), CNT, CNTO
                 WRITE (HIST,390) RFLAG(5), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(6).EQ.1) THEN
             CALL SMSAMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(6) , RFLAG(6), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(6).EQ.0) THEN
                 WRITE (   *,395) STAT(6)
                 WRITE (HIST,395) STAT(6)
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,400) RFLAG(6), CNT, CNTO
                 WRITE (HIST,400) RFLAG(6), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(7).EQ.1) THEN
C            STORE THE ORIGINAL TBF DATA AND CONVERT DATA TO TTF UNITS.
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = IDX, IDXE)
             CALL SMFTRN
     G                   (NS      , DM      , DM      , 6
     B                   ,DAT(IDX)
     Y                   ,RFLG    )
C
             CALL SNPTMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(7) , RFLAG(7), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(7).EQ.0) THEN
                 WRITE (   *,405) STAT(7)
                 WRITE (HIST,405) STAT(7)
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,410) RFLAG(7), CNT, CNTO
                 WRITE (HIST,410) RFLAG(7), CNT, CNTO
             ENDIF
C
C            RESTORE THE TBF DATA.
             REWIND TMPB
             READ  (TMPB) (DAT(I), I = IDX, IDXE)
         ENDIF
         RETURN
         END
         SUBROUTINE MAATQK
     G                     (IDX   , NS    , NSB   , NSE   , NSR
     Y                     ,RFLAG , FLAG  , STAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MAATQK
C    G                     (IDX   , NS    , NSB   , NSE   , NSR
C    Y                     ,RFLAG , FLAG  , STAT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE THE MENUS AND TRANSFERS FOR THE EXECUTIONS OF ALL OF
C        THE  MODEL APPLICABILITY  ANALYSES  (ACCURACY, BIAS, NOISE, AND
C        TREND) IN A SINGLE EXECUTION.
C   DESCRIPTION
C        FUNCTIONALLY, THIS ROUTINE IS A COMPOSITE OF THE FOUR ROUTINES;
C        HOWEVER, TWO MAJOR CHANGES WERE MADE. FIRST, THE ONLY OUTPUT IS
C        ERROR MESSAGES.  SECOND, THE ERROR FLAGS FROM THE ACCURACY PART
C        ARE USED TO CANCEL THE MODELS  FROM EXECUTIONS DURING THE OTHER
C        THREE ANALYSES.
C
C        NOTE THAT SINCE THIS ROUTINE PERFORMS ALL FOUR ANALYSES, ALL OF
C        THE SELECTION, ERROR, AND STATISTICS ARRAYS ARE ALSO PASSED IN.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C                          (BUT RETURNED TO ORIGINAL VALUES)
C        YIELDED
C             TM3C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS SCATTER PLOT.
C             TM4C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS SCRATCH ARRAY.
C   ARGUMENT LIST
C        GIVEN
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C        YIELDED
C             FLAG   (I) = MODEL SELECTION FLAGS
C             RFLAG  (I) = MODEL RESULTS ERROR FLAGS
C             STAT   (R) = ANALYSIS STATISTICS
C   LOCAL GLOSSARY
C             ALLFLG (I) = OPTION FLAG TO INDICATE ALL MODELS WANTED
C             CNT    (I) = ANALYSIS ITERATION COUNT (AT RETURN)
C             CNTO   (I) = SAMPLE OBSERVATION COUNT
C             DM     (R) = DUMMY ARGUMENT FOR SMFTRN ACCESS
C             IDXE   (I) = INDEX FOR TBF DATA ENDING ADDRESS
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS (SET TO 100)
C             PHIIND (I) = LAV PHI INDICATOR FLAG (LINEAR OR QUADRATIC)
C             RFLG   (I) = DUMMY ARGUMENT FOR SMFTRN ACCESS
C             TYP    (I) = FLAG TO INDICATE ANALYSIS TYPE
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
         INTEGER              END1
         PARAMETER           (END1   =   1000)
         INTEGER              END2
         PARAMETER           (END2   =   2000)
C
C                             GLOBAL SPECIFICATIONS
C
C               /DATCOM/      DATA AND PROCESSING COMMON
         COMMON /DATCOM/      DAT(END2)     ,DATTYP        ,PRC(3,3)
     1                       ,UPDFLG
         INTEGER              DATTYP        ,PRC           ,UPDFLG
C
C               /MAACOM/      MODEL APPLICABILITY ANALYSIS COMMON
         COMMON /MAACOM/      TM3C(END1)    ,TM4C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              FLAG(7,4)     ,IDX           ,NS
     1                       ,NSB           ,NSE           ,NSR
     2                       ,RFLAG(7,4)
         DIMENSION            STAT(7,4)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              ALLFLG        ,CNT           ,CNTO
     1                       ,I             ,IDXE          ,J
     2                       ,MAXIC         ,PHIIND        ,RFLG
     3                       ,TYP
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'    ENTER ONE TO SELECT THE MODELS, OR ZERO TO RUN AL'
     2          ,'L SEVEN.')
  305    FORMAT ('    ENTER ONE FOR GEOMETRIC MODEL; ELSE ZERO.')
  310    FORMAT ('    ENTER ONE FOR JELINSKI/MORANDA MODEL; ELSE ZERO.')
  315    FORMAT ('    ENTER ONE FOR LITTLEWOOD AND VERRALL-LIN  MODEL; '
     1          ,'ELSE ZERO.')
  320    FORMAT ('    ENTER ONE FOR LITTLEWOOD AND VERRALL-QUAD MODEL; '
     1          ,'ELSE ZERO.')
  325    FORMAT ('    ENTER ONE FOR MUSA BASIC EXECUTION TIME MODEL; EL'
     1          ,'SE ZERO.')
  330    FORMAT ('    ENTER ONE FOR MUSA LOG POISSON EXE. TIME MODEL;  '
     1          ,'ELSE ZERO.')
  335    FORMAT ('    ENTER ONE FOR NHPP MODEL FOR TBF DATA; ELSE ZERO.'
     1          )
  340    FORMAT (' '
     1        ,/,'    CURRENTLY WORKING ON THE MODEL ACCURACY ANALYSIS.'
     2          )
  345    FORMAT ('  **GEOMETRIC                   FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  350    FORMAT ('  **JELINSKI/MORANDA            FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  355    FORMAT ('  **LITTLEWOOD AND VERRALL-LIN  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  360    FORMAT ('  **LITTLEWOOD AND VERRALL-QUAD FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  365    FORMAT ('  **MUSA BASIC EXECUTION        FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  370    FORMAT ('  **MUSA LOG POISSON EXECUTION  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  375    FORMAT ('  **NHPP FOR TBF DATA           FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  380    FORMAT (' '
     1        ,/,'    CURRENTLY WORKING ON THE MODEL BIAS ANALYSIS.')
  385    FORMAT (' '
     1        ,/,'    CURRENTLY WORKING ON THE MODEL NOISE ANALYSIS.')
  390    FORMAT (' '
     1        ,/,'    CURRENTLY WORKING ON THE MODEL TREND ANALYSIS.')
  395    FORMAT ('  **GEOMETRIC                   SKIPPED BECAUSE OF P'
     1          ,'RIOR FAILURE')
  400    FORMAT ('  **JELINSKI/MORANDA            SKIPPED BECAUSE OF P'
     1          ,'RIOR FAILURE')
  405    FORMAT ('  **LITTLEWOOD AND VERRALL-LIN  SKIPPED BECAUSE OF P'
     1          ,'RIOR FAILURE')
  410    FORMAT ('  **LITTLEWOOD AND VERRALL-QUAD SKIPPED BECAUSE OF P'
     1          ,'RIOR FAILURE')
  415    FORMAT ('  **MUSA BASIC EXECUTION        SKIPPED BECAUSE OF P'
     1          ,'RIOR FAILURE')
  420    FORMAT ('  **MUSA LOG POISSON EXECUTION  SKIPPED BECAUSE OF P'
     1          ,'RIOR FAILURE')
  425    FORMAT ('  **NHPP FOR TBF DATA           SKIPPED BECAUSE OF P'
     1          ,'RIOR FAILURE')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE DATA VALUES TREATED AS CONSTANTS IN THIS ROUTINE.
         IDXE  = IDX + NS - 1
         MAXIC = 100
C
C        DETERMINE IF ALL EXECUTION TIME MODELS ARE TO BE EXECUTED.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) ALLFLG
         WRITE (HIST,  *) ALLFLG
C
         IF (ALLFLG.NE.1) THEN
C            ALL MODELS ARE DESIRED IN THIS EXECUTION.
             DO 1000 I = 1, 7
                 FLAG(I,1) = 1
 1000            CONTINUE
C            ENDDO
         ELSE
C            ONLY CERTAIN MODELS ARE DESIRED IN THIS EXECUTION.
             WRITE (   *,305)
             WRITE (HIST,305)
             READ  (   *,  *) FLAG(1,1)
             WRITE (HIST,  *) FLAG(1,1)
C
             WRITE (   *,310)
             WRITE (HIST,310)
             READ  (   *,  *) FLAG(2,1)
             WRITE (HIST,  *) FLAG(2,1)
C
             WRITE (   *,315)
             WRITE (HIST,315)
             READ  (   *,  *) FLAG(3,1)
             WRITE (HIST,  *) FLAG(3,1)
C
             WRITE (   *,320)
             WRITE (HIST,320)
             READ  (   *,  *) FLAG(4,1)
             WRITE (HIST,  *) FLAG(4,1)
C
             WRITE (   *,325)
             WRITE (HIST,325)
             READ  (   *,  *) FLAG(5,1)
             WRITE (HIST,  *) FLAG(5,1)
C
             WRITE (   *,330)
             WRITE (HIST,330)
             READ  (   *,  *) FLAG(6,1)
             WRITE (HIST,  *) FLAG(6,1)
C
             WRITE (   *,335)
             WRITE (HIST,335)
             READ  (   *,  *) FLAG(7,1)
             WRITE (HIST,  *) FLAG(7,1)
         ENDIF
C
C        TRANSFER THE MODEL SELECTION FLAGS AND ZERO ALL ERROR FLAGS.
         DO 1075 I = 1, 7
             DO 1025 J = 2, 4
                 FLAG(I,J)  = FLAG(I,1)
 1025            CONTINUE
C            ENDDO
             DO 1050 J = 1, 4
                 RFLAG(I,J) = 0
 1050            CONTINUE
C            ENDDO
 1075        CONTINUE
C        ENDDO
C
C        SCRIPT ACCURACY HEADER AND SET ANALYSIS TYPE FLAG.
         WRITE (   *,340)
         WRITE (HIST,340)
         TYP   = 1
C
         IF (FLAG(1,1).EQ.1) THEN
             CALL SGEOMA
     G                   (DAT(IDX) , NS        , NSB   , NSE   , NSR
     G                   ,TYP
     Y                   ,STAT(1,1), RFLAG(1,1), CNT   , TM3C  , TM4C  )
C
             IF (RFLAG(1,1).NE.0) THEN
                 CNTO = NSB + CNT - 1
                 WRITE (   *,345) RFLAG(1,1), CNT, CNTO
                 WRITE (HIST,345) RFLAG(1,1), CNT, CNTO
                 RFLAG(1,2) = -1
                 RFLAG(1,3) = -1
                 RFLAG(1,4) = -1
             ENDIF
         ENDIF
C
         IF (FLAG(2,1).EQ.1) THEN
             CALL SJAMMA
     G                   (DAT(IDX) , NS        , NSB   , NSE   , NSR
     G                   ,TYP
     Y                   ,STAT(2,1), RFLAG(2,1), CNT   , TM3C  , TM4C  )
C
             IF (RFLAG(2,1).NE.0) THEN
                 CNTO = NSB + CNT - 1
                 WRITE (   *,350) RFLAG(2,1), CNT, CNTO
                 WRITE (HIST,350) RFLAG(2,1), CNT, CNTO
                 RFLAG(2,2) = -1
                 RFLAG(2,3) = -1
                 RFLAG(2,4) = -1
             ENDIF
         ENDIF
C
         IF (FLAG(3,1).EQ.1) THEN
C            SET THE FLAG FOR LINEAR MODEL.
             PHIIND = 1
C
             CALL SLAVMA
     G                   (DAT(IDX) , NS        , NSB   , NSE   , NSR
     G                   ,TYP      , MAXIC     , PHIIND
     Y                   ,STAT(3,1), RFLAG(3,1), CNT   , TM3C  , TM4C  )
C
             IF (RFLAG(3,1).NE.0) THEN
                 CNTO = NSB + CNT - 1
                 WRITE (   *,355) RFLAG(3,1), CNT, CNTO
                 WRITE (HIST,355) RFLAG(3,1), CNT, CNTO
                 RFLAG(3,2) = -1
                 RFLAG(3,3) = -1
                 RFLAG(3,4) = -1
             ENDIF
         ENDIF
C
         IF (FLAG(4,1).EQ.1) THEN
C            SET THE FLAG FOR QUADRATIC MODEL.
             PHIIND = 2
C
             CALL SLAVMA
     G                   (DAT(IDX) , NS        , NSB   , NSE   , NSR
     G                   ,TYP      , MAXIC     , PHIIND
     Y                   ,STAT(4,1), RFLAG(4,1), CNT   , TM3C  , TM4C  )
C
             IF (RFLAG(4,1).NE.0) THEN
                 CNTO = NSB + CNT - 1
                 WRITE (   *,360) RFLAG(4,1), CNT, CNTO
                 WRITE (HIST,360) RFLAG(4,1), CNT, CNTO
                 RFLAG(4,2) = -1
                 RFLAG(4,3) = -1
                 RFLAG(4,4) = -1
             ENDIF
         ENDIF
C
         IF (FLAG(5,1).EQ.1) THEN
             CALL SMUSMA
     G                   (DAT(IDX) , NS        , NSB   , NSE   , NSR
     G                   ,TYP
     Y                   ,STAT(5,1), RFLAG(5,1), CNT   , TM3C  , TM4C  )
C
             IF (RFLAG(5,1).NE.0) THEN
                 CNTO = NSB + CNT - 1
                 WRITE (   *,365) RFLAG(5,1), CNT, CNTO
                 WRITE (HIST,365) RFLAG(5,1), CNT, CNTO
                 RFLAG(5,2) = -1
                 RFLAG(5,3) = -1
                 RFLAG(5,4) = -1
             ENDIF
         ENDIF
C
         IF (FLAG(6,1).EQ.1) THEN
             CALL SMSAMA
     G                   (DAT(IDX) , NS        , NSB   , NSE   , NSR
     G                   ,TYP
     Y                   ,STAT(6,1), RFLAG(6,1), CNT   , TM3C  , TM4C  )
C
             IF (RFLAG(6,1).NE.0) THEN
                 CNTO = NSB + CNT - 1
                 WRITE (   *,370) RFLAG(6,1), CNT, CNTO
                 WRITE (HIST,370) RFLAG(6,1), CNT, CNTO
                 RFLAG(6,2) = -1
                 RFLAG(6,3) = -1
                 RFLAG(6,4) = -1
             ENDIF
         ENDIF
C
         IF (FLAG(7,1).EQ.1) THEN
C            STORE THE ORIGINAL TBF DATA AND CONVERT DATA TO TTF UNITS.
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = IDX, IDXE)
             CALL SMFTRN
     G                   (NS      , DM      , DM      , 6
     B                   ,DAT(IDX)
     Y                   ,RFLG    )
C
             CALL SNPTMA
     G                   (DAT(IDX) , NS        , NSB   , NSE   , NSR
     G                   ,TYP
     Y                   ,STAT(7,1), RFLAG(7,1), CNT   , TM3C  , TM4C  )
C
             IF (RFLAG(7,1).NE.0) THEN
                 CNTO = NSB + CNT - 1
                 WRITE (   *,375) RFLAG(7,1), CNT, CNTO
                 WRITE (HIST,375) RFLAG(7,1), CNT, CNTO
                 RFLAG(7,2) = -1
                 RFLAG(7,3) = -1
                 RFLAG(7,4) = -1
             ENDIF
C
C            RESTORE THE TBF DATA.
             REWIND TMPB
             READ  (TMPB) (DAT(I), I = IDX, IDXE)
         ENDIF
C
         DO 1100 I = 2, 4
C            PERFORM THE REMAINING THREE ANALYSES,  START WITH OUTPUT OF
C            AN APPROPRIATE HEADER FOR EACH.
             IF     (I.EQ.2) THEN
                 WRITE (   *,380)
                 WRITE (HIST,380)
             ELSEIF (I.EQ.3) THEN
                 WRITE (   *,385)
                 WRITE (HIST,385)
             ELSE
                 WRITE (   *,390)
                 WRITE (HIST,390)
             ENDIF
             TYP   = I
C
C            NOTE, THAT THE ERROR PROCESSING FOR THE CURRENT ANALYSIS IS
C            RESIDENT ONLY FOR THE SPECIAL BIAS AND TREND ERROR NUMBER 5
C            AND SHOULD NOT RESULT IN ANY OTHER ERROR NUMBERS.
C
             IF (FLAG(1,I).EQ.1) THEN
                 IF (RFLAG(1,I).EQ.-1) THEN
                     WRITE (   *,395)
                     WRITE (HIST,395)
                 ELSE
                     CALL SGEOMA
     G                           (DAT(IDX) , NS        , NSB   , NSE
     G                           ,NSR      , TYP
     Y                           ,STAT(1,I), RFLAG(1,I), CNT   , TM3C
     Y                           ,TM4C     )
C
                     IF (RFLAG(1,I).NE.0) THEN
                         CNTO = NSB + CNT - 1
                         WRITE (   *,345) RFLAG(1,I), CNT, CNTO
                         WRITE (HIST,345) RFLAG(1,I), CNT, CNTO
                     ENDIF
                 ENDIF
             ENDIF
C
             IF (FLAG(2,I).EQ.1) THEN
                 IF (RFLAG(2,I).EQ.-1) THEN
                     WRITE (   *,400)
                     WRITE (HIST,400)
                 ELSE
                     CALL SJAMMA
     G                           (DAT(IDX) , NS        , NSB   , NSE
     G                           ,NSR      , TYP
     Y                           ,STAT(2,I), RFLAG(2,I), CNT   , TM3C
     Y                           ,TM4C     )
C
                     IF (RFLAG(2,I).NE.0) THEN
                         CNTO = NSB + CNT - 1
                         WRITE (   *,350) RFLAG(2,I), CNT, CNTO
                         WRITE (HIST,350) RFLAG(2,I), CNT, CNTO
                     ENDIF
                 ENDIF
             ENDIF
C
             IF (FLAG(3,I).EQ.1) THEN
                 IF (RFLAG(3,I).EQ.-1) THEN
                     WRITE (   *,405)
                     WRITE (HIST,405)
                 ELSE
C                    SET THE FLAG FOR LINEAR MODEL.
                     PHIIND = 1
C
                     CALL SLAVMA
     G                           (DAT(IDX) , NS        , NSB   , NSE
     G                           ,NSR      , TYP       , MAXIC , PHIIND
     Y                           ,STAT(3,I), RFLAG(3,I), CNT   , TM3C
     Y                           ,TM4C     )
C
                     IF (RFLAG(3,I).NE.0) THEN
                         CNTO = NSB + CNT - 1
                         WRITE (   *,355) RFLAG(3,I), CNT, CNTO
                         WRITE (HIST,355) RFLAG(3,I), CNT, CNTO
                     ENDIF
                 ENDIF
             ENDIF
C
             IF (FLAG(4,I).EQ.1) THEN
                 IF (RFLAG(4,I).EQ.-1) THEN
                     WRITE (   *,410)
                     WRITE (HIST,410)
                 ELSE
C                    SET THE FLAG FOR QUADRATIC MODEL.
                     PHIIND = 2
C
                     CALL SLAVMA
     G                           (DAT(IDX) , NS        , NSB   , NSE
     G                           ,NSR      , TYP       , MAXIC , PHIIND
     Y                           ,STAT(4,I), RFLAG(4,I), CNT   , TM3C
     Y                           ,TM4C     )
C
                     IF (RFLAG(4,I).NE.0) THEN
                         CNTO = NSB + CNT - 1
                         WRITE (   *,360) RFLAG(4,I), CNT, CNTO
                         WRITE (HIST,360) RFLAG(4,I), CNT, CNTO
                     ENDIF
                 ENDIF
             ENDIF
C
             IF (FLAG(5,I).EQ.1) THEN
                 IF (RFLAG(5,I).EQ.-1) THEN
                     WRITE (   *,415)
                     WRITE (HIST,415)
                 ELSE
                     CALL SMUSMA
     G                           (DAT(IDX) , NS        , NSB   , NSE
     G                           ,NSR      , TYP
     Y                           ,STAT(5,I), RFLAG(5,I), CNT   , TM3C
     Y                           ,TM4C     )
C
                     IF (RFLAG(5,I).NE.0) THEN
                         CNTO = NSB + CNT - 1
                         WRITE (   *,365) RFLAG(5,I), CNT, CNTO
                         WRITE (HIST,365) RFLAG(5,I), CNT, CNTO
                     ENDIF
                 ENDIF
             ENDIF
C
             IF (FLAG(6,I).EQ.1) THEN
                 IF (RFLAG(6,I).EQ.-1) THEN
                     WRITE (   *,420)
                     WRITE (HIST,420)
                 ELSE
                     CALL SMSAMA
     G                           (DAT(IDX) , NS        , NSB   , NSE
     G                           ,NSR      , TYP
     Y                           ,STAT(6,I), RFLAG(6,I), CNT   , TM3C
     Y                           ,TM4C     )
C
                     IF (RFLAG(6,I).NE.0) THEN
                         CNTO = NSB + CNT - 1
                         WRITE (   *,370) RFLAG(6,I), CNT, CNTO
                         WRITE (HIST,370) RFLAG(6,I), CNT, CNTO
                     ENDIF
                 ENDIF
             ENDIF
C
             IF (FLAG(7,I).EQ.1) THEN
                 IF (RFLAG(7,I).EQ.-1) THEN
                     WRITE (   *,425)
                     WRITE (HIST,425)
                 ELSE
C                    STORE THE ORIGINAL TBF DATA AND CONVERT DATA TO TTF
C                    UNITS.
                     REWIND TMPB
                     WRITE (TMPB) (DAT(J), J = IDX, IDXE)
                     CALL SMFTRN
     G                           (NS      , DM      , DM      , 6
     B                           ,DAT(IDX)
     Y                           ,RFLG    )
C
                     CALL SNPTMA
     G                           (DAT(IDX) , NS        , NSB   , NSE
     G                           ,NSR      , TYP
     Y                           ,STAT(7,I), RFLAG(7,I), CNT   , TM3C
     Y                           ,TM4C     )
C
                     IF (RFLAG(7,I).NE.0) THEN
                         CNTO = NSB + CNT - 1
                         WRITE (   *,375) RFLAG(7,I), CNT, CNTO
                         WRITE (HIST,375) RFLAG(7,I), CNT, CNTO
                     ENDIF
C
C                    RESTORE THE TBF DATA.
                     REWIND TMPB
                     READ  (TMPB) (DAT(J), J = IDX, IDXE)
                 ENDIF
             ENDIF
 1100        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE MAATTD
     G                     (IDX   , NS    , NSB   , NSE   , NSR
     Y                     ,RFLAG , FLAG  , STAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MAATTD
C    G                     (IDX   , NS    , NSB   , NSE   , NSR
C    Y                     ,RFLAG , FLAG  , STAT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE THE MENUS AND TRANSFERS FOR THE  EXECUTION OF  TREND
C        ANALYSIS OF THE TBF DATA PRIOR TO THE EXECUTION OF THE SOFTWARE
C        RELIABILITY MODELS (VIA MODULE 8).
C   DESCRIPTION
C        THE ONLY REAL PROCESSING INVOLVED  IN THIS ROUTINE INVOLVES THE
C        SETTING OF SOME  PRELIMINARY POINTERS FOR THE DATA  AND THE SE-
C        LECTION OF  WHICH OF THE MODELS  ARE TO BE EXECUTED.  A SPECIAL
C        OPTION CAN BE SET TO INDICATE THAT ALL MODELS ARE TO BE EXECUT-
C        ED DURING THE ANALYSIS.
C
C        THE REQUIRED COMPUTATIONS WILL BE PERFORMED  BY ROUTINES OF THE
C        SMFLIB LIBRARY.  UPON RETURN  THIS ROUTINE WILL OUTPUT THE COM-
C        PUTED STATISTIC  AND  ACCESS THE  PLTUAY ROUTINE  TO  PRODUCE A
C        Y-PLOT.
C
C        THE MODEL SELECTION FLAG, THE MODEL ERROR FLAG, AND THE RESULT-
C        ING STATISTICS ARE RETURNED TO THE CALLING ROUTINE  SO THAT THE
C        OVERALL (SUMMARY) MODEL APPLICABILITY RANKING CAN BE COMPUTED.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C                          (BUT RETURNED TO ORIGINAL VALUES)
C        YIELDED
C             TM3C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS Y-PLOT.
C             TM4C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          MODEL APPLICABILITY ANALYSIS SCRATCH ARRAY.
C   ARGUMENT LIST
C        GIVEN
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             NSB    (I) = ANALYSIS ITERATION INDEX BEGINNING
C             NSE    (I) = ANALYSIS ITERATION INDEX ENDING
C             NSR    (I) = ANALYSIS ITERATION RANGE
C        YIELDED
C             FLAG   (I) = MODEL SELECTION FLAGS
C             RFLAG  (I) = MODEL RESULTS ERROR FLAGS
C             STAT   (R) = KOLMOGOROV STATISTICS
C   LOCAL GLOSSARY
C             ALLFLG (I) = OPTION FLAG TO INDICATE ALL MODELS WANTED
C             CNT    (I) = ANALYSIS ITERATION COUNT (AT RETURN)
C             CNTO   (I) = SAMPLE OBSERVATION COUNT
C             DM     (R) = DUMMY ARGUMENT FOR SMFTRN ACCESS
C             IDXE   (I) = INDEX FOR TBF DATA ENDING ADDRESS
C             MAXIC  (I) = MAXIMUM NUMBER OF ITERATIONS (SET TO 100)
C             PHIIND (I) = LAV PHI INDICATOR FLAG (LINEAR OR QUADRATIC)
C             RFLG   (I) = DUMMY ARGUMENT FOR SMFTRN ACCESS
C             TYP    (I) = FLAG TO INDICATE TREND ANALYSIS (4)
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
         INTEGER              END1
         PARAMETER           (END1   =   1000)
         INTEGER              END2
         PARAMETER           (END2   =   2000)
C
C                             GLOBAL SPECIFICATIONS
C
C               /DATCOM/      DATA AND PROCESSING COMMON
         COMMON /DATCOM/      DAT(END2)     ,DATTYP        ,PRC(3,3)
     1                       ,UPDFLG
         INTEGER              DATTYP        ,PRC           ,UPDFLG
C
C               /MAACOM/      MODEL APPLICABILITY ANALYSIS COMMON
         COMMON /MAACOM/      TM3C(END1)    ,TM4C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              FLAG(7)       ,IDX           ,NS
     1                       ,NSB           ,NSE           ,NSR
     2                       ,RFLAG(7)
         DIMENSION            STAT(7)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              ALLFLG        ,CNT           ,CNTO
     1                       ,I             ,IDXE          ,MAXIC
     2                       ,PHIIND        ,RFLG          ,TYP
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'    ENTER ONE TO SELECT THE MODELS, OR ZERO TO RUN AL'
     2          ,'L SEVEN.')
  305    FORMAT ('    ENTER ONE FOR GEOMETRIC MODEL; ELSE ZERO.')
  310    FORMAT ('    ENTER ONE FOR JELINSKI/MORANDA MODEL; ELSE ZERO.')
  315    FORMAT ('    ENTER ONE FOR LITTLEWOOD AND VERRALL-LIN  MODEL; '
     1          ,'ELSE ZERO.')
  320    FORMAT ('    ENTER ONE FOR LITTLEWOOD AND VERRALL-QUAD MODEL; '
     1          ,'ELSE ZERO.')
  325    FORMAT ('    ENTER ONE FOR MUSA BASIC EXECUTION TIME MODEL; EL'
     1          ,'SE ZERO.')
  330    FORMAT ('    ENTER ONE FOR MUSA LOG POISSON EXE. TIME MODEL;  '
     1          ,'ELSE ZERO.')
  335    FORMAT ('    ENTER ONE FOR NHPP MODEL FOR TBF DATA; ELSE ZERO.'
     1          )
  345    FORMAT ('                     Y-PLOT FOR GEOMETRIC'
     1        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     2        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  350    FORMAT ('  **GEOMETRIC                   FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  355    FORMAT ('                     Y-PLOT FOR JELINSKI/MORANDA'
     1        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     2        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  360    FORMAT ('  **JELINSKI/MORANDA            FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  365    FORMAT ('                     Y-PLOT FOR LITTLEWOOD AND VERRAL'
     1          ,'L-LIN'
     2        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     3        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  370    FORMAT ('  **LITTLEWOOD AND VERRALL-LIN  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  375    FORMAT ('                     Y-PLOT FOR LITTLEWOOD AND VERRAL'
     1          ,'L-QUAD'
     2        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     3        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  380    FORMAT ('  **LITTLEWOOD AND VERRALL-QUAD FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  385    FORMAT ('                     Y-PLOT FOR MUSA BASIC EXECUTION'
     1        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     2        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  390    FORMAT ('  **MUSA BASIC EXECUTION        FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  395    FORMAT ('                     Y-PLOT FOR MUSA LOG POISSON EXEC'
     1          ,'UTION'
     2        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     3        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  400    FORMAT ('  **MUSA LOG POISSON EXECUTION  FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
  405    FORMAT ('                     Y-PLOT FOR NHPP FOR TBF DATA'
     1        ,/,'                     KOLMOGOROV DISTANCE IS ',E15.8
     2        ,/,'                     BASED ON',I4,' OF',I4,' POINTS')
  410    FORMAT ('  **NHPP FOR TBF DATA           FAILED RFLAG ',I2
     1          ,' ON ITER.',I4,' (OBS.',I4,')')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE DATA VALUES TREATED AS CONSTANTS IN THIS ROUTINE.
         IDXE  = IDX + NS - 1
         MAXIC = 100
         TYP   = 4
C
C        DETERMINE IF ALL EXECUTION TIME MODELS ARE TO BE EXECUTED.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) ALLFLG
         WRITE (HIST,  *) ALLFLG
C
         IF (ALLFLG.NE.1) THEN
C            ALL MODELS ARE DESIRED IN THIS EXECUTION.
             DO 1000 I = 1, 7
                 FLAG(I) = 1
 1000            CONTINUE
C            ENDDO
         ELSE
C            ONLY CERTAIN MODELS ARE DESIRED IN THIS EXECUTION.
             WRITE (   *,305)
             WRITE (HIST,305)
             READ  (   *,  *) FLAG(1)
             WRITE (HIST,  *) FLAG(1)
C
             WRITE (   *,310)
             WRITE (HIST,310)
             READ  (   *,  *) FLAG(2)
             WRITE (HIST,  *) FLAG(2)
C
             WRITE (   *,315)
             WRITE (HIST,315)
             READ  (   *,  *) FLAG(3)
             WRITE (HIST,  *) FLAG(3)
C
             WRITE (   *,320)
             WRITE (HIST,320)
             READ  (   *,  *) FLAG(4)
             WRITE (HIST,  *) FLAG(4)
C
             WRITE (   *,325)
             WRITE (HIST,325)
             READ  (   *,  *) FLAG(5)
             WRITE (HIST,  *) FLAG(5)
C
             WRITE (   *,330)
             WRITE (HIST,330)
             READ  (   *,  *) FLAG(6)
             WRITE (HIST,  *) FLAG(6)
C
             WRITE (   *,335)
             WRITE (HIST,335)
             READ  (   *,  *) FLAG(7)
             WRITE (HIST,  *) FLAG(7)
         ENDIF
C
         IF (FLAG(1).EQ.1) THEN
             CALL SGEOMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(1) , RFLAG(1), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(1).EQ.0) THEN
                 WRITE (   *,345) STAT(1), NSR   , NS
                 WRITE (HIST,345) STAT(1), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 2       )
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,350) RFLAG(1), CNT, CNTO
                 WRITE (HIST,350) RFLAG(1), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(2).EQ.1) THEN
             CALL SJAMMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(2) , RFLAG(2), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(2).EQ.0) THEN
                 WRITE (   *,355) STAT(2), NSR   , NS
                 WRITE (HIST,355) STAT(2), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 2       )
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,360) RFLAG(2), CNT, CNTO
                 WRITE (HIST,360) RFLAG(2), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(3).EQ.1) THEN
C            SET THE FLAG FOR LINEAR MODEL.
             PHIIND = 1
C
             CALL SLAVMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP     , MAXIC   , PHIIND
     Y                   ,STAT(3) , RFLAG(3), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(3).EQ.0) THEN
                 WRITE (   *,365) STAT(3), NSR   , NS
                 WRITE (HIST,365) STAT(3), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 2       )
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,370) RFLAG(3), CNT, CNTO
                 WRITE (HIST,370) RFLAG(3), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(4).EQ.1) THEN
C            SET THE FLAG FOR QUADRATIC MODEL.
             PHIIND = 2
C
             CALL SLAVMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP     , MAXIC   , PHIIND
     Y                   ,STAT(4) , RFLAG(4), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(4).EQ.0) THEN
                 WRITE (   *,375) STAT(4), NSR   , NS
                 WRITE (HIST,375) STAT(4), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 2       )
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,380) RFLAG(4), CNT, CNTO
                 WRITE (HIST,380) RFLAG(4), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(5).EQ.1) THEN
             CALL SMUSMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(5) , RFLAG(5), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(5).EQ.0) THEN
                 WRITE (   *,385) STAT(5), NSR   , NS
                 WRITE (HIST,385) STAT(5), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 2       )
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,390) RFLAG(5), CNT, CNTO
                 WRITE (HIST,390) RFLAG(5), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(6).EQ.1) THEN
             CALL SMSAMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(6) , RFLAG(6), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(6).EQ.0) THEN
                 WRITE (   *,395) STAT(6), NSR   , NS
                 WRITE (HIST,395) STAT(6), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 2       )
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,400) RFLAG(6), CNT, CNTO
                 WRITE (HIST,400) RFLAG(6), CNT, CNTO
             ENDIF
         ENDIF
C
         IF (FLAG(7).EQ.1) THEN
C            STORE THE ORIGINAL TBF DATA AND CONVERT DATA TO TTF UNITS.
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = IDX, IDXE)
             CALL SMFTRN
     G                   (NS      , DM      , DM      , 6
     B                   ,DAT(IDX)
     Y                   ,RFLG    )
C
             CALL SNPTMA
     G                   (DAT(IDX), NS      , NSB     , NSE     , NSR
     G                   ,TYP
     Y                   ,STAT(7) , RFLAG(7), CNT     , TM3C    , TM4C )
C
             IF (RFLAG(7).EQ.0) THEN
                 WRITE (   *,405) STAT(7), NSR   , NS
                 WRITE (HIST,405) STAT(7), NSR   , NS
                 CALL PLTUAY
     G                       (NSR     , TM3C    , 2       )
             ELSE
                 CNTO = NSB + CNT - 1
                 WRITE (   *,410) RFLAG(7), CNT, CNTO
                 WRITE (HIST,410) RFLAG(7), CNT, CNTO
             ENDIF
C
C            RESTORE THE TBF DATA.
             REWIND TMPB
             READ  (TMPB) (DAT(I), I = IDX, IDXE)
         ENDIF
         RETURN
         END
