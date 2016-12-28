         SUBROUTINE MDLBAM
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLBAM
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
C        TO PROVIDE THE  INTERACTIVE PROMPTS AND  INPUTS FOR  THE BROOKS
C        AND MOTLEY DISCRETE SOFTWARE  RELIABILITY MODELS FOR A SOFTWARE
C        SYSTEM.  ADDITIONALLY, THE ROUTINE  WILL PERFORM  THE ACCESS TO
C        THE SMFLIB ROUTINES NEEDED TO PERFORM  THE MODEL EXECUTIONS AND
C        PREDICTED DATA VECTOR CREATIONS.
C   DESCRIPTION
C        A DRIVER ROUTINE  (EXVBAM) AND TWO SMFLIB  ROUTINES (SBAMMD AND
C        SBAMPR) WILL BE USED WITHIN THIS ROUTINE.
C
C        THE FIRST ROUTINE  (EXVBAM) CREATES THE  EXTRA TWO DATA VECTORS
C        REQUIRED IN THIS MODEL. THESE TWO VECTORS INCLUDE ONE THAT CON-
C        TAINS THE FRACTIONAL PORTION OF CODE BEING TESTED FOR EACH TES-
C        TING INTERVAL, AND A SECOND THAT CONTAINS THE CUMULATIVE NUMBER
C        OF FAULTS  DETECTED IN THE PREVIOUS  TESTING PERIODS  (FOR THAT
C        FRACTIONAL PART).
C
C        THE SECOND ROUTINE  (SBAMMD) PROVIDES  THE CALCULATIONS  OF THE
C        RELIABILITY VIA THE BINOMIAL AND POISSON MODELS WITH THE PROBA-
C        BILITY OF  CORRECTING FAULTS  IN THE PROGRAM  WITHOUT INSERTING
C        NEW ONES SPECIFIED.  THE FIRST ARGUMENT OF THE SBAMMD CALL LINE
C        PROVIDES  THE ROUTINE WITH  THE NUMBER OF THE MODEL DESIRED.
C
C        ONCE THE  EXECUTION OF  SBAMMD IS PERFORMED,  THE RETURN STATUS
C        FLAG (RFLAG) WILL INDICATE THE SUCCESSFULNESS  OF THE MODEL ES-
C        TIMATION PROCESS. THE FOLLOWING VALUES ARE POSSIBLE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             1 - MAXIMUM ITERATIONS
C             2 - THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS THAN THE  OBSERVED
C                 FAULTS TO DATE
C
C        FOR RFLAG VALUES OF 0 AND 1 THE MODEL ESTIMATES WILL BE LISTED.
C
C        THE THIRD ROUTINE (SBAMPR) PROVIDES FUTURE PREDICTIONS AND GEN-
C        ERATES THE VECTOR OF PREDICTED FAULT COUNTS.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C        BOTH
C             PMAX   (R) = MAXIMUM NUMBER OF ITERATIONS FOR PREDICTIONS
C        YIELDED
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             EST    (I) = NUMBER OF ESTIMATIONS FROM THE MODEL
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1
C             TM2C   (R) = TEMPORARY STORAGE AREA OF SIZE END1
C             XPRD   (R) = FUTURE PREDICTIONS (NUMBER AND 95% C.I.)
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             CNTP   (R) = COUNT OF PERIODS TO FIND M FAULTS
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             COUNT  (I) = ITERATION COUNTER
C             DNOF   (R) = DESIRED NUMBER OF FAULTS (M)
C             ERM    (R) = FAULTS FOUND TO DATE IN FRACTIONAL PART
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             EXF    (R) = EXPECTED  FRACTIONAL PART  OF CODE  TESTED IN
C                          THE NEXT PERIOD
C             EXL    (R) = EXPECTED TESTING LENGTH OF NEXT PERIOD
C             FLAG   (I) = OPTION SELECTION FLAG
C             FLAG2  (I) = PROGRAM MAXIMUM OVER-RIDE FLAG
C             LB     (C) = OUTPUT MODEL LABELS
C             MAXIC  (I) = USER-SPECIFIED MAX. NUMBER OF ITERATIONS
C             NPV    (I) = NUMBER OF PREDICTED VALUES FLAG
C             NS     (I) = ENTRIES IN DATA VECTORS
C             PEC    (R) = PROBABILITY OF CORRECTING FAULTS  WITHOUT IN-
C                          TRODUCING NEW ONES (SPECIFIED VALUE)
C             PED    (R) = PROBABILITY OF DETECTING FAULTS INITIAL ESTI-
C                          MATE
C             PREDAT (R) = PREDICTION DATA VALUES ARRAY
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = STATISTICS RESULTS VECTOR
C             TNOF   (R) = TOTAL NUMBER OF FAULTS INITIAL ESTIMATE
C             TNOFR  (R) = TOTAL NUMBER OF FAULTS REMAINING ESTIMATE
C             TOTL   (R) = TOTAL EXPECTED LENGTHS FOR PERIOD PREDICTION
C             WT     (I) = FLAG TO PAUSE THE ASSUMPTIONS LIST OUTPUT
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
C               /PMXCOM/      PREDICTION MAXIMUM ITERATION COMMON
         COMMON /PMXCOM/      PMAX
C
C               /PRECOM/      PREDICTION DATA STORAGE COMMON
         COMMON /PRECOM/      PRD(END1)     ,XPRD(3)       ,D
     1                       ,DFLG          ,ESC           ,ESD
     2                       ,EST
         INTEGER              DFLG          ,ESC           ,ESD
     1                       ,EST
C
C               /TMPCOM/      TEMPORARY STORAGE COMMON
         COMMON /TMPCOM/      TM1C(END1)    ,TM2C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CONFLG        ,COUNT         ,ESF
     1                       ,FLAG          ,FLAG2         ,J
     2                       ,MAXIC         ,NPV           ,NS
     3                       ,RFLAG         ,WT
         DIMENSION            PREDAT(2,4)   ,STATS(3)
         CHARACTER*10         LB(2)
C
C                             LOCAL ASSIGNMENTS
         DATA                 LB(1) /' BINOMIAL '/ ,LB(2) /' POISSON  '/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR BROOKS AND MOTLEY MODEL DESCRIPTION;'
     1          ,' ELSE ZERO.')
  301    FORMAT (' '
     1        ,/,'    THE BROOKS AND MOTLEY DISCRETE SR MODEL ASSUMPTIO'
     2          ,'NS'
     3       ,//,'     1. THE SOFTWARE IS OPERATED IN A SIMILAR MANNER '
     4          ,'AS THE ANTICI-'
     5        ,/,'        PATED OPERATIONAL USAGE.'
     6       ,//,'     2. ALL FAULTS ARE EQUALLY LIKELY TO OCCUR  AND A'
     7          ,'RE INDEPENDENT'
     8        ,/,'        OF EACH OTHER.'
     9       ,//,'     3. EACH FAULT IS OF THE SAME ORDER OF "IMPACT" A'
     A          ,'S ANY OTHER.'
     B       ,//,'     4. THE NUMBER OF  SOFTWARE FAULTS DETECTED ON EA'
     C          ,'CH TESTING OC-')
  302    FORMAT ('        CASION IS PROPORTIONAL TO THE NUMBER OF  FAUL'
     1          ,'TS AT RISK FOR'
     2        ,/,'        DETECTION.  THIS, IN TURN, IS PROPORTIONAL TO'
     3          ,' THE NUMBER OF'
     4        ,/,'        FAULTS REMAINING.'
     5       ,//,'     5. THE PROPORTIONALITY FACTOR OR PROBABILITY (DE'
     6          ,'NOTED AS Q FOR'
     7        ,/,'        THE BINOMIAL MODEL AND PHI FOR THE POISSON MO'
     8          ,'DEL) OF DETEC-'
     9        ,/,'        TING ANY FAULT DURING A SPECIFIED  UNIT INTER'
     A          ,'VAL OF TESTING'
     B        ,/,'        IS CONSTANT OVER ALL OCCASIONS AND INDEPENDEN'
     C          ,'T OF THE FAULT'
     D        ,/,'        DETECTIONS.'
     E       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  303    FORMAT ('     6. THE FAULTS REINTRODUCED  IN THE CORRECTION PR'
     1          ,'OCESS ARE PRO-'
     2        ,/,'        PORTIONAL TO THE NUMBER OF FAULTS DETECTED.'
     3       ,//,'    MODEL DATA REQUIREMENTS'
     4       ,//,'     1. THE NUMBER OF FAULTS  DETECTED IN EACH INTERV'
     5          ,'AL OF TESTING.'
     6       ,//,'     2. THE LENGTHS OF THE VARIOUS TESTING INTERVALS.'
     7       ,//,'        "1 & 2 SHOULD HAVE BEEN ENTERED VIA THE DATA '
     8          ,'INPUT MODULE."'
     9       ,//,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     A          ,'HE APPROPRIATE'
     B        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     C          ,'EL;  OTHERWISE'
     D        ,/,'    ZERO TO RETURN TO THE MENU OF MODELS.')
  305    FORMAT (' '
     1        ,/,'   ENTER ONE FOR DETAILS ON THE BINOMIAL AND POISSON '
     2          ,'METHODS;  ELSE'
     3        ,/,'   ZERO.')
  306    FORMAT (' '
     1        ,/,'    IN THE BINOMIAL MODEL,  IT IS ASSUMED THAT THE NU'
     2          ,'MBER OF FAULTS'
     3        ,/,'    DETECTED IN THE  I-TH  INTERVAL OF TESTING  FOLLO'
     4          ,'WS  A BINOMIAL'
     5        ,/,'    DISTRIBUTION WITH PARAMETERS N(I) AND Q(I), WHERE'
     6          ,':'
     7       ,//,'       Q(I) = 1 - (1 - Q) ** T(I);'
     8        ,/,'         T(I) IS THE LENGTH OF THE I-TH TESTING PERIO'
     9          ,'D, AND'
     A        ,/,'         Q IS THE FAULTS DETECTION PROBABILITY,'
     B       ,//,'    AND'
     C       ,//,'       N(I) = N * W(I) - ALPHA * NCUM(I-1);'
     D        ,/,'         N IS THE TOTAL NUMBER OF FAULTS IN THE PROGR'
     E          ,'AM,'
     F        ,/,'         W(I) IS THE  FRACTION  OF THE PROGRAM  BEING'
     G          ,' TESTED ON THE')
  307    FORMAT ('         I-TH TESTING OCCASION,'
     1        ,/,'         NCUM(I-1) IS THE CUMULATIVE NUMBER OF FAULTS'
     2          ,' FOUND IN THIS'
     3        ,/,'         SECTION OF THE CODE THROUGH THE (I-1)ST TEST'
     4          ,'ING PERIOD AND'
     5        ,/,'         ALPHA IS THE PROBABILITY OF CORRECTING FAULT'
     6          ,'S  WITHOUT RE-'
     7        ,/,'         INSERTING NEW ONES.'
     8       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  308    FORMAT ('    IN THE POISSON  MODEL,  IT IS ASSUMED THAT THE NU'
     1          ,'MBER OF FAULTS'
     2        ,/,'    DETECTED IN THE  I-TH  INTERVAL OF TESTING  FOLLO'
     3          ,'WS  A  POISSON'
     4        ,/,'    DISTRIBUTION WITH PARAMETER'
     5        ,/,'    LAMDA(I) = N(I) * PHI(I) WHERE:'
     6       ,//,'       PHI(I) = 1 - (1 - PHI) ** T(I);'
     7        ,/,'         T(I) IS THE LENGTH OF THE I-TH TESTING PERIO'
     8          ,'D, AND')
  309    FORMAT ('         PHI IS THE FAULT DETECTION PROBABILITY'
     1       ,//,'    AND'
     2       ,//,'       N(I) = N * W(I) - ALPHA * NCUM(I-1);'
     3        ,/,'         N IS THE TOTAL NUMBER OF FAULTS IN THE PROGR'
     4          ,'AM,'
     5        ,/,'         W(I) IS THE  FRACTION OF THE  PROGRAM BEING '
     6          ,'TESTED  ON THE'
     7        ,/,'         I-TH TESTING OCCASION,'
     8        ,/,'         NCUM(I-1) IS THE CUMULATIVE NUMBER OF FAULTS'
     9          ,' FOUND IN THIS'
     A        ,/,'         SECTION OF THE CODE THROUGH  THE (I-1)ST TES'
     B          ,'TING PERIOD, &'
     C        ,/,'         ALPHA IS THE PROBABILITY OF CORRECTING FAULT'
     D          ,'S  WITHOUT RE-'
     E        ,/,'         INSERTING NEW ONES.')
  310    FORMAT (' '
     1        ,/,'   ENTER ONE TO INPUT  THE FRACTION OF THE PROGRAM  B'
     2          ,'EING TESTED AT'
     3        ,/,'   EACH TESTING PERIOD, OR ZERO TO SET THE FRACTIONS '
     4          ,'ALL EQUAL TO 1'
     5        ,/,'   (I.E., THE ENTIRE PROGRAM WAS UNDER TEST IN EACH O'
     6          ,'CCASION).')
  315    FORMAT (' '
     1        ,/,'   ENTER ONE FOR THE BINOMIAL MODEL,  TWO FOR THE POI'
     2          ,'SSON MODEL, OR'
     3        ,/,'   THREE TO TERMINATE MODEL EXECUTION.')
  320    FORMAT (' '
     1        ,/,'   THE INITIAL ESTIMATES TO BE USED IN THE ESTIMATION'
     2          ,' PROCESS ARE:'
     3        ,/,'    PROB. OF DETECTING FAULTS',E12.5
     4        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5
     5       ,//,'   ENTER ONE TO USE DIFFERENT INITIAL ESTIMATES, OR Z'
     6          ,'ERO TO USE THE'
     7        ,/,'   INITIAL ESTIMATES.')
  325    FORMAT (' '
     1        ,/,'    ENTER INITIAL ESTIMATE FOR THE PROBABILITY OF DET'
     2          ,'ECTING FAULTS'
     3        ,/,'    (BETWEEN 0.10000E-06 AND 0.99999E-00).')
  330    FORMAT (' '
     1        ,/,'    ENTER INITIAL ESTIMATE FOR THE TOTAL NUMBER OF FA'
     2          ,'ULTS'
     3        ,/,'    (BETWEEN',E12.5,' AND 0.10000E+08).')
  335    FORMAT (' '
     1        ,/,'   ENTER THE DESIRED ALPHA (PROBABILITY OF CORRECTING'
     2          ,' FAULTS IN THE'
     3        ,/,'   PROGRAM WITHOUT INSERTING NEW ONES).')
  340    FORMAT ('   ENTER MAXIMUM NUMBER OF ITERATIONS.')
  345    FORMAT (' '
     1        ,/,' **THE MAXIMUM NUMBER OF ITERATIONS WAS REACHED  WITH'
     2          ,' THE FOLLOWING'
     3        ,/,'   FINAL',A10,'MODEL ESTIMATES:')
  350    FORMAT ('    PROB. OF DETECTING FAULTS',E12.5
     1        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5
     2        ,/,'    # OF FAULTS REMAINING    ',E12.5)
  355    FORMAT (' '
     1        ,/,' **THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY FO'
     2          ,'R THE',A10
     3        ,/,'   MODEL.')
  356    FORMAT (' '
     1        ,/,' **THE ESTIMATE  FOR THE TOTAL  NUMBER OF FAULTS  TO '
     2          ,'BE DETECTED IN'
     3        ,/,'   THE PROGRAM BEFORE  ALL FAULTS ARE UNCOVERED  CAME'
     4          ,' OUT LESS THAN'
     5        ,/,'   THE NUMBER OF FAULTS FOUND TO DATE WITHIN THE'
     6          ,A10,'MODEL.')
  360    FORMAT (' '
     1        ,/,'   THE',A10,'MODEL ESTIMATES, AFTER',I4,' ITERATIONS'
     2          ,' ARE:')
  361    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   (ROUNDED) NUMBER OF FAULTS REMAINING IS LESS THAN '
     4          ,'ONE.')
  365    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE NUMBER OF FAULTS EXPECTED IN THE NEXT TEST'
     3          ,'ING PERIOD'
     4        ,/,'    2) THE NUMBER OF PERIODS NEEDED TO DISCOVER THE N'
     5          ,'EXT M FAULTS'
     6        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     7          ,'S.')
  370    FORMAT ('    ENTER FRACTION OF THE PROGRAM TO BE TESTED (FOR E'
     1          ,'NTIRE PROGRAM,'
     2        ,/,'    ENTER ONE).')
  375    FORMAT ('    HOW MANY FAULTS  HAVE BEEN FOUND TO DATE  IN THE '
     1          ,'SECTION OF THE'
     2        ,/,'    CODE TO BE TESTED?')
  380    FORMAT ('    ENTER PROJECTED LENGTH OF THE PERIOD, OR ZERO TO '
     1          ,'END.')
  385    FORMAT ('    # OF FAULTS EXPECTED     ',E12.5,/)
  390    FORMAT ('    ENTER PROJECTED LENGTH OF THE PERIODS.')
  395    FORMAT ('    ENTER VALUE OF M (BETWEEN ONE AND',E12.5,'), OR Z'
     1          ,'ERO TO END.')
  400    FORMAT ('    # OF PERIODS EXPECTED    ',E12.5,/)
  401    FORMAT ('  **# OF PERIODS EXCEEDS     ',E12.5,' (PROGRAM MAXIM'
     1          ,'UM)'
     2        ,/,'    ENTER ONE TO CHANGE THE PROGRAM MAXIMUM; ELSE ZER'
     3          ,'O.')
  402    FORMAT ('    ENTER THE NEW VALUE FOR THE PROGRAM MAXIMUM.')
  403    FORMAT (' ')
  405    FORMAT (' '
     1        ,/,'   ENTER ONE TO PERFORM AN ANALYSIS OF THE MODEL FIT '
     2          ,'USING THE PRE-'
     3        ,/,'   DICTIONS OF THIS MODEL; ELSE ZERO.')
  410    FORMAT ('   ENTER ONE FOR PREDICTIONS BASED ON BINOMIAL, OR TW'
     1          ,'O FOR POISSON.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE VECTOR SIZE COUNTER.
         NS = PRC(3,1)
C
C        ZERO THE SUCCESSFULNESS  ELEMENTS OF THE PREDICTION DATA ARRAY.
         PREDAT(1,1) = 0.0
         PREDAT(2,1) = 0.0
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,301)
             WRITE (HIST,301)
             WRITE (   *,302)
             WRITE (HIST,302)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,303)
             WRITE (HIST,303)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
C
         IF (CONFLG.EQ.1) THEN
C            ISSUE THE  ADDITIONAL  MODEL DESCRIPTION PROMPT AND PROVIDE
C            THE INFORMATION.
             WRITE (   *,305)
             WRITE (HIST,305)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
             IF (FLAG.EQ.1) THEN
                 WRITE (   *,306)
                 WRITE (HIST,306)
                 WRITE (   *,307)
                 WRITE (HIST,307)
                 READ  (   *,  *) WT
                 WRITE (HIST,  *) WT
                 WRITE (   *,308)
                 WRITE (HIST,308)
                 WRITE (   *,309)
                 WRITE (HIST,309)
             ENDIF
C
C            ACCESS TO THE EXVBAM ROUTINE TO  GENERATE THE TM1C AND TM2C
C            VECTORS.
             WRITE (   *,310)
             WRITE (HIST,310)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
             CALL EXVBAM
     G                   (FLAG  , NS    , DAT
     Y                   ,TM2C  , TM1C  )
C
C            DOWHILE (ESF .NE. TERMINATION)
 1000            WRITE (   *,315)
                 WRITE (HIST,315)
                 READ  (   *,  *) ESF
                 WRITE (HIST,  *) ESF
                 IF ((ESF.GE.1) .AND. (ESF.LE.2)) THEN
                 PREDAT(ESF,1) = 0.0
C
C                SET THE INITIAL ESTIMATES FOR PROBABILITY OF  DETECTING
C                FAULTS AND TOTAL NUMBER OF FAULTS.
                 PED  = 0.0
                 TNOF = 0.0
                 DO 1025 J = 1, NS
                     PED = PED   + DAT(J) / ((DAT(NS) + TM2C(NS))
     1                   * DAT(J + END1)  *   TM1C(J))
                     IF (TNOF.LT.(TM2C(J) + DAT(J)) / TM1C(J)) THEN
                         TNOF =  (TM2C(J) + DAT(J)) / TM1C(J)
                     ENDIF
 1025                CONTINUE
C                ENDDO
                 PED  = PED  / REAL(NS)
                 TNOF = TNOF + 1.0
C
C                ALLOW THE USER TO REPLACE THE INITIAL ESTIMATES, PROMPT
C                FOR THE REMAINING VALUES, AND ACCESS THE MODEL.
                 WRITE (   *,320) PED, TNOF
                 WRITE (HIST,320) PED, TNOF
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
                     WRITE (   *,325)
                     WRITE (HIST,325)
                     READ  (   *,  *) PED
                     WRITE (HIST,  *) PED
                     TNOF = TNOF - 0.999
                     WRITE (   *,330) TNOF
                     WRITE (HIST,330) TNOF
                     READ  (   *,  *) TNOF
                     WRITE (HIST,  *) TNOF
                 ENDIF
                 WRITE (   *,335)
                 WRITE (HIST,335)
                 READ  (   *,  *) PEC
                 WRITE (HIST,  *) PEC
                 WRITE (   *,340)
                 WRITE (HIST,340)
                 READ  (   *,  *) MAXIC
                 WRITE (HIST,  *) MAXIC
C
                 CALL SBAMMD
     G                       (ESF   , MAXIC , NS   , PEC   , PED
     G                       ,TNOF  , TM1C  , TM2C , DAT(1), DAT(STR2)
     Y                       ,STATS , COUNT , RFLAG)
C
                 IF (RFLAG.EQ.1) THEN
C                    THE MAXIMUM  ITERATION COUNT  WAS REACHED  BEFORE A
C                    SOLUTION WAS FOUND;  ISSUE ERROR MESSAGE AND OUTPUT
C                    THE APPLICABLE FINAL ESTIMATES.
                     TNOFR = STATS(1) - (TM2C(NS) + DAT(NS))
                     WRITE (   *,345) LB(ESF)
                     WRITE (HIST,345) LB(ESF)
                     WRITE (   *,350) STATS(2), STATS(1), TNOFR
                     WRITE (HIST,350) STATS(2), STATS(1), TNOFR
                 ELSEIF (RFLAG.EQ.2) THEN
C                    THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY.
                     WRITE (   *,355) LB(ESF)
                     WRITE (HIST,355) LB(ESF)
                 ELSEIF (RFLAG.EQ.4) THEN
C                    THE  TOTAL ERRORS  ESTIMATE  WAS LESS THAN  THE OB-
C                    SERVED ERRORS TO DATE.
                     WRITE (   *,356) LB(ESF)
                     WRITE (HIST,356) LB(ESF)
                 ELSE
C                ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY-----------
C                        A SOLUTION WAS FOUND;   COMPUTE EXTRA NUMBER OF
C                        FAULTS REMAINING.
                         TNOFR = STATS(1) - (TM2C(NS) + DAT(NS))
C
C                        OUTPUT THE RESULTS.
                         WRITE (   *,360) LB(ESF) , COUNT
                         WRITE (HIST,360) LB(ESF) , COUNT
                         WRITE (   *,350) STATS(2), STATS(1), TNOFR
                         WRITE (HIST,350) STATS(2), STATS(1), TNOFR
C
C                        SET THE SUCCESSFULNESS FLAG;  AND STORE THE TO-
C                        TAL NUMBER OF FAULTS, & THE TWO PROBABILITY ES-
C                        TIMATES.
                         PREDAT(ESF,1) = 1.0
                         PREDAT(ESF,2) = STATS(1)
                         PREDAT(ESF,3) = STATS(2)
                         PREDAT(ESF,4) = STATS(3)
C
                         IF (TNOFR.LT.0.5) THEN
C                            FUTURE PREDICTIONS ARE NOT ALLOWED (TNOFR).
                             WRITE (   *,361)
                             WRITE (HIST,361)
                         ELSE
C                            DOWHILE (FLAG .NE. TERMINATION)
 1050                            WRITE (   *,365)
                                 WRITE (HIST,365)
                                 READ  (   *,  *) FLAG
                                 WRITE (HIST,  *) FLAG
                                 IF ((FLAG.LT.1) .OR. (FLAG.GT.2))
     1                                                        GO TO 1200
C
                                 IF     (FLAG.EQ.1) THEN
C                                    FAULTS IN NEXT TESTING PERIOD.
                                     WRITE (   *,370)
                                     WRITE (HIST,370)
                                     READ  (   *,  *) EXF
                                     WRITE (HIST,  *) EXF
                                     IF (EXF.LT.1.0) THEN
                                         WRITE (   *,375)
                                         WRITE (HIST,375)
                                         READ  (   *,  *) ERM
                                         WRITE (HIST,  *) ERM
                                     ELSE
                                         ERM = TM2C(NS) + DAT(NS)
                                     ENDIF
                                     NPV = 1
C                                    DOWHILE (EXL .NE. TERMINATION)
 1075                                    WRITE (   *,380)
                                         WRITE (HIST,380)
                                         READ  (   *,  *) EXL
                                         WRITE (HIST,  *) EXL
                                         IF (EXL.EQ.0.0) GO TO 1100
                                         CALL SBAMPR
     G                                               (NPV     ,NS
     G                                               ,STATS(1),STATS(2)
     G                                               ,STATS(3),EXF
     G                                               ,EXL     ,ERM
     G                                               ,TM1C    ,DAT(STR2)
     G                                               ,TM2C
     Y                                               ,XPRD    )
                                         IF (XPRD(1).GT.TNOFR) THEN
                                             XPRD(1) = TNOFR
                                         ENDIF
                                         WRITE (   *,385) XPRD(1)
                                         WRITE (HIST,385) XPRD(1)
                                         GO TO 1075
 1100                                    CONTINUE
C                                    ENDWHILE
C
                                 ELSEIF (FLAG.EQ.2) THEN
C                                    PERIODS TO DETECT NEXT M FAULTS.
                                     WRITE (   *,390)
                                     WRITE (HIST,390)
                                     READ  (   *,  *) EXL
                                     WRITE (HIST,  *) EXL
                                     WRITE (   *,370)
                                     WRITE (HIST,370)
                                     READ  (   *,  *) EXF
                                     WRITE (HIST,  *) EXF
                                     IF (EXF.LT.1.0) THEN
                                         WRITE (   *,375)
                                         WRITE (HIST,375)
                                         READ  (   *,  *) ERM
                                         WRITE (HIST,  *) ERM
                                     ELSE
                                         ERM = TM2C(NS) + DAT(NS)
                                     ENDIF
                                     NPV = 1
C                                    DOWHILE (DNOF .NE. TERMINATION)
 1125                                    WRITE (   *,395) TNOFR
                                         WRITE (HIST,395) TNOFR
                                         READ  (   *,  *) DNOF
                                         WRITE (HIST,  *) DNOF
                                         IF (DNOF.EQ.0.0) GO TO 1175
                                         TOTL = 0.0
                                         CNTP = 0.0
C                                        DOUNTIL (PREDICTED.GE.DESIRED)
 1150                                        TOTL = TOTL + EXL
                                             CNTP = CNTP + 1.0
                                             CALL SBAMPR
     G                                                   (NPV      , NS
     G                                                   ,STATS(1)
     G                                                   ,STATS(2)
     G                                                   ,STATS(3) , EXF
     G                                                   ,TOTL     , ERM
     G                                                   ,TM1C
     G                                                   ,DAT(STR2)
     G                                                   ,TM2C
     Y                                                   ,XPRD     )
                                             IF ((XPRD(1).LT.DNOF) .AND.
     1                                           (CNTP   .LE.PMAX)
     2                                                      ) GO TO 1150
C                                        ENDUNTIL
                                         IF (CNTP.LE.PMAX) THEN
                                             WRITE (   *,400) CNTP
                                             WRITE (HIST,400) CNTP
                                         ELSE
                                             WRITE (   *,401) PMAX
                                             WRITE (HIST,401) PMAX
                                             READ  (   *,  *) FLAG2
                                             WRITE (HIST,  *) FLAG2
                                             IF (FLAG2.EQ.1) THEN
                                                 WRITE (   *,402)
                                                 WRITE (HIST,402)
                                                 READ  (   *,  *) PMAX
                                                 WRITE (HIST,  *) PMAX
                                             ENDIF
                                             WRITE (   *,403)
                                             WRITE (HIST,403)
                                         ENDIF
                                         GO TO 1125
 1175                                    CONTINUE
C                                    ENDWHILE
                                 ENDIF
                                 GO TO 1050
 1200                            CONTINUE
C                            ENDWHILE
                         ENDIF
C                ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY-----------
                 ENDIF
                 GO TO 1000
                 ENDIF
C            ENDWHILE
C
             IF ((PREDAT(1,1).EQ.1.0) .OR. (PREDAT(2,1).EQ.1.0)) THEN
C                AT LEAST ONE EXECUTION  WAS SUCCESSFUL, PROMPT USER AND
C                SEE IF THE PREDICTED VECTOR CREATION IS DESIRED.
                 WRITE (   *,405)
                 WRITE (HIST,405)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    THE VECTOR OF PREDICTED DATA IS DESIRED.
                     IF (PREDAT(1,1).NE.PREDAT(2,1)) THEN
C                        ONLY ONE EXECUTION  WAS SUCCESSFUL;  SET ESF TO
C                        INDICATE BINOMIAL OR POISSON.
                         ESF = PREDAT(1,1) * 1.0 + PREDAT(2,1) * 2.0
                     ELSE
C                        BOTH EXECUTIONS WERE  PERFORMED AND SUCCESSFUL;
C                        SEE WHICH THE USER DESIRES.
                         WRITE (   *,410)
                         WRITE (HIST,410)
                         READ  (   *,  *) ESF
                         WRITE (HIST,  *) ESF
                     ENDIF
C
C                    ACCESS THE SBAMPR ROUTINE TO GENERATE THE PREDICTED
C                    FAULT COUNT DATA VECTOR.
                     CALL SBAMPR
     G                           (NS            , NS
     G                           ,PREDAT(ESF,2) , PREDAT(ESF,3)
     G                           ,PREDAT(ESF,4) , EXF
     G                           ,EXL           , ERM
     G                           ,TM1C          , DAT(STR2)
     G                           ,TM2C
     Y                           ,PRD           )
C
C                    SET THE ESTIMATION SELECTION CODE TO INDICATE WHICH
C                    MODEL WAS RESPONSIBLE FOR  THE PREDICTIONS; AND SET
C                    ESD TO INDICATE THAT THE INTERVAL DATA TYPE WAS RE-
C                    SPONSIBLE.
                     ESC = 9
                     ESD = 3
C
C                    SET THE GOODNESS-OF-FIT ESTIMATION CONSTANT.
                     EST = 2
C
C                    PERFORM ANALYSIS OF MODEL FIT.
                     CALL FITDRV
                 ENDIF
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE MDLDRV
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLDRV
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE THE MENUS AND TRANSFERS  FOR THE EXECUTION OF THE 11
C        SOFTWARE RELIABILITY MODELS.
C   DESCRIPTION
C        THE ONLY REAL PROCESSING INVOLVED  IN THIS ROUTINE INVOLVES THE
C        SELECTION OF SETTING  OF THE  PARAMETERS FOR  THE TBF ANALYSES.
C        THE FIRST SIMPLY POINTS  TO THE FIRST (WC)  OR THE SECOND (CPU)
C        DATA SECTION OF THE DATA VECTOR.   THE SECOND PARAMETER HOLDS A
C        MODIFIED SAMPLE SIZE, WHICH REMOVES THE FINAL ENTERED VALUE FOR
C        DATA SETS WHERE TESTING CONTINUED AFTER THE LAST FAILURE.
C
C        ALL TRANSFERS WILL  USE SECONDARY  ROUTINES.  THE ROUTINES WILL
C        ONLY HANDLE PRELIMINARY INPUT AND OUTPUT. ALL REQUIRED COMPUTA-
C        TIONS WILL BE PERFORMED BY ROUTINES OF THE SMFLIB LIBRARY.
C
C        THE PRD COMMON VECTOR  WILL BE SET TO CONTAIN  THE MODEL'S PRE-
C        DICTED ESTIMATIONS  OF THE DATA UPON RETURN TO THE ROUTINE, FOR
C        SUBSEQUENT GOODNESS-OF-FIT TESTS AND GRAPHIC DISPLAYS.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             FLAG   (I) = OPTION FLAG
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C   ERRORS
C        "DATA TYPE ERROR; TRY AGAIN (AFTER THE PROMPT)."
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
C               /LBLCOM/      DATA TYPES AND TBF UNITS LABELS COMMON
         COMMON /LBLCOM/      TYPES(3)      ,UNITS(7)
         CHARACTER* 7         UNITS
         CHARACTER*10         TYPES
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              FLAG          ,I             ,IDX
     1                       ,NS
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'   THIS DATA ANALYSIS WILL BE PERFORMED IN UNITS CONS'
     2          ,'ISTENT WITH THE'
     3        ,/,'   STORED DATA.  THIS MEANS THAT:'
     4        ,/,' ')
  305    FORMAT ('     WC  TBF ESTIMATIONS AND PREDICTIONS WILL BE MADE'
     1          ,' IN ',A7)
  310    FORMAT ('     CPU TBF ESTIMATIONS AND PREDICTIONS WILL BE MADE'
     1          ,' IN ',A7)
  315    FORMAT (' '
     1        ,/,'   ENTER ONE TO CONTINUE WITH THE ANALYSIS;  ELSE ZER'
     2          ,'O TO RETURN TO'
     3        ,/,'   THE MAIN MODULE MENU, SO CONVERSION CAN BE MADE BY'
     4          ,' MODULE THREE.')
  320    FORMAT (' '
     1        ,/,'   ENTER TIME MODEL OPTION, OR ZERO FOR A LIST.')
  325    FORMAT ('1')
  330    FORMAT ('   THE AVAILABLE WALL CLOCK OR CPU TIME MODELS ARE:'
     1        ,/,'    1 THE GEOMETRIC MODEL'
     2        ,/,'    2 THE JELINSKI/MORANDA DE-EUTROPHICATION MODEL'
     3        ,/,'    3 THE LITTLEWOOD AND VERRALL BAYESIAN MODEL'
     4        ,/,'    4 THE MUSA BASIC EXECUTION TIME MODEL'
     5        ,/,'    5 THE MUSA LOG POISSON EXECUTION TIME MODEL'
     6        ,/,'    6 THE NHPP MODEL FOR TIME-BETWEEN-FAILURE OCC.'
     7        ,/,'    7 RETURN TO THE MAIN PROGRAM'
     8        ,/,'   ENTER MODEL OPTION.')
  335    FORMAT (' '
     1        ,/,'   ENTER ONE FOR WC TBF OR TWO FOR CPU TBF.')
  340    FORMAT (' **DATA TYPE ERROR; TRY AGAIN (AFTER THE PROMPT).')
  345    FORMAT (' '
     1        ,/,'   ENTER COUNT MODEL OPTION, OR ZERO FOR A LIST.')
  350    FORMAT ('   THE AVAILABLE FAULT COUNT MODELS ARE:'
     1        ,/,'    1 THE BROOKS AND MOTLEY MODEL'
     2        ,/,'    2 THE GENERALIZED POISSON MODEL'
     3        ,/,'    3 THE NON-HOMOGENEOUS POISSON MODEL'
     4        ,/,'    4 THE SCHNEIDEWIND MODEL'
     5        ,/,'    5 THE S-SHAPED RELIABILITY GROWTH MODEL'
     6        ,/,'    6 RETURN TO THE MAIN PROGRAM'
     7        ,/,'   ENTER MODEL OPTION.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
         IF (DATTYP.LE.3) THEN
C            TBF DATA ARE PRESENT, ALLOW THE USER TO ABORT THE MODULE IF
C            THE ESTIMATION AND PREDICTION UNITS ARE NOT ACCEPTABLE.
             WRITE (   *,300)
             WRITE (HIST,300)
             IF (PRC(1,1).NE.0) THEN
C                WALL CLOCK DATA ARE PRESENT.
                 WRITE (   *,305) UNITS(PRC(1,3))
                 WRITE (HIST,305) UNITS(PRC(1,3))
             ENDIF
             IF (PRC(2,1).NE.0) THEN
C                CPU DATA ARE PRESENT.
                 WRITE (   *,310) UNITS(PRC(2,3))
                 WRITE (HIST,310) UNITS(PRC(2,3))
             ENDIF
             WRITE (   *,315)
             WRITE (HIST,315)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
C
             IF (FLAG.EQ.1) THEN
C                DOWHILE (TIME MODEL SELECTION .NE. RETURN)
 1000                WRITE (   *,320)
                     WRITE (HIST,325)
                     WRITE (HIST,320)
                     READ  (   *,  *) FLAG
                     WRITE (HIST,  *) FLAG
                     IF ((FLAG.LT.1) .OR. (FLAG.GT.7)) THEN
                         WRITE (   *,330)
                         WRITE (HIST,330)
                         READ  (   *,  *) FLAG
                         WRITE (HIST,  *) FLAG
                     ENDIF
                     IF ((FLAG.LT.1) .OR. (FLAG.GT.6)) GO TO 1075
C
C                    DETERMINE DATA TYPE, THE BEGINNING LOCATION NUMBER,
C                    AND THE "REAL" SAMPLE SIZE.
                     IF (DATTYP.LE.2) THEN
                         I = DATTYP
                     ELSE
C                        DOWHILE (ERROR ON INPUT)
 1025                        WRITE (   *,335)
                             WRITE (HIST,335)
                             READ  (   *,  *) I
                             WRITE (HIST,  *) I
                             IF ((I.EQ.1) .OR. (I.EQ.2)) GO TO 1050
                             WRITE (   *,340)
                             WRITE (HIST,340)
                             GO TO 1025
 1050                        CONTINUE
C                        ENDWHILE
                     ENDIF
                     IDX = 1 + (END1 * (I - 1))
                     NS  = PRC(I,1) + (PRC(I,2) - 1)
C
                     IF     (FLAG.EQ.1) THEN
                         CALL MDLGEO
     G                               (IDX   , NS    )
C
                     ELSEIF (FLAG.EQ.2) THEN
                         CALL MDLJAM
     G                               (IDX   , NS    )
C
                     ELSEIF (FLAG.EQ.3) THEN
                         CALL MDLLAV
     G                               (IDX   , NS    )
C
                     ELSEIF (FLAG.EQ.4) THEN
                         CALL MDLMUS
     G                               (IDX   , NS    )
C
                     ELSEIF (FLAG.EQ.5) THEN
                         CALL MDLMSA
     G                               (IDX   , NS    )
C
                     ELSE
                         CALL MDLNPT
     G                               (IDX   , NS    )
C
                     ENDIF
                     GO TO 1000
 1075                CONTINUE
C                ENDWHILE
             ENDIF
         ELSE
C            INTERVAL DATA ARE PRESENT.
C            DOWHILE (COUNT MODEL SELECTION .NE. RETURN)
 1100            WRITE (   *,345)
                 WRITE (HIST,325)
                 WRITE (HIST,345)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF ((FLAG.LT.1) .OR. (FLAG.GT.6)) THEN
                     WRITE (   *,350)
                     WRITE (HIST,350)
                     READ  (   *,  *) FLAG
                     WRITE (HIST,  *) FLAG
                 ENDIF
                 IF ((FLAG.LT.1) .OR. (FLAG.GT.5)) GO TO 1125
C
                 IF     (FLAG.EQ.1) THEN
                     CALL MDLBAM
C
                 ELSEIF (FLAG.EQ.2) THEN
                     CALL MDLGPO
C
                 ELSEIF (FLAG.EQ.3) THEN
                     CALL MDLNPI
C
                 ELSEIF (FLAG.EQ.4) THEN
                     CALL MDLSDW
C
                 ELSE
                     CALL MDLESH
C
                 ENDIF
                 GO TO 1100
 1125            CONTINUE
C            ENDWHILE
         ENDIF
         RETURN
         END
         SUBROUTINE MDLESH
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLESH
C
C   AUTHOR
C        PAUL J. KRAUS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO PROVIDE THE INTERACTIVE PROMPTS AND INPUTS FOR THE  S-SHAPED
C        RELIABILITY GROWTH MODEL.  ADDITIONALLY, THE ROUTINE  WILL PER-
C        FORM THE ACCESS TO  THE SMFLIB  ROUTINES NEEDED  TO PERFORM THE
C        MODEL EXECUTIONS AND PREDICTED DATA VECTOR CREATION.
C   DESCRIPTION
C        THREE SMFLIB ROUTINES  (SMFTRN, SESHMD, & SESHPR)  WILL BE USED
C        WITHIN THIS ROUTINE.
C
C        THE FIRST ROUTINE  (SMFTRN) CONVERTS THE  STORED TESTING LENGTH
C        PER PERIOD DATA  VECTOR TO CONTAIN THE  CUMULATIVE LENGTHS OVER
C        ALL PERIODS.
C
C        THE SECOND ROUTINE  (SESHMD) PROVIDES  THE CALCULATIONS  OF THE
C        RELIABILITY VIA THE  MAXIMUM LIKELIHOOD METHOD OF EXECUTION.
C
C        ONCE THE  EXECUTION OF  SESHMD IS PERFORMED,  THE RETURN STATUS
C        FLAG (RFLAG) WILL INDICATE THE SUCCESSFULNESS  OF THE MODEL ES-
C        TIMATION PROCESS. THE FOLLOWING VALUES ARE POSSIBLE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS THAN THE  OBSERVED
C                 FAULTS TO DATE
C
C        FOR RFLAG VALUE OF 0 THE MODEL ESTIMATES WILL BE LISTED.
C
C        THE THIRD ROUTINE (SESHPR) PROVIDES FUTURE PREDICTIONS AND GEN-
C        ERATES THE VECTOR OF PREDICTED FAULT COUNTS.
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
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C                          (BUT RETURNED TO ORIGINAL VALUES)
C             PMAX   (R) = MAXIMUM NUMBER OF ITERATIONS FOR PREDICTIONS
C        YIELDED
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             EST    (I) = NUMBER OF ESTIMATIONS FROM THE MODEL
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             XPRD   (R) = FUTURE PREDICTIONS (NUMBER AND 95% C.I.)
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             CNTP   (R) = COUNT OF PERIODS TO FIND M FAULTS
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             CTOT   (R) = TOTAL FAULTS FOUND TO DATE
C             DM     (R) = DUMMY ARGUMENT OF SMFTRN CALL
C             DNOF   (R) = DESIRED NUMBER OF FAULTS (M)
C             EXL    (R) = EXPECTED TESTING LENGTH OF NEXT PERIOD
C             FLAG   (I) = OPTION SELECTION FLAG
C             FLAG2  (I) = PROGRAM MAXIMUM OVER-RIDE FLAG
C             IDXE   (I) = ENDING INDEX OF THE DATA
C             NPV    (I) = NUMBER OF PREDICTED VALUES FLAG
C             NS     (I) = NUMBER OF ENTRIES IN DATA VECTOR
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = STATISTICS RESULTS ARRAY
C             TNOFR  (R) = TOTAL NUMBER OF FAULTS REMAINING ESTIMATE
C             TOTL   (R) = TOTAL EXPECTED LENGTHS FOR PERIOD PREDICTION
C             WT     (I) = FLAG TO PAUSE THE ASSUMPTIONS LIST OUTPUT
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
C               /PMXCOM/      PREDICTION MAXIMUM ITERATION COMMON
         COMMON /PMXCOM/      PMAX
C
C               /PRECOM/      PREDICTION DATA STORAGE COMMON
         COMMON /PRECOM/      PRD(END1)     ,XPRD(3)       ,D
     1                       ,DFLG          ,ESC           ,ESD
     2                       ,EST
         INTEGER              DFLG          ,ESC           ,ESD
     1                       ,EST
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CONFLG        ,FLAG          ,FLAG2
     1                       ,I             ,IDXE          ,NPV
     2                       ,NS            ,RFLAG         ,WT
         DIMENSION            STATS(2,3)    ,TNOFR(3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR S-SHAPED MODEL DESCRIPTION; ELSE ZER'
     1          ,'O.')
  301    FORMAT (' '
     1        ,/,'    THE S-SHAPED RELIABILITY GROWTH MODEL ASSUMPTIONS'
     2       ,//,'     1. THE SOFTWARE IS OPERATED IN A SIMILAR MANNER '
     3          ,'AS THE ANTICI-'
     4        ,/,'        PATED OPERATIONAL USAGE.'
     5       ,//,'     2. ALL FAULTS ARE EQUALLY LIKELY TO OCCUR  AND A'
     6          ,'RE INDEPENDENT'
     7        ,/,'        OF EACH OTHER.'
     8       ,//,'     3. EACH FAULT IS OF THE SAME ORDER OF "IMPACT" A'
     9          ,'S ANY OTHER.'
     A       ,//,'     4. THE  INITIAL  FAULT  CONTENT OF  THE  SOFTWAR'
     B          ,'E  SYSTEM IS A')
  302    FORMAT ('        RANDOM VARIABLE.'
     1       ,//,'     5. THE TIME BETWEEN FAILURES (K - 1) AND K DEPEN'
     2          ,'DS ON THE TIME'
     3        ,/,'        TO FAILURE (K - 1).'
     4       ,//,'     6. FAULTS ARE CORRECTED  IMMEDIATELY  WITHOUT  I'
     5          ,'NTRODUCING NEW'
     6        ,/,'        ONES.'
     7       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  303    FORMAT ('    MODEL DATA REQUIREMENTS'
     1       ,//,'     1. THE NUMBER OF FAULTS  DETECTED IN EACH INTERV'
     2          ,'AL OF TESTING.'
     3       ,//,'     2. THE LENGTHS OF THE VARIOUS TESTING INTERVALS.'
     4       ,//,'        "1 & 2 SHOULD HAVE BEEN ENTERED VIA THE DATA '
     5          ,'INPUT MODULE."'
     6       ,//,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     7          ,'HE APPROPRIATE'
     8        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     9          ,'EL;  OTHERWISE'
     A        ,/,'    ZERO TO RETURN TO THE MENU OF MODELS.')
  305    FORMAT (' '
     1        ,/,' **THE DATA ARE NOT APPROPRIATE FOR THE MODEL.')
  306    FORMAT (' '
     1        ,/,' **THE ESTIMATE  FOR THE TOTAL  NUMBER OF FAULTS  TO '
     2          ,'BE DETECTED IN'
     3        ,/,'   THE PROGRAM BEFORE  ALL FAULTS ARE UNCOVERED  CAME'
     4          ,' OUT LESS THAN'
     5        ,/,'   THE NUMBER OF FAULTS FOUND TO DATE WITHIN THE MODE'
     6          ,'L.')
  310    FORMAT (' '
     1        ,/,'   MODEL ESTIMATES ARE:'
     2        ,/,'   (THE APPROXIMATE 95% CONFIDENCE INTERVALS APPEAR I'
     3          ,'N PARENTHESES)'
     4        ,/,'    PROPORTIONALITY CONSTANT ',E12.5
     5          ,' (',E11.4,',',E11.4,')'
     6        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5
     7          ,' (',E11.4,',',E11.4,')'
     8        ,/,'    # OF FAULTS REMAINING    ',E12.5
     9          ,' (',E11.4,',',E11.4,')')
  311    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   (ROUNDED) NUMBER OF FAULTS REMAINING IS LESS THAN '
     4          ,'ONE.')
  315    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE NUMBER OF FAULTS EXPECTED IN THE NEXT TEST'
     3          ,'ING PERIOD'
     4        ,/,'    2) THE NUMBER OF PERIODS NEEDED TO DISCOVER THE N'
     5          ,'EXT M FAULTS'
     6        ,/,'    3) THE SOFTWARE RELIABILITY IN THE NEXT PERIOD'
     7        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     8          ,'S.')
  320    FORMAT ('    ENTER PROJECTED LENGTH OF THE PERIOD, OR ZERO TO '
     1          ,'END.')
  325    FORMAT ('    # OF FAULTS EXPECTED     ',E12.5,/)
  330    FORMAT ('    ENTER PROJECTED LENGTH OF THE PERIODS.')
  335    FORMAT ('    ENTER VALUE OF M (BETWEEN ONE AND',E12.5,'), OR Z'
     1          ,'ERO TO END.')
  340    FORMAT ('    # OF PERIODS EXPECTED    ',E12.5,/)
  341    FORMAT ('  **# OF PERIODS EXCEEDS     ',E12.5,' (PROGRAM MAXIM'
     1          ,'UM)'
     2        ,/,'    ENTER ONE TO CHANGE THE PROGRAM MAXIMUM; ELSE ZER'
     3          ,'O.')
  342    FORMAT ('    ENTER THE NEW VALUE FOR THE PROGRAM MAXIMUM.')
  343    FORMAT (' ')
  345    FORMAT ('    PROB. OF OPERATION WITH- '
     1        ,/,'     OUT FAILURE IN PERIOD   ',E12.5,/)
  350    FORMAT (' '
     1        ,/,'   ENTER ONE TO PERFORM AN ANALYSIS OF THE MODEL FIT '
     2          ,'USING THE PRE-'
     3        ,/,'   DICTIONS OF THIS MODEL; ELSE ZERO.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE VECTOR SIZE COUNTER.
         NS = PRC(3,1)
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,301)
             WRITE (HIST,301)
             WRITE (   *,302)
             WRITE (HIST,302)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,303)
             WRITE (HIST,303)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
C
         IF (CONFLG.EQ.1) THEN
C            STORE THE INTERVAL TESTING LENGTH DATA AND CONVERT THE DATA
C            TO A CUMULATIVE REPRESENTATION.
             IDXE = END1 + NS
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = STR2, IDXE)
             CALL SMFTRN
     G                   (NS       , DM       , DM       , 6
     B                   ,DAT(STR2)
     Y                   ,RFLAG    )
C
C            DETERMINE THE NUMBER OF FAULTS FOUND TO DATE  FOR THE ESTI-
C            MATE OF THE TOTAL NUMBER OF FAULTS REMAINING.
             CTOT = 0.0
             DO 1000 I = 1, NS
                 CTOT = CTOT + DAT(I)
 1000            CONTINUE
C            ENDDO
C
             CALL SESHMD
     G                   (DAT       , DAT(STR2) , NS
     Y                   ,STATS     , RFLAG     )
C
             IF (RFLAG.EQ.3) THEN
C                THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
                 WRITE (   *,305)
                 WRITE (HIST,305)
             ELSEIF (RFLAG.EQ.4) THEN
C                THE  TOTAL ERRORS ESTIMATE  WAS LESS THAN  THE OBSERVED
C                ERRORS TO DATE.
                 WRITE (   *,306)
                 WRITE (HIST,306)
             ELSE
C            ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY---------------
C                    A SOLUTION WAS FOUND;  COMPUTE THE EXTRA TOTAL NUM-
C                    BER OF FAULTS REMAINING.
                     DO 1025 I = 1, 3
                         TNOFR(I) = STATS(2,I) - CTOT
 1025                    CONTINUE
C                    ENDDO
                     IF (TNOFR(2).LT.0.0) THEN
                         TNOFR(2) = 0.0
                     ENDIF
C
C                    OUTPUT THE RESULTS.
                     WRITE (   *,310) (STATS(I,1), STATS(I,2)
     1                                ,STATS(I,3), I = 1, 2)
     2                                ,TNOFR(1)  , TNOFR(2)  , TNOFR(3)
                     WRITE (HIST,310) (STATS(I,1), STATS(I,2)
     1                                ,STATS(I,3), I = 1, 2)
     2                                ,TNOFR(1)  , TNOFR(2)  , TNOFR(3)
C
                     IF (TNOFR(1).LT.0.5) THEN
C                        FUTURE PREDICTIONS ARE NOT ALLOWED (TNOFR).
                         WRITE (   *,311)
                         WRITE (HIST,311)
                     ELSE
C                        DOWHILE (FLAG .NE. TERMINATION)
 1050                        WRITE (   *,315)
                             WRITE (HIST,315)
                             READ  (   *,  *) FLAG
                             WRITE (HIST,  *) FLAG
                             IF ((FLAG.LT.1) .OR. (FLAG.GT.3))GO TO 1250
C
                             IF     (FLAG.EQ.1) THEN
C                                FAULTS IN NEXT TESTING PERIOD.
                                 NPV = 1
C                                DOWHILE (EXL .NE. TERMINATION)
 1075                                WRITE (   *,320)
                                     WRITE (HIST,320)
                                     READ  (   *,  *) EXL
                                     WRITE (HIST,  *) EXL
                                     IF (EXL.EQ.0.0) GO TO 1100
                                     CALL SESHPR
     G                                           (NPV       , NS
     G                                           ,STATS(2,1), STATS(1,1)
     G                                           ,DAT(STR2) , EXL
     Y                                           ,XPRD      )
                                     IF (XPRD(1).GT.TNOFR(1)) THEN
                                         XPRD(1) = TNOFR(1)
                                     ENDIF
                                     WRITE (   *,325) XPRD(1)
                                     WRITE (HIST,325) XPRD(1)
                                     GO TO 1075
 1100                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.2) THEN
C                                PERIODS TO DETECT NEXT M FAULTS.
                                 WRITE (   *,330)
                                 WRITE (HIST,330)
                                 READ  (   *,  *) EXL
                                 WRITE (HIST,  *) EXL
                                 NPV = 1
C                                DOWHILE (DNOF .NE. TERMINATION)
 1125                                WRITE (   *,335) TNOFR(1)
                                     WRITE (HIST,335) TNOFR(1)
                                     READ  (   *,  *) DNOF
                                     WRITE (HIST,  *) DNOF
                                     IF (DNOF.EQ.0.0) GO TO 1175
                                     TOTL = 0.0
                                     CNTP = 0.0
C                                    DOUNTIL (PREDICTED .GE. DESIRED)
 1150                                    TOTL = TOTL + EXL
                                         CNTP = CNTP + 1.0
                                         CALL SESHPR
     G                                               (NPV        , NS
     G                                               ,STATS(2,1)
     G                                               ,STATS(1,1)
     G                                               ,DAT(STR2)  , TOTL
     Y                                               ,XPRD       )
                                         IF ((XPRD(1).LT.DNOF) .AND.
     1                                       (CNTP.LE.PMAX))  GO TO 1150
C                                    ENDUNTIL
                                     IF (CNTP.LE.PMAX) THEN
                                         WRITE (   *,340) CNTP
                                         WRITE (HIST,340) CNTP
                                     ELSE
                                         WRITE (   *,341) PMAX
                                         WRITE (HIST,341) PMAX
                                         READ  (   *,  *) FLAG2
                                         WRITE (HIST,  *) FLAG2
                                         IF (FLAG2.EQ.1) THEN
                                             WRITE (   *,342)
                                             WRITE (HIST,342)
                                             READ  (   *,  *) PMAX
                                             WRITE (HIST,  *) PMAX
                                         ENDIF
                                         WRITE (   *,343)
                                         WRITE (HIST,343)
                                     ENDIF
                                     GO TO 1125
 1175                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.3) THEN
C                                RELIABILITY IN THE NEXT PERIOD.
                                 NPV = 2
C                                DOWHILE (FLAG .EQ. 1)
 1200                                WRITE (   *,320)
                                     WRITE (HIST,320)
                                     READ  (   *,  *) EXL
                                     WRITE (HIST,  *) EXL
                                     IF (EXL.EQ.0.0) GO TO 1225
                                     CALL SESHPR
     G                                           (NPV        , NS
     G                                           ,STATS(2,1)
     G                                           ,STATS(1,1)
     G                                           ,DAT(STR2)  , EXL
     Y                                           ,XPRD       )
                                     WRITE (   *,345) XPRD(1)
                                     WRITE (HIST,345) XPRD(1)
                                     GO TO 1200
 1225                                CONTINUE
C                                ENDWHILE
                             ENDIF
                             GO TO 1050
 1250                        CONTINUE
C                        ENDWHILE
                     ENDIF
C            ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY---------------
             ENDIF
C
             FLAG = 0
             IF (RFLAG.EQ.0) THEN
C                THE FINAL EXECUTION WAS SUCCESSFUL, PROMPT USER AND SEE
C                IF THE PREDICTED VECTOR CREATION IS DESIRED.
                 WRITE (   *,350)
                 WRITE (HIST,350)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    ACCESS THE SESHPR ROUTINE TO GENERATE THE PREDICTED
C                    FAULT COUNT DATA VECTOR.
                     CALL SESHPR
     G                           (NS         , NS        , STATS(2,1)
     G                           ,STATS(1,1) , DAT(STR2) , EXL
     Y                           ,PRD        )
C
C                    SET THE ESTIMATION SELECTION CODE TO INDICATE WHICH
C                    MODEL WAS RESPONSIBLE FOR  THE PREDICTIONS; AND SET
C                    ESD TO INDICATE THAT THE INTERVAL DATA TYPE WAS RE-
C                    SPONSIBLE.
                     ESC = 11
                     ESD = 3
C
C                    SET THE GOODNESS-OF-FIT ESTIMATION CONSTANT.
                     EST = 2
C
C                    RESTORE TESTING LENGTH DATA AND PERFORM ANALYSIS OF
C                    MODEL FIT.
                     REWIND TMPB
                     READ  (TMPB) (DAT(I), I = STR2, IDXE)
                     CALL FITDRV
                 ENDIF
             ENDIF
C
             IF (FLAG.EQ.0) THEN
C                TESTING LENGTH DATA HAS NOT BEEN RESTORED, YET.
                 REWIND TMPB
                 READ  (TMPB) (DAT(I), I = STR2, IDXE)
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE MDLGEO
     G                     (IDX   , NS    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLGEO
C    G                     (IDX   , NS    )
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
C        TO PROVIDE THE INTERACTIVE PROMPTS AND INPUTS FOR THE GEOMETRIC
C        MODEL. ADDITIONALLY, THE ROUTINE WILL PERFORM THE ACCESS TO THE
C        SMFLIB ROUTINES NEEDED TO PERFORM THE MODEL EXECUTIONS AND PRE-
C        DICTED DATA VECTOR CREATION.
C   DESCRIPTION
C        TWO SMFLIB ROUTINES (SGEOMD AND SGEOPR) WILL BE USED WITHIN THE
C        ROUTINE.
C
C        THE FIRST ROUTINE (SGEOMD) PROVIDES THE CALCULATIONS OF THE RE-
C        LIABILITY VIA THE  MAXIMUM LIKELIHOOD AND LEAST SQUARES METHODS
C        OF EXECUTION.  THE FIRST ARGUMENT OF THE  SGEOMD CALL LINE PRO-
C        VIDES THE ROUTINE WITH THE NUMBER OF THE METHOD DESIRED.
C
C        ONCE THE  EXECUTION OF  SGEOMD IS PERFORMED,  THE RETURN STATUS
C        FLAG (RFLAG) WILL INDICATE THE SUCCESSFULNESS  OF THE MODEL ES-
C        TIMATION PROCESS. THE FOLLOWING VALUES ARE POSSIBLE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C
C        FOR RFLAG VALUE OF 0 THE MODEL ESTIMATES WILL BE LISTED.
C
C        THE SECOND ROUTINE (SGEOPR) GENERATES THE  VECTOR OF  PREDICTED
C        TBF DATA.
C
C        THE SGEOPR ROUTINE WILL ALSO DETERMINE THE GOODNESS-OF-FIT WHEN
C        CREATING THE PREDICTED VECTOR; ALTHOUGH THE VALUES FOR THE KOL-
C        MOGOROV DISTANCE STATISTIC AND THE DETERMINED SIGNIFICANCE WILL
C        NOT BE OUTPUT UNTIL THE USER ENTERS THE ANALYSIS DRIVER.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C        YIELDED
C             D      (R) = CALCULATED KOLMOGOROV DISTANCE
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          GOODNESS-OF-FIT CALCULATION
C   ARGUMENT LIST
C        GIVEN
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C   LOCAL GLOSSARY
C             CIF    (R) = CURRENT INTENSITY FUNCTION
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             FLAG   (I) = OPTION SELECTION FLAG
C             IIF    (R) = INITIAL INTENSITY FUNCTION
C             LB     (C) = OUTPUT MODEL LABELS
C             PREDAT (R) = PREDICTION DATA VALUES ARRAY
C             PRINI  (I) = PREDICTION INPUT VALUE FOR PR2
C             PRINR  (R) = PREDICTION INPUT VALUE FOR PR1, PR3 - PR5
C             PR1    (R) = FUTURE RELIABILTIY FOR SPECIFIED TIME (N/A)
C             PR2    (R) = MTBNF TO LOCATE NEXT K FAILURES
C             PR3    (R) = ADDITIONAL TESTING TIME TO REACH DIF
C             PR4    (R) = ADDITIONAL FAILURES FOUND DURING PR3
C             PR5    (R) = FUTURE FAILURES WITHIN A SPECIFIED TIME
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = STATISTICS RESULTS ARRAY
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
C               /PRECOM/      PREDICTION DATA STORAGE COMMON
         COMMON /PRECOM/      PRD(END1)     ,XPRD(3)       ,D
     1                       ,DFLG          ,ESC           ,ESD
     2                       ,EST
         INTEGER              DFLG          ,ESC           ,ESD
     1                       ,EST
C
C               /TMPCOM/      TEMPORARY STORAGE COMMON
         COMMON /TMPCOM/      TM1C(END1)    ,TM2C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              IDX           ,NS
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CONFLG        ,ESF           ,FLAG
     1                       ,I             ,PRINI         ,RFLAG
         DIMENSION            PREDAT(2,3)   ,STATS(4,3)
         CHARACTER* 2         LB(2)
C
C                             LOCAL ASSIGNMENTS
         DATA                 LB(1) /'ML'/      ,    LB(2) /'LS'/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR GEOMETRIC MODEL DESCRIPTION; ELSE ZE'
     1          ,'RO.')
  301    FORMAT (' '
     1        ,/,'    THE GEOMETRIC MODEL ASSUMPTIONS'
     2       ,//,'     1. THE SOFTWARE IS OPERATED IN A SIMILAR MANNER '
     3          ,'AS THE ANTICI-'
     4        ,/,'        PATED OPERATIONAL USAGE.'
     5       ,//,'     2. ALL FAULTS ARE EQUALLY LIKELY TO OCCUR  AND A'
     6          ,'RE INDEPENDENT'
     7        ,/,'        OF EACH OTHER.'
     8       ,//,'     3. THE FAULT DETECTION RATE FORMS A GEOMETRIC  P'
     9          ,'ROGRESSION AND'
     A        ,/,'        IS CONSTANT BETWEEN FAULT OCCURRENCES.'
     B       ,//,'     4. THERE IS AN INFINITE NUMBER OF TOTAL FAULTS ('
     C          ,'I.E., THE PRO-')
  302    FORMAT ('        GRAM WILL NEVER BE FAULT-FREE).'
     1       ,//,'    MODEL DATA REQUIREMENTS'
     2       ,//,'     1. THE TIME-BETWEEN-FAILURES AS MEASURED IN WALL'
     3          ,' CLOCK OR COM-'
     4        ,/,'        PUTER CPU TIME.  THE DATA SHOULD HAVE  BEEN E'
     5          ,'NTERED VIA THE'
     6        ,/,'        INPUT MODULE.'
     7       ,//,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     8          ,'HE APPROPRIATE'
     9        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     A          ,'EL;  OTHERWISE'
     B        ,/,'    ZERO TO RETURN TO THE MENU OF MODELS.')
  305    FORMAT (' '
     1        ,/,'   ENTER ONE FOR  MAXIMUM LIKELIHOOD METHOD,  TWO FOR'
     2          ,' LEAST SQUARES'
     3        ,/,'   METHOD, OR THREE TO TERMINATE MODEL EXECUTION.')
  310    FORMAT (' '
     1        ,/,' **THE DATA ARE NOT APPROPRIATE FOR THE ',A2,' MODEL.'
     2          )
  315    FORMAT (' '
     1        ,/,3X,A2,' MODEL ESTIMATES ARE:')
  320    FORMAT ('   (THE APPROXIMATE 95% CONFIDENCE INTERVALS APPEAR I'
     1          ,'N PARENTHESES)'
     2        ,/,'    PROPORTIONALITY CONSTANT ',E12.5
     3          ,' (',E11.4,',',E11.4,')'
     4        ,/,'    HAZARD RATE PARAMETER    ',E12.5
     5          ,' (',E11.4,',',E11.4,')'
     6        ,/,'    INIT. INTENSITY FUNCTION ',E12.5
     7        ,/,'    CUR.  INTENSITY FUNCTION ',E12.5
     8        ,/,'    MTBNF                    ',E12.5
     9          ,' (',E11.4,',',E11.4,')'
     A        ,/,'    PURIFICATION LEVEL       ',E12.5
     B          ,' (',E11.4,',',E11.4,')')
  325    FORMAT ('    PROPORTIONALITY CONSTANT ',E12.5
     1        ,/,'    HAZARD RATE PARAMETER    ',E12.5
     2        ,/,'    INIT. INTENSITY FUNCTION ',E12.5
     3        ,/,'    CUR.  INTENSITY FUNCTION ',E12.5
     4        ,/,'    MTBNF                    ',E12.5
     5        ,/,'    PURIFICATION LEVEL       ',E12.5)
  326    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   MTBNF IS LESS THAN ZERO.')
  330    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE TOTAL MTBNF FOR THE NEXT K FAILURES'
     3        ,/,'    2) THE TIME AND NUMBER OF FAILURES TO REACH A DES'
     4          ,'IRED INTENSITY'
     5        ,/,'       FUNCTION'
     6        ,/,'    3) THE NUMBER OF FAILURES EXPECTED IN A SPECIFIED'
     7          ,' TIME'
     8        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     9          ,'S.')
  335    FORMAT ('    ENTER VALUE OF K, OR ZERO TO END.')
  340    FORMAT ('    MTBNF EXPECTED          ',E12.5,/)
  345    FORMAT ('    ENTER INTENSITY FUNC. (LESS THAN',E12.5,'), OR ZE'
     1          ,'RO TO END.')
  350    FORMAT ('    ADDITIONAL TESTING TIME ',E12.5
     1        ,/,'    # OF FAILURES EXPECTED  ',E12.5,/)
  355    FORMAT ('    ENTER LENGTH OF THE PERIOD, OR ZERO TO END.')
  360    FORMAT ('    # OF FAILURES EXPECTED  ',E12.5,/)
  365    FORMAT (' '
     1        ,/,'   ENTER ONE TO PERFORM AN ANALYSIS OF THE MODEL FIT '
     2          ,'USING THE PRE-'
     3        ,/,'   DICTIONS OF THIS MODEL; ELSE ZERO.')
  370    FORMAT ('   ENTER ONE FOR PREDICTIONS BASED ON MAXIMUM LIKELIH'
     1          ,'OOD METHOD, OR'
     2        ,/,'   TWO FOR LEAST SQUARES METHOD.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        ZERO THE SUCCESSFULNESS  ELEMENTS OF THE PREDICTION DATA ARRAY.
         PREDAT(1,1) = 0.0
         PREDAT(2,1) = 0.0
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,301)
             WRITE (HIST,301)
             WRITE (   *,302)
             WRITE (HIST,302)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
C
         IF (CONFLG.EQ.1) THEN
C            DOWHILE (ESF .NE. TERMINATION)
 1000            WRITE (   *,305)
                 WRITE (HIST,305)
                 READ  (   *,  *) ESF
                 WRITE (HIST,  *) ESF
                 IF ((ESF.GE.1) .AND. (ESF.LE.2)) THEN
                 PREDAT(ESF,1) = 0.0
C
                 CALL SGEOMD
     G                       (ESF   , NS    ,  DAT(IDX)
     Y                       ,STATS , RFLAG )
C
                 IF (RFLAG.EQ.3) THEN
C                    THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
                     WRITE (   *,310) LB(ESF)
                     WRITE (HIST,310) LB(ESF)
                 ELSE
C                    A SOLUTION WAS FOUND; COMPUTE THE EXTRA INITIAL AND
C                    CURRENT INTENSITY FUNCTIONS.
                     SUMTBF = 0.0
                     DO 1025 I = 0, (NS-1)
                         SUMTBF = SUMTBF + DAT(IDX+I)
 1025                    CONTINUE
C                    ENDDO
                     IIF = STATS(2,1) / STATS(1,1)
                     CIF = STATS(2,1)
     1                   / ((-STATS(2,1) * LOG(STATS(1,1))) * SUMTBF
     2                   + STATS(1,1))
C
C                    OUTPUT THE RESULTS.
                     WRITE (   *,315) LB(ESF)
                     WRITE (HIST,315) LB(ESF)
                     IF (ESF.EQ.1) THEN
                         WRITE (   *,320) (STATS(1,I) , I = 1, 3)
     1                                  , (STATS(2,I) , I = 1, 3)
     2                                  ,  IIF        , CIF
     3                                  , (STATS(3,I) , I = 1, 3)
     4                                  , (STATS(4,I) , I = 1, 3)
                         WRITE (HIST,320) (STATS(1,I) , I = 1, 3)
     1                                  , (STATS(2,I) , I = 1, 3)
     2                                  ,  IIF        , CIF
     3                                  , (STATS(3,I) , I = 1, 3)
     4                                  , (STATS(4,I) , I = 1, 3)
                     ELSE
                         WRITE (   *,325)  STATS(1,1) ,  STATS(2,1)
     1                                  ,  IIF        ,  CIF
     2                                  ,  STATS(3,1) ,  STATS(4,1)
                         WRITE (HIST,325)  STATS(1,1) ,  STATS(2,1)
     1                                  ,  IIF        ,  CIF
     2                                  ,  STATS(3,1) ,  STATS(4,1)
                     ENDIF
C
C                    SET THE SUCCESSFULNESS FLAG,  AND STORE THE PHI AND
C                    D ESTIMATES.
                     PREDAT(ESF,1) = 1.0
                     PREDAT(ESF,3) = STATS(1,1)
                     PREDAT(ESF,2) = STATS(2,1)
C
                     IF (STATS(3,1).LT.0.0) THEN
C                        FUTURE PREDICTIONS ARE NOT ALLOWED (MTBNF).
                         WRITE (   *,326)
                         WRITE (HIST,326)
                     ELSE
C                        DOWHILE (FLAG .NE. TERMINATION)
 1050                        WRITE (   *,330)
                             WRITE (HIST,330)
                             READ  (   *,  *) FLAG
                             WRITE (HIST,  *) FLAG
                             IF ((FLAG.LT.1) .OR. (FLAG.GT.3))GO TO 1250
C
                             IF     (FLAG.EQ.1) THEN
C                                MTBNF TO LOCATE K FAILURES.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1075                                WRITE (   *,335)
                                     WRITE (HIST,335)
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1125
                                     PRINI = INT(PRINR + 0.5)
                                     PR2 = 0.0
                                     DO 1100 I = 0, (PRINI-1)
                                         PR2 = PR2 + (1.0 / (STATS(2,1)
     1                                       * STATS(1,1)**(NS+I)))
 1100                                    CONTINUE
C                                    ENDDO
                                     WRITE (   *,340) PR2
                                     WRITE (HIST,340) PR2
                                     GO TO 1075
 1125                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.2) THEN
C                                TIME AND FAILURES  TO REACH  A  DESIRED
C                                INTENSITY FUNCTION.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1150                                WRITE (   *,345) CIF
                                     WRITE (HIST,345) CIF
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1175
                                     PR3 = (-1.0 / LOG(STATS(1,1)))
     1                                   * (1.0 / PRINR - 1.0 / CIF)
                                     PR4 = (-1.0 / LOG(STATS(1,1)))
     1                                   * (LOG(CIF / PRINR))
                                     WRITE (   *,350) PR3, PR4
                                     WRITE (HIST,350) PR3, PR4
                                     GO TO 1150
 1175                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.3) THEN
C                                FAILURES FOUND IN A SPECIFIED TIME.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1200                                WRITE (   *,355)
                                     WRITE (HIST,355)
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1225
                                     PR5 = (-1.0 / LOG(STATS(1,1))
     1                                   * (LOG(((-STATS(2,1)
     2                                   / STATS(1,1))
     3                                   * LOG(STATS(1,1)))
     4                                   * (SUMTBF+PRINR) + 1.0)))
     5                                   - (-1.0 / LOG(STATS(1,1))
     6                                   * (LOG(((-STATS(2,1)
     7                                   / STATS(1,1))
     8                                   * LOG(STATS(1,1)))
     9                                   * (SUMTBF      )  + 1.0)))
                                     WRITE (   *,360) PR5
                                     WRITE (HIST,360) PR5
                                     GO TO 1200
 1225                                CONTINUE
C                                ENDWHILE
                             ENDIF
                             GO TO 1050
 1250                        CONTINUE
C                        ENDWHILE
                     ENDIF
                 ENDIF
                 GO TO 1000
                 ENDIF
C            ENDWHILE
C
             IF ((PREDAT(1,1).EQ.1.0) .OR. (PREDAT(2,1).EQ.1.0)) THEN
C                AT LEAST ONE EXECUTION  WAS SUCCESSFUL, PROMPT USER AND
C                SEE IF THE PREDICTED VECTOR CREATION IS DESIRED.
                 WRITE (   *,365)
                 WRITE (HIST,365)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    THE VECTOR OF PREDICTED DATA IS DESIRED.
                     IF (PREDAT(1,1).NE.PREDAT(2,1)) THEN
C                        ONLY ONE EXECUTION  WAS SUCCESSFUL;  SET ESF TO
C                        INDICATE ML OR LS.
                         ESF = PREDAT(1,1) * 1.0 + PREDAT(2,1) * 2.0
                     ELSE
C                        BOTH EXECUTIONS WERE  PERFORMED AND SUCCESSFUL;
C                        SEE WHICH THE USER DESIRES.
                         WRITE (   *,370)
                         WRITE (HIST,370)
                         READ  (   *,  *) ESF
                         WRITE (HIST,  *) ESF
                     ENDIF
C
C                    ACCESS THE SGEOPR ROUTINE TO GENERATE THE PREDICTED
C                    TBF DATA VECTOR.
                     CALL SGEOPR
     G                           (PREDAT(ESF,2) , PREDAT(ESF,3) , NS
     G                           ,DAT(IDX)
     Y                           ,PRD           , D             , DFLG
     Y                           ,TM1C          )
C
C                    SET THE ESTIMATION SELECTION CODE TO INDICATE WHICH
C                    MODEL WAS RESPONSIBLE FOR  THE PREDICTIONS; AND SET
C                    ESD TO INDICATE WHICH DATA TYPE (WC OR CPU) WAS RE-
C                    SPONSIBLE.
                     ESC = 3
                     ESD = 1
                     IF (IDX.EQ.STR2) THEN
                         ESD = 2
                     ENDIF
C
C                    PERFORM ANALYSIS OF MODEL FIT.
                     CALL FITDRV
                 ENDIF
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE MDLGPO
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLGPO
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
C        TO PROVIDE THE INTERACTIVE PROMPTS  AND INPUTS FOR THE GENERAL-
C        IZED POISSON MODEL.  ADDITIONALLY, THE ROUTINE WILL PERFORM THE
C        ACCESS TO THE SMFLIB ROUTINES  NEEDED TO PERFORM THE MODEL EXE-
C        CUTIONS AND PREDICTED DATA VECTOR CREATION.
C   DESCRIPTION
C        A DRIVER ROUTINE  (EXVGPO) AND TWO  SMFLIB ROUTINES (SGPOMD AND
C        SGPOPR) WILL BE USED WITHIN THIS ROUTINE.
C
C        THE FIRST ROUTINE  (EXVGPO) CREATES  THE EXTRA DATA  VECTOR AND
C        VARIABLE REQUIRED  IN THE MODEL AND  PREDICTION  ROUTINES.  THE
C        VECTOR (TM1C)  CONTAINS THE  CUMULATIVE NUMBER OF FAULT CORREC-
C        TIONS PERFORMED AT THE END OF EACH TESTING PERIOD; AND THE VAR-
C        IABLE (LCOR) CONTAINS THE NUMBER  OF FAULTS CORRECTED AFTER THE
C        LAST TESTING PERIOD.
C
C        THE SECOND ROUTINE  (SGPOMD)  PROVIDES THE  CALCULATIONS OF THE
C        RELIABILITY VIA THE  MAXIMUM LIKELIHOOD AND LEAST SQUARES METH-
C        ODS OF EXECUTION.  THE FIRST  ARGUMENT OF THE  SGPOMD CALL LINE
C        PROVIDES THE ROUTINE WITH THE NUMBER OF THE METHOD DESIRED.
C
C        SGPOMD ALSO SUPPLIES  UP TO 3 DIFFERENT  WEIGHTING FUNCTIONS TO
C        BE APPLIED TO THE  VECTOR OF INTERVAL LENGTHS.  UNDER THE LEAST
C        SQUARES METHOD THE LENGTH MAY BE SCALED BY THE SCHICK-WOLVERTON
C        OR ALPHA INPUT VALUE. THE MAXIMUM LIKELIHOOD METHOD HAS THE AD-
C        DITIONAL WEIGHTING FUNCTION OF ALPHA ESTIMATED WHEN THE TESTING
C        LENGTH DATA VECTOR IS OF VARIABLE LENGTHS.  THE SECOND ARGUMENT
C        OF THE  SGPOMD CALL LINE  PROVIDES THE  ROUTINE WITH THE NUMBER
C        OF THE WEIGHTING FUNCTION DESIRED.
C
C        ONCE THE  EXECUTION OF  SGPOMD IS PERFORMED,  THE RETURN STATUS
C        FLAG (RFLAG) WILL INDICATE THE SUCCESSFULNESS  OF THE MODEL ES-
C        TIMATION PROCESS. THE FOLLOWING VALUES ARE POSSIBLE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             1 - MAXIMUM ITERATIONS
C             2 - THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS THAN THE  OBSERVED
C                 FAULTS TO DATE
C
C        FOR RFLAG VALUES OF 0 AND 1 THE MODEL ESTIMATES WILL BE LISTED.
C
C        THE THIRD ROUTINE (SGPOPR) PROVIDES FUTURE PREDICTIONS AND GEN-
C        ERATES THE VECTOR OF PREDICTED FAULT COUNTS.
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
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C                          (BUT RETURNED TO ORIGINAL VALUES)
C             PMAX   (R) = MAXIMUM NUMBER OF ITERATIONS FOR PREDICTIONS
C        YIELDED
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             EST    (I) = NUMBER OF ESTIMATIONS FROM THE MODEL
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1
C             XPRD   (R) = FUTURE PREDICTIONS (NUMBER AND 95% C.I.)
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             AFLAG  (I) = TYPE OF PARAMETER ESTIMATION PROBLEM FLAG
C             ALPHA  (R) = INPUT OR INIT. EST. OF ALPHA
C             AN     (R) = INIT. EST. OF TOTAL NUMBER OF FAULTS
C             CNTP   (R) = COUNT OF PERIODS TO FIND M FAULTS
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             COUNT  (I) = ITERATION COUNTER
C             DNOF   (R) = DESIRED NUMBER OF FAULTS (M)
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             EXL    (R) = EXPECTED TESTING LENGTH OF NEXT PERIOD
C             FLAG   (I) = OPTION SELECTION FLAG
C             FLAG2  (I) = PROGRAM MAXIMUM OVER-RIDE FLAG
C             IDXE   (I) = ENDING INDEX OF THE DATA
C             LB     (C) = OUTPUT MODEL LABELS
C             LCOR   (R) = FAULT CORRECTIONS AFTER LAST PERIOD
C             MAXIC  (I) = USER-SPECIFIED MAX. NUMBER OF ITERATIONS
C             NPV    (I) = NUMBER OF PREDICTED VALUES FLAG
C             NS     (I) = NUMBER OF ENTRIES IN DATA VECTORS
C             PREDAT (R) = PREDICTED DATA VALUES ARRAY
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = STATISTICS RESULTS ARRAY
C             TOTL   (R) = TOTAL EXPECTED LENGTHS FOR PERIOD PREDICTION
C             WFSF   (I) = WEIGHTING FUNCTION SELECTION FLAG
C             WT     (I) = FLAG TO PAUSE THE ASSUMPTIONS LIST OUTPUT
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
C               /PMXCOM/      PREDICTION MAXIMUM ITERATION COMMON
         COMMON /PMXCOM/      PMAX
C
C               /PRECOM/      PREDICTION DATA STORAGE COMMON
         COMMON /PRECOM/      PRD(END1)     ,XPRD(3)       ,D
     1                       ,DFLG          ,ESC           ,ESD
     2                       ,EST
         INTEGER              DFLG          ,ESC           ,ESD
     1                       ,EST
C
C               /TMPCOM/      TEMPORARY STORAGE COMMON
         COMMON /TMPCOM/      TM1C(END1)    ,TM2C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              AFLAG         ,CONFLG        ,COUNT
     1                       ,ESF           ,FLAG          ,FLAG2
     2                       ,I             ,ICOUNT        ,IDXE
     3                       ,MAXIC         ,NPV           ,NS
     4                       ,RFLAG         ,WFSF          ,WT
         DIMENSION            PREDAT(2,5)   ,STATS(4,3)
         CHARACTER* 2         LB(2)
C
C                             LOCAL ASSIGNMENTS
         DATA                 LB(1) /'ML'/      ,   LB(2) /'LS'/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR GENERALIZED POISSON MODEL DESCRIPTIO'
     1          ,'N; ELSE ZERO.')
  301    FORMAT (' '
     1        ,/,'    THE GENERALIZED POISSON MODEL ASSUMPTIONS'
     2       ,//,'     1. THE SOFTWARE IS OPERATED IN A SIMILAR MANNER '
     3          ,'AS THE ANTICI-'
     4        ,/,'        PATED OPERATIONAL USAGE.'
     5       ,//,'     2. ALL FAULTS ARE EQUALLY LIKELY TO OCCUR  AND A'
     6          ,'RE INDEPENDENT'
     7        ,/,'        OF EACH OTHER.'
     8       ,//,'     3. EACH FAULT IS OF THE SAME ORDER OF "IMPACT" A'
     9          ,'S ANY OTHER.'
     A       ,//,'     4. THE EXPECTED NUMBER OF FAULTS  OCCURRING IN A'
     B          ,'NY TIME INTER-')
  302    FORMAT ('        VAL IS  PROPORTIONAL TO THE  FAULT CONTENT  A'
     1          ,'T THE  TIME OF'
     2        ,/,'        TESTING, AND TO SOME  FUNCTION OF THE  AMOUNT'
     3          ,' OF TIME SPENT'
     4        ,/,'        IN TESTING.'
     5       ,//,'     5. FAULTS ARE CORRECTED  AT THE ENDS OF THE TEST'
     6          ,'ING INTERVALS,'
     7        ,/,'        WITHOUT INTRODUCING NEW ONES.'
     8       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  303    FORMAT ('    MODEL DATA REQUIREMENTS'
     1       ,//,'     1. THE NUMBER OF FAULTS  DETECTED IN EACH INTERV'
     2          ,'AL OF TESTING.'
     3       ,//,'     2. THE LENGTHS OF THE VARIOUS TESTING INTERVALS.'
     4       ,//,'        "1 & 2 SHOULD HAVE BEEN ENTERED VIA THE DATA '
     5          ,'INPUT MODULE."'
     6       ,//,'     3. THE NUMBER OF FAULTS  CORRECTED AT THE  END O'
     7          ,'F EACH TESTING'
     8        ,/,'        PERIOD.'
     9       ,//,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     A          ,'HE APPROPRIATE'
     B        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     C          ,'EL;  OTHERWISE'
     D        ,/,'    ZERO TO RETURN TO THE MENU OF MODELS.')
  305    FORMAT (' '
     1        ,/,'   ENTER ONE IF ANY FAULTS DETECTED  IN A GIVEN TESTI'
     2          ,'NG PERIOD WERE'
     3        ,/,'   CORRECTED IN A LATER PERIOD; ELSE ZERO.')
  310    FORMAT (' '
     1        ,/,'   ENTER ONE FOR  MAXIMUM LIKELIHOOD METHOD,  TWO FOR'
     2          ,' LEAST SQUARES'
     3        ,/,'   METHOD, OR THREE TO TERMINATE MODEL EXECUTION.')
  315    FORMAT (' '
     1        ,/,'   ENTER WEIGHTING FUNCTION NUMBER, OR ZERO FOR A LIS'
     2          ,'T.')
  320    FORMAT ('   THE AVAILABLE WEIGHTING FUNCTIONS ARE:'
     1        ,/,'    1 X(I) ** 2 / 2 (SCHICK-WOLVERTON MODEL)'
     2        ,/,'    2 X(I) ** ALPHA (WHERE ALPHA IS INPUT)')
  325    FORMAT ('    3 X(I) ** ALPHA (WHERE ALPHA IS ESTIMATED)')
  330    FORMAT ('    * ALPHA CANNOT BE ESTIMATED IF THE INTERVAL LENGT'
     1          ,'HS ARE FIXED.')
  335    FORMAT ('   ENTER WEIGHTING FUNCTION NUMBER.')
  340    FORMAT ('   ENTER DESIRED ALPHA.')
  345    FORMAT (' '
     1        ,/,'   THE INITIAL ESTIMATES TO BE USED IN THE ESTIMATION'
     2          ,' PROCESS ARE:'
     3        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5
     4        ,/,'    ALPHA                    ',E12.5
     5       ,//,'   ENTER ONE TO USE DIFFERENT INITIAL ESTIMATES, OR Z'
     6          ,'ERO TO USE THE'
     7        ,/,'   INITIAL ESTIMATES.')
  350    FORMAT (' '
     1        ,/,'    ENTER INITIAL ESTIMATE FOR THE TOTAL NUMBER OF FA'
     2          ,'ULTS'
     3        ,/,'    (BETWEEN',E12.5,' AND 0.10000E+08).')
  355    FORMAT ('    ENTER INITIAL ESTIMATE FOR ALPHA'
     1        ,/,'    (BETWEEN 0.00000E+00 AND 0.10000E+08).')
  360    FORMAT ('   ENTER MAXIMUM NUMBER OF ITERATIONS.')
  365    FORMAT (' '
     1        ,/,' **THE MAXIMUM NUMBER OF ITERATIONS WAS REACHED  WITH'
     2          ,' THE FOLLOWING'
     3        ,/,'   FINAL ',A2,' MODEL ESTIMATES, USING THE WEIGHTING '
     4          ,'FUNCTION TYPE',I2,':')
  366    FORMAT ('   (THE APPROXIMATE 95% CONFIDENCE INTERVALS APPEAR I'
     1          ,'N PARENTHESES)')
  370    FORMAT ('    PROPORTIONALITY CONSTANT ',E12.5
     1          ,' (',E11.4,',',E11.4,')'
     2        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5
     3          ,' (',E11.4,',',E11.4,')'
     4        ,/,'    # OF FAULTS REMAINING    ',E12.5
     5          ,' (',E11.4,',',E11.4,')')
  375    FORMAT ('    PROPORTIONALITY CONSTANT ',E12.5
     1        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5
     2        ,/,'    # OF FAULTS REMAINING    ',E12.5)
  380    FORMAT ('    AND ALPHA                ',E12.5)
  385    FORMAT (' '
     1        ,/,' **THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY FO'
     2          ,'R THE ',A2,' MODEL'
     3        ,/,'   USING THE WEIGHTING FUNCTION TYPE',I2,'.')
  390    FORMAT (' '
     1        ,/,' **THE DATA ARE NOT APPROPRIATE  FOR THE ',A2,' MODEL'
     2          ,', USING THE WEIGH-'
     3        ,/,'   TING FUNCTION TYPE',I2,'.')
  391    FORMAT (' '
     1        ,/,' **THE ESTIMATE  FOR THE TOTAL  NUMBER OF FAULTS  TO '
     2          ,'BE DETECTED IN'
     3        ,/,'   THE PROGRAM BEFORE  ALL FAULTS ARE UNCOVERED  CAME'
     4          ,' OUT LESS THAN'
     5        ,/,'   THE NUMBER OF FAULTS FOUND  TO DATE WITHIN  THE '
     6          ,A2,' MODEL,  USING'
     7        ,/,'   THE WEIGHTING FUNCTION TYPE',I2,'.')
  395    FORMAT (' '
     1        ,/,3X,A2,' MODEL ESTIMATES,  USING THE WEIGHTING FUNCTION'
     2          ,' TYPE',I2,',  AFTER'
     3        ,/,I6,' ITERATIONS ARE:')
  400    FORMAT (' '
     1        ,/,3X,A2,' MODEL ESTIMATES, USING THE WEIGHTING FUNCTION '
     2          ,'TYPE',I2,' ARE:')
  401    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   (ROUNDED) NUMBER OF FAULTS REMAINING IS LESS THAN '
     4          ,'ONE.')
  405    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE NUMBER OF FAULTS EXPECTED IN THE NEXT TEST'
     3          ,'ING PERIOD'
     4        ,/,'    2) THE NUMBER OF PERIODS NEEDED TO DISCOVER THE N'
     5          ,'EXT M FAULTS'
     6        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     7          ,'S.')
  410    FORMAT ('    ENTER PROJECTED LENGTH OF THE PERIOD, OR ZERO TO '
     1          ,'END.')
  415    FORMAT ('    # OF FAULTS EXPECTED     ',E12.5,/)
  420    FORMAT ('    # OF FAULTS EXPECTED     ',E12.5
     1          ,' (',E11.4,',',E11.4,')',/)
  425    FORMAT ('    ENTER PROJECTED LENGTH OF THE PERIODS.')
  430    FORMAT ('    ENTER VALUE OF M (BETWEEN ONE AND',E12.5,'), OR Z'
     1          ,'ERO TO END.')
  435    FORMAT ('    # OF PERIODS EXPECTED    ',E12.5,/)
  436    FORMAT ('  **# OF PERIODS EXCEEDS     ',E12.5,' (PROGRAM MAXIM'
     1          ,'UM)'
     2        ,/,'    ENTER ONE TO CHANGE THE PROGRAM MAXIMUM; ELSE ZER'
     3          ,'O.')
  437    FORMAT ('    ENTER THE NEW VALUE FOR THE PROGRAM MAXIMUM.')
  438    FORMAT (' ')
  440    FORMAT (' '
     1        ,/,'   ENTER ONE TO PERFORM AN ANALYSIS OF THE MODEL FIT '
     2          ,'USING THE PRE-'
     3        ,/,'   DICTIONS OF THIS MODEL; ELSE ZERO.')
  445    FORMAT ('   ENTER ONE FOR PREDICTIONS BASED ON MAXIMUM LIKELIH'
     1          ,'OOD METHOD, OR'
     2        ,/,'   TWO FOR LEAST SQUARES METHOD.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE VECTOR SIZE COUNTER.
         NS = PRC(3,1)
C
C        ZERO THE SUCCESSFULNESS  ELEMENTS OF THE PREDICTION DATA ARRAY.
         PREDAT(1,1) = 0.0
         PREDAT(2,1) = 0.0
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,301)
             WRITE (HIST,301)
             WRITE (   *,302)
             WRITE (HIST,302)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,303)
             WRITE (HIST,303)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
C
         IF (CONFLG.EQ.1) THEN
C            THE MODEL EXECUTION  IS DESIRED,  START PROCESSING WITH THE
C            STORAGE OF THE INTERVAL TESTING LENGTH DATA.
             IDXE = END1 + NS
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = STR2, IDXE)
C
C            PROMPT AND INPUT THE FLAG  INDICATING PARALLEL FAULT DETEC-
C            TIONS AND CORRECTIONS; AND ACCESS THE EXVGPO ROUTINE TO AS-
C            SIGN THE TM1C VECTOR AND LCOR VARIABLE.
             WRITE (   *,305)
             WRITE (HIST,305)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
             CALL EXVGPO
     G                   (DAT   , NS    , FLAG
     Y                   ,TM1C  , LCOR  )
C
C            DOWHILE (ESF .NE. TERMINATION)
 1000            WRITE (   *,310)
                 WRITE (HIST,310)
                 READ  (   *,  *) ESF
                 WRITE (HIST,  *) ESF
                 IF ((ESF.GE.1) .AND. (ESF.LE.2)) THEN
                 PREDAT(ESF,1) = 0.0
C
C                DETERMINE THE WEIGHTING FUNCTION TYPE.
                 WRITE (   *,315)
                 WRITE (HIST,315)
                 READ  (   *,  *) WFSF
                 WRITE (HIST,  *) WFSF
C
                 IF (ESF.EQ.1) THEN
C                    DETERMINE WHETHER A ONE PARAMETER  OR TWO PARAMETER
C                    ESTIMATION PROBLEM.
                     ICOUNT = STR2 + 1
                     AFLAG  = 1
C                    DOWHILE ((AFLAG .EQ. 1) .AND. (ICOUNT .LE. IDXE))
 1025                    IF (DAT(ICOUNT).EQ.DAT(ICOUNT-1)) THEN
                             ICOUNT = ICOUNT + 1
                         ELSE
                             AFLAG = 0
                         ENDIF
                         IF ((AFLAG.EQ.1) .AND. (ICOUNT.LE.IDXE))
     1                                                        GO TO 1025
C                    ENDWHILE
                 ENDIF
C
                 IF ((WFSF.EQ.0) .OR. ((WFSF.GT.3) .AND. (ESF.EQ.1))
     1               .OR. ((WFSF.GT.2) .AND. (ESF.EQ.2))
     2               .OR. ((WFSF.GT.2) .AND. (ESF.EQ.1) .AND.
     3               (AFLAG.EQ.1))) THEN
C
C                    LIST THE AVAILABLE WEIGHTING FUNCTION TYPES.
                     WRITE (   *,320)
                     WRITE (HIST,320)
                     IF ((ESF.EQ.1) .AND. (AFLAG.EQ.0)) THEN
                         WRITE (   *,325)
                         WRITE (HIST,325)
                     ENDIF
                     IF ((ESF.EQ.1) .AND. (AFLAG.EQ.1)) THEN
                         WRITE (   *,330)
                         WRITE (HIST,330)
                     ENDIF
                     WRITE (   *,335)
                     WRITE (HIST,335)
                     READ  (   *,  *) WFSF
                     WRITE (HIST,  *) WFSF
                 ENDIF
C
                 IF (WFSF.EQ.2) THEN
                     WRITE (   *,340)
                     WRITE (HIST,340)
                     READ  (   *,  *) ALPHA
                     WRITE (HIST,  *) ALPHA
                 ELSEIF (WFSF.EQ.3) THEN
C                    SET THE INITIAL ESTIMATES  FOR THE  TOTAL NUMBER OF
C                    FAULTS AND ALPHA.
                     AN    = TM1C(NS) + DAT(NS) + 1.0
                     ALPHA = 0.0
C
C                    ALLOW THE USER TO  REPLACE THE  INITIAL  ESTIMATES,
C                    PROMPT FOR MAX. ITERATIONS, AND ACCESS THE MODEL.
                     WRITE (   *,345) AN, ALPHA
                     WRITE (HIST,345) AN, ALPHA
                     READ  (   *,  *) FLAG
                     WRITE (HIST,  *) FLAG
                     IF (FLAG.EQ.1) THEN
                         AN = TM1C(NS+1) + 0.001
                         WRITE (   *,350) AN
                         WRITE (HIST,350) AN
                         READ  (   *,  *) AN
                         WRITE (HIST,  *) AN
                         WRITE (   *,355)
                         WRITE (HIST,355)
                         READ  (   *,  *) ALPHA
                         WRITE (HIST,  *) ALPHA
                     ENDIF
                     WRITE (   *,360)
                     WRITE (HIST,360)
                     READ  (   *,  *) MAXIC
                     WRITE (HIST,  *) MAXIC
                 ENDIF
C
                 CALL SGPOMD
     G                       (ESF      , WFSF     , ALPHA    , AN
     G                       ,MAXIC    , DAT      , TM1C     , NS
     B                       ,DAT(STR2)
     Y                       ,STATS    , COUNT    , RFLAG    )
C
C                RESTORE THE INTERVAL TESTING LENGTH DATA VECTOR.
                 REWIND TMPB
                 READ  (TMPB) (DAT(I), I = STR2, IDXE)
C
                 IF (RFLAG.EQ.1) THEN
C                    THE MAXIMUM  ITERATION COUNT  WAS REACHED  BEFORE A
C                    SOLUTION WAS FOUND;  ISSUE ERROR MESSAGE AND OUTPUT
C                    THE ALPHA ESTIMATION WEIGHTING FUNCTION RESULTS.
                     WRITE (   *,365)  LB(ESF), WFSF
                     WRITE (HIST,365)  LB(ESF), WFSF
                     WRITE (   *,375) (STATS(I,1) , I = 1, 3)
                     WRITE (HIST,375) (STATS(I,1) , I = 1, 3)
                     WRITE (   *,380)  STATS(4,1)
                     WRITE (HIST,380)  STATS(4,1)
                 ELSEIF (RFLAG.EQ.2) THEN
C                    THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY.
                     WRITE (   *,385) LB(ESF), WFSF
                     WRITE (HIST,385) LB(ESF), WFSF
                 ELSEIF (RFLAG.EQ.3) THEN
C                    THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
                     WRITE (   *,390) LB(ESF), WFSF
                     WRITE (HIST,390) LB(ESF), WFSF
                 ELSEIF (RFLAG.EQ.4) THEN
C                    THE  TOTAL ERRORS  ESTIMATE  WAS LESS THAN  THE OB-
C                    SERVED ERRORS TO DATE.
                     WRITE (   *,391) LB(ESF), WFSF
                     WRITE (HIST,391) LB(ESF), WFSF
                 ELSE
C                ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY-----------
C                        A SOLUTION WAS FOUND; OUTPUT THE RESULTS.
                         IF (WFSF.EQ.3) THEN
                             WRITE (   *,395) LB(ESF), WFSF, COUNT
                             WRITE (HIST,395) LB(ESF), WFSF, COUNT
                         ELSE
                             WRITE (   *,400) LB(ESF), WFSF
                             WRITE (HIST,400) LB(ESF), WFSF
                         ENDIF
C
                         IF (WFSF.NE.3) THEN
                             IF (ESF.EQ.1) THEN
                                 WRITE (   *,366)
                                 WRITE (HIST,366)
                                 WRITE (   *,370) (STATS(I,1)
     1                                            ,STATS(I,2)
     2                                            ,STATS(I,3)
     3                                            ,I = 1, 3)
                                 WRITE (HIST,370) (STATS(I,1)
     1                                            ,STATS(I,2)
     2                                            ,STATS(I,3)
     3                                            ,I = 1, 3)
                             ELSE
                                 WRITE (   *,375) (STATS(I,1)
     1                                            ,I = 1, 3)
                                 WRITE (HIST,375) (STATS(I,1)
     1                                            ,I = 1, 3)
                             ENDIF
                         ELSE
                             WRITE (   *,375) (STATS(I,1) , I = 1, 3)
                             WRITE (HIST,375) (STATS(I,1) , I = 1, 3)
                             WRITE (   *,380)  STATS(4,1)
                             WRITE (HIST,380)  STATS(4,1)
                         ENDIF
C
C                        SET THE SUCCESSFULNESS FLAG AND STORE THE NEED-
C                        ED DATA FOR THE PREDICTION COMPUTATIONS.
                         PREDAT(ESF,1) = 1.0
                         PREDAT(ESF,2) = WFSF
                         PREDAT(ESF,3) = ALPHA
                         IF (WFSF.EQ.3) THEN
                             PREDAT(ESF,3) = STATS(4,1)
                         ENDIF
                         PREDAT(ESF,4) = STATS(1,1)
                         PREDAT(ESF,5) = STATS(2,1)
C
                         IF (STATS(3,1).LT.0.5) THEN
C                            FUTURE PREDICTIONS ARE NOT ALLOWED (TNOFR).
                             WRITE (   *,401)
                             WRITE (HIST,401)
                         ELSE
C                            DOWHILE (FLAG .NE. TERMINATION)
 1050                            WRITE (   *,405)
                                 WRITE (HIST,405)
                                 READ  (   *,  *) FLAG
                                 WRITE (HIST,  *) FLAG
                                 IF ((FLAG.LT.1) .OR. (FLAG.GT.2))
     1                                                        GO TO 1200
C
                                 IF     (FLAG.EQ.1) THEN
C                                    FAULTS IN NEXT TESTING PERIOD.
                                     NPV = 1
                                     IF ((ESF.EQ.1) .AND. (WFSF.NE.3))
     1                                                              THEN
                                         NPV = 3
                                     ENDIF
C                                    DOWHILE (EXL .NE. TERMINATION)
 1075                                    WRITE (   *,410)
                                         WRITE (HIST,410)
                                         READ  (   *,  *) EXL
                                         WRITE (HIST,  *) EXL
                                         IF (EXL.EQ.0.0) GO TO 1100
                                         CALL SGPOPR
     G                                               (NPV          ,NS
     G                                               ,WFSF         ,DAT
     G                                               ,DAT(STR2)    ,TM1C
     G                                               ,EXL          ,LCOR
     G                                               ,PREDAT(ESF,4)
     G                                               ,PREDAT(ESF,5)
     G                                               ,PREDAT(ESF,3)
     Y                                               ,XPRD         )
                                         IF (XPRD(1).GT.STATS(3,1)) THEN
                                             XPRD(1) = STATS(3,1)
                                             XPRD(2) = STATS(3,1)
                                         ENDIF
                                         IF (NPV.NE.3) THEN
                                             WRITE (   *,415)  XPRD(1)
                                             WRITE (HIST,415)  XPRD(1)
                                         ELSE
                                             WRITE (   *,420) (XPRD(I)
     1                                                      , I = 1, 3)
                                             WRITE (HIST,420) (XPRD(I)
     1                                                      , I = 1, 3)
                                         ENDIF
                                         GO TO 1075
 1100                                    CONTINUE
C                                    ENDWHILE
C
                                 ELSEIF (FLAG.EQ.2) THEN
C                                    PERIODS TO DETECT NEXT M FAULTS.
                                     WRITE (   *,425)
                                     WRITE (HIST,425)
                                     READ  (   *,  *) EXL
                                     WRITE (HIST,  *) EXL
                                     NPV = 1
C                                    DOWHILE (DNOF .NE. TERMINATION)
 1125                                    WRITE (   *,430) STATS(3,1)
                                         WRITE (HIST,430) STATS(3,1)
                                         READ  (   *,  *) DNOF
                                         WRITE (HIST,  *) DNOF
                                         IF (DNOF.EQ.0.0) GO TO 1175
                                         TOTL = 0.0
                                         CNTP = 0.0
C                                        DOUNTIL (PREDICTED.GE.DESIRED)
 1150                                        TOTL = TOTL + EXL
                                             CNTP = CNTP + 1.0
                                             CALL SGPOPR
     G                                                 (NPV        ,NS
     G                                                 ,WFSF       ,DAT
     G                                                 ,DAT(STR2)  ,TM1C
     G                                                 ,TOTL       ,LCOR
     G                                                 ,PREDAT(ESF,4)
     G                                                 ,PREDAT(ESF,5)
     G                                                 ,PREDAT(ESF,3)
     Y                                                 ,XPRD       )
                                             IF ((XPRD(1).LT.DNOF) .AND.
     1                                           (CNTP   .LE.PMAX)
     2                                                      ) GO TO 1150
C                                        ENDUNTIL
                                         IF (CNTP.LE.PMAX) THEN
                                             WRITE (   *,435) CNTP
                                             WRITE (HIST,435) CNTP
                                         ELSE
                                             WRITE (   *,436) PMAX
                                             WRITE (HIST,436) PMAX
                                             READ  (   *,  *) FLAG2
                                             WRITE (HIST,  *) FLAG2
                                             IF (FLAG2.EQ.1) THEN
                                                 WRITE (   *,437)
                                                 WRITE (HIST,437)
                                                 READ  (   *,  *) PMAX
                                                 WRITE (HIST,  *) PMAX
                                             ENDIF
                                             WRITE (   *,438)
                                             WRITE (HIST,438)
                                         ENDIF
                                         GO TO 1125
 1175                                    CONTINUE
C                                    ENDWHILE
                                 ENDIF
                                 GO TO 1050
 1200                            CONTINUE
C                            ENDWHILE
                         ENDIF
C                ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY-----------
                 ENDIF
                 GO TO 1000
                 ENDIF
C            ENDWHILE
C
             IF ((PREDAT(1,1).EQ.1.0) .OR. (PREDAT(2,1).EQ.1.0)) THEN
C                AT LEAST ONE EXECUTION  WAS SUCCESSFUL, PROMPT USER AND
C                SEE IF THE PREDICTED VECTOR CREATION IS DESIRED.
                 WRITE (   *,440)
                 WRITE (HIST,440)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    THE VECTOR OF PREDICTED DATA IS DESIRED.
                     IF (PREDAT(1,1).NE.PREDAT(2,1)) THEN
C                        ONLY ONE EXECUTION  WAS SUCCESSFUL;  SET ESF TO
C                        INDICATE ML OR LS.
                         ESF = PREDAT(1,1) * 1.0 + PREDAT(2,1) * 2.0
                     ELSE
C                        BOTH EXECUTIONS WERE  PERFORMED AND SUCCESSFUL;
C                        SEE WHICH THE USER DESIRES.
                         WRITE (   *,445)
                         WRITE (HIST,445)
                         READ  (   *,  *) ESF
                         WRITE (HIST,  *) ESF
                     ENDIF
C
C                    SET THE  WEIGHTING  FUNCTION  SELECTION FLAG TO THE
C                    VALUE STORED IN THE PREDICTION DATA ARRAY.
                     WFSF = PREDAT(ESF,2)
C
C                    ACCESS THE SGPOPR ROUTINE TO GENERATE THE PREDICTED
C                    FAULT COUNT DATA VECTOR.
                     CALL SGPOPR
     G                           (NS           , NS
     G                           ,WFSF         , DAT
     G                           ,DAT(STR2)    , TM1C
     G                           ,EXL          , LCOR
     G                           ,PREDAT(ESF,4), PREDAT(ESF,5)
     G                           ,PREDAT(ESF,3)
     Y                           ,PRD          )
C
C                    SET THE ESTIMATION SELECTION CODE TO INDICATE WHICH
C                    MODEL WAS RESPONSIBLE FOR  THE PREDICTIONS; AND SET
C                    ESD TO INDICATE THAT THE INTERVAL DATA TYPE WAS RE-
C                    SPONSIBLE.
                     ESC = 7
                     ESD = 3
C
C                    SET THE GOODNESS-OF-FIT ESTIMATION CONSTANT.
                     EST = 2
                     IF (PREDAT(ESF,2).EQ.3.0) THEN
                         EST = 3
                     ENDIF
C
C                    PERFORM ANALYSIS OF MODEL FIT.
                     CALL FITDRV
                 ENDIF
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE MDLJAM
     G                     (IDX   , NS    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLJAM
C    G                     (IDX   , NS    )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE THE INTERACTIVE PROMPTS AND INPUTS FOR THE JELINSKI/
C        MORANDA DE-EUTROPHICATION MODEL. ADDITIONALLY, THE ROUTINE WILL
C        PERFORM THE ACCESS TO THE SMFLIB ROUTINES NEEDED TO PERFORM THE
C        MODEL EXECUTIONS AND PREDICTED DATA VECTOR CREATION.
C   DESCRIPTION
C        TWO SMFLIB ROUTINES (SJAMMD AND SJAMPR) WILL BE USED WITHIN THE
C        ROUTINE.
C
C        THE FIRST ROUTINE (SJAMMD) PROVIDES THE CALCULATIONS OF THE RE-
C        LIABILITY VIA THE  MAXIMUM LIKELIHOOD AND LEAST SQUARES METHODS
C        OF EXECUTION.  THE FIRST ARGUMENT OF THE  SJAMMD CALL LINE PRO-
C        VIDES THE ROUTINE WITH THE NUMBER OF THE METHOD DESIRED.
C
C        ONCE THE  EXECUTION OF  SJAMMD IS PERFORMED,  THE RETURN STATUS
C        FLAG (RFLAG) WILL INDICATE THE SUCCESSFULNESS  OF THE MODEL ES-
C        TIMATION PROCESS. THE FOLLOWING VALUES ARE POSSIBLE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS THAN THE  OBSERVED
C                 FAULTS TO DATE
C
C        FOR RFLAG VALUE OF 0 THE MODEL ESTIMATES WILL BE LISTED.
C
C        THE SECOND ROUTINE (SJAMPR) PROVIDES FUTURE PREDICTIONS &  GEN-
C        ERATES THE VECTOR OF PREDICTED TBF DATA.
C
C        THE SJAMPR ROUTINE WILL ALSO DETERMINE THE GOODNESS-OF-FIT WHEN
C        CREATING THE PREDICTED VECTOR; ALTHOUGH THE VALUES FOR THE KOL-
C        MOGOROV DISTANCE STATISTIC AND THE DETERMINED SIGNIFICANCE WILL
C        NOT BE OUTPUT UNTIL THE USER ENTERS THE ANALYSIS DRIVER.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C        YIELDED
C             D      (R) = CALCULATED KOLMOGOROV DISTANCE
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          GOODNESS-OF-FIT CALCULATION
C   ARGUMENT LIST
C        GIVEN
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C   LOCAL GLOSSARY
C             CIF    (R) = CURRENT INTENSITY FUNCTION
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             EXE    (I) = EXTRA FAILURES TO BE LOCATED, IN ESTIMATION
C             FLAG   (I) = OPTION SELECTION FLAG
C             LB     (C) = OUTPUT MODEL LABELS
C             NPV    (I) = NUMBER OF PREDICTED VALUES FLAG
C             PREDAT (R) = PREDICTION DATA VALUES ARRAY
C             PRINI  (I) = PREDICTION INPUT VALUE FOR PR2 (N/A)
C             PRINR  (R) = PREDICTION INPUT VALUE FOR PR1, PR3 - PR5
C             PR1    (R) = FUTURE RELIABILTIY FOR SPECIFIED TIME
C             PR2    (R) = MTBNF TO LOCATE NEXT K FAILURES (N/A)
C             PR3    (R) = ADDITIONAL TESTING TIME TO REACH DIF
C             PR4    (R) = ADDITIONAL FAILURES FOUND DURING PR3
C             PR5    (R) = FUTURE FAILURES WITHIN A SPECIFIED TIME
C             PURLVL (R) = PURIFICATION LEVEL
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = STATISTICS RESULTS ARRAY
C             SUMTBF (R) = SUM OF THE TBF DATA
C             WT     (I) = FLAG TO PAUSE THE ASSUMPTIONS LIST OUTPUT
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
C               /PRECOM/      PREDICTION DATA STORAGE COMMON
         COMMON /PRECOM/      PRD(END1)     ,XPRD(3)       ,D
     1                       ,DFLG          ,ESC           ,ESD
     2                       ,EST
         INTEGER              DFLG          ,ESC           ,ESD
     1                       ,EST
C
C               /TMPCOM/      TEMPORARY STORAGE COMMON
         COMMON /TMPCOM/      TM1C(END1)    ,TM2C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              IDX           ,NS
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CONFLG        ,ESF           ,EXE
     1                       ,FLAG          ,I             ,NPV
     2                       ,RFLAG         ,WT
         DIMENSION            PREDAT(2,3)   ,STATS(6)
         CHARACTER* 2         LB(2)
C
C                             LOCAL ASSIGNMENTS
         DATA                 LB(1) /'ML'/      ,    LB(2) /'LS'/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR JELINSKI/MORANDA MODEL DESCRIPTION; '
     1          ,'ELSE ZERO.')
  301    FORMAT (' '
     1        ,/,'    THE JELINSKI/MORANDA "DE-EUTROPHICATION MODEL" AS'
     2          ,'SUMPTIONS'
     3       ,//,'     1. THE SOFTWARE IS OPERATED IN A SIMILAR MANNER '
     4          ,'AS  THE ANTICI-'
     5        ,/,'        PATED OPERATIONAL USAGE.'
     6       ,//,'     2. ALL FAULTS ARE EQUALLY LIKELY TO OCCUR  AND A'
     7          ,'RE INDEPENDENT'
     8        ,/,'        OF EACH OTHER.'
     9       ,//,'     3. EACH FAULT IS OF THE SAME ORDER OF "IMPACT" A'
     A          ,'S ANY OTHER.'
     B       ,//,'     4. THE RATE OF FAULT DETECTION IS  PROPORTIONAL '
     C          ,'TO THE CURRENT')
  302    FORMAT ('        FAULT CONTENT OF A PROGRAM.'
     1       ,//,'     5. THE FAILURE RATE REMAINS CONSTANT  OVER THE T'
     2          ,'IME PERIOD BE-'
     3        ,/,'        TWEEN FAILURE OCCURRENCES.'
     4       ,//,'     6. THE FAULTS ARE CORRECTED  AT THE TIME OF OCCU'
     5          ,'RRENCE WITHOUT'
     6        ,/,'        INTRODUCTION OF NEW ONES.'
     7       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  303    FORMAT ('    MODEL DATA REQUIREMENTS'
     1       ,//,'     1. THE TIME-BETWEEN-FAILURES AS MEASURED IN WALL'
     2          ,' CLOCK OR COM-'
     3        ,/,'        PUTER CPU TIME.  THE DATA SHOULD HAVE  BEEN E'
     4          ,'NTERED VIA THE'
     5        ,/,'        INPUT MODULE.'
     6       ,//,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     7          ,'HE APPROPRIATE'
     8        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     9          ,'EL;  OTHERWISE'
     A        ,/,'    ZERO TO RETURN TO THE MENU OF MODELS.')
  305    FORMAT (' '
     1        ,/,'   ENTER ONE FOR  MAXIMUM LIKELIHOOD METHOD,  TWO FOR'
     2          ,' LEAST SQUARES'
     3        ,/,'   METHOD, OR THREE TO TERMINATE MODEL EXECUTION.')
  310    FORMAT (' '
     1        ,/,' **THE DATA ARE NOT APPROPRIATE FOR THE ',A2,' MODEL.'
     2          )
  315    FORMAT (' '
     1        ,/,' **THE ESTIMATE  FOR THE TOTAL  NUMBER OF FAULTS  TO '
     2          ,'BE DETECTED IN'
     3        ,/,'   THE PROGRAM BEFORE  ALL FAULTS ARE UNCOVERED  CAME'
     4          ,' OUT LESS THAN'
     5        ,/,'   THE NUMBER OF FAULTS FOUND TO DATE WITHIN THE '
     6          ,A2,' MODEL.')
  320    FORMAT (' '
     1        ,/,3X,A2,' MODEL ESTIMATES ARE:'
     2        ,/,'    PROPORTIONALITY CONSTANT ',E12.5
     3        ,/,'    INIT. INTENSITY FUNCTION ',E12.5
     4        ,/,'    CUR.  INTENSITY FUNCTION ',E12.5
     5        ,/,'    MTBNF                    ',E12.5
     6        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5
     7        ,/,'    # OF FAULTS REMAINING    ',E12.5
     8        ,/,'    PURIFICATION LEVEL       ',E12.5)
  321    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   (ROUNDED) NUMBER OF FAULTS REMAINING IS LESS THAN '
     4          ,'ONE.')
  322    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   MTBNF IS LESS THAN ZERO.')
  325    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE EXPECTED RELIABILITY FOR A SPECIFIED TIME'
     3        ,/,'    2) THE TOTAL MTBNF FOR THE NEXT K FAILURES'
     4        ,/,'    3) THE TIME AND NUMBER OF FAILURES TO REACH A DES'
     5          ,'IRED INTENSITY'
     6        ,/,'       FUNCTION'
     7        ,/,'    4) THE NUMBER OF FAILURES EXPECTED IN A SPECIFIED'
     8          ,' TIME'
     9        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     A          ,'S.')
  330    FORMAT ('    ENTER LENGTH OF THE PERIOD, OR ZERO TO END.')
  335    FORMAT ('    FUTURE RELIABILITY      ',E12.5,/)
  340    FORMAT ('    ENTER VALUE OF K (BETWEEN ONE AND',E12.5,'), OR Z'
     1          ,'ERO TO END.')
  345    FORMAT ('    MTBNF EXPECTED          ',E12.5,/)
  350    FORMAT ('    ENTER INTENSITY FUNC. (LESS THAN',E12.5,'), OR ZE'
     1          ,'RO TO END.')
  355    FORMAT ('    ADDITIONAL TESTING TIME ',E12.5
     1        ,/,'    # OF FAILURES EXPECTED  ',E12.5,/)
  360    FORMAT ('    # OF FAILURES EXPECTED  ',E12.5,/)
  365    FORMAT (' '
     1        ,/,'   ENTER ONE TO PERFORM AN ANALYSIS OF THE MODEL FIT '
     2          ,'USING THE PRE-'
     3        ,/,'   DICTIONS OF THIS MODEL; ELSE ZERO.')
  370    FORMAT ('   ENTER ONE FOR PREDICTIONS BASED ON MAXIMUM LIKELIH'
     1          ,'OOD METHOD, OR'
     2        ,/,'   TWO FOR LEAST SQUARES METHOD.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        ZERO THE SUCCESSFULNESS  ELEMENTS OF THE PREDICTION DATA ARRAY.
         PREDAT(1,1) = 0.0
         PREDAT(2,1) = 0.0
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,301)
             WRITE (HIST,301)
             WRITE (   *,302)
             WRITE (HIST,302)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,303)
             WRITE (HIST,303)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
C
         IF (CONFLG.EQ.1) THEN
C            COMPUTE THE SUM OF THE TBF DATA.
             SUMTBF = 0.0
             DO 1000 I = 0, (NS-1)
                 SUMTBF = SUMTBF + DAT(IDX+I)
 1000            CONTINUE
C            ENDDO
C
C            DOWHILE (ESF .NE. TERMINATION)
 1025            WRITE (   *,305)
                 WRITE (HIST,305)
                 READ  (   *,  *) ESF
                 WRITE (HIST,  *) ESF
                 IF ((ESF.GE.1) .AND. (ESF.LE.2)) THEN
                 PREDAT(ESF,1) = 0.0
C
                 CALL SJAMMD
     G                       (ESF   , NS    ,  DAT(IDX)
     Y                       ,STATS , RFLAG )
C
                 IF (RFLAG.EQ.3) THEN
C                    THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
                     WRITE (   *,310) LB(ESF)
                     WRITE (HIST,310) LB(ESF)
                 ELSEIF (RFLAG.EQ.4) THEN
C                    THE  TOTAL ERRORS  ESTIMATE  WAS LESS THAN  THE OB-
C                    SERVED ERRORS TO DATE.
                     WRITE (   *,315) LB(ESF)
                     WRITE (HIST,315) LB(ESF)
                 ELSE
C                    A SOLUTION WAS FOUND;  COMPUTE EXTRA CURRENT INTEN-
C                    SITY FUNCTION AND  PURIFICATION LEVEL.  THE INITIAL
C                    INTENSITY FUNCTION IS  EQUAL TO THE  INITIAL HAZARD
C                    RATE; THEREFORE, IT IS NOT RE-COMPUTED.
                     CIF    = STATS(4) * STATS(1)
     1                      * (EXP(-STATS(1) * SUMTBF))
                     PURLVL = REAL(NS) / STATS(4)
C
C                    OUTPUT THE RESULTS.  NOTE, THE CURRENT  HAZARD RATE
C                    FROM THE SMFLIB  IS NOT OUTPUT.  THE CURRENT INTEN-
C                    SITY FUNCTION IS PRINTED INSTEAD.
                     WRITE (   *,320) LB(ESF)  , STATS(1) , STATS(2)
     1                               ,CIF      , STATS(6) , STATS(4)
     2                               ,STATS(5) , PURLVL
                     WRITE (HIST,320) LB(ESF)  , STATS(1) , STATS(2)
     1                               ,CIF      , STATS(6) , STATS(4)
     2                               ,STATS(5) , PURLVL
C
C                    SET THE SUCCESSFULNESS  FLAG, AND STORE PCON & TNOF
C                    ESTIMATES.
                     PREDAT(ESF,1) = 1.0
                     PREDAT(ESF,2) = STATS(1)
                     PREDAT(ESF,3) = STATS(4)
C
                     IF (STATS(5).LT.0.5) THEN
C                        FUTURE PREDICTIONS ARE NOT ALLOWED (TNOFR).
                         WRITE (   *,321)
                         WRITE (HIST,321)
                     ENDIF
                     IF (STATS(6).LT.0.0) THEN
C                        FUTURE PREDICTIONS ARE NOT ALLOWED (MTBNF).
                         WRITE (   *,322)
                         WRITE (HIST,322)
                     ENDIF
                     IF ((STATS(5).GE.0.5) .AND. (STATS(6).GE.0.0)) THEN
C                        DOWHILE (FLAG .NE. TERMINATION)
 1050                        WRITE (   *,325)
                             WRITE (HIST,325)
                             READ  (   *,  *) FLAG
                             WRITE (HIST,  *) FLAG
                             IF ((FLAG.LT.1) .OR. (FLAG.GT.4))GO TO 1275
C
                             IF     (FLAG.EQ.1) THEN
C                                FUTURE RELIABILITY FOR SPECIFIED TIME.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1075                                WRITE (   *,330)
                                     WRITE (HIST,330)
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1100
                                     PR1 = EXP(-STATS(5) * STATS(1)
     1                                   * PRINR)
                                     WRITE (   *,335) PR1
                                     WRITE (HIST,335) PR1
                                     GO TO 1075
 1100                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.2) THEN
C                                MTBNF TO LOCATE K FAILURES.
                                 NPV = 1
C                                DOWHILE (PRINR .NE. TERMINATION)
 1125                                WRITE (   *,340) STATS(5)
                                     WRITE (HIST,340) STATS(5)
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1150
                                     EXE = INT(PRINR + 0.5)
                                     CALL SJAMPR
     G                                           (NPV      , NS
     G                                           ,STATS(1) , STATS(4)
     G                                           ,EXE      , DAT(IDX)
     Y                                           ,XPRD     , D
     Y                                           ,DFLG     , TM1C     )
                                     WRITE (   *,345) XPRD(1)
                                     WRITE (HIST,345) XPRD(1)
                                     GO TO 1125
 1150                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.3) THEN
C                                TIME AND FAILURES  TO REACH  A  DESIRED
C                                INTENSITY FUNCTION.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1175                                WRITE (   *,350) CIF
                                     WRITE (HIST,350) CIF
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1200
                                     PR3 = 1.0 / STATS(1)
     1                                   * LOG(CIF / PRINR)
                                     PR4 = 1.0 / STATS(1)
     1                                   * (CIF - PRINR)
                                     IF (PR4.GT.STATS(5)) THEN
                                         PR4 = STATS(5)
                                     ENDIF
                                     WRITE (   *,355) PR3, PR4
                                     WRITE (HIST,355) PR3, PR4
                                     GO TO 1175
 1200                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.4) THEN
C                                FAILURES FOUND IN A SPECIFIED TIME.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1225                                WRITE (   *,330)
                                     WRITE (HIST,330)
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1250
                                     PR5 = STATS(4)
     1                                   * (EXP(-STATS(1)
     2                                   *  SUMTBF)
     3                                   -  EXP(-STATS(1)
     4                                   * (SUMTBF + PRINR)))
                                     IF (PR5.GT.STATS(5)) THEN
                                         PR5 = STATS(5)
                                     ENDIF
                                     WRITE (   *,360) PR5
                                     WRITE (HIST,360) PR5
                                     GO TO 1225
 1250                                CONTINUE
C                                ENDWHILE
                             ENDIF
                             GO TO 1050
 1275                        CONTINUE
C                        ENDWHILE
                     ENDIF
                 ENDIF
                 GO TO 1025
                 ENDIF
C            ENDWHILE
C
             IF ((PREDAT(1,1).EQ.1.0) .OR. (PREDAT(2,1).EQ.1.0)) THEN
C                AT LEAST ONE EXECUTION  WAS SUCCESSFUL, PROMPT USER AND
C                SEE IF THE PREDICTED VECTOR CREATION IS DESIRED.
                 WRITE (   *,365)
                 WRITE (HIST,365)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    THE VECTOR OF PREDICTED DATA IS DESIRED.
                     IF (PREDAT(1,1).NE.PREDAT(2,1)) THEN
C                        ONLY ONE EXECUTION  WAS SUCCESSFUL;  SET ESF TO
C                        INDICATE ML OR LS.
                         ESF = PREDAT(1,1) * 1.0 + PREDAT(2,1) * 2.0
                     ELSE
C                        BOTH EXECUTIONS WERE  PERFORMED AND SUCCESSFUL;
C                        SEE WHICH THE USER DESIRES.
                         WRITE (   *,370)
                         WRITE (HIST,370)
                         READ  (   *,  *) ESF
                         WRITE (HIST,  *) ESF
                     ENDIF
C
C                    ACCESS THE SJAMPR ROUTINE TO GENERATE THE PREDICTED
C                    TBF DATA VECTOR.
                     CALL SJAMPR
     G                           (NS            , NS
     G                           ,PREDAT(ESF,2) , PREDAT(ESF,3)
     G                           ,EXE           , DAT(IDX)
     Y                           ,PRD           , D
     Y                           ,DFLG          , TM1C         )
C
C                    SET THE ESTIMATION SELECTION CODE TO INDICATE WHICH
C                    MODEL WAS RESPONSIBLE FOR  THE PREDICTIONS; AND SET
C                    ESD TO INDICATE WHICH DATA TYPE (WC OR CPU) WAS RE-
C                    SPONSIBLE.
                     ESC = 6
                     ESD = 1
                     IF (IDX.EQ.STR2) THEN
                         ESD = 2
                     ENDIF
C
C                    PERFORM ANALYSIS OF MODEL FIT.
                     CALL FITDRV
                 ENDIF
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE MDLLAV
     G                     (IDX   , NS    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLLAV
C    G                     (IDX   , NS    )
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
C        TO PROVIDE THE  INTERACTIVE PROMPTS AND INPUTS  FOR THE LITTLE-
C        WOOD AND VERRALL BAYESIAN RELIABILITY GROWTH MODEL. ADDITIONAL-
C        LY, THE ROUTINE WILL PERFORM THE  ACCESS TO THE SMFLIB ROUTINES
C        NEEDED TO PERFORM THE MODEL  EXECUTIONS AND PREDICTED DATA VEC-
C        TOR CREATION.
C   DESCRIPTION
C        TWO SMFLIB ROUTINES (SLAVMD AND SLAVPR) WILL BE USED WITHIN THE
C        ROUTINE.
C
C        THE FIRST ROUTINE (SLAVMD) PROVIDES THE CALCULATIONS OF THE RE-
C        LIABILITY VIA THE  MAXIMUM LIKELIHOOD AND LEAST SQUARES METHODS
C        OF EXECUTION.   ADDITIONALLY, SLAVMD PROVIDES THE  CALCULATIONS
C        OF  THE INITIAL  ESTIMATES FOR THE MAXIMUM LIKELIHOOD METHOD OF
C        EXECUTION.   THE SECOND ARGUMENT OF THE  SLAVMD  CALL LINE PRO-
C        VIDES THE ROUTINE  WITH THE NUMBER OF THE METHOD DESIRED. ADDI-
C        TIONALLY,  THE SLAVMD ROUTINE  IS DESIGNED TO  PROCESS FOR BOTH
C        THE  LINEAR AND QUADRATIC FUNCTIONS.  THE SIXTH ARGUMENT OF THE
C        SLAVMD CALL LINE  PROVIDES THE  ROUTINE WITH THE  NUMBER OF THE
C        FUNCTION DESIRED.
C
C        ONCE THE  EXECUTION OF SLAVMD IS PERFORMED,  THE RETURN  STATUS
C        FLAG (RFLAG) WILL INDICATE  THE SUCCESSFULNESS OF THE MODEL ES-
C        TIMATION PROCESS.  THE FOLLOWING VALUES  ARE POSSIBLE  FOR  THE
C        MAXIMUM LIKELIHOOD METHOD OF EXECUTION:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             1 - MAXIMUM ITERATIONS
C             2 - THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY
C
C        FOR BOTH THE  LEAST SQUARES AND  MAXIMUM LIKELIHOOD  METHODS OF
C        EXECUTION, RFLAG MAY ALSO BE RETURNED AS FOUR  TO INDICATE THAT
C        THE MODEL ESTIMATES WERE DEEMED INVALID BECAUSE THE REQUIREMENT
C        THAT THE SUM OF BETA0 AND BETA1 BE POSITIVE AS NOT ACHIEVED.
C
C        FOR RFLAG VALUES OF 0 AND 1  RETURNED FROM THE  MAXIMUM LIKELI-
C        HOOD OR LEAST SQUARES METHODS OF EXECUTION, THE MODEL ESTIMATES
C        WILL BE LISTED.
C
C        THE SECOND ROUTINE (SLAVPR) GENERATES THE  VECTOR OF  PREDICTED
C        TBF DATA.
C
C        THE SLAVPR ROUTINE WILL ALSO DETERMINE THE GOODNESS-OF-FIT WHEN
C        CREATING THE PREDICTED VECTOR; ALTHOUGH THE VALUES FOR THE KOL-
C        MOGOROV DISTANCE STATISTIC AND THE DETERMINED SIGNIFICANCE WILL
C        NOT BE OUTPUT UNTIL THE USER ENTERS THE ANALYSIS DRIVER.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C        YIELDED
C             D      (R) = CALCULATED KOLMOGOROV DISTANCE
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          GOODNESS-OF-FIT CALCULATION
C             XPRD   (R) = FUTURE PREDICTIONS (NUMBER AND 95% C.I.)
C   ARGUMENT LIST
C        GIVEN
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C   LOCAL GLOSSARY
C             ALPHA  (R) = ALPHA ESTIMATE FROM SLAVMD
C             AM1    (R) = ALPHA - 1.0
C             AM12   (R) = ALPHA - 1.0 SQUARED
C             BETA   (R) = INITIAL BETA ESTIMATES FOR SLAVMD
C             CIF    (R) = CURRENT INTENSITY FUNCTION
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             COUNT  (I) = ITERATION COUNTER
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             FLAG   (I) = OPTION SELECTION FLAG
C             IIF    (R) = INITIAL INTENSITY FUNCTION
C             LB     (C) = OUTPUT MODEL LABELS
C             MAXIC  (I) = USER-SPECIFIED MAX. NUMBER OF ITERATIONS
C             MTBF   (R) = MTBNF
C             NPV    (I) = NUMBER OF PREDICTED VALUES FLAG
C             PHIIND (I) = PHI INDICATOR FOR SLAVMD
C             PREDAT (R) = PREDICTION DATA VALUES ARRAY
C             PRINI  (I) = PREDICTION INPUT VALUE FOR PR2
C             PRINR  (R) = PREDICTION INPUT VALUE FOR PR1, PR3 - PR5
C             PR1    (R) = FUTURE RELIABILTIY FOR SPECIFIED TIME (N/A)
C             PR2    (R) = MTBNF TO LOCATE NEXT K FAILURES
C             PR3    (R) = ADDITIONAL TESTING TIME TO REACH DIF (L ONLY)
C             PR4    (R) = ADDITIONAL FAILURES FOUND DURING PR3 (L ONLY)
C             PR5    (R) = FUTURE FAILURES WITHIN A SPECIFIED TIME
C             RFLAG  (I) = RETURN STATUS FLAG
C             SUMTBF (R) = SUM OF THE TBF DATA
C             SUMTB2 (R) = SUM OF THE TBF DATA SQUARED
C             SUMTX  (R) = SUM OF THE TBF DATA PLUS EXTRA TIME
C             SUMTX2 (R) = SUM OF THE TBF DATA PLUS EXTRA TIME SQUARED
C             THRD   (R) = 1/3 FOR QUADRATIC CUBE ROOT
C             WT     (I) = FLAG TO PAUSE THE ASSUMPTIONS LIST OUTPUT
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
C               /PRECOM/      PREDICTION DATA STORAGE COMMON
         COMMON /PRECOM/      PRD(END1)     ,XPRD(3)       ,D
     1                       ,DFLG          ,ESC           ,ESD
     2                       ,EST
         INTEGER              DFLG          ,ESC           ,ESD
     1                       ,EST
C
C               /TMPCOM/      TEMPORARY STORAGE COMMON
         COMMON /TMPCOM/      TM1C(END1)    ,TM2C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              IDX           ,NS
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CONFLG        ,COUNT         ,ESF
     1                       ,FLAG          ,I             ,MAXIC
     2                       ,NPV           ,PHIIND        ,PRINI
     3                       ,RFLAG         ,WT
         DIMENSION            BETA(2)       ,PREDAT(2,5)   ,X(2)
         CHARACTER* 2         LB(2)
C
C                             LOCAL ASSIGNMENTS
         DATA                 LB(1) /'ML'/      ,    LB(2) /'LS'/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR LITTLEWOOD & VERRALL MODEL DESCRIPTI'
     1          ,'ON; ELSE ZERO.')
  301    FORMAT (' '
     1        ,/,'    THE LITTLEWOOD & VERRALL BAYESIAN MODEL ASSUMPTIO'
     2          ,'NS'
     3       ,//,'     1. THE SOFTWARE IS OPERATED IN A SIMILAR MANNER '
     4          ,'AS THE ANTICI-'
     5        ,/,'        PATED OPERATIONAL USAGE.'
     6       ,//,'     2. SUCCESSIVE  TIME-BETWEEN-FAILURES  ARE  INDEP'
     7          ,'ENDENT  RANDOM'
     8        ,/,'        VARIABLES EACH HAVING AN EXPONENTIAL DISTRIBU'
     9          ,'TION. THE DIS-'
     A        ,/,'        TRIBUTION FOR THE I-TH FAILURE HAS A MEAN OF '
     B          ,'1/LAMDA(I).'
     C       ,//,'     3. THE LAMDA(I) FORM A SEQUENCE OF INDEPENDENT V'
     D          ,'ARIABLES, EACH'
     E        ,/,'        HAVING A GAMMA  DISTRIBUTION  WITH THE PARAME'
     F          ,'TERS ALPHA AND')
  302    FORMAT ('        PHI(I). PHI(I) HAS EITHER THE FORM:'
     1       ,//,'         BETA(0) + BETA(1) * I     (LINEAR)'
     2        ,/,'        OR'
     3        ,/,'         BETA(0) + BETA(1) * I**2  (QUADRATIC).'
     4       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  303    FORMAT ('    MODEL DATA REQUIREMENTS'
     1       ,//,'     1. THE TIME-BETWEEN-FAILURES AS MEASURED IN WALL'
     2          ,' CLOCK OR COM-'
     3        ,/,'        PUTER CPU TIME.  THE DATA SHOULD HAVE  BEEN E'
     4          ,'NTERED VIA THE'
     5        ,/,'        INPUT MODULE.'
     6       ,//,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     7          ,'HE APPROPRIATE'
     8        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     9          ,'EL;  OTHERWISE'
     A        ,/,'    ZERO TO RETURN TO THE MENU OF MODELS.')
  305    FORMAT (' '
     1        ,/,'   ENTER ONE FOR  MAXIMUM LIKELIHOOD METHOD,  TWO FOR'
     2          ,' LEAST SQUARES'
     3        ,/,'   METHOD, OR THREE TO TERMINATE MODEL EXECUTION.')
  310    FORMAT (' '
     1        ,/,'   WHICH FUNCTION IS DESIRED TO BE USED AS THE  PHI(I'
     2          ,') IN THE GAMMA'
     3        ,/,'   DISTRIBUTION?  GAMMA IS USED AS THE PRIOR  WITH PA'
     4          ,'RAMETERS ALPHA'
     5        ,/,'   AND PHI(I)'
     6       ,//,'    1. PHI(I) = BETA(0) + BETA(1) * I    (LINEAR)'
     7        ,/,'   OR'
     8        ,/,'    2. PHI(I) = BETA(0) + BETA(1) * I**2 (QUADRATIC).'
     9          )
  315    FORMAT (' '
     1        ,/,'   THE INITIAL ESTIMATES TO BE USED IN THE ESTIMATION'
     2          ,' PROCESS ARE:'
     3        ,/,'    BETA(0)                  ',E12.5
     4        ,/,'    BETA(1)                  ',E12.5
     5       ,//,'   ENTER ONE TO USE DIFFERENT INITIAL ESTIMATES, OR Z'
     6          ,'ERO TO USE THE'
     7        ,/,'   INITIAL ESTIMATES.')
  320    FORMAT ('    ENTER INITIAL ESTIMATES FOR BETA(0) AND BETA(1)'
     1        ,/,'    (BETWEEN 0.10000E-06 AND 0.10000E+08).')
  325    FORMAT ('   ENTER MAXIMUM NUMBER OF ITERATIONS.')
  330    FORMAT (' '
     1        ,/,' **THE MAXIMUM NUMBER OF ITERATIONS WAS REACHED  WITH'
     2          ,' THE FOLLOWING'
     3        ,/,'   FINAL ML MODEL ESTIMATES:')
  335    FORMAT ('    ALPHA                    ',E12.5)
  340    FORMAT ('    BETA(0)                  ',E12.5
     1        ,/,'    BETA(1)                  ',E12.5
     2        ,/,'    FUNCTION EVALUATED AT    '
     3        ,/,'     THESE POINTS            ',E12.5
     4        ,/,'    INIT. INTENSITY FUNCTION ',E12.5
     5        ,/,'    CUR.  INTENSITY FUNCTION ',E12.5
     6        ,/,'    MTBNF                    ',E12.5)
  345    FORMAT (' '
     1        ,/,' **THE TRUST REGION COULD NOT BE ADJUSTED PROPERLY IN'
     2          ,' THE ML MODEL.')
  350    FORMAT (' '
     1        ,/,' **THE ESTIMATES OF THE ',A2,' MODEL PARAMETERS:'
     2        ,/,'    BETA(0)',E12.5,' AND BETA(1)',E12.5
     3        ,/,'   RESULTED IN A NEGATIVE ESTIMATE FOR THE INITIAL MT'
     4          ,'BNF. THE MODEL'
     5        ,/,'   THUS CANNOT BE APPLIED FOR THIS DATA SET.')
  355    FORMAT (' '
     1        ,/,'   ML MODEL ESTIMATES AFTER',I4,' ITERATIONS ARE:')
  360    FORMAT (' '
     1        ,/,'   LS MODEL ESTIMATES ARE:')
  361    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   MTBNF IS LESS THAN ZERO.')
  365    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE TOTAL MTBNF FOR THE NEXT K FAILURES'
     3        ,/,'    2) THE TIME AND NUMBER OF FAILURES TO REACH A DES'
     4          ,'IRED INTENSITY'
     5        ,/,'       FUNCTION'
     6        ,/,'    3) THE NUMBER OF FAILURES EXPECTED IN A SPECIFIED'
     7          ,' TIME'
     8        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     9          ,'S.')
  370    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE TOTAL MTBNF FOR THE NEXT K FAILURES'
     3        ,/,'    2) THE NUMBER OF FAILURES EXPECTED IN A SPECIFIED'
     4          ,' TIME'
     5        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     6          ,'S.')
  375    FORMAT ('    ENTER VALUE OF K, OR ZERO TO END.')
  380    FORMAT ('    MTBNF EXPECTED          ',E12.5,/)
  385    FORMAT ('    ENTER INTENSITY FUNC. (LESS THAN',E12.5,'), OR ZE'
     1          ,'RO TO END.')
  390    FORMAT ('    ADDITIONAL TESTING TIME ',E12.5
     1        ,/,'    # OF FAILURES EXPECTED  ',E12.5,/)
  395    FORMAT ('    ENTER LENGTH OF THE PERIOD, OR ZERO TO END.')
  400    FORMAT ('    # OF FAILURES EXPECTED  ',E12.5,/)
  405    FORMAT (' '
     1        ,/,'   ENTER ONE TO PERFORM AN ANALYSIS OF THE MODEL FIT '
     2          ,'USING THE PRE-'
     3        ,/,'   DICTIONS OF THIS MODEL; ELSE ZERO.')
  410    FORMAT ('   ENTER ONE FOR PREDICTIONS BASED ON MAXIMUM LIKELIH'
     1          ,'OOD METHOD, OR'
     2        ,/,'   TWO FOR LEAST SQUARES METHOD.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        ZERO THE SUCCESSFULNESS  ELEMENTS OF THE PREDICTION DATA ARRAY.
         PREDAT(1,1) = 0.0
         PREDAT(2,1) = 0.0
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,301)
             WRITE (HIST,301)
             WRITE (   *,302)
             WRITE (HIST,302)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,303)
             WRITE (HIST,303)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
C
         IF (CONFLG.EQ.1) THEN
C            COMPUTE THE SUM OF THE TBF DATA AND THE SQUARED VALUE.
             SUMTBF = 0.0
             DO 1000 I = 0, (NS-1)
                 SUMTBF = SUMTBF + DAT(IDX+I)
 1000            CONTINUE
C            ENDDO
             SUMTB2 = SUMTBF**2
C
C            DOWHILE (ESF .NE. TERMINATION)
 1025            WRITE (   *,305)
                 WRITE (HIST,305)
                 READ  (   *,  *) ESF
                 WRITE (HIST,  *) ESF
                 IF ((ESF.GE.1) .AND. (ESF.LE.2)) THEN
                 PREDAT(ESF,1) = 0.0
                 WRITE (   *,310)
                 WRITE (HIST,310)
                 READ  (   *,  *) PHIIND
                 WRITE (HIST,  *) PHIIND
                 IF (ESF.EQ.1) THEN
C
C                    SET THE INITIAL ESTIMATES FOR  BETA(0) AND  BETA(1)
C                    FOR MAXIMUM LIKELIHOOD ESTIMATION.
                     ESF = 3
                     CALL SLAVMD
     G                           (DAT(IDX), ESF   , MAXIC , NS
     G                           ,3       , PHIIND, BETA
     Y                           ,ALPHA   , COUNT , RFLAG
     Y                           ,X       , Z     )
                     ESF = 1
                     BETA(1) = X(1)
                     BETA(2) = X(2)
                     IF (BETA(1).LT.BETA(2)) THEN
                         BETA(1) = 2.0 * REAL(PHIIND) * BETA(2)
                     ENDIF
C
C                    ALLOW  THE  USER TO REPLACE  THE INITIAL  ESTIMATES
C                    AND PROMPT FOR MAX. ITERATIONS.
                     WRITE (   *,315) BETA(1), BETA(2)
                     WRITE (HIST,315) BETA(1), BETA(2)
                     READ  (   *,  *) FLAG
                     WRITE (HIST,  *) FLAG
                     IF (FLAG.EQ.1) THEN
                         WRITE (   *,320)
                         WRITE (HIST,320)
                         READ  (   *,  *) BETA(1), BETA(2)
                         WRITE (HIST,  *) BETA(1), BETA(2)
                     ENDIF
                     WRITE (   *,325)
                     WRITE (HIST,325)
                     READ  (   *,  *) MAXIC
                     WRITE (HIST,  *) MAXIC
                 ENDIF
C
C                COMPUTE  MAXIMUM  LIKELIHOOD  OR  LEAST  SQUARES  MODEL
C                ESTIMATES.
                 CALL SLAVMD
     G                       (DAT(IDX), ESF   , MAXIC , NS    , 3
     G                       ,PHIIND  , BETA
     Y                       ,ALPHA   , COUNT , RFLAG , X     , Z     )
                 IF (RFLAG.EQ.1) THEN
C                    THE MAXIMUM  ITERATION COUNT  WAS REACHED  BEFORE A
C                    SOLUTION WAS FOUND;  COMPUTE EXTRA INITIAL AND CUR-
C                    RENT INTENSITY FUNCTIONS AND MTBNF. THE ML CAN ONLY
C                    HIT THIS LEG OF THE PROCESSING.
                     AM1  = ALPHA - 1.0
                     AM12 = AM1**2
                     IF (PHIIND.EQ.1) THEN
                         IIF = AM1 / X(1)
                         CIF = AM1
     1                       / SQRT(X(1)**2 + 2.0 * X(2) * SUMTBF * AM1)
                     ELSE
                         THRD = 1.0 / 3.0
                         TMP0 = AM1**THRD / (18.0 * X(2))**THRD
                         TMP1 = ( 4.0 * X(1)**3) / (9.0 * AM12 * X(2))
                         TMP2 = SQRT(SUMTB2 + TMP1)
                         TMP3 = (SUMTBF + TMP2)**THRD
                         TMP4 =  SUMTBF - TMP2
                         TMP5 = ABS(TMP4)**THRD
                         IF (TMP4.LT.0.0) THEN
                             TMP5 = 0.0 - TMP5
                         ENDIF
                         IIF  = 2.0 * TMP0 * TMP1**(-THRD)
                         CIF  = TMP0 / TMP2 * (TMP3 - TMP5)
                     ENDIF
                     MTBF = (X(1) + X(2) * REAL(NS+1)**PHIIND) / AM1
C
C                    OUTPUT THE FINAL ESTIMATES.
                     WRITE (   *,330)
                     WRITE (HIST,330)
                     WRITE (   *,335) ALPHA
                     WRITE (HIST,335) ALPHA
                     WRITE (   *,340) X(1), X(2), Z, IIF, CIF, MTBF
                     WRITE (HIST,340) X(1), X(2), Z, IIF, CIF, MTBF
                 ELSEIF (RFLAG.EQ.2) THEN
C                    THE  TRUST REGION  COULD  NOT BE  ADJUSTED PROPERLY
C                    (MAXIMUM LIKELIHOOD ONLY).
                     WRITE (   *,345)
                     WRITE (HIST,345)
                 ELSEIF (RFLAG.EQ.4) THEN
C                    THE ESTIMATES ARE DEEMED INVALID  SINCE BETA0+BETA1
C                    IS NOT POSITIVE.
                     WRITE (   *,350) LB(ESF), X(1), X(2)
                     WRITE (HIST,350) LB(ESF), X(1), X(2)
                 ELSE
C                    A SOLUTION WAS FOUND; COMPUTE THE EXTRA CURRENT IN-
C                    TENSITY FUNCTION AND MTBNF.
                     AM1  = ALPHA - 1.0
                     AM12 = AM1**2
                     IF (PHIIND.EQ.1) THEN
                         IIF = AM1 / X(1)
                         CIF = AM1
     1                       / SQRT(X(1)**2 + 2.0 * X(2) * SUMTBF * AM1)
                     ELSE
                         THRD = 1.0 / 3.0
                         TMP0 = AM1**THRD / (18.0 * X(2))**THRD
                         TMP1 = ( 4.0 * X(1)**3) / (9.0 * AM12 * X(2))
                         TMP2 = SQRT(SUMTB2 + TMP1)
                         TMP3 = (SUMTBF + TMP2)**THRD
                         TMP4 =  SUMTBF - TMP2
                         TMP5 = ABS(TMP4)**THRD
                         IF (TMP4.LT.0.0) THEN
                             TMP5 = 0.0 - TMP5
                         ENDIF
                         IIF  = 2.0 * TMP0 * TMP1**(-THRD)
                         CIF  = TMP0 / TMP2 * (TMP3 - TMP5)
                     ENDIF
                     MTBF = (X(1) + X(2) * REAL(NS+1)**PHIIND) / AM1
C
C                    OUTPUT THE RESULTS.
                     IF (ESF.EQ.1) THEN
                         WRITE (   *,355) COUNT
                         WRITE (HIST,355) COUNT
                         WRITE (   *,335) ALPHA
                         WRITE (HIST,335) ALPHA
                         WRITE (   *,340) X(1), X(2), Z, IIF, CIF, MTBF
                         WRITE (HIST,340) X(1), X(2), Z, IIF, CIF, MTBF
                     ELSE
                         WRITE (   *,360)
                         WRITE (HIST,360)
                         WRITE (   *,340) X(1), X(2), Z, IIF, CIF, MTBF
                         WRITE (HIST,340) X(1), X(2), Z, IIF, CIF, MTBF
                     ENDIF
C
C                    SET THE SUCCESSFULNESS  FLAG, AND STORE THE ALPHA &
C                    BETA ESTIMATES.
                     PREDAT(ESF,1) = 1.0
                     PREDAT(ESF,2) = X(1)
                     PREDAT(ESF,3) = X(2)
                     PREDAT(ESF,4) = ALPHA
                     PREDAT(ESF,5) = REAL(PHIIND)
C
                     IF (MTBF.LT.0.0) THEN
C                        FUTURE PREDICTIONS ARE NOT ALLOWED (MTBNF).
                         WRITE (   *,361)
                         WRITE (HIST,361)
                     ELSE
C                        DOWHILE (FLAG .NE. TERMINATION)
 1050                        IF (PHIIND.EQ.1) THEN
C                                LINEAR FUNCTION HAS THREE PREDICTIONS.
                                 WRITE (   *,365)
                                 WRITE (HIST,365)
                                 READ  (   *,  *) FLAG
                                 WRITE (HIST,  *) FLAG
                                 IF ((FLAG.LT.1) .OR. (FLAG.GT.3))
     1                                                        GO TO 1250
                             ELSE
C                                QUADRATIC FUNCTION ONLY HAS TWO.
                                 WRITE (   *,370)
                                 WRITE (HIST,370)
                                 READ  (   *,  *) FLAG
                                 WRITE (HIST,  *) FLAG
                                 IF ((FLAG.LT.1) .OR. (FLAG.GT.2))
     1                                                        GO TO 1250
                             ENDIF
C
                             IF     (FLAG.EQ.1) THEN
C                                MTBNF TO LOCATE K FAILURES.
                                 NPV = 1
C                                DOWHILE (PRINR .NE. TERMINATION)
 1075                                WRITE (   *,375)
                                     WRITE (HIST,375)
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1125
                                     PRINI = INT(PRINR + 0.5)
                                     PR2 = 0.0
                                     DO 1100 I = 1, PRINI
                                         PR2 = PR2 + ((X(1) + X(2)
     1                                       * REAL(NS + I)**PHIIND)
     2                                       / AM1)
 1100                                    CONTINUE
C                                    ENDDO
                                     WRITE (   *,380) PR2
                                     WRITE (HIST,380) PR2
                                     GO TO 1075
 1125                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.2 .AND. PHIIND.EQ.1) THEN
C                                TIME AND FAILURES  TO REACH  A  DESIRED
C                                INTENSITY FUNCTION.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1150                                WRITE (   *,385) CIF
                                     WRITE (HIST,385) CIF
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1175
                                     PR3 = AM1 / (2.0 * X(2))
     1                                   * (1.0 / PRINR**2
     2                                   -  1.0 / CIF**2)
                                     PR4 = AM1 / X(2)
     1                                   * (1.0 / PRINR - 1.0 / CIF)
                                     WRITE (   *,390) PR3, PR4
                                     WRITE (HIST,390) PR3, PR4
                                     GO TO 1150
 1175                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF((FLAG.EQ.3 .AND. PHIIND.EQ.1) .OR.
     1                              (FLAG.EQ.2 .AND. PHIIND.EQ.2))THEN
C                                FAILURES FOUND IN A SPECIFIED TIME.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1200                                WRITE (   *,395)
                                     WRITE (HIST,395)
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1225
                                     IF (PHIIND.EQ.1) THEN
                                         SUMTX = SUMTBF + PRINR
                                         PR5  = 1.0 / X(2)
     1                                        * (SQRT(X(1)**2 + 2.0
     2                                        * X(2) * AM1 * SUMTX)
     3                                        -  SQRT(X(1)**2 + 2.0
     4                                        * X(2) * AM1 * SUMTBF))
                                     ELSE
C                                        DEFINE THE CONSTANTS  AND FIRST
C                                        PART OF THE EQUATION.
                                         SUMTX  = SUMTBF + PRINR
                                         SUMTX2 = SUMTX**2
                                         TMP2   = 3.0 * TMP0
C
C                                        COMPUTE THE TOTAL NUMBER PART.
                                         TMP3   = SQRT(SUMTX2 + TMP1)
                                         TMP4   =(SUMTX + TMP3)**THRD
                                         TMP5   = SUMTX - TMP3
                                         TMP6   = ABS(TMP5)**THRD
                                         IF (TMP5.LT.0.0) THEN
                                             TMP6 = 0.0 - TMP6
                                         ENDIF
C
C                                        COMPUTE THE NUMBER FOUND PART.
                                         TMP7   = SQRT(SUMTB2 + TMP1)
                                         TMP8   =(SUMTBF + TMP7)**THRD
                                         TMP9   = SUMTBF - TMP7
                                         TMPA   = ABS(TMP9)**THRD
                                         IF (TMP9.LT.0.0) THEN
                                             TMPA = 0.0 - TMPA
                                         ENDIF

                                         PR5  = TMP2 * (TMP4 + TMP6)
     1                                        - TMP2 * (TMP8 + TMPA)
                                     ENDIF
                                     WRITE (   *,400) PR5
                                     WRITE (HIST,400) PR5
                                     GO TO 1200
 1225                                CONTINUE
C                                ENDWHILE
                             ENDIF
                             GO TO 1050
 1250                        CONTINUE
C                        ENDWHILE
                     ENDIF
                 ENDIF
                 GO TO 1025
                 ENDIF
C            ENDWHILE
C
             IF ((PREDAT(1,1).EQ.1.0) .OR. (PREDAT(2,1).EQ.1.0)) THEN
C                AT LEAST ONE EXECUTION  WAS SUCCESSFUL, PROMPT USER AND
C                SEE IF THE PREDICTED VECTOR CREATION IS DESIRED.
                 WRITE (   *,405)
                 WRITE (HIST,405)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    THE VECTOR OF PREDICTED DATA IS DESIRED.
                     IF (PREDAT(1,1).NE.PREDAT(2,1)) THEN
C                        ONLY ONE EXECUTION  WAS SUCCESSFUL;  SET ESF TO
C                        INDICATE ML OR LS.
                         ESF = PREDAT(1,1) * 1.0 + PREDAT(2,1) * 2.0
                     ELSE
C                        BOTH EXECUTIONS WERE  PERFORMED AND SUCCESSFUL;
C                        SEE WHICH THE USER DESIRES.
                         WRITE (   *,410)
                         WRITE (HIST,410)
                         READ  (   *,  *) ESF
                         WRITE (HIST,  *) ESF
                     ENDIF
                     PHIIND= INT(PREDAT(ESF,5))
C
C                    ACCESS THE SLAVPR ROUTINE TO GENERATE THE PREDICTED
C                    TBF DATA VECTOR.
                     CALL SLAVPR
     G                           (NS            , NS
     G                           ,PREDAT(ESF,4) , PREDAT(ESF,2)
     G                           ,PREDAT(ESF,3) , PHIIND
     G                           ,DAT(IDX)
     Y                           ,PRD           , D
     Y                           ,DFLG          , TM1C         )
C
C                    SET THE ESTIMATION SELECTION CODE TO INDICATE WHICH
C                    MODEL WAS RESPONSIBLE FOR  THE PREDICTIONS; AND SET
C                    ESD TO INDICATE WHICH DATA TYPE (WC OR CPU) WAS RE-
C                    SPONSIBLE.
                     ESC = 1
                     ESD = 1
                     IF (IDX.EQ.STR2) THEN
                         ESD = 2
                     ENDIF
C
C                    PERFORM ANALYSIS OF MODEL FIT.
                     CALL FITDRV
                 ENDIF
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE MDLMSA
     G                     (IDX   , NS    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLMSA
C    G                     (IDX   , NS    )
C
C   AUTHOR
C        PAUL J. KRAUS (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        TO PROVIDE THE ITERATIVE PROMPTS AND INPUTS FOR THE MUSA  LOGA-
C        RITHMIC POISSON EXECUTION TIME MODEL. ADDITIONALLY, THE ROUTINE
C        WILL PERFORM THE  ACCESS TO THE SMFLIB ROUTINES  NEEDED TO PER-
C        FORM THE MODEL EXECUTIONS AND PREDICTED DATA VECTOR CREATION.
C   DESCRIPTION
C        TWO SMFLIB ROUTINES  (SMSAMD & SMSAPR) WILL BE USED WITHIN THIS
C        ROUTINE.
C
C        THE  FIRST ROUTINE  (SMSAMD)  PROVIDES THE  CALCULATIONS OF THE
C        ESTIMATES IN THE MUSA LOGARITHMIC POISSON EXECUTION TIME MODEL.
C
C        ONCE THE  EXECUTION OF  SMSAMD IS  PERFORMED, THE RETURN STATUS
C        FLAG (RFLAG) WILL INDICATE THE  SUCCESSFULNESS OF THE MODEL ES-
C        TIMATION PROCESS. THE FOLLOWING VALUES ARE POSSIBLE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C
C        FOR AN RFLAG OF 0 THE COMPLETE MODEL ESTIMATES WILL BE LISTED.
C
C        THE SECOND ROUTINE (SMSAPR) PROVIDES FUTURE PREDICTIONS &  GEN-
C        ERATES THE VECTOR OF PREDICTED TBF DATA.
C
C        THE SMSAPR ROUTINE WILL ALSO DETERMINE THE GOODNESS-OF-FIT WHEN
C        CREATING THE PREDICTED VECTOR; ALTHOUGH THE VALUES FOR THE KOL-
C        MOGOROV DISTANCE STATISTIC AND THE DETERMINED SIGNIFICANCE WILL
C        NOT BE OUTPUT UNTIL THE USER ENTERS THE ANALYSIS DRIVER.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C        YIELDED
C             D      (R) = CALCULATED KOLMOGOROV DISTANCE
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          GOODNESS-OF-FIT CALCULATION
C             XPRD   (R) = FUTURE PREDICTIONS (NUMBER AND 95% C.I.)
C   ARGUMENT LIST
C        GIVEN
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C   LOCAL GLOSSARY
C             BETA0  (R) = BETA(0) CONSTANT OF THE MODEL
C             BETA1  (R) = BETA(1) CONSTANT OF THE MODEL
C             CIF    (R) = CURRENT INTENSITY FUNCTION
C             CMTBF  (R) = CURRENT MTBNF
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             DIF    (R) = DESIRED INTENSITY FUNCTION
C             EXTM   (R) = ADDITIONAL EXECUTION TIME
C             FLAG   (I) = OPTION SELECTION FLAG
C             IIF    (R) = INITIAL INTENSITY FUNCTION
C             NPV    (I) = NUMBER OF PREDICTED VALUES FLAG
C             PRINI  (I) = PREDICTION INPUT VALUE FOR PR2
C             PRINR  (R) = PREDICTION INPUT VALUE FOR PR1, PR3 - PR5
C             PR1    (R) = FUTURE RELIABILTIY FOR SPECIFIED TIME
C             PR2    (R) = MTBNF TO LOCATE NEXT K FAILURES
C             PR3    (R) = ADDITIONAL TESTING TIME TO REACH DIF (N/A)
C             PR4    (R) = ADDITIONAL FAILURES FOUND DURING PR3 (N/A)
C             PR5    (R) = FUTURE FAILURES WITHIN A SPECIFIED TIME (N/A)
C             PURLVL (R) = PURIFICATION LEVEL
C             RFLAG  (I) = RETURN STATUS FLAG
C             SUMTBF (R) = SUM OF THE TBF DATA, INCLUDING  ANY  FAILURE-
C                          FREE TEST TIME
C             WT     (I) = FLAG TO PAUSE THE ASSUMPTIONS LIST OUTPUT
C             XLTM   (R) = FAILURE-FREE TEST TIME
C             ZTEST  (R) = ZERO TEST CONSTANT
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
C               /PRECOM/      PREDICTION DATA STORAGE COMMON
         COMMON /PRECOM/      PRD(END1)     ,XPRD(3)       ,D
     1                       ,DFLG          ,ESC           ,ESD
     2                       ,EST
         INTEGER              DFLG          ,ESC           ,ESD
     1                       ,EST
C
C               /TMPCOM/      TEMPORARY STORAGE COMMON
         COMMON /TMPCOM/      TM1C(END1)    ,TM2C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              IDX           ,NS
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CONFLG        ,FLAG          ,I
     1                       ,PRINI         ,NPV           ,RFLAG
     2                       ,WT
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR MUSA (LOG) MODEL DESCRIPTION; ELSE Z'
     1          ,'ERO.')
  301    FORMAT (' '
     1        ,/,'    THE MUSA LOGARITHMIC POISSON EXECUTION TIME MODEL'
     2          ,' ASSUMPTIONS'
     3       ,//,'     1. THE SOFTWARE IS OPERATED IN A SIMILAR MANNER '
     4          ,'AS THE ANTICI-'
     5        ,/,'        PATED OPERATIONAL USAGE.'
     6       ,//,'     2. ALL FAULTS ARE EQUALLY LIKELY TO OCCUR  AND A'
     7          ,'RE INDEPENDENT'
     8        ,/,'        OF EACH OTHER.'
     9       ,//,'     3. THE EXPECTED NUMBER OF FAULTS IS A LOGARITHMI'
     A          ,'C FUNCTION  OF'
     B        ,/,'        TIME.'
     C       ,//,'     4. THE FAILURE INTENSITY DECREASES EXPONENTIALLY'
     D          ,' WITH THE  EX-')
  302    FORMAT ('        PECTED FAILURES EXPERIENCED.'
     1       ,//,'     5. THE  SOFTWARE WILL  EXPERIENCE AN  INFINITE N'
     2          ,'UMBER OF FAIL-'
     3        ,/,'        URES.'
     4       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  303    FORMAT ('    MODEL DATA REQUIREMENTS'
     1       ,//,'     1. THE TIME-BETWEEN-FAILURES AS MEASURED IN COMP'
     2          ,'UTER CPU TIME.'
     3        ,/,'        THE DATA SHOULD HAVE BEEN ENTERED VIA THE INP'
     4          ,'UT MODULE.'
     5       ,//,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     6          ,'HE APPROPRIATE'
     7        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     8          ,'EL;  OTHERWISE'
     9        ,/,'    ZERO TO RETURN TO THE MENU OF MODELS.')
  305    FORMAT (' '
     1        ,/,' **THE DATA ARE NOT APPROPRIATE FOR THE MODEL.')
  310    FORMAT (' '
     1        ,/,'   THE MAXIMUM LIKELIHOOD ESTIMATES ARE:'
     2        ,/,'    BETA0 PARAMETER          ',E12.5
     3        ,/,'    BETA1 PARAMETER          ',E12.5
     4        ,/,'    INIT. INTENSITY FUNCTION ',E12.5
     5        ,/,'    CUR.  INTENSITY FUNCTION ',E12.5)
  311    FORMAT ('    CUR.  MTBNF              ',E12.5)
  312    FORMAT ('    PURIFICATION LEVEL       ',E12.5)
  315    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS  AND PREDICTED TBFS  ARE NOT AL'
     2          ,'LOWED  BECAUSE'
     3        ,/,'   THE BETA0 PARAMETER OF THE MODEL IS LESS THAN OR E'
     4          ,'QUAL TO ONE.')
  316    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   CURRENT MTBNF IS LESS THAN ZERO.')
  320    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE EXPECTED RELIABILITY FOR A SPECIFIED TIME'
     3        ,/,'    2) THE TOTAL MTBNF FOR THE NEXT K FAILURES'
     4        ,/,'    3) THE TIME AND NUMBER OF FAILURES TO REACH A DES'
     5          ,'IRED INTENSITY'
     6        ,/,'       FUNCTION'
     7        ,/,'    4) THE NUMBER OF FAILURES EXPECTED IN A SPECIFIED'
     8          ,' TIME'
     9        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     A          ,'S.')
  325    FORMAT ('    ENTER LENGTH OF THE PERIOD, OR ZERO TO END.')
  330    FORMAT ('    FUTURE RELIABILITY      ',E12.5,/)
  335    FORMAT ('    ENTER VALUE OF K, OR ZERO TO END.')
  340    FORMAT ('    MTBNF EXPECTED          ',E12.5,/)
  345    FORMAT ('    ENTER INTENSITY FUNC. (LESS THAN',E12.5,'), OR ZE'
     1          ,'RO TO END.')
  350    FORMAT ('    ADDITIONAL TESTING TIME ',E12.5
     1        ,/,'    # OF FAILURES EXPECTED  ',E12.5,/)
  355    FORMAT ('    # OF FAILURES EXPECTED  ',E12.5,/)
  360    FORMAT (' '
     1        ,/,'   ENTER ONE TO PERFORM AN ANALYSIS OF THE MODEL FIT '
     2          ,'USING THE PRE-'
     3        ,/,'   DICTIONS OF THIS MODEL; ELSE ZERO.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE ZERO TEST CONSTANT.
         ZTEST = 0.0000000001
C
C        SET XLTM TO CONTAIN THE FINAL FAILURE-FREE TESTING TIME.
         XLTM = 0.0
         IF (((IDX.EQ.   1) .AND. (PRC(1,2).EQ.0))  .OR.
     1       ((IDX.EQ.STR2) .AND. (PRC(2,2).EQ.0))) THEN
             XLTM = DAT(IDX + NS)
         ENDIF
C
C        CALCULATE THE SUM  OF THE TBF DATA,  INCLUDING ANY FAILURE-FREE
C        TEST TIME.
         SUMTBF = 0.0
         DO 1000 I = 0, (NS-1)
             SUMTBF = SUMTBF + DAT(IDX+I)
 1000        CONTINUE
C        ENDDO
         SUMTBF = SUMTBF + XLTM
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,301)
             WRITE (HIST,301)
             WRITE (   *,302)
             WRITE (HIST,302)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,303)
             WRITE (HIST,303)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
         IF (CONFLG.EQ.1) THEN
             CALL SMSAMD
     G                   (NS    , DAT(IDX), SUMTBF
     Y                   ,BETA0 , BETA1   , CIF   , IIF
     Y                   ,RFLAG )
             IF (RFLAG.EQ.3) THEN
C                THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
                 WRITE (   *,305)
                 WRITE (HIST,305)
             ELSE
C                A SOLUTION WAS FOUND;   REMOVE FAILURE-FREE  TEST TIME,
C                YIELDING THE TIME TO THE LAST FAILURE.
                 SUMTBF = SUMTBF - XLTM
C
C                OUTPUT THE RESULTS.
                 WRITE (   *,310) BETA0 , BETA1 , IIF   , CIF
                 WRITE (HIST,310) BETA0 , BETA1 , IIF   , CIF
C
                 IF (BETA0.GT.1.0) THEN
C                    COMPUTE EXTRA CURRENT MTBNF AND ADD TO PRINTOUT.
                     CMTBF = (1.0 / BETA1)
     1                     * (((BETA0 / (BETA0 - 1.0))**(NS+1)) - 1.0)
     2                     - SUMTBF
                     WRITE (   *,311) CMTBF
                     WRITE (HIST,311) CMTBF
                 ENDIF
C
C                COMPUTE EXTRA PURIFICATION LEVEL AND ADD TO PRINTOUT.
                 PURLVL = (IIF - IIF * EXP(-BETA1 * SUMTBF)) / IIF
                 WRITE (   *,312) PURLVL
                 WRITE (HIST,312) PURLVL
C
                 IF (BETA0.LE.1.0) THEN
C                    FUTURE PREDICTIONS ARE NOT ALLOWED (BETA0).
                     WRITE (   *,315)
                     WRITE (HIST,315)
                 ENDIF
                 IF (CMTBF.LT.0.0) THEN
C                    FUTURE PREDICTIONS ARE NOT ALLOWED (MTBNF).
                     WRITE (   *,316)
                     WRITE (HIST,316)
                 ENDIF
                 IF ((BETA0.GT.1.0) .AND. (CMTBF.GE.0.0)) THEN
C                    DOWHILE (FLAG .NE. TERMINATION)
 1025                    WRITE (   *,320)
                         WRITE (HIST,320)
                         READ  (   *,  *) FLAG
                         WRITE (HIST,  *) FLAG
                         IF ((FLAG.LT.1) .OR. (FLAG.GT.4)) GO TO 1250
C
                         IF     (FLAG.EQ.1) THEN
C                            FUTURE RELIABILITY FOR SPECIFIED TIME.
C                            DOWHILE (PRINR .NE. TERMINATION)
 1050                            WRITE (   *,325)
                                 WRITE (HIST,325)
                                 READ  (   *,  *) PRINR
                                 WRITE (HIST,  *) PRINR
                                 IF (PRINR.EQ.0.0) GO TO 1075
                                 PR1 = ((BETA1 *  SUMTBF          +1.0)
     1                               /  (BETA1 * (SUMTBF + PRINR) +1.0))
     2                               ** BETA0
                                 WRITE (   *,330) PR1
                                 WRITE (HIST,330) PR1
                                 GO TO 1050
 1075                            CONTINUE
C                            ENDWHILE
C
                         ELSEIF (FLAG.EQ.2) THEN
C                            MTBNF TO LOCATE K FAILURES.
C                            DOWHILE (PRINR .NE. TERMINATION)
 1100                            WRITE (   *,335)
                                 WRITE (HIST,335)
                                 READ  (   *,  *) PRINR
                                 WRITE (HIST,  *) PRINR
                                 IF (PRINR.EQ.0.0) GO TO 1125
                                 PRINI = INT(PRINR + 0.5)
                                 PR2 = ((1.0 / BETA1)
     1                               * ((BETA0 / (BETA0 - 1.0))
     2                               **(NS+PRINI) - 1.0)) - SUMTBF
                                 WRITE (   *,340) PR2
                                 WRITE (HIST,340) PR2
                                 GO TO 1100
 1125                            CONTINUE
C                            ENDWHILE
C
                         ELSEIF (FLAG.EQ.3) THEN
C                            TIME AND FAILURES TO REACH A DESIRED INTEN-
C                            SITY FUNCTION.
                             NPV = 2
C                            DOWHILE (DIF .NE. TERMINATION)
 1150                            WRITE (   *,345) CIF
                                 WRITE (HIST,345) CIF
                                 READ  (   *,  *) DIF
                                 WRITE (HIST,  *) DIF
                                 IF (DIF.EQ.0.0) GO TO 1175
                                 CALL SMSAPR
     G                                       (NPV   , NS     , CIF
     G                                       ,DIF   , BETA0  , BETA1
     G                                       ,EXTM  , DAT(IDX)
     Y                                       ,XPRD  , D      , DFLG
     Y                                       ,TM1C  )
                                 WRITE (   *,350) XPRD(2), XPRD(1)
                                 WRITE (HIST,350) XPRD(2), XPRD(1)
                                 GO TO 1150
 1175                            CONTINUE
C                            ENDWHILE
C
                         ELSEIF (FLAG.EQ.4) THEN
C                            FAILURES FOUND IN A SPECIFIED TIME.
                             NPV = 1
C                            DOWHILE (EXTM .NE. TERMINATION)
 1200                            WRITE (   *,325)
                                 WRITE (HIST,325)
                                 READ  (   *,  *) EXTM
                                 WRITE (HIST,  *) EXTM
                                 IF (EXTM.EQ.0.0) GO TO 1225
                                 EXTM = EXTM + SUMTBF
                                 CALL SMSAPR
     G                                       (NPV    , NS      , CIF
     G                                       ,DIF    , BETA0   , BETA1
     G                                       ,EXTM   , DAT(IDX)
     Y                                       ,XPRD   , D       , DFLG
     Y                                       ,TM1C)
                                 WRITE (   *,355) XPRD(1)
                                 WRITE (HIST,355) XPRD(1)
                                 GO TO 1200
 1225                            CONTINUE
C                            ENDWHILE
                         ENDIF
                         GO TO 1025
 1250                    CONTINUE
C                    ENDWHILE
                 ENDIF
             ENDIF
C
             IF ((RFLAG.EQ.0) .AND. (BETA0.GT.1.0)) THEN
C                THE SMSAMD EXECUTION IS SUCCESSFUL AND THE ESTIMATE AND
C                PARAMETER ALLOW FOR THE COMPUTATION OF THE TBF VECTOR.
                 WRITE (   *,360)
                 WRITE (HIST,360)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    ACCESS THE SMSAPR ROUTINE TO GENERATE THE PREDICTED
C                    TBF DATA VECTOR.
                     CALL SMSAPR
     G                           (NS    , NS    , CIF    , DIF   , BETA0
     G                           ,BETA1 , EXTM  , DAT(IDX)
     Y                           ,PRD   , D     , DFLG   , TM1C  )
C
C                    SET THE ESTIMATION SELECTION CODE TO INDICATE WHICH
C                    MODEL WAS RESPONSIBLE FOR  THE PREDICTIONS; AND SET
C                    ESD TO INDICATE WHICH DATA TYPE (WC OR CPU) WAS RE-
C                    SPONSIBLE.
                     ESC = 5
                     ESD = 1
                     IF (IDX.EQ.STR2) THEN
                         ESD = 2
                     ENDIF
C
C                    PERFORM ANALYSIS OF MODEL FIT.
                     CALL FITDRV
                 ENDIF
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE MDLMSC
     G                     (IDX   , NS    , CMTBF , IMTBF , TNOF  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLMCS
C    G                     (IDX   , NS    , CMTBF , IMTBF , TNOF  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE THE ITERATIVE PROMPTS AND INPUTS FOR THE MUSA CALEN-
C        DAR TIME MODEL COMPONENT, AND ACCESS THE  SMFLIB ROUTINE NEEDED
C        TO PERFORM THE EXECUTION.
C   DESCRIPTION
C        THE SMUSCT ROUTINE PROVIDES  THE CALCULATIONS  FOR THE CALENDAR
C        TIME COMPONENT OF THE MUSA (BASIC) MODEL.  THROUGH EXECUTION OF
C        THE ROUTINE, AN ESTIMATE OF THE NUMBER OF HOURS NEEDED TO REACH
C        A SPECIFIED DESIRED MTBNF CAN BE MADE.  THIS ESTIMATE IS TO IN-
C        CLUDE ALL FACETS OF TESTING,  INCLUDING PERSONNEL  AND COMPUTER
C        USAGE DURING THE STAGES OF ERROR DETECTION, IDENTIFICATION, AND
C        CORRECTION.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             CNV    (R) = TBF DATA CONVERSION CONSTANTS
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C   ARGUMENT LIST
C        GIVEN
C             CMTBF  (R) = CURRENT MTBNF
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             IMTBF  (R) = INITIAL MTBNF
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C             TNOF   (R) = TOTAL NUMBER OF FAILURES
C   LOCAL GLOSSARY
C             CMTBFH (R) = CURRENT MTBNF (CONVERTED TO HOURS)
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             DMTBFH (R) = DESIRED MTBNF (IN HOURS)
C             FLAG   (I) = OPTION SELECTION FLAG
C             IMTBFH (R) = INITIAL MTBNF (CONVERTED TO HOURS)
C             IUNIT  (I) = UNITS OF THE PROCESSED DATA
C             PC     (R) = NUMBER OF COMPUTER SHIFTS
C             PF     (R) = NUMBER OF FAILURE CORRECTION PERSONNEL
C             PI     (R) = NUMBER OF FAILURE IDENTIFICATION PERS.
C             PMQ    (R) = PROB. THAT QUEUE LENGTH .LE. MAX. QUEUE
C             RHOC   (R) = COMPUTER UTILIZATION FACTOR
C             TC     (R) = COMPUTER WC EXPENDED PER UNIT OF CP
C             TI     (R) = IDENTIFICATION TIME PER CPU TIME
C             WT     (I) = FLAG TO PAUSE THE ASSUMPTIONS LIST OUTPUT
C             XMC    (R) = COMPUTER TIME EXPENDED PER FAILURE
C             XMF    (R) = CORRECTION TIME PER FAILURE
C             XMI    (R) = IDENTIFICATION TIME PER FAILURE
C             XMQ    (R) = MAXIMUM QUEUE LENGTH PER DEBUGGER
C             XINT   (R) = ADDITIONAL HOURS TO MEET DESIRED MTBNF
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
         INTEGER              END2
         PARAMETER           (END2   =   2000)
C
C                             GLOBAL SPECIFICATIONS
C
C               /CNVCOM/      TBF DATA CONVERSION CONSTANTS COMMON
         COMMON /CNVCOM/      CNV(6)
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
         INTEGER              IDX           ,NS
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CONFLG        ,FLAG          ,IUNIT
     1                       ,WT
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR CALENDAR TIME COMPONENT DESCRIPTION;'
     1          ,' ELSE ZERO.')
  305    FORMAT (' '
     1        ,/,'    CALENDAR TIME MODEL ASSUMPTIONS'
     2       ,//,'     1. THE QUANTITIES OF THE  AVAILABLE RESOURCES (F'
     3          ,'AILURE-IDENTI-'
     4        ,/,'        FICATION PERSONNEL, FAILURE-CORRECTION PERSON'
     5          ,'NEL,  AND COM-'
     6        ,/,'        PUTER TIME) ARE CONSTANT OVER TESTING SEGMENT'
     7          ,'S.'
     8       ,//,'     2. RESOURCE EXPENDITURES FOR THE ITH  RESOURCE A'
     9          ,'SSOCIATED WITH'
     A        ,/,'        A CHANGE IN MTBNF FROM T1 TO T2 CAN BE APPROX'
     B          ,'IMATED BY'
     C       ,//,'            THETA(I) * DELTA(TAU) + MU(I) * DELTA(M)'
     D       ,//,'        WHERE'
     E       ,//,'            THETA(I)  - EXECUTION TIME  COEFFICIENT O'
     F          ,'F RESOURCE EX-')
  310    FORMAT ('                        PENDITURE FOR THE ITH RESOURC'
     1          ,'E'
     2        ,/,'            DELTA(TAU)- INCREMENT OF EXECUTION TIME A'
     3          ,'SSOCIATED WITH'
     4        ,/,'                        THE CHANGE IN MTBNF'
     5       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  315    FORMAT ('            MU(I)     - FAILURE COEFFICIENT OF RESOUR'
     1          ,'CE EXPENDITURE'
     2        ,/,'                        FOR THE ITH RESOURCE'
     3        ,/,'            DELTA(M)  - THE INCREMENT OF  FAILURES EX'
     4          ,'PERIENCED WITH'
     5        ,/,'                        THE CHANGE IN MTBNF'
     6       ,//,'     3. FAILURE IDENTIFICATION PERSONNEL CAN BE FULLY'
     7          ,'  UTILIZED AND'
     8        ,/,'        COMPUTER UTILIZATION IS CONSTANT.'
     9       ,//,'     4. FAILURE-CORRECTION PERSONNEL  UTILIZATION IS '
     A          ,'ESTABLISHED BY')
  320    FORMAT ('        LIMITATION OF  ERROR QUEUE LENGTH  FOR ANY  D'
     1          ,'EBUGGER. ERROR'
     2        ,/,'        QUEUE LENGTH IS DETERMINED, IN TURN, BY ASSUM'
     3          ,'ING  THE ERROR'
     4        ,/,'        CORRECTION PROCESS IS POISSON  AND THAT THE F'
     5          ,'AILURE-CORREC-'
     6        ,/,'        TION PERSONNEL ARE RANDOMLY ASSIGNED ERRORS O'
     7          ,'VER TIME.'
     8       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  325    FORMAT ('    MODEL DATA REQUIREMENTS'
     1       ,//,'     1. THE AVAILABLE RESOURCES FOR BOTH FAILURE-IDEN'
     2          ,'TIFICATION AND'
     3        ,/,'        FAILURE-CORRECTION PERSONNEL.'
     4       ,//,'     2. THE NUMBER OF COMPUTER SHIFTS.'
     5       ,//,'     3. THE EXECUTION TIME  COEFFICIENT OF RESOURCE E'
     6          ,'XPENDITURE FOR'
     7        ,/,'        EACH RESOURCE.'
     8       ,//,'     4. THE FAILURE  COEFFICIENT OF  RESOURCE EXPENDI'
     9          ,'TURE  FOR EACH'
     A        ,/,'        RESOURCE.'
     B       ,//,'     5. THE MAXIMUM ERROR QUEUE LENGTH FOR EACH DEBUG'
     C          ,'GER.'
     D       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  330    FORMAT ('     6. THE PROBABILITY  THAT THE ERROR  QUEUE  LENGT'
     1          ,'H IS NO LARGER'
     2        ,/,'        THAN THE MAXIMUM QUEUE LENGTH.'
     3       ,//,'     7. THE COMPUTER UTILIZATION RATIO.'
     4       ,//,'     8. THE DESIRED MTBNF.'
     5       ,//,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     6          ,'HE APPROPRIATE'
     7        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     8          ,'EL;  OTHERWISE'
     9        ,/,'    ZERO TO SKIP THE CALENDAR TIME MODEL.')
  335    FORMAT (' '
     1        ,/,'     ENTER THE APPROPRIATE DATA AFTER EACH PROMPT'
     2        ,/,'      1 NUMBER OF FAILURE IDENTIFICATION PERSONNEL.')
  340    FORMAT ('      2 NUMBER OF FAILURE CORRECTION PERSONNEL.')
  345    FORMAT ('      3 NUMBER OF COMPUTER SHIFTS (EXAMPLE - IF THE W'
     1          ,'ORK WEEK IS 40'
     2        ,/,'        HRS. AND THE COMPUTER IS AVAILABLE 80 HRS. A '
     3          ,'WEEK, THEN EN-'
     4        ,/,'        TER THE NUMBER 2).')
  350    FORMAT ('      4 AVERAGE  AMOUNT OF COMPUTER  (WALLCLOCK)  TIM'
     1          ,'E EXPENDED PER'
     2        ,/,'        UNIT OF EXECUTION TIME (CPU).')
  355    FORMAT ('      5 AVERAGE  AMOUNT OF  IDENTIFICATION  WORK  TIM'
     1          ,'E EXPENDED PER'
     2        ,/,'        UNIT OF EXECUTION TIME.')
  360    FORMAT ('      6 AVERAGE AMOUNT OF COMPUTER TIME (HRS) EXPENDE'
     1          ,'D PER FAILURE.')
  365    FORMAT ('      7 AVERAGE  AMOUNT OF  IDENTIFICATION WORK  (HRS'
     1          ,') REQUIRED PER'
     2        ,/,'        FAILURE.')
  370    FORMAT ('      8 AVERAGE  AMOUNT OF CORRECTION WORK (HRS) REQU'
     1          ,'IRED PER FAIL-'
     2        ,/,'        URE.')
  375    FORMAT ('      9 MAXIMUM ERROR QUEUE LENGTH FOR A DEBUGGER.')
  380    FORMAT ('     10 ASSOCIATED  PROBABILITY THAT  THE QUEUE  LENG'
     1          ,'TH  WILL BE NO'
     2        ,/,'        LARGER THAN VALUE JUST INPUT.')
  385    FORMAT ('     11 COMPUTER UTILIZATION FACTOR (0.LT.X.LE.1).')
  390    FORMAT ('     12 ENTER A DESIRED MTBNF  FOR THE PROGRAM (IN HO'
     1          ,'URS AND LARGER'
     2        ,/,'        THAN THE CURRENT ESTIMATED VALUE OF',E12.5,')'
     3          ,'.')
  395    FORMAT (' '
     1        ,/,'     ADDITIONAL HOURS TO MTBNF',E12.5
     2       ,//,'     ENTER ONE TO USE A DIFFERENT SPECIFIED MTBNF; EL'
     3          ,'SE ZERO.')
  400    FORMAT (' '
     1        ,/,'     ENTER ONE FOR ALL NEW CALENDAR TIME MODEL INPUTS'
     2          ,'; ELSE ZERO.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,305)
             WRITE (HIST,305)
             WRITE (   *,310)
             WRITE (HIST,310)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,315)
             WRITE (HIST,315)
             WRITE (   *,320)
             WRITE (HIST,320)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,325)
             WRITE (HIST,325)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,330)
             WRITE (HIST,330)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
C
         IF (CONFLG.EQ.1) THEN
C            PERFORM THE UNIT CONVERSIONS TO HOURS. START BY SETTING THE
C            CURRENT UNIT FLAG.
             IF (IDX.EQ.1) THEN
                 IUNIT = PRC(1,3)
             ELSE
                 IUNIT = PRC(2,3)
             ENDIF
C
             CMTBFH = CMTBF
             IMTBFH = IMTBF
C
             IF (IUNIT.NE.3) THEN
C                DOUNTIL (UNITS DO REPRESENT HOURS)
 1000                IF (IUNIT.GT.3) THEN
                         IUNIT  = IUNIT  - 1
                         CMTBFH = CMTBFH * CNV(IUNIT)
                         IMTBFH = IMTBFH * CNV(IUNIT)
                     ELSE
                         CMTBFH = CMTBFH / CNV(IUNIT)
                         IMTBFH = IMTBFH / CNV(IUNIT)
                         IUNIT  = IUNIT  + 1
                     ENDIF
                     IF (IUNIT.NE.3) GO TO 1000
C                ENDUNTIL
             ENDIF
C
C            DOWHILE (FLAG .EQ. 1)
 1025            WRITE (   *,335)
                 WRITE (HIST,335)
                 READ  (   *,  *) PI
                 WRITE (HIST,  *) PI
                 WRITE (   *,340)
                 WRITE (HIST,340)
                 READ  (   *,  *) PF
                 WRITE (HIST,  *) PF
                 WRITE (   *,345)
                 WRITE (HIST,345)
                 READ  (   *,  *) PC
                 WRITE (HIST,  *) PC
                 WRITE (   *,350)
                 WRITE (HIST,350)
                 READ  (   *,  *) TC
                 WRITE (HIST,  *) TC
                 WRITE (   *,355)
                 WRITE (HIST,355)
                 READ  (   *,  *) TI
                 WRITE (HIST,  *) TI
                 WRITE (   *,360)
                 WRITE (HIST,360)
                 READ  (   *,  *) XMC
                 WRITE (HIST,  *) XMC
                 WRITE (   *,365)
                 WRITE (HIST,365)
                 READ  (   *,  *) XMI
                 WRITE (HIST,  *) XMI
                 WRITE (   *,370)
                 WRITE (HIST,370)
                 READ  (   *,  *) XMF
                 WRITE (HIST,  *) XMF
                 WRITE (   *,375)
                 WRITE (HIST,375)
                 READ  (   *,  *) XMQ
                 WRITE (HIST,  *) XMQ
                 WRITE (   *,380)
                 WRITE (HIST,380)
                 READ  (   *,  *) PMQ
                 WRITE (HIST,  *) PMQ
                 WRITE (   *,385)
                 WRITE (HIST,385)
                 READ  (   *,  *) RHOC
                 WRITE (HIST,  *) RHOC
C                DOWHILE (FLAG .EQ. 1)
 1050                WRITE (   *,390) CMTBFH
                     WRITE (HIST,390) CMTBFH
                     READ  (   *,  *) DMTBFH
                     WRITE (HIST,  *) DMTBFH
                     CALL SMUSCT
     G                           (TNOF  , CMTBFH , DMTBFH , IMTBFH
     G                           ,NS    , PC     , PF     , PI
     G                           ,PMQ   , RHOC   , TC     , TI
     G                           ,XMC   , XMF    , XMI    , XMQ
     Y                           ,XINT  )
                     WRITE (   *,395) XINT
                     WRITE (HIST,395) XINT
                     READ  (   *,  *) FLAG
                     WRITE (HIST,  *) FLAG
                     IF (FLAG.EQ.1) GO TO 1050
C                ENDWHILE
                 WRITE (   *,400)
                 WRITE (HIST,400)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) GO TO 1025
C            ENDWHILE
         ENDIF
         RETURN
         END
         SUBROUTINE MDLMUS
     G                     (IDX   , NS    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLMUS
C    G                     (IDX   , NS    )
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
C        TO PROVIDE THE ITERATIVE PROMPTS AND INPUTS FOR THE MUSA  BASIC
C        EXECUTION TIME MODEL AND ITS SECONDARY CALENDAR TIME MODEL COM-
C        PONENT.   ADDITIONALLY, THE ROUTINE  WILL PERFORM THE ACCESS TO
C        THE SMFLIB ROUTINES NEEDED TO  PERFORM THE MODEL EXECUTIONS AND
C        PREDICTED DATA VECTOR CREATION.
C   DESCRIPTION
C        THREE SMFLIB ROUTINES  (SMUSMD, SMUSCT, & SMUSPR)  WILL BE USED
C        WITHIN THIS ROUTINE.
C
C        THE  FIRST ROUTINE  (SMUSMD)  PROVIDES THE  CALCULATIONS OF THE
C        ESTIMATES IN THE MUSA BASIC EXECUTION TIME MODEL.
C
C        ONCE THE  EXECUTION OF  SMUSMD IS PERFORMED,  THE RETURN STATUS
C        FLAG (RFLAG) WILL INDICATE THE SUCCESSFULNESS  OF THE MODEL ES-
C        TIMATION PROCESS. THE FOLLOWING VALUES ARE POSSIBLE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS THAN THE  OBSERVED
C                 FAULTS TO DATE
C
C        FOR AN RFLAG OF 0 THE COMPLETE MODEL ESTIMATES WILL BE LISTED.
C
C        PROVIDED THAT THE  RFLAG FROM  THE SMUSMD  INDICATED SUCCESSFUL
C        CONVERGENCE, THE NEXT TWO SMFLIB ROUTINES (SMUSCT & SMUSPR) CAN
C        BE EXECUTED.  THE SMUSCT ROUTINE  PROVIDES THE CALCULATIONS FOR
C        THE CALENDAR TIME  COMPONENT OF THE MUSA MODEL.  THROUGH EXECU-
C        TION OF THIS ROUTINE, AN ESTIMATE OF THE NUMBER OF HOURS NEEDED
C        TO REACH A SPECIFIED DESIRED MTBNF CAN BE MADE. THE ESTIMATE IS
C        TO INCLUDE ALL FACETS OF TESTING,  INCLUDING PERSONNEL AND COM-
C        PUTER USAGE DURING THE STAGES OF ERROR DETECTION, ERROR IDENTI-
C        FICATION, AND ERROR CORRECTION.
C
C        THE THIRD ROUTINE (SMUSPR) PROVIDES FUTURE PREDICTIONS AND GEN-
C        ERATES THE VECTOR OF PREDICTED TBF DATA.
C
C        THE SMUSPR ROUTINE WILL ALSO DETERMINE THE GOODNESS-OF-FIT WHEN
C        CREATING THE PREDICTED VECTOR; ALTHOUGH THE VALUES FOR THE KOL-
C        MOGOROV DISTANCE STATISTIC AND THE DETERMINED SIGNIFICANCE WILL
C        NOT BE OUTPUT UNTIL THE USER ENTERS THE ANALYSIS DRIVER.
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
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C        YIELDED
C             D      (R) = CALCULATED KOLMOGOROV DISTANCE
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          GOODNESS-OF-FIT CALCULATION
C             XPRD   (R) = FUTURE PREDICTIONS (NUMBER AND 95% C.I.)
C   ARGUMENT LIST
C        GIVEN
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C   LOCAL GLOSSARY
C             BETA0  (R) = RECLAIMED MODEL PARAMETER FOR PREDICTIONS
C             BETA1  (R) = RECLAIMED MODEL PARAMETER FOR PREDICTIONS
C             CIF    (R) = CURRENT INTENSITY FUNCTION
C             CMTBF  (R) = CURRENT MTBNF
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             DMTBF  (R) = DESIRED MTBNF FOR FUTURE RELIABILITY PORTION
C             ECR    (R) = ESTIMATE OF CURRENT RELIABILITY
C             FLAG   (I) = OPTION SELECTION FLAG
C             IFLG   (I) = FLAG SIGNALING C.I. AVAILABILITY IN IMTBF
C             IIF    (R) = INITIAL INTENSITY FUNCTION
C             IMTBF  (R) = ARRAY OF INITIAL MTBNF AND 95% C.I.
C             NPV    (I) = NUMBER OF PREDICTED VALUES FLAG
C             PRINI  (I) = PREDICTION INPUT VALUE FOR PR2
C             PRINR  (R) = PREDICTION INPUT VALUE FOR PR1, PR3 - PR5
C             PR1    (R) = FUTURE RELIABILTIY FOR SPECIFIED TIME
C             PR2    (R) = MTBNF TO LOCATE NEXT K FAILURES
C             PR3    (R) = ADDITIONAL TESTING TIME TO REACH DIF
C             PR4    (R) = ADDITIONAL FAILURES FOUND DURING PR3
C             PR5    (R) = FUTURE FAILURES WITHIN A SPECIFIED TIME
C             PURLVL (R) = PURIFICATION LEVEL
C             RFLAG  (I) = RETURN STATUS FLAG
C             SUMTBF (R) = SUM OF THE TBF DATA
C             TNOF   (R) = ARRAY FOR TOTAL # OF FAULTS AND 95% C.I.
C             TNOFR  (R) = TOTAL NUMBER OF FAULTS REMAINING ESTIMATE
C             WT     (I) = FLAG TO PAUSE THE ASSUMPTIONS LIST OUTPUT
C             XLTM   (R) = FAILURE-FREE TEST TIME
C             ZTEST  (R) = ZERO TEST CONSTANT
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
C               /PRECOM/      PREDICTION DATA STORAGE COMMON
         COMMON /PRECOM/      PRD(END1)     ,XPRD(3)       ,D
     1                       ,DFLG          ,ESC           ,ESD
     2                       ,EST
         INTEGER              DFLG          ,ESC           ,ESD
     1                       ,EST
C
C               /TMPCOM/      TEMPORARY STORAGE COMMON
         COMMON /TMPCOM/      TM1C(END1)    ,TM2C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              IDX           ,NS
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CONFLG        ,FLAG          ,I
     1                       ,IFLG          ,NPV           ,PRINI
     2                       ,RFLAG         ,WT
         DIMENSION            IMTBF(3)      ,TNOF(3)       ,TNOFR(3)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR MUSA (BASIC) MODEL DESCRIPTION; ELSE'
     1          ,' ZERO.')
  301    FORMAT (' '
     1        ,/,'    THE MUSA BASIC EXECUTION TIME MODEL ASSUMPTIONS'
     2       ,//,'     1. THE SOFTWARE IS OPERATED IN A SIMILAR MANNER '
     3          ,'AS THE ANTICI-'
     4        ,/,'        PATED OPERATIONAL USAGE.'
     5       ,//,'     2. ALL FAULTS ARE EQUALLY LIKELY TO OCCUR  AND A'
     6          ,'RE INDEPENDENT'
     7        ,/,'        OF EACH OTHER.'
     8       ,//,'     3. ALL SOFTWARE FAULTS ARE OBSERVED.'
     9       ,//,'     4. THE EXECUTION TIMES (MEASURED IN CPU TIME) BE'
     A          ,'TWEEN FAILURES'
     B        ,/,'        ARE PIECEWISE EXPONENTIALLY DISTRIBUTED.'
     C       ,//,'     5. THE INTENSITY FUNCTION  IS PROPORTIONAL  TO T'
     D          ,'HE  NUMBER  OF')
  302    FORMAT ('        FAULTS REMAINING IN THE PROGRAM.'
     1       ,//,'     6. THE FAULT CORRECTION  RATE IS  PROPORTIONAL T'
     2          ,'O THE  FAILURE'
     3        ,/,'        OCCURRENCE RATE.'
     4       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  303    FORMAT ('    MODEL DATA REQUIREMENTS'
     1       ,//,'     1. THE TIME-BETWEEN-FAILURES AS MEASURED IN COMP'
     2          ,'UTER CPU TIME.'
     3        ,/,'        THE DATA SHOULD HAVE BEEN ENTERED VIA THE INP'
     4          ,'UT MODULE.'
     5       ,//,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     6          ,'HE APPROPRIATE'
     7        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     8          ,'EL;  OTHERWISE'
     9        ,/,'    ZERO TO RETURN TO THE MENU OF MODELS.')
  305    FORMAT (' '
     1        ,/,' **THE DATA ARE NOT APPROPRIATE FOR THE MODEL.')
  306    FORMAT (' '
     1        ,/,' **THE ESTIMATE  FOR THE TOTAL  NUMBER OF FAULTS  TO '
     2          ,'BE DETECTED IN'
     3        ,/,'   THE PROGRAM BEFORE  ALL FAULTS ARE UNCOVERED  CAME'
     4          ,' OUT LESS THAN'
     5        ,/,'   THE NUMBER OF FAULTS FOUND TO DATE WITHIN THE MODE'
     6          ,'L.')
  310    FORMAT (' '
     1        ,/,'   THE MAXIMUM LIKELIHOOD ESTIMATES ARE:'
     2        ,/,'   (THE APPROXIMATE 95% CONFIDENCE INTERVALS APPEAR I'
     3          ,'N PARENTHESES)'
     4        ,/,'    INIT. INTENSITY FUNCTION ',E12.5
     5        ,/,'    CUR.  INTENSITY FUNCTION ',E12.5)
  315    FORMAT ('    INIT. MTBNF              ',E12.5
     1          ,' (',E11.4,',',E11.4,')')
  320    FORMAT ('    INIT. MTBNF              ',E12.5)
  325    FORMAT ('    CUR.  MTBNF              ',E12.5
     1        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5
     2          ,' (',E11.4,',',E11.4,')'
     3        ,/,'    # OF FAULTS REMAINING    ',E12.5
     4          ,' (',E11.4,',',E11.4,')'
     5        ,/,'    PURIFICATION LEVEL       ',E12.5)
  326    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   (ROUNDED) NUMBER OF FAULTS REMAINING IS LESS THAN '
     4          ,'ONE.')
  327    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   CURRENT MTBNF IS LESS THAN ZERO.')
  330    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE EXPECTED RELIABILITY FOR A SPECIFIED TIME'
     3        ,/,'    2) THE TOTAL MTBNF FOR THE NEXT K FAILURES'
     4        ,/,'    3) THE TIME AND NUMBER OF FAILURES TO REACH A DES'
     5          ,'IRED INTENSITY'
     6        ,/,'       FUNCTION'
     7        ,/,'    4) THE TIME AND NUMBER OF FAILURES TO REACH A DES'
     8          ,'IRED MTBNF'
     9        ,/,'    5) THE NUMBER OF FAILURES EXPECTED IN A SPECIFIED'
     A          ,' TIME'
     B        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     C          ,'S.')
  335    FORMAT ('    ENTER LENGTH OF THE PERIOD, OR ZERO TO END.')
  340    FORMAT ('    FUTURE RELIABILITY      ',E12.5,/)
  345    FORMAT ('    ENTER VALUE OF K (BETWEEN ONE AND',E12.5,'), OR Z'
     1          ,'ERO TO END.')
  350    FORMAT ('    MTBNF EXPECTED          ',E12.5,/)
  355    FORMAT ('    ENTER INTENSITY FUNC. (LESS THAN',E12.5,'), OR ZE'
     1          ,'RO TO END.')
  360    FORMAT ('    ADDITIONAL TESTING TIME ',E12.5
     1        ,/,'    # OF FAILURES EXPECTED  ',E12.5,/)
  365    FORMAT ('    ENTER DESIRED MTBNF (GREATER THAN',E12.5,'), OR Z'
     1          ,'ERO TO END.')
  370    FORMAT ('    # OF FAILURES EXPECTED  ',E12.5,/)
  375    FORMAT (' '
     1        ,/,'   ENTER ONE TO WORK WITH CALENDAR TIME COMPONENT; EL'
     2          ,'SE ZERO.')
  380    FORMAT (' '
     1        ,/,'   ENTER ONE TO PERFORM AN ANALYSIS OF THE MODEL FIT '
     2          ,'USING THE PRE-'
     3        ,/,'   DICTIONS OF THIS MODEL; ELSE ZERO.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE ZERO TEST CONSTANT.
         ZTEST = 0.0000000001
C
C        SET XLTM TO CONTAIN THE FINAL FAILURE-FREE TESTING TIME.
         XLTM = 0.0
         IF (((IDX.EQ.   1) .AND. (PRC(1,2).EQ.0))  .OR.
     1       ((IDX.EQ.STR2) .AND. (PRC(2,2).EQ.0))) THEN
             XLTM = DAT(IDX + NS)
         ENDIF
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,301)
             WRITE (HIST,301)
             WRITE (   *,302)
             WRITE (HIST,302)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,303)
             WRITE (HIST,303)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
C
         IF (CONFLG.EQ.1) THEN
C            COMPUTE THE SUM OF THE TBF DATA.
             SUMTBF = 0.0
             DO 1000 I = 0, (NS-1)
                 SUMTBF = SUMTBF + DAT(IDX+I)
 1000            CONTINUE
C            ENDDO
C
             CALL SMUSMD
     G                   (NS    , DAT(IDX), XLTM
     Y                   ,CMTBF , ECR     , IMTBF , IFLG  , TNOF
     Y                   ,IIF   , CIF     , RFLAG )
C
             IF (RFLAG.EQ.3) THEN
C                THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
                 WRITE (   *,305)
                 WRITE (HIST,305)
             ELSEIF (RFLAG.EQ.4) THEN
C                THE  TOTAL ERRORS ESTIMATE  WAS LESS THAN  THE OBSERVED
C                ERRORS TO DATE.
                 WRITE (   *,306)
                 WRITE (HIST,306)
             ELSE
C            ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY---------------
C                    A SOLUTION WAS FOUND;  COMPUTE THE EXTRA TOTAL NUM-
C                    BER OF FAULTS REMAINING AND PURIFICATION LEVEL.
                     DO 1025 I = 1, 3
                         TNOFR(I) = TNOF(I) - REAL(NS)
 1025                    CONTINUE
C                    ENDDO
                     IF (TNOFR(2).LT.0.0) THEN
                         TNOFR(2) = 0.0
                     ENDIF
                     PURLVL = REAL(NS) / TNOF(1)
C
C                    OUTPUT THE RESULTS.
                     WRITE (   *,310)     IIF     , CIF
                     WRITE (HIST,310)     IIF     , CIF
                     IF (IFLG.EQ.1) THEN
                         WRITE (   *,315) IMTBF(1), IMTBF(2), IMTBF(3)
                         WRITE (HIST,315) IMTBF(1), IMTBF(2), IMTBF(3)
                     ELSE
                         WRITE (   *,320) IMTBF(1)
                         WRITE (HIST,320) IMTBF(1)
                     ENDIF
                     WRITE (   *,325)     CMTBF
     1                                   ,TNOF (1), TNOF (2), TNOF (3)
     2                                   ,TNOFR(1), TNOFR(2), TNOFR(3)
     3                                   ,PURLVL
                     WRITE (HIST,325)     CMTBF
     1                                   ,TNOF (1), TNOF (2), TNOF (3)
     2                                   ,TNOFR(1), TNOFR(2), TNOFR(3)
     3                                   ,PURLVL
C
                     IF (TNOFR(1).LT.0.5) THEN
C                        FUTURE PREDICTIONS ARE NOT ALLOWED (TNOFR).
                         WRITE (   *,326)
                         WRITE (HIST,326)
                     ENDIF
                     IF (CMTBF.LT.0.0) THEN
C                        FUTURE PREDICTIONS ARE NOT ALLOWED (MTBNF).
                         WRITE (   *,327)
                         WRITE (HIST,327)
                     ENDIF
                     IF ((TNOFR(1).GE.0.5) .AND. (CMTBF.GE.0.0)) THEN
C                        PREPARE FOR THE PREDICTIONS BY  RE-CLAIMING THE
C                        VALUES FOR BETA0 AND BETA1.
                         BETA0 = TNOF(1)
                         BETA1 = IIF  / BETA0
C
C                        DOWHILE (FLAG .NE. TERMINATION)
 1050                        WRITE (   *,330)
                             WRITE (HIST,330)
                             READ  (   *,  *) FLAG
                             WRITE (HIST,  *) FLAG
                             IF ((FLAG.LT.1) .OR. (FLAG.GT.5))GO TO 1350
C
                             IF     (FLAG.EQ.1) THEN
C                                FUTURE RELIABILITY FOR SPECIFIED TIME.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1075                                WRITE (   *,335)
                                     WRITE (HIST,335)
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1100
                                     PR1 = EXP(-(BETA0
     1                                   * EXP(-BETA1 * SUMTBF))
     2                                   * (1.0
     3                                   - EXP(-BETA1 * PRINR)))
                                     WRITE (   *,340) PR1
                                     WRITE (HIST,340) PR1
                                     GO TO 1075
 1100                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.2) THEN
C                                MTBNF TO LOCATE K FAILURES.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1125                                WRITE (   *,345) TNOFR(1)
                                     WRITE (HIST,345) TNOFR(1)
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1175
                                     PRINI  = INT(PRINR + 0.5)
                                     TMPTBF = SUMTBF
                                     PR2    = 0.0
                                     DO 1150 I = 1, PRINI
                                         TMP    = (1.0
     1                                          / (BETA0 * BETA1))
     2                                          * EXP(TMPTBF * BETA1)
                                         TMPTBF = TMPTBF + TMP
                                         PR2    = PR2    + TMP
 1150                                    CONTINUE
C                                    ENDDO
                                     WRITE (   *,350) PR2
                                     WRITE (HIST,350) PR2
                                     GO TO 1125
 1175                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.3) THEN
C                                TIME AND FAILURES  TO REACH  A  DESIRED
C                                INTENSITY FUNCTION.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1200                                WRITE (   *,355) CIF
                                     WRITE (HIST,355) CIF
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1225
                                     PR3 = 1.0 / BETA1
     1                                   * LOG(CIF / PRINR)
                                     PR4 = 1.0 / BETA1
     1                                   * (CIF - PRINR)
                                     IF (PR4.GT.TNOFR(1)) THEN
                                         PR4 = TNOFR(1)
                                     ENDIF
                                     WRITE (   *,360) PR3, PR4
                                     WRITE (HIST,360) PR3, PR4
                                     GO TO 1200
 1225                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.4) THEN
C                                TIME AND FAILURES  TO REACH THE DESIRED
C                                MTBNF.
                                 NPV = 2
C                                DOWHILE (DMTBF .NE. TERMINATION)
 1250                                WRITE (   *,365) CMTBF
                                     WRITE (HIST,365) CMTBF
                                     READ  (   *,  *) DMTBF
                                     WRITE (HIST,  *) DMTBF
                                     IF (DMTBF.EQ.0.0) GO TO 1275
                                     CALL SMUSPR
     G                                           (NPV     , NS
     G                                           ,DAT(IDX), IMTBF(1)
     G                                           ,CMTBF   , DMTBF
     G                                           ,TNOF(1) , IIF
     Y                                           ,XPRD    , D
     Y                                           ,DFLG    , TM1C    )
                                     IF (XPRD(1).GT.TNOFR(1)) THEN
                                         XPRD(1) = TNOFR(1)
                                     ENDIF
                                     WRITE (   *,360) XPRD(2), XPRD(1)
                                     WRITE (HIST,360) XPRD(2), XPRD(1)
                                     GO TO 1250
 1275                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.5) THEN
C                                FAILURES FOUND IN A SPECIFIED TIME.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1300                                WRITE (   *,335)
                                     WRITE (HIST,335)
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1325
                                     PR5 = BETA0
     1                                   * (EXP(-BETA1 *  SUMTBF)
     2                                   -  EXP(-BETA1 * (SUMTBF
     3                                   +  PRINR)))
                                     IF (PR5.GT.TNOFR(1)) THEN
                                         PR5 = TNOFR(1)
                                     ENDIF
                                     WRITE (   *,370) PR5
                                     WRITE (HIST,370) PR5
                                     GO TO 1300
 1325                                CONTINUE
C                                ENDWHILE
                             ENDIF
                             GO TO 1050
 1350                        CONTINUE
C                        ENDWHILE
C
C                        CALENDAR TIME COMPONENT OF THE MODEL.
                         WRITE (   *,375)
                         WRITE (HIST,375)
                         READ  (   *,  *) FLAG
                         WRITE (HIST,  *) FLAG
                         IF (FLAG.EQ.1) THEN
                             CALL MDLMSC
     G                                   (IDX     , NS     , CMTBF
     G                                   ,IMTBF(1), TNOF(1))
                         ENDIF
                     ENDIF
C            ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY---------------
             ENDIF
C
             IF ((RFLAG.EQ.0) .AND. (IMTBF(1).GT.ZTEST)) THEN
C                THE SMUSMD EXECUTION IS SUCCESSFUL AND THE ESTIMATE FOR
C                THE INITIAL MTBNF IS POSITIVE;  THEREFORE  THE TBF DATA
C                VECTOR CAN BE COMPUTED.
                 WRITE (   *,380)
                 WRITE (HIST,380)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    ACCESS THE SMUSPR ROUTINE TO GENERATE THE PREDICTED
C                    TBF DATA VECTOR.
                     CALL SMUSPR
     G                           (NS      , NS     , DAT(IDX) , IMTBF(1)
     G                           ,CMTBF   , DMTBF  , TNOF(1)  , IIF
     Y                           ,PRD     , D      , DFLG     , TM1C   )
C
C                    SET THE ESTIMATION SELECTION CODE TO INDICATE WHICH
C                    MODEL WAS RESPONSIBLE FOR  THE PREDICTIONS; AND SET
C                    ESD TO INDICATE WHICH DATA TYPE (WC OR CPU) WAS RE-
C                    SPONSIBLE.
                     ESC = 2
                     ESD = 1
                     IF (IDX.EQ.STR2) THEN
                         ESD = 2
                     ENDIF
C
C                    PERFORM ANALYSIS OF MODEL FIT.
                     CALL FITDRV
                 ENDIF
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE MDLNPI
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLNPI
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
C        TO PROVIDE THE INTERACTIVE PROMPTS AND INPUTS FOR THE NON-HOMO-
C        GENEOUS POISSON MODEL.  ADDITIONALLY, THE ROUTINE  WILL PERFORM
C        THE ACCESS TO THE SMFLIB  ROUTINES NEEDED  TO PERFORM THE MODEL
C        EXECUTIONS AND PREDICTED DATA VECTOR CREATION.
C   DESCRIPTION
C        THREE SMFLIB ROUTINES  (SMFTRN, SNPIMD, & SNPIPR)  WILL BE USED
C        WITHIN THIS ROUTINE.
C
C        THE FIRST ROUTINE  (SMFTRN) CONVERTS THE  STORED TESTING LENGTH
C        PER PERIOD DATA  VECTOR TO CONTAIN THE  CUMULATIVE LENGTHS OVER
C        ALL PERIODS.
C
C        THE SECOND ROUTINE  (SNPIMD) PROVIDES  THE CALCULATIONS  OF THE
C        RELIABILITY VIA THE  MAXIMUM LIKELIHOOD AND LEAST SQUARES METH-
C        ODS OF EXECUTION.  THE FIRST ARGUMENT  OF THE SNPIMD  CALL LINE
C        PROVIDES THE ROUTINE WITH THE NUMBER OF THE METHOD DESIRED.
C
C        ONCE THE  EXECUTION OF  SNPIMD IS PERFORMED,  THE RETURN STATUS
C        FLAG (RFLAG) WILL INDICATE THE SUCCESSFULNESS  OF THE MODEL ES-
C        TIMATION PROCESS. THE FOLLOWING VALUES ARE POSSIBLE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS THAN THE  OBSERVED
C                 FAULTS TO DATE
C
C        FOR RFLAG VALUE OF 0 THE MODEL ESTIMATES WILL BE LISTED.
C
C        THE THIRD ROUTINE (SNPIPR) PROVIDES FUTURE PREDICTIONS AND GEN-
C        ERATES THE VECTOR OF PREDICTED FAULT COUNTS.
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
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C                          (BUT RETURNED TO ORIGINAL VALUES)
C             PMAX   (R) = MAXIMUM NUMBER OF ITERATIONS FOR PREDICTIONS
C        YIELDED
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             EST    (I) = NUMBER OF ESTIMATIONS FROM THE MODEL
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             XPRD   (R) = FUTURE PREDICTIONS (NUMBER AND 95% C.I.)
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             CNTP   (R) = COUNT OF PERIODS TO FIND M FAULTS
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             CTOT   (R) = TOTAL FAULTS FOUND TO DATE
C             DM     (R) = DUMMY ARGUMENT OF SMFTRN CALL
C             DNOF   (R) = DESIRED NUMBER OF FAULTS (M)
C             ESF    (I) = ESTIMATION SELECTION FLAG
C             EXL    (R) = EXPECTED TESTING LENGTH OF NEXT PERIOD
C             FLAG   (I) = OPTION SELECTION FLAG
C             FLAG2  (I) = PROGRAM MAXIMUM OVER-RIDE FLAG
C             IDXE   (I) = ENDING INDEX OF THE DATA
C             LB     (C) = OUTPUT MODEL LABELS
C             NPV    (I) = NUMBER OF PREDICTED VALUES FLAG
C             NS     (I) = NUMBER OF ENTRIES IN DATA VECTOR
C             PREDAT (R) = PREDICTION DATA VALUES ARRAY
C             RFLAG  (I) = RETURN STATUS FLAG
C             STATS  (R) = STATISTICS RESULTS ARRAY
C             TNOFR  (R) = TOTAL NUMBER OF FAULTS REMAINING ESTIMATE
C             TOTL   (R) = TOTAL EXPECTED LENGTHS FOR PERIOD PREDICTION
C             WT     (I) = FLAG TO PAUSE THE ASSUMPTIONS LIST OUTPUT
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
C               /PMXCOM/      PREDICTION MAXIMUM ITERATION COMMON
         COMMON /PMXCOM/      PMAX
C
C               /PRECOM/      PREDICTION DATA STORAGE COMMON
         COMMON /PRECOM/      PRD(END1)     ,XPRD(3)       ,D
     1                       ,DFLG          ,ESC           ,ESD
     2                       ,EST
         INTEGER              DFLG          ,ESC           ,ESD
     1                       ,EST
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CONFLG        ,ESF           ,FLAG
     1                       ,FLAG2         ,I             ,IDXE
     2                       ,NPV           ,NS            ,RFLAG
     3                       ,WT
         DIMENSION            PREDAT(2,3)   ,STATS(2,3)    ,TNOFR(3)
         CHARACTER* 2         LB(2)
C
C                             LOCAL ASSIGNMENTS
         DATA                 LB(1) /'ML'/      ,    LB(2) /'LS'/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR NHPP MODEL DESCRIPTION; ELSE ZERO.')
  301    FORMAT (' '
     1        ,/,'    THE NON-HOMOGENEOUS POISSON MODEL ASSUMPTIONS'
     2       ,//,'     1. THE SOFTWARE IS OPERATED IN A SIMILAR MANNER '
     3          ,'AS THE ANTICI-'
     4        ,/,'        PATED OPERATIONAL USAGE.'
     5       ,//,'     2. ALL FAULTS ARE EQUALLY LIKELY TO OCCUR  AND A'
     6          ,'RE INDEPENDENT'
     7        ,/,'        OF EACH OTHER.'
     8       ,//,'     3. EACH FAULT IS OF THE SAME ORDER OF "IMPACT" A'
     9          ,'S ANY OTHER.'
     A       ,//,'     4. THE  CUMULATIVE NUMBER  OF FAULTS DETECTED AT'
     B          ,' ANY TIME FOL-')
  302    FORMAT ('        LOWS A POISSON DISTRIBUTION  WITH MEAN M(T). '
     1          ,' THAT  MEAN IS'
     2        ,/,'        SUCH THAT THE EXPECTED  NUMBER OF FAULTS IN  '
     3          ,'ANY SMALL TIME'
     4        ,/,'        INTERVAL ABOUT T IS PROPORTIONAL  TO THE NUMB'
     5          ,'ER OF UNDETEC-'
     6        ,/,'        TED FAULTS AT TIME T.'
     7       ,//,'     5. THE MEAN IS ASSUMED TO BE A BOUNDED NON-DECRE'
     8          ,'ASING FUNCTION'
     9        ,/,'        WITH M(T) APPROACHING IN THE LIMIT, "A" (THE '
     A          ,'EXPECTED TOTAL'
     B        ,/,'        NUMBER OF FAULTS TO BE, EVENTUALLY, DETECTED '
     C          ,'IN THE TESTING'
     D        ,/,'        PROCESS), AS THE LENGTH OF TESTING GOES TO IN'
     E          ,'FINITY.'
     F       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  303    FORMAT ('    MODEL DATA REQUIREMENTS'
     1       ,//,'     1. THE NUMBER OF FAULTS  DETECTED IN EACH INTERV'
     2          ,'AL OF TESTING.'
     3       ,//,'     2. THE LENGTHS OF THE VARIOUS TESTING INTERVALS.'
     4       ,//,'        "1 & 2 SHOULD HAVE BEEN ENTERED VIA THE DATA '
     5          ,'INPUT MODULE."'
     6       ,//,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     7          ,'HE APPROPRIATE'
     8        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     9          ,'EL;  OTHERWISE'
     A        ,/,'    ZERO TO RETURN TO THE MENU OF MODELS.')
  305    FORMAT (' '
     1        ,/,'   ENTER ONE FOR  MAXIMUM LIKELIHOOD METHOD,  TWO FOR'
     2          ,' LEAST SQUARES'
     3        ,/,'   METHOD, OR THREE TO TERMINATE MODEL EXECUTION.')
  310    FORMAT (' '
     1        ,/,' **THE DATA ARE NOT APPROPRIATE FOR THE ',A2,' MODEL.'
     2          )
  315    FORMAT (' '
     1        ,/,' **THE ESTIMATE  FOR THE TOTAL  NUMBER OF FAULTS  TO '
     2          ,'BE DETECTED IN'
     3        ,/,'   THE PROGRAM BEFORE  ALL FAULTS ARE UNCOVERED  CAME'
     4          ,' OUT LESS THAN'
     5        ,/,'   THE NUMBER OF FAULTS FOUND TO DATE WITHIN THE '
     6          ,A2,' MODEL.')
  320    FORMAT (' '
     1        ,/,3X,A2,' MODEL ESTIMATES ARE:')
  321    FORMAT ('   (THE APPROXIMATE 95% CONFIDENCE INTERVALS APPEAR I'
     1          ,'N PARENTHESES)')
  325    FORMAT ('    PROPORTIONALITY CONSTANT ',E12.5
     1          ,' (',E11.4,',',E11.4,')'
     2        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5
     3          ,' (',E11.4,',',E11.4,')'
     4        ,/,'    # OF FAULTS REMAINING    ',E12.5
     5          ,' (',E11.4,',',E11.4,')')
  330    FORMAT ('    PROPORTIONALITY CONSTANT ',E12.5
     1        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5
     2        ,/,'    # OF FAULTS REMAINING    ',E12.5)
  331    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   (ROUNDED) NUMBER OF FAULTS REMAINING IS LESS THAN '
     4          ,'ONE.')
  335    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE NUMBER OF FAULTS EXPECTED IN THE NEXT TEST'
     3          ,'ING PERIOD'
     4        ,/,'    2) THE NUMBER OF PERIODS NEEDED TO DISCOVER THE N'
     5          ,'EXT M FAULTS'
     6        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     7          ,'S.')
  340    FORMAT ('    ENTER PROJECTED LENGTH OF THE PERIOD, OR ZERO TO '
     1          ,'END.')
  345    FORMAT ('    # OF FAULTS EXPECTED     ',E12.5,/)
  350    FORMAT ('    ENTER PROJECTED LENGTH OF THE PERIODS.')
  355    FORMAT ('    ENTER VALUE OF M (BETWEEN ONE AND',E12.5,'), OR Z'
     1          ,'ERO TO END.')
  360    FORMAT ('    # OF PERIODS EXPECTED    ',E12.5,/)
  361    FORMAT ('  **# OF PERIODS EXCEEDS     ',E12.5,' (PROGRAM MAXIM'
     1          ,'UM)'
     2        ,/,'    ENTER ONE TO CHANGE THE PROGRAM MAXIMUM; ELSE ZER'
     3          ,'O.')
  362    FORMAT ('    ENTER THE NEW VALUE FOR THE PROGRAM MAXIMUM.')
  363    FORMAT (' ')
  365    FORMAT (' '
     1        ,/,'   ENTER ONE TO PERFORM AN ANALYSIS OF THE MODEL FIT '
     2          ,'USING THE PRE-'
     3        ,/,'   DICTIONS OF THIS MODEL; ELSE ZERO.')
  370    FORMAT ('   ENTER ONE FOR PREDICTIONS BASED ON MAXIMUM LIKELIH'
     1          ,'OOD METHOD, OR'
     2        ,/,'   TWO FOR LEAST SQUARES METHOD.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE VECTOR SIZE COUNTER.
         NS = PRC(3,1)
C
C        ZERO THE SUCCESSFULNESS  ELEMENTS OF THE PREDICTION DATA ARRAY.
         PREDAT(1,1) = 0.0
         PREDAT(2,1) = 0.0
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,301)
             WRITE (HIST,301)
             WRITE (   *,302)
             WRITE (HIST,302)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,303)
             WRITE (HIST,303)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
C
         IF (CONFLG.EQ.1) THEN
C            STORE THE INTERVAL TESTING LENGTH DATA AND CONVERT THE DATA
C            TO A CUMULATIVE REPRESENTATION.
             IDXE = END1 + NS
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = STR2, IDXE)
             CALL SMFTRN
     G                   (NS       , DM       , DM       , 6
     B                   ,DAT(STR2)
     Y                   ,RFLAG    )
C
C            DOWHILE (ESF .NE. TERMINATION)
 1000            WRITE (   *,305)
                 WRITE (HIST,305)
                 READ  (   *,  *) ESF
                 WRITE (HIST,  *) ESF
                 IF ((ESF.GE.1) .AND. (ESF.LE.2)) THEN
                 PREDAT(ESF,1) = 0.0
C
                 CALL SNPIMD
     G                       (ESF      , DAT       , DAT(STR2)
     G                       ,NS
     Y                       ,STATS    , RFLAG     )
C
                 IF (RFLAG.EQ.3) THEN
C                    THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
                     WRITE (   *,310) LB(ESF)
                     WRITE (HIST,310) LB(ESF)
                 ELSEIF (RFLAG.EQ.4) THEN
C                    THE  TOTAL ERRORS  ESTIMATE  WAS LESS THAN  THE OB-
C                    SERVED ERRORS TO DATE.
                     WRITE (   *,315) LB(ESF)
                     WRITE (HIST,315) LB(ESF)
                 ELSE
C                    A SOLUTION WAS FOUND; OUTPUT THE RESULTS.
                     WRITE (   *,320) LB(ESF)
                     WRITE (HIST,320) LB(ESF)
C
C                    DETERMINE THE NUMBER OF FAULTS FOUND TO DATE FOR AN
C                    ESTIMATE OF THE NUMBER OF FAULTS REMAINING.
                     CTOT = 0.0
                     DO 1025 I = 1, NS
                         CTOT = CTOT + DAT(I)
 1025                    CONTINUE
C                    ENDDO

                     IF (ESF.EQ.1) THEN
C                        COMPUTE THE EXTRA NUMBER OF FAULTS REMAINING.
                         DO 1050 I = 1, 3
                             TNOFR(I) = STATS(2,I) - CTOT
 1050                        CONTINUE
C                        ENDDO
                         IF (TNOFR(2).LT.0.0) THEN
                             TNOFR(2) = 0.0
                         ENDIF
C
C                        OUTPUT THE APPLICABLE RESULTS.
                         WRITE (   *,321)
                         WRITE (HIST,321)
                         WRITE (   *,325) (STATS(I,1), STATS(I,2)
     1                                    ,STATS(I,3), I = 1, 2)
     2                                    ,TNOFR(1)  , TNOFR(2)
     3                                    ,TNOFR(3)
                         WRITE (HIST,325) (STATS(I,1), STATS(I,2)
     1                                    ,STATS(I,3), I = 1, 2)
     2                                    ,TNOFR(1)  , TNOFR(2)
     3                                    ,TNOFR(3)
                     ELSE
C                        COMPUTE THE EXTRA NUMBER OF FAULTS REMAINING.
                         TNOFR(1) = STATS(2,1) - CTOT
C
C                        OUTPUT THE APPLICABLE RESULTS.
                         WRITE (   *,330)  STATS(1,1), STATS(2,1)
     1                                    ,TNOFR(1)
                         WRITE (HIST,330)  STATS(1,1), STATS(2,1)
     1                                    ,TNOFR(1)
                     ENDIF
C
C                    SET THE  SUCCESSFULNESS FLAG,  AND STORE  THE TOTAL
C                    NUMBER OF FAULTS AND PROPORTIONALITY CONSTANT ESTI-
C                    MATES.
                     PREDAT(ESF,1) = 1.0
                     PREDAT(ESF,2) = STATS(2,1)
                     PREDAT(ESF,3) = STATS(1,1)
C
                     IF (TNOFR(1).LT.0.5) THEN
C                        FUTURE PREDICTIONS ARE NOT ALLOWED (TNOFR).
                         WRITE (   *,331)
                         WRITE (HIST,331)
                     ELSE
C                        DOWHILE (FLAG .NE. TERMINATION)
 1075                        WRITE (   *,335)
                             WRITE (HIST,335)
                             READ  (   *,  *) FLAG
                             WRITE (HIST,  *) FLAG
                             IF ((FLAG.LT.1) .OR. (FLAG.GT.2))GO TO 1225
C
                             IF     (FLAG.EQ.1) THEN
C                                FAULTS IN NEXT TESTING PERIOD.
                                 NPV = 1
C                                DOWHILE (EXL .NE. TERMINATION)
 1100                                WRITE (   *,340)
                                     WRITE (HIST,340)
                                     READ  (   *,  *) EXL
                                     WRITE (HIST,  *) EXL
                                     IF (EXL.EQ.0.0) GO TO 1125
                                     CALL SNPIPR
     G                                           (NPV           , NS
     G                                           ,PREDAT(ESF,2)
     G                                           ,PREDAT(ESF,3)
     G                                           ,DAT(STR2)     , EXL
     Y                                           ,XPRD          )
                                     IF (XPRD(1).GT.TNOFR(1)) THEN
                                         XPRD(1) = TNOFR(1)
                                     ENDIF
                                     WRITE (   *,345) XPRD(1)
                                     WRITE (HIST,345) XPRD(1)
                                     GO TO 1100
 1125                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.2) THEN
C                                PERIODS TO DETECT NEXT M FAULTS.
                                 WRITE (   *,350)
                                 WRITE (HIST,350)
                                 READ  (   *,  *) EXL
                                 WRITE (HIST,  *) EXL
                                 NPV = 1
C                                DOWHILE (DNOF .NE. TERMINATION)
 1150                                WRITE (   *,355) TNOFR(1)
                                     WRITE (HIST,355) TNOFR(1)
                                     READ  (   *,  *) DNOF
                                     WRITE (HIST,  *) DNOF
                                     IF (DNOF.EQ.0.0) GO TO 1200
                                     TOTL = 0.0
                                     CNTP = 0.0
C                                    DOUNTIL (PREDICTED .GE. DESIRED)
 1175                                    TOTL = TOTL + EXL
                                         CNTP = CNTP + 1.0
                                         CALL SNPIPR
     G                                               (NPV          ,NS
     G                                               ,PREDAT(ESF,2)
     G                                               ,PREDAT(ESF,3)
     G                                               ,DAT(STR2)    ,TOTL
     Y                                               ,XPRD         )
                                         IF ((XPRD(1).LT.DNOF) .AND.
     1                                       (CNTP.LE.PMAX))  GO TO 1175
C                                    ENDUNTIL
                                     IF (CNTP.LE.PMAX) THEN
                                         WRITE (   *,360) CNTP
                                         WRITE (HIST,360) CNTP
                                     ELSE
                                         WRITE (   *,361) PMAX
                                         WRITE (HIST,361) PMAX
                                         READ  (   *,  *) FLAG2
                                         WRITE (HIST,  *) FLAG2
                                         IF (FLAG2.EQ.1) THEN
                                             WRITE (   *,362)
                                             WRITE (HIST,362)
                                             READ  (   *,  *) PMAX
                                             WRITE (HIST,  *) PMAX
                                         ENDIF
                                         WRITE (   *,363)
                                         WRITE (HIST,363)
                                     ENDIF
                                     GO TO 1150
 1200                                CONTINUE
C                                ENDWHILE
                             ENDIF
                             GO TO 1075
 1225                        CONTINUE
C                        ENDWHILE
                     ENDIF
                 ENDIF
                 GO TO 1000
                 ENDIF
C            ENDWHILE
C
             FLAG = 0
             IF ((PREDAT(1,1).EQ.1.0) .OR. (PREDAT(2,1).EQ.1.0)) THEN
C                AT LEAST ONE EXECUTION  WAS SUCCESSFUL, PROMPT USER AND
C                SEE IF THE PREDICTED VECTOR CREATION IS DESIRED.
                 WRITE (   *,365)
                 WRITE (HIST,365)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    THE VECTOR OF PREDICTED DATA IS DESIRED.
                     IF (PREDAT(1,1).NE.PREDAT(2,1)) THEN
C                        ONLY ONE EXECUTION  WAS SUCCESSFUL;  SET ESF TO
C                        INDICATE ML OR LS.
                         ESF = PREDAT(1,1) * 1.0 + PREDAT(2,1) * 2.0
                     ELSE
C                        BOTH EXECUTIONS WERE  PERFORMED AND SUCCESSFUL;
C                        SEE WHICH THE USER DESIRES.
                         WRITE (   *,370)
                         WRITE (HIST,370)
                         READ  (   *,  *) ESF
                         WRITE (HIST,  *) ESF
                     ENDIF
C
C                    ACCESS THE SNPIPR ROUTINE TO GENERATE THE PREDICTED
C                    FAULT COUNT DATA VECTOR.
                     CALL SNPIPR
     G                           (NS            , NS
     G                           ,PREDAT(ESF,2) , PREDAT(ESF,3)
     G                           ,DAT(STR2)     , EXL
     Y                           ,PRD           )
C
C                    SET THE ESTIMATION SELECTION CODE TO INDICATE WHICH
C                    MODEL WAS RESPONSIBLE FOR  THE PREDICTIONS; AND SET
C                    ESD TO INDICATE THAT THE INTERVAL DATA TYPE WAS RE-
C                    SPONSIBLE.
                     ESC = 8
                     ESD = 3
C
C                    SET THE GOODNESS-OF-FIT ESTIMATION CONSTANT.
                     EST = 2
C
C                    RESTORE TESTING LENGTH DATA AND PERFORM ANALYSIS OF
C                    MODEL FIT.
                     REWIND TMPB
                     READ  (TMPB) (DAT(I), I = STR2, IDXE)
                     CALL FITDRV
                 ENDIF
             ENDIF
C
             IF (FLAG.EQ.0) THEN
C                TESTING LENGTH DATA HAS NOT BEEN RESTORED, YET.
                 REWIND TMPB
                 READ  (TMPB) (DAT(I), I = STR2, IDXE)
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE MDLNPT
     G                     (IDX   , NS    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLNPT
C    G                     (IDX   , NS    )
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
C        TO PROVIDE THE INTERACTIVE PROMPTS AND INPUTS FOR THE NON-HOMO-
C        GENEOUS POISSON MODEL.  ADDITIONALLY,  THE ROUTINE WILL PERFORM
C        THE ACCESS TO THE SMFLIB ROUTINES  NEEDED TO PERFORM THE EXECU-
C        TION TIME MODEL EXECUTIONS AND PREDICTED DATA VECTOR CREATIONS.
C   DESCRIPTION
C        THREE SMFLIB ROUTINES  (SMFTRN, SNPTMD, & SNPTPR)  WILL BE USED
C        WITHIN THIS ROUTINE.
C
C        THE FIRST ROUTINE (SMFTRN) CONVERTS THE  STORED TBF DATA TO TTF
C        DATA. TTF DATA IS THE SUMMATION OF THE PREVIOUS TBF ENTRIES FOR
C        EACH ENTRY IN THE DATA BASE.  THIS MEANS  THAT THE FIRST STORED
C        TBF IS THE FIRST TTF,  THE SUM OF THE  FIRST AND SECOND TBFS IS
C        THE SECOND TTF, THE SUM OF THE FIRST, SECOND, AND THIRD TBFS IS
C        THE THIRD TTF, AND SO ON.
C
C        THE SECOND ROUTINE  (SNPTMD)  PROVIDES THE  CALCULATIONS OF THE
C        RELIABILITY VIA THE MAXIMUM LIKELIHOOD METHOD.
C
C        ONCE THE  EXECUTION OF  SNPTMD IS PERFORMED,  THE RETURN STATUS
C        FLAG (RFLAG) WILL INDICATE THE SUCCESSFULNESS  OF THE MODEL ES-
C        TIMATION PROCESS. THE FOLLOWING VALUES ARE POSSIBLE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS THAN THE  OBSERVED
C                 FAULTS TO DATE
C
C        FOR RFLAG VALUE OF 0 THE MODEL ESTIMATES WILL BE LISTED.
C
C        THE THIRD ROUTINE (SNPTPR) PROVIDES FUTURE PREDICTIONS AND GEN-
C        ERATES THE VECTOR OF PREDICTED TBF DATA.
C
C        THE SNPTPR ROUTINE WILL ALSO DETERMINE THE GOODNESS-OF-FIT WHEN
C        CREATING THE PREDICTED VECTOR; ALTHOUGH THE VALUES FOR THE KOL-
C        MOGOROV DISTANCE STATISTIC AND THE DETERMINED SIGNIFICANCE WILL
C        NOT BE OUTPUT UNTIL THE USER ENTERS THE ANALYSIS DRIVER.
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
C             D      (R) = CALCULATED KOLMOGOROV DISTANCE
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          GOODNESS-OF-FIT CALCULATION
C             XPRD   (R) = FUTURE PREDICTIONS (NUMBER AND 95% C.I.)
C   ARGUMENT LIST
C        GIVEN
C             IDX    (I) = INDEX FOR TBF DATA STARTING ADDRESS
C             NS     (I) = THE "REAL" NUMBER OF ENTRIES  (I.E., THE LAST
C                          POINT IS NOT INCLUDED IF THE FATALITY FLAG IS
C                          NOT SET)
C   LOCAL GLOSSARY
C             CIF    (R) = CURRENT INTENSITY FUNCTION
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             DM     (R) = DUMMY ARGUMENT OF SMFTRN CALL
C             FLAG   (I) = OPTION SELECTION FLAG
C             IDXE   (I) = ENDING INDEX OF THE DATA
C             IIF    (R) = INITIAL INTENSITY FUNCTION
C             PRINI  (I) = PREDICTION INPUT VALUE FOR PR2 (N/A)
C             PRINR  (R) = PREDICTION INPUT VALUE FOR PR1, PR3 - PR5
C             PR1    (R) = FUTURE RELIABILTIY FOR SPECIFIED TIME (N/A)
C             PR2    (R) = MTBNF TO LOCATE NEXT K FAILURES (N/A)
C             PR3    (R) = ADDITIONAL TESTING TIME TO REACH DIF
C             PR4    (R) = ADDITIONAL FAILURES FOUND DURING PR3 (N/A)
C             PR5    (R) = FUTURE FAILURES WITHIN A SPECIFIED TIME
C             PURLVL (R) = PURIFICATION LEVEL
C             RFLAG  (I) = RETURN STATUS FLAG
C             SOT    (R) = SPECIFIED OPERATIONAL TIME
C             SR     (R) = SPECIFIED RELIABILITY
C             STATS  (R) = STATISTICS RESULTS VECTOR
C             TNOFR  (R) = TOTAL NUMBER OF FAULTS REMAINING ESTIMATE
C             WT     (I) = FLAG TO PAUSE THE ASSUMPTIONS LIST OUTPUT
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
C               /PRECOM/      PREDICTION DATA STORAGE COMMON
         COMMON /PRECOM/      PRD(END1)     ,XPRD(3)       ,D
     1                       ,DFLG          ,ESC           ,ESD
     2                       ,EST
         INTEGER              DFLG          ,ESC           ,ESD
     1                       ,EST
C
C               /TMPCOM/      TEMPORARY STORAGE COMMON
         COMMON /TMPCOM/      TM1C(END1)    ,TM2C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              IDX           ,NS
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CONFLG        ,FLAG          ,I
     1                       ,IDXE          ,NPV           ,RFLAG
     2                       ,WT
         DIMENSION            STATS(2)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR NHPP MODEL DESCRIPTION; ELSE ZERO.')
  301    FORMAT (' '
     1        ,/,'    THE NON-HOMOGENEOUS POISSON MODEL ASSUMPTIONS'
     2       ,//,'     1. THE SOFTWARE IS OPERATED IN A SIMILAR MANNER '
     3          ,'AS THE ANTICI-'
     4        ,/,'        PATED OPERATIONAL USAGE.'
     5       ,//,'     2. ALL FAULTS ARE EQUALLY LIKELY TO OCCUR  AND A'
     6          ,'RE INDEPENDENT'
     7        ,/,'        OF EACH OTHER.'
     8       ,//,'     3. THE  CUMULATIVE NUMBER  OF FAULTS DETECTED AT'
     9          ,' ANY TIME FOL-'
     A        ,/,'        LOWS A POISSON DISTRIBUTION  WITH MEAN M(T). '
     B          ,' THAT  MEAN IS'
     C        ,/,'        SUCH THAT THE EXPECTED  NUMBER OF FAULTS IN  '
     D          ,'ANY SMALL TIME')
  302    FORMAT ('        INTERVAL ABOUT T IS PROPORTIONAL  TO THE NUMB'
     1          ,'ER OF UNDETEC-'
     2        ,/,'        TED FAULTS AT TIME T.'
     3       ,//,'     4. THE MEAN IS ASSUMED TO BE A BOUNDED NON-DECRE'
     4          ,'ASING FUNCTION'
     5        ,/,'        WITH M(T) APPROACHING IN THE LIMIT, "A" (THE '
     6          ,'EXPECTED TOTAL'
     7        ,/,'        NUMBER OF FAULTS TO BE, EVENTUALLY, DETECTED '
     8          ,'IN THE TESTING'
     9        ,/,'        PROCESS), AS THE LENGTH OF TESTING GOES TO IN'
     A          ,'FINITY.'
     B       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  303    FORMAT ('    MODEL DATA REQUIREMENTS'
     1       ,//,'     1. THE TIME-BETWEEN-FAILURES AS MEASURED IN WALL'
     2          ,' CLOCK OR COM-'
     3        ,/,'        PUTER CPU TIME.  THE DATA SHOULD HAVE  BEEN E'
     4          ,'NTERED VIA THE'
     5        ,/,'        INPUT MODULE.'
     6       ,//,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     7          ,'HE APPROPRIATE'
     8        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     9          ,'EL;  OTHERWISE'
     A        ,/,'    ZERO TO RETURN TO THE MENU OF MODELS.')
  305    FORMAT (' '
     1        ,/,' **THE DATA ARE NOT APPROPRIATE FOR THE MODEL. ')
  306    FORMAT (' '
     1        ,/,' **THE ESTIMATE  FOR THE TOTAL  NUMBER OF FAULTS  TO '
     2          ,'BE DETECTED IN'
     3        ,/,'   THE PROGRAM BEFORE  ALL FAULTS ARE UNCOVERED  CAME'
     4          ,' OUT LESS THAN'
     5        ,/,'   THE NUMBER OF FAULTS FOUND TO DATE WITHIN THE MODE'
     6          ,'L.')
  310    FORMAT (' '
     1        ,/,'   MODEL ESTIMATES ARE:'
     2        ,/,'    PROPORTIONALITY CONSTANT ',E12.5
     3        ,/,'    INIT. INTENSITY FUNCTION ',E12.5
     5        ,/,'    CUR.  INTENSITY FUNCTION ',E12.5
     7        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5
     8        ,/,'    # OF FAULTS REMAINING    ',E12.5
     9        ,/,'    PURIFICATION LEVEL       ',E12.5)
  311    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   (ROUNDED) NUMBER OF FAULTS REMAINING IS LESS THAN '
     4          ,'ONE.')
  315    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE EXPECTED RELIABILITY FOR A SPECIFIED TIME'
     3        ,/,'    2) THE TIME TO  REACH A SPECIFIED  RELIABILITY F'
     4          ,'OR A SPECIFIED'
     5        ,/,'       OPERATIONAL TIME'
     6        ,/,'    3) THE TIME TO REACH A DESIRED INTENSITY FUNCTION'
     7        ,/,'    4) THE NUMBER OF FAILURES EXPECTED IN A SPECIFIED'
     8          ,' TIME'
     9        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     A          ,'S.')
  320    FORMAT ('    ENTER LENGTH OF THE PERIOD, OR ZERO TO END.')
  325    FORMAT ('    FUTURE RELIABILITY      ',E12.5,/)
  330    FORMAT ('    ENTER DESIRED RELIABILITY, OR ZERO TO END.')
  335    FORMAT ('    ENTER SPECIFIED OPERATIONAL TIME.')
  340    FORMAT ('    ADDITIONAL TESTING TIME ',E12.5,/)
  345    FORMAT ('    ENTER INTENSITY FUNC. (LESS THAN',E12.5,'), OR ZE'
     1          ,'RO TO END.')
  350    FORMAT ('    # OF FAILURES EXPECTED  ',E12.5,/)
  355    FORMAT (' '
     1        ,/,'   ENTER ONE TO PERFORM AN ANALYSIS OF THE MODEL FIT '
     2          ,'USING THE PRE-'
     3        ,/,'   DICTIONS OF THIS MODEL; ELSE ZERO.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,301)
             WRITE (HIST,301)
             WRITE (   *,302)
             WRITE (HIST,302)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,303)
             WRITE (HIST,303)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
C
         IF (CONFLG.EQ.1) THEN
C            STORE THE TBF DATA; AND CONVERT DATA TO TTF UNITS.
             IDXE = IDX + NS - 1
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = IDX, IDXE)
             CALL SMFTRN
     G                   (NS      , DM      , DM     , 6
     B                   ,DAT(IDX)
     Y                   ,RFLAG   )
C
             CALL SNPTMD
     G                   (DAT(IDX) , NS
     Y                   ,STATS    , RFLAG    )
C
             IF (RFLAG.EQ.3) THEN
C                THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
                 WRITE (   *,305)
                 WRITE (HIST,305)
             ELSEIF (RFLAG.EQ.4) THEN
C                THE  TOTAL ERRORS ESTIMATE  WAS LESS THAN  THE OBSERVED
C                ERRORS TO DATE.
                 WRITE (   *,306)
                 WRITE (HIST,306)
             ELSE
C            ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY---------------
C                    A SOLUTION WAS FOUND; COMPUTE THE EXTRA INITIAL AND
C                    CURRENT INTENSITY FUNCTIONS,  THE NUMBER  OF FAULTS
C                    REMAINING, AND THE PURIFICATION LEVEL.
                     IIF    = STATS(2) * STATS(1)
                     CIF    = IIF      * EXP(-STATS(1) * DAT(IDX+NS-1))
                     TNOFR  = STATS(2) - REAL(NS)
                     PURLVL = REAL(NS) / STATS(2)
C
C                    OUTPUT THE RESULTS.
                     WRITE (   *,310) STATS(1), IIF     , CIF
     1                               ,STATS(2), TNOFR   , PURLVL
                     WRITE (HIST,310) STATS(1), IIF     , CIF
     1                               ,STATS(2), TNOFR   , PURLVL
C
                     IF (TNOFR.LT.0.5) THEN
C                        FUTURE PREDICTIONS ARE NOT ALLOWED (TNOFR).
                         WRITE (   *,311)
                         WRITE (HIST,311)
                     ELSE
C                        DOWHILE (FLAG .NE. TERMINATION)
 1000                        WRITE (   *,315)
                             WRITE (HIST,315)
                             READ  (   *,  *) FLAG
                             WRITE (HIST,  *) FLAG
                             IF ((FLAG.LT.1) .OR. (FLAG.GT.4))GO TO 1225
C
                             IF     (FLAG.EQ.1) THEN
C                                FUTURE RELIABILITY FOR SPECIFIED TIME.
                                 NPV =  1
                                 SR  = -1.0
C                                DOWHILE (SOT .NE. TERMINATION)
 1025                                WRITE (   *,320)
                                     WRITE (HIST,320)
                                     READ  (   *,  *) SOT
                                     WRITE (HIST,  *) SOT
                                     IF (SOT.EQ.0.0) GO TO 1050
                                     CALL SNPTPR
     G                                           (NPV      , NS
     G                                           ,DAT(IDX) , STATS(1)
     G                                           ,STATS(2) , SOT
     G                                           ,SR
     Y                                           ,XPRD     , D
     Y                                           ,DFLG     ,TM1C     )
                                     WRITE (   *,325) XPRD(1)
                                     WRITE (HIST,325) XPRD(1)
                                     GO TO 1025
 1050                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.2) THEN
C                                TESTING TIME TO ACHIEVE SPECIFIED RELI-
C                                ABILITY FOR SPECIFIED OPERATIONAL TIME.
                                 NPV = 1
C                                DOWHILE (SR .NE. TERMINATION)
 1075                                WRITE (   *,330)
                                     WRITE (HIST,330)
                                     READ  (   *,  *) SR
                                     WRITE (HIST,  *) SR
                                     IF (SR.EQ.0.0) GO TO 1100
                                     WRITE (   *,335)
                                     WRITE (HIST,335)
                                     READ  (   *,  *) SOT
                                     WRITE (HIST,  *) SOT
                                     CALL SNPTPR
     G                                           (NPV      , NS
     G                                           ,DAT(IDX) , STATS(1)
     G                                           ,STATS(2) , SOT
     G                                           ,SR
     Y                                           ,XPRD     , D
     Y                                           ,DFLG     , TM1C     )
                                     WRITE (   *,340) XPRD(1)
                                     WRITE (HIST,340) XPRD(1)
                                     GO TO 1075
 1100                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.3) THEN
C                                TIME TO REACH A DESIRED INTENSITY FUNC-
C                                TION.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1125                                WRITE (   *,345) CIF
                                     WRITE (HIST,345) CIF
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1150
                                     PR3 = 1.0 / STATS(1)
     1                                   * (LOG(CIF) - LOG(PRINR))
                                     WRITE (   *,340) PR3
                                     WRITE (HIST,340) PR3
                                     GO TO 1125
 1150                                CONTINUE
C                                ENDWHILE
C
                             ELSEIF (FLAG.EQ.4) THEN
C                                FAILURES FOUND IN A SPECIFIED TIME.
C                                DOWHILE (PRINR .NE. TERMINATION)
 1175                                WRITE (   *,320)
                                     WRITE (HIST,320)
                                     READ  (   *,  *) PRINR
                                     WRITE (HIST,  *) PRINR
                                     IF (PRINR.EQ.0.0) GO TO 1200
                                     PR5 = (STATS(2) * EXP(-STATS(1)
     1                                   *   DAT(IDX+NS-1)))
     2                                   - (STATS(2) * EXP(-STATS(1)
     1                                   *  (DAT(IDX+NS-1) + PRINR)))
                                     IF (PR5.GT.TNOFR) THEN
                                         PR5 = TNOFR
                                     ENDIF
                                     WRITE (   *,350) PR5
                                     WRITE (HIST,350) PR5
                                     GO TO 1175
 1200                                CONTINUE
C                                ENDWHILE
                             ENDIF
                             GO TO 1000
 1225                        CONTINUE
C                        ENDWHILE
                     ENDIF
C            ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY---------------
             ENDIF
C
             FLAG = 0
             IF (RFLAG.EQ.0) THEN
C                THE FINAL EXECUTION WAS SUCCESSFUL, PROMPT USER AND SEE
C                IF THE PREDICTED VECTOR CREATION IS DESIRED.
                 WRITE (   *,355)
                 WRITE (HIST,355)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    ACCESS THE SNPTPR ROUTINE TO GENERATE THE PREDICTED
C                    TBF DATA VECTOR.
                     CALL SNPTPR
     G                           (NS       , NS       , DAT(IDX)
     G                           ,STATS(1) , STATS(2) , SOT         , SR
     Y                           ,PRD      , D        , DFLG
     Y                           ,TM1C     )
C
C                    SET THE ESTIMATION SELECTION CODE TO INDICATE WHICH
C                    MODEL WAS RESPONSIBLE FOR  THE PREDICTIONS; AND SET
C                    ESD TO INDICATE WHICH DATA TYPE (WC OR CPU) WAS RE-
C                    SPONSIBLE.
                     ESC = 4
                     ESD = 1
                     IF (IDX.EQ.STR2) THEN
                         ESD = 2
                     ENDIF
C
C                    RESTORE TBF DATA AND PERFORM ANALYSIS OF MODEL FIT.
                     REWIND TMPB
                     READ  (TMPB) (DAT(I), I = IDX, IDXE)
                     CALL FITDRV
                 ENDIF
             ENDIF
C
             IF (FLAG.EQ.0) THEN
C                TBF DATA HAS NOT BEEN RESTORED, YET.
                 REWIND TMPB
                 READ  (TMPB) (DAT(I), I = IDX, IDXE)
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE MDLSDW
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE MDLSDW
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
C        TO PROVIDE THE INTERACTIVE PROMPTS AND INPUTS FOR THE SCHNEIDE-
C        WIND  MODEL FOR INTERVAL DATA.  ADDITIONALLY,  THE ROUTINE WILL
C        PERFORM THE ACCESS TO THE SMFLIB ROUTINES NEEDED TO PERFORM THE
C        MODEL EXECUTIONS AND PREDICTED DATA VECTOR CREATION.
C   DESCRIPTION
C        TWO SMFLIB  ROUTINES  (SSDWMD AND SSDWPR)  WILL BE USED  WITHIN
C        THIS ROUTINE.
C
C        THE FIRST ROUTINE (SSDWMD) PROVIDES THE CALCULATIONS OF THE RE-
C        LIABILITY VIA SCHNEIDEWIND'S MAXIMUM LIKELIHOOD MODEL. THIS MO-
C        DEL ALLOWS FOR THREE DIFFERENT TREATMENT TYPES TO BE APPLIED TO
C        TO THE FAULT COUNT DATA. (THE THREE TYPES CAN BE EXTRACTED FROM
C        FORMAT NUMBER 310  WITHIN THIS ROUTINE.)  THE THIRD ARGUMENT OF
C        THE SSDWMD CALL LINE  PROVIDES THE  ROUTINE WITH  THE NUMBER OF
C        THE TREATMENT TYPE DESIRED.
C
C        ONCE THE  EXECUTION OF  SSDWMD IS PERFORMED,  THE RETURN STATUS
C        FLAG (RFLAG) WILL INDICATE THE SUCCESSFULNESS  OF THE MODEL ES-
C        TIMATION PROCESS. THE FOLLOWING VALUES ARE POSSIBLE:
C             0 - SUCCESSFUL CONVERGENCE WITHIN THE MODEL
C             3 - THE DATA ARE NOT APPROPRIATE FOR THE MODEL
C             4 - THE TOTAL FAULTS  ESTIMATE WAS LESS THAN THE  OBSERVED
C                 FAULTS TO DATE
C
C        FOR AN RFLAG OF 0 THE COMPLETE MODEL ESTIMATES WILL BE LISTED.
C
C        THE SECOND ROUTINE (SBAMPR) PROVIDES FUTURE PREDICTIONS &  GEN-
C        ERATES THE VECTOR OF PREDICTED FAULT COUNTS.
C
C        NOTE THAT THE GOODNESS OF FIT ANALYSES FOR  THE TREATMENT TYPES
C        2 AND 3 WILL NOT BE PERFORMED USING THE ENTIRE FAULT COUNT DATA
C        SET.  THE ORIGINAL DATA VECTOR IS REDUCED TO REFLECT THE TREAT-
C        MENT TYPE.  SINCE ALL SPECIAL  STORAGE FILES AND STORAGE ARRAYS
C        ARE NEEDED, THE COMMON BLOCK FOR THE MODEL APPLICABILITY MODULE
C        IS USED TO STORE THE DATA WHILE THE ANALYSES ARE PERFORMED. AF-
C        TER ALL ANALYSES ARE COMPLETED, THE DATA IS RESTORED TO REFLECT
C        ALL OBSERVED FAULTS.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C        YIELDED
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             EST    (I) = NUMBER OF ESTIMATIONS FROM THE MODEL
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             TM3C   (R) = MODEL APPLICABILITY TEMP. ARRAY, USED HERE TO
C                          STORE ORIGINAL FAULT DATA (SEE DESCRIPTION)
C             XPRD   (R) = FUTURE PREDICTIONS (NUMBER AND 95% C.I.)
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             CONFLG (I) = CONTINUE WITH MODEL EXECUTION FLAG
C             CTOT   (R) = TOTAL FAULTS FOUND TO DATE
C             DNOF   (R) = DESIRED NUMBER OF FAULTS
C             DNOP   (R) = DESIRED NUMBER OF PERIODS
C             FLAG   (I) = OPTION SELECTION FLAG
C             NPV    (I) = NUMBER OF PREDICTED VALUES FLAG
C             NS     (I) = NUMBER OF ENTRIES IN DATA VECTOR
C             OTSB   (I) = OPTIMUM TREATMENT SELECTION BEGIN  POINT
C             OTSF   (I) = OPTIMUM TREATMENT SELECTION FINISH POINT
C             PREDAT (R) = PREDICTION DATA VALUES ARRAY
C             RFLAG  (I) = RETURN STATUS FLAG
C             SHFT   (I) = VALUE OF SHIFT FOR EXTRA MESSAGE 395
C             STATS  (R) = STATISTICAL RESULTS VECTOR
C             TMP    (R) = INTERMEDIATE CALCULATION
C             TNOFR  (R) = TOTAL NUMBER OF FAULTS REMAINING ESTIMATE
C             TSF    (I) = TREATMENT SELECTION FLAG
C             TSS    (I) = TREATMENT SELECTION STARTING POINT
C             WT     (I) = FLAG TO PAUSE THE ASSUMPTIONS LIST OUTPUT
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
C               /PRECOM/      PREDICTION DATA STORAGE COMMON
         COMMON /PRECOM/      PRD(END1)     ,XPRD(3)       ,D
     1                       ,DFLG          ,ESC           ,ESD
     2                       ,EST
         INTEGER              DFLG          ,ESC           ,ESD
     1                       ,EST
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CONFLG        ,FLAG          ,I
     1                       ,J             ,NPV           ,NS
     2                       ,OTSB          ,OTSF          ,RFLAG
     3                       ,SHFT          ,TSF           ,TSS
     4                       ,WT
         DIMENSION            PREDAT(3,4)   ,STATS(6)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR SCHNEIDEWIND MODEL DESCRIPTION; ELSE'
     1          ,' ZERO.')
  301    FORMAT (' '
     1        ,/,'    THE SCHNEIDEWIND MODEL ASSUMPTIONS'
     2       ,//,'     1. THE SOFTWARE IS OPERATED IN A SIMILAR MANNER '
     3          ,'AS THE ANTICI-'
     4        ,/,'        PATED OPERATIONAL USAGE.'
     5       ,//,'     2. ALL FAULTS ARE EQUALLY LIKELY TO OCCUR  AND A'
     6          ,'RE INDEPENDENT'
     7        ,/,'        OF EACH OTHER.'
     8       ,//,'     3. EACH FAULT IS OF THE SAME ORDER OF "IMPACT" A'
     9          ,'S ANY OTHER.'
     A       ,//,'     4. THE MEAN NUMBER OF DETECTED FAULTS DECREASES '
     B          ,'FROM ONE TEST-')
  302    FORMAT ('        ING INTERVAL TO THE NEXT.'
     1       ,//,'     5. THE FAULT CORRECTION  RATE IS PROPORTIONAL TO'
     2          ,' THE NUMBER OF'
     3        ,/,'        FAULTS TO BE CORRECTED.'
     4       ,//,'     6. ALL TESTING PERIODS ARE OF THE SAME LENGTH.'
     5       ,//,'    ENTER ONE TO CONTINUE THE DESCRIPTION LIST.')
  303    FORMAT ('     7. THE RATE OF  FAULT DETECTION IS  PROPORTIONAL'
     1          ,' TO THE NUMBER'
     2        ,/,'        OF FAULTS WITHIN THE PROGRAM AT THE TIME OF T'
     3          ,'EST. THE FAULT'
     4        ,/,'        DETECTION PROCESS IS A NON-HOMOGENEOUS POISSO'
     5          ,'N PROCESS WITH'
     6        ,/,'        AN EXPONENTIALLY DECREASING FAILURE RATE.'
     7       ,//,'    MODEL DATA REQUIREMENTS'
     8       ,//,'     1. THE NUMBER OF FAULTS  DETECTED IN EACH INTERV'
     9          ,'AL OF TESTING.'
     A        ,/,'        THE DATA SHOULD HAVE BEEN ENTERED VIA THE INP'
     B          ,'UT MODULE.')
  304    FORMAT (' '
     1        ,/,'    IF THE ASSUMPTIONS ARE  GENERALLY SATISFIED AND T'
     2          ,'HE APPROPRIATE'
     3        ,/,'    DATA ARE AVAILABLE, ENTER ONE TO CONTINUE THE MOD'
     4          ,'EL;  OTHERWISE'
     5        ,/,'    ZERO TO RETURN TO THE MENU OF MODELS.')
  305    FORMAT ('   ENTER ONE FOR DESCRIPTION OF TREATMENT TYPES; ELSE'
     1          ,' ZERO.')
  310    FORMAT ('    TREATMENT 1 - UTILIZE ALL FAULT COUNTS FROM EACH '
     1          ,'OF THE TESTING'
     2        ,/,'                  PERIODS.'
     3       ,//,'    TREATMENT 2 - IGNORE FAULT COUNTS FROM THE  FIRST'
     4          ,' "S-1" TESTING'
     5        ,/,'                  PERIODS  AND USE  ONLY THE FAULT CO'
     6          ,'UNTS FROM PER-'
     7        ,/,'                  IOD "S" THROUGH THE TOTAL NUMBER OF'
     8          ,' PERIODS.'
     9       ,//,'    TREATMENT 3 - USE CUMULATIVE NUMBER OF FAULTS FRO'
     A          ,'M  PERIODS ONE'
     B        ,/,'                  THROUGH "S-1" AND THE INDIVIDUAL CO'
     C          ,'UNTS FROM PER-'
     D        ,/,'                  IOD "S" THROUGH THE TOTAL NUMBER OF'
     E          ,' PERIODS.')
  315    FORMAT (' '
     1        ,/,'   ENTER ONE TO INVESTIGATE FOR THE OPTIMUM S (USING '
     2          ,'TREATMENT TYPE'
     3        ,/,'   NUMBER 2); ELSE ZERO TO CONTINUE WITH THE MODEL EX'
     4          ,'ECUTION.')
  320    FORMAT ('    ENTER RANGE OVER WHICH S  SHOULD BE TESTED.  NOTE'
     1          ,', AN EXECUTION'
     2        ,/,'    ON A GIVEN S WHICH FAILED THE CONVERGENCE CRITERI'
     3          ,'A  WILL NOT BE'
     4        ,/,'    INCLUDED IN THE FOLLOWING RESULTS TABLE.  THE OPT'
     5          ,'IMUM S FOR EI-'
     6        ,/,'    THER MSE-F OR MSE-T IS THE ONE  RESULTING IN THE '
     7          ,'SMALLEST VALUE'
     8        ,/,'    FOR YOUR CHOSEN CRITERIA.')
  325    FORMAT (' '
     1        ,/,'     S     BETA        ALPHA        WLS        MSE-F '
     2          ,'      MSE-T   '
     3        ,/,'    === =========== =========== =========== ========='
     4          ,'== ===========')
  330    FORMAT (3X,I4,5E12.5)
  335    FORMAT (' '
     1        ,/,'    ENTER ONE TO INVESTIGATE ANOTHER RANGE FOR S; ELS'
     2          ,'E ZERO.')
  340    FORMAT (' '
     1        ,/,'   ENTER DESIRED MODEL TREATMENT NUMBER, OR FOUR TO T'
     2          ,'ERMINATE MODEL'
     3        ,/,'   EXECUTION.')
  345    FORMAT ('    ENTER ASSOCIATED VALUE OF S (LESS THAN THE NUMBER'
     1          ,' OF PERIODS).')
  350    FORMAT (' '
     1        ,/,' **THE DATA ARE NOT APPROPRIATE FOR THE TREATMENT NUM'
     2          ,'BER',I2,' MODEL.')
  351    FORMAT (' '
     1        ,/,' **THE ESTIMATE  FOR THE TOTAL  NUMBER OF FAULTS  TO '
     2          ,'BE DETECTED IN'
     3        ,/,'   THE PROGRAM BEFORE  ALL FAULTS ARE UNCOVERED  CAME'
     4          ,' OUT LESS THAN'
     5        ,/,'   THE NUMBER OF FAULTS FOUND TO DATE WITHIN THE TREA'
     6          ,'TMENT NUMBER',I2
     7        ,/,'   MODEL.')
  355    FORMAT (' '
     1        ,/,'   TREATMENT',I2,' MODEL ESTIMATES ARE:'
     2        ,/,'    BETA                     ',E12.5
     3        ,/,'    ALPHA                    ',E12.5
     4        ,/,'    TOTAL NUMBER OF FAULTS   ',E12.5)
  356    FORMAT ('     PLUS THOSE SKIPPED      ',E12.5,' IN PERIODS 1 T'
     1          ,'HROUGH',I4)
  357    FORMAT ('    # OF FAULTS REMAINING    ',E12.5
     1        ,/,'    WEIGHTED SUMS-OF-SQUARES '
     2        ,/,'     BETWEEN PREDICTED AND   '
     3        ,/,'     OBSERVED FAULTS         ',E12.5
     4        ,/,'    MEAN SQUARE ERROR FOR    '
     5        ,/,'     CUMULATIVE FAULTS       ',E12.5)
  358    FORMAT ('    MEAN SQUARE ERROR FOR    '
     1        ,/,'     TIME TO NEXT FAILURE    ',E12.5)
  359    FORMAT (' '
     1        ,/,' **FUTURE PREDICTIONS ARE NOT ALLOWED  BECAUSE THE ES'
     2          ,'TIMATE FOR THE'
     3        ,/,'   (ROUNDED) NUMBER OF FAULTS REMAINING IS LESS THAN '
     4          ,'ONE.')
  360    FORMAT (' '
     1        ,/,'   THE AVAILABLE FUTURE PREDICTIONS ARE:'
     2        ,/,'    1) THE NUMBER OF FAULTS EXPECTED IN THE NEXT TEST'
     3          ,'ING PERIOD'
     4        ,/,'    2) THE NUMBER OF PERIODS NEEDED TO DISCOVER THE N'
     5          ,'EXT M FAULTS'
     6        ,/,'   ENTER PREDICTION OPTION, OR ZERO TO END PREDICTION'
     7          ,'S.')
  364    FORMAT ('    ENTER NUMBER OF PERIODS TO EXAMINE, OR ZERO TO EN'
     1          ,'D.')
  365    FORMAT ('    # OF FAULTS EXPECTED     ',E12.5,/)
  370    FORMAT ('    ENTER VALUE OF M (BETWEEN ONE AND',E12.5,'), OR Z'
     1          ,'ERO TO END.')
  375    FORMAT (' ** THE ESTIMATE CANNOT BE MADE FOR THE SPECIFIED M V'
     1          ,'ALUE.')
  380    FORMAT ('    # OF PERIODS EXPECTED    ',E12.5,/)
  385    FORMAT (' '
     1        ,/,'   ENTER ONE TO PERFORM AN ANALYSIS OF THE MODEL FIT '
     2          ,'USING THE PRE-'
     3        ,/,'   DICTIONS OF THIS MODEL; ELSE ZERO.')
  390    FORMAT ('   ENTER TREATMENT NUMBER BY WHICH THE PREDICTIONS AR'
     1          ,'E TO BE MADE.')
  395    FORMAT (' '
     1        ,/,'   NOTE: THE STARTING INDEX OF THE DATA IS SHIFTED BY'
     2          ,I4,' UNITS TO'
     3        ,/,'   REFLECT TREATMENT TYPE',I2)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE VECTOR SIZE COUNTER.
         NS = PRC(3,1)
C
C        ZERO THE SUCCESSFULNESS  ELEMENTS OF THE PREDICTION DATA ARRAY.
         PREDAT(1,1) = 0.0
         PREDAT(2,1) = 0.0
         PREDAT(3,1) = 0.0
C
C        ISSUE THE MODEL DESCRIPTION PROMPT AND PROVIDE THE INFORMATION.
         WRITE (   *,300)
         WRITE (HIST,300)
         READ  (   *,  *) FLAG
         WRITE (HIST,  *) FLAG
         CONFLG = 1
         IF (FLAG.EQ.1) THEN
             WRITE (   *,301)
             WRITE (HIST,301)
             WRITE (   *,302)
             WRITE (HIST,302)
             READ  (   *,  *) WT
             WRITE (HIST,  *) WT
             WRITE (   *,303)
             WRITE (HIST,303)
             WRITE (   *,304)
             WRITE (HIST,304)
             READ  (   *,  *) CONFLG
             WRITE (HIST,  *) CONFLG
         ENDIF
C
         IF (CONFLG.EQ.1) THEN
C            ISSUE THE TREATMENT TYPE DESCRIPTION PROMPT, INPUT THE USER
C            RESPONSE, AND OUTPUT THE DESCRIPTION IF DESIRED.
             WRITE (   *,305)
             WRITE (HIST,305)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
             IF (FLAG.EQ.1) THEN
                 WRITE (   *,310)
                 WRITE (HIST,310)
             ENDIF
C
C            DETERMINE THE NUMBER OF FAULTS FOUND TO DATE  FOR THE ESTI-
C            MATE OF THE TOTAL NUMBER OF FAULTS REMAINING.
             CTOT = 0.0
             DO 1000 I = 1, NS
                 CTOT = CTOT + DAT(I)
 1000            CONTINUE
C            ENDDO
C
C            DETERMINE IF THE INVESTIGATION FOR OPTIMUM S IS DESIRED.
             WRITE (   *,315)
             WRITE (HIST,315)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
             IF (FLAG.EQ.1) THEN
                 TSF = 2
C                DOWHILE (FLAG .EQ. 1)
 1025                WRITE (   *,320)
                     WRITE (HIST,320)
                     READ  (   *,  *) OTSB, OTSF
                     WRITE (HIST,  *) OTSB, OTSF
                     WRITE (   *,325)
                     WRITE (HIST,325)
                     DO 1050 I = OTSB, OTSF
                         CALL SSDWMD
     G                               (DAT   , NS    , TSF   , I
     Y                               ,STATS , RFLAG )
                         IF (RFLAG.EQ.0) THEN
                             IF (STATS(6).NE.-1.0) THEN
                                 WRITE (   *,330) I, STATS(1), STATS(2)
     1                                             , STATS(4), STATS(5)
     2                                             , STATS(6)
                                 WRITE (HIST,330) I, STATS(1), STATS(2)
     1                                             , STATS(4), STATS(5)
     2                                             , STATS(6)
                             ELSE
                                 WRITE (   *,330) I, STATS(1), STATS(2)
     1                                             , STATS(4), STATS(5)
                                 WRITE (HIST,330) I, STATS(1), STATS(2)
     1                                             , STATS(4), STATS(5)
                             ENDIF
                         ENDIF
 1050                    CONTINUE
C                    ENDDO
                     WRITE (   *,335)
                     WRITE (HIST,335)
                     READ  (   *,  *) FLAG
                     WRITE (HIST,  *) FLAG
                     IF (FLAG.EQ.1) GO TO 1025
C                ENDWHILE
             ENDIF
C
C            DOWHILE (TSF .NE. TERMINATION)
 1075            WRITE (   *,340)
                 WRITE (HIST,340)
                 READ  (   *,  *) TSF
                 WRITE (HIST,  *) TSF
                 IF ((TSF.GE.1) .AND. (TSF.LE.3)) THEN
                 PREDAT(TSF,1) = 0.0
                 IF (TSF.EQ.1) THEN
                     TSS = 1
                 ELSE
                     WRITE (   *,345)
                     WRITE (HIST,345)
                     READ  (   *,  *) TSS
                     WRITE (HIST,  *) TSS
                 ENDIF
C
                 CALL SSDWMD
     G                       (DAT   , NS    , TSF  , TSS
     Y                       ,STATS , RFLAG )
C
                 IF (RFLAG.EQ.3) THEN
C                    THE DATA ARE NOT APPROPRIATE FOR THE MODEL.
                     WRITE (   *,350) TSF
                     WRITE (HIST,350) TSF
                 ELSEIF (RFLAG.EQ.4) THEN
C                    THE  TOTAL ERRORS  ESTIMATE  WAS LESS THAN  THE OB-
C                    SERVED ERRORS TO DATE.
                     WRITE (   *,351) TSF
                     WRITE (HIST,351) TSF
                 ELSE
C                ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY-----------
C                        A SOLUTION WAS FOUND;  COMPUTE THE EXTRA NUMBER
C                        OF FAULTS REMAINING.
                         TNOFR = STATS(3) - CTOT
C
C                        OUTPUT THE RESULTS.
                         WRITE (   *,355) TSF     , STATS(1), STATS(2)
     1                                  , STATS(3)
                         WRITE (HIST,355) TSF     , STATS(1), STATS(2)
     1                                  , STATS(3)
C
                         IF (TSF.EQ.2) THEN
                             J   = TSS    - 1
                             TMP = DAT(1)
                             DO 1080 I = 2, J
                                 TMP = TMP + DAT(I)
 1080                            CONTINUE
C                            ENDDO
                             WRITE (   *,356) TMP     , J
                             WRITE (HIST,356) TMP     , J
                         ENDIF
C
                         WRITE (   *,357) TNOFR   , STATS(4), STATS(5)
                         WRITE (HIST,357) TNOFR   , STATS(4), STATS(5)
C
                         IF (STATS(6).NE.-1.0) THEN
                             WRITE (   *,358) STATS(6)
                             WRITE (HIST,358) STATS(6)
                         ENDIF
C
C                        SET THE SUCCESSFULNESS FLAG, AND STORE THE BETA
C                        AND ALPHA ESTIMATES, AND THE VALUE FOR S.
                         PREDAT(TSF,1) = 1.0
                         PREDAT(TSF,2) = STATS(1)
                         PREDAT(TSF,3) = STATS(2)
                         PREDAT(TSF,4) = REAL(TSS)
C
                         IF (TNOFR.LT.0.5) THEN
C                            FUTURE PREDICTIONS ARE NOT ALLOWED (TNOFR).
                             WRITE (   *,359)
                             WRITE (HIST,359)
                         ELSE
C                            DOWHILE (FLAG .NE. TERMINATION)
 1100                            WRITE (   *,360)
                                 WRITE (HIST,360)
                                 READ  (   *,  *) FLAG
                                 WRITE (HIST,  *) FLAG
                                 IF ((FLAG.LT.1) .OR. (FLAG.GT.2))
     1                                                        GO TO 1175
C
                                 IF     (FLAG.EQ.1) THEN
C                                    FAULTS IN NEXT TESTING PERIOD.
                                     NPV  = 1
                                     DNOF = -1.0
C                                    DOWHILE (DNOF .NE. TERMINATION)
 1105                                    WRITE (   *,364)
                                         WRITE (HIST,364)
                                         READ  (   *,  *) DNOP
                                         WRITE (HIST,  *) DNOP
                                         IF (DNOP.EQ.0.0) GO TO 1110
                                         CALL SSDWPR
     G                                               (NPV     , NS
     G                                               ,DNOF    , DNOP
     G                                               ,STATS(1), STATS(2)
     G                                               ,DAT     , TSF
     G                                               ,TSS
     Y                                               ,XPRD    , RFLAG  )
                                         IF (XPRD(1).GT.TNOFR) THEN
                                             XPRD(1) = TNOFR
                                         ENDIF
                                         WRITE (   *,365) XPRD(1)
                                         WRITE (HIST,365) XPRD(1)
                                         GO TO 1105
 1110                                    CONTINUE
C                                   ENDWHILE
C
                                 ELSEIF (FLAG.EQ.2) THEN
C                                    PERIODS TO DETECT NEXT M FAULTS.
                                     NPV = 1
C                                    DOWHILE (DNOF .NE. TERMINATION)
 1125                                    WRITE (   *,370) TNOFR
                                         WRITE (HIST,370) TNOFR
                                         READ  (   *,  *) DNOF
                                         WRITE (HIST,  *) DNOF
                                         IF (DNOF.EQ.0.0) GO TO 1150
                                         CALL SSDWPR
     G                                               (NPV     , NS
     G                                               ,DNOF    , DNOP
     G                                               ,STATS(1), STATS(2)
     G                                               ,DAT     , TSF
     G                                               ,TSS
     Y                                               ,XPRD    , RFLAG  )
                                         IF (RFLAG.EQ.1) THEN
C                                            THE ESTIMATE CANNOT BE MADE
C                                            FOR THE INPUT DNOF VALUE.
                                             WRITE (   *,375)
                                             WRITE (HIST,375)
                                         ELSE
                                             WRITE (   *,380) XPRD(1)
                                             WRITE (HIST,380) XPRD(1)
                                         ENDIF
                                         GO TO 1125
 1150                                    CONTINUE
C                                   ENDWHILE
                                ENDIF
                                GO TO 1100
 1175                           CONTINUE
C                            ENDWHILE
                         ENDIF
C                ----MISALIGNED BY TNOF TEST MOVED TO LIBRARY-----------
                 ENDIF
                 GO TO 1075
                 ENDIF
C            ENDWHILE
C
             IF ((PREDAT(1,1).EQ.1.0) .OR. (PREDAT(2,1).EQ.1.0)
     1                                .OR. (PREDAT(3,1).EQ.1.0)) THEN
C                AT LEAST ONE EXECUTION  WAS SUCCESSFUL, PROMPT USER AND
C                SEE IF THE PREDICTED VECTOR CREATION IS DESIRED.
                 WRITE (   *,385)
                 WRITE (HIST,385)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    THE VECTOR OF PREDICTED DATA IS DESIRED.
                     TMP = PREDAT(1,1) + PREDAT(2,1) + PREDAT(3,1)
                     IF (TMP.EQ.1.0) THEN
C                        ONLY ONE EXECUTION  WAS SUCCESSFUL;  SET TSF TO
C                        INDICATE THE TREATMENT TYPE.
                         TSF = 1
                         IF (PREDAT(2,1).EQ.1.0) THEN
                             TSF = 2
                         ELSEIF (PREDAT(3,1).EQ.1.0) THEN
                             TSF = 3
                         ENDIF
                     ELSE
C                        AT LEAST  TWO TREATMENT  TYPES WERE SUCCESSFUL;
C                        SEE WHICH THE USER DESIRES.
                         WRITE (   *,390)
                         WRITE (HIST,390)
                         READ  (   *,  *) TSF
                         WRITE (HIST,  *) TSF
                     ENDIF
C
C                    SET THE VALUE OF S AS DEFINED IN THE EXECUTION.
                     TSS = INT(PREDAT(TSF,4))
C
C                    ACCESS THE SSDWPR ROUTINE TO GENERATE THE PREDICTED
C                    FAULT COUNT DATA VECTOR.
                     CALL SSDWPR
     G                           (NS           , NS
     G                           ,DNOF         , DNOP
     G                           ,PREDAT(TSF,2), PREDAT(TSF,3)
     G                           ,DAT          , TSF
     G                           ,TSS
     Y                           ,PRD          , RFLAG        )
C
C                    SET THE ESTIMATION SELECTION CODE TO INDICATE WHICH
C                    MODEL WAS RESPONSIBLE FOR  THE PREDICTIONS; AND SET
C                    ESD TO INDICATE THAT THE INTERVAL DATA TYPE WAS RE-
C                    SPONSIBLE.
                     ESC = 10
                     ESD = 3
C                    SET THE GOODNESS-OF-FIT ESTIMATION CONSTANT.
                     EST = 2
C
                     IF (TSF.NE.1) THEN
C                        THE DATA VECTOR MUST BE ALTERRED TO REFLECT THE
C                        POINTS USED IN THE MODEL.  START BY STORING THE
C                        ORIGINAL DATA VECTOR.
                         DO 1200 I = 1, NS
                             TM3C(I) = DAT(I)
 1200                        CONTINUE
C                        ENDDO
C
                         IF (TSF.EQ.2) THEN
C                            INTERVALS WERE OMITTED; SET VALUE FOR INDEX
C                            SHIFT, AND SET THE TRANSFER INDEX  TO START
C                            AT INTERVAL ONE.
                             SHFT = TSS - 1
                             J    = 1
                         ELSE
C                            INTERVALS WERE MERGED;  SET VALUE FOR INDEX
C                            SHIFT, AND SET THE TRANSFER INDEX  TO START
C                            AT TWO. INTERVAL ONE WILL BE SET TO THE SUM
C                            OF THE FIRST S-1 INTERVALS.
                             SHFT = TSS - 2
                             J    = 2
C
C                            MERGE S-1 INTERVALS ONTO LOCATION ONE.
                             DO 1225 I = 2, (TSS-1)
                                 DAT(1) = DAT(1) + DAT(I)
 1225                            CONTINUE
C                            ENDDO
                         ENDIF
C
C                        SHIFT ALL REMAINING POINTS TOWARD THE START.
                         DO 1250 I = TSS, NS
                             DAT(J) = DAT(I)
                             J      = J      + 1
 1250                        CONTINUE
C                        ENDDO
C
C                        SET THE NEW NUMBER OF INTERVALS  AND SCRIPT THE
C                        EXTRA MESSAGE.
                         PRC(3,1) = NS - SHFT
                         WRITE (   *,395) SHFT, TSF
                         WRITE (HIST,395) SHFT, TSF
                     ENDIF

C                    PERFORM ANALYSIS OF MODEL FIT.
                     CALL FITDRV
C
                     IF (TSF.NE.1) THEN
C                        RESTORE THE ORIGINAL DATA VECTOR AND THE NUMBER
C                        OF INTERVALS TO REFLECT ALL TEST INTERVALS.
                         DO 1275 I = 1, NS
                             DAT(I) = TM3C(I)
 1275                        CONTINUE
C                        ENDDO
                         PRC(3,1) = NS
                     ENDIF
                 ENDIF
             ENDIF
         ENDIF
         RETURN
         END
