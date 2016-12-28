         PROGRAM SMERFS
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   IFDEF UPDATE
C        D O U B L E    P R E C I S I O N    M O D E L
C   PROGRAM STATEMENT
C        PROGRAM SMERFS
C
C   POINT OF CONTACT
C        DR. WILLIAM H. FARR (B10)
C        NAVAL SURFACE WARFARE CENTER DAHLGREN DIVISION (NSWCDD)
C        DAHLGREN, VIRGINIA  22448-5000
C
C        OFFICE PHONE   (703) 663-8388
C   AUTHOR
C        THE BLOOD, SWEAT, AND SLEEPLESS NIGHTS OF THE FOLLOWING PERSONS
C        ARE RESPONSIBLE FOR THE COMPLETE SMERFS PROGRAM; INDIVIDUAL RE-
C        FERENCES WILL BE MADE AT THE ROUTINE LEVEL.
C
C        WILLIAM H. FARR         (B10, NSWCDD)
C        OLIVER  D. SMITH        (EG&G WASC)
C        JAMES   S. VANDERGRAFT  (COMPUTATIONAL ENGINEERING, INC)
C        PAUL    J. KRAUS        (COMPUTATIONAL ENGINEERING, INC)
C        CINDY   A. WELLS        (COMPUTATIONAL ENGINEERING, INC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD) & OLIVER D. SMITH (EG&G WASC)
C   PURPOSE
C        THE  STATISTICAL MODELING  AND ESTIMATION OF  RELIABILITY FUNC-
C        TIONS FOR SOFTWARE (SMERFS) PROGRAM ACTS AS A VEHICLE TO ACCESS
C        THE SMERFS LIBRARY (SMFLIB).  IT IS IN THOSE ROUTINES  THAT THE
C        REAL PROCESSING FOR THE RESULTANT  SOFTWARE RELIABILITY STATIS-
C        TICS WILL BE CONDUCTED.
C   DESCRIPTION
C        THE SMERFS PROGRAM IS DESIGNED TO  ACCEPT THREE  TYPES OF ERROR
C        DATA.  THESE THREE TYPES ARE:
C          1) WALL CLOCK (WC) TIME-BETWEEN-FAILURES (TBF) DATA,
C          2) CENTRAL PROCESSING UNITS (CPU) TBF DATA, AND
C          3) INTERVAL (FAULT COUNTS AND TESTING LENGTHS) DATA.
C
C        THE COMBINATION OF THE ABOVE TYPES OF DATA WITHIN A SINGLE EXE-
C        CUTION IS GOVERNED BY CERTAIN RULES. THESE RULES ARE THE RESULT
C        OF THE METHOD OF STORAGE OF THE DATA IN THE PROGRAM.
C          1) IF INTERVAL DATA (OF FAULTS AND TESTING LENGTHS) ARE BEING
C             ANALYZED, WC AND CPU TBF DATA CANNOT BE.
C          2) IF INTERVAL DATA ARE NOT BEING ANALYZED,  EITHER WC AND/OR
C             CPU TBF DATA CAN BE ANALYZED.
C          3) IF BOTH WC AND CPU TBF DATA ARE TO BE ANALYZED,  BOTH SETS
C             OF DATA MUST HAVE THE SAME NUMBER OF ENTRIES.
C
C        THE PROGRAM ALSO ALLOWS FOR THREE TYPES OF DATA INPUT.
C          1) KEYBOARD ENTRY - WHERE THE PROGRAM PROMPTS FOR ALL DATA,
C          2) ASCII FILE INPUT - WHERE THE ERROR DATA  ARE READ FROM THE
C             FILE, AND THE ADDITIONAL INFORMATION (SUCH AS THE UNITS OF
C             THE TBF DATA) ARE PROMPTED FOR, AND
C          3) "STANDARD" INPUT - WHERE THE FILE  CREATED FROM A PREVIOUS
C             EXECUTION IS READ.
C
C        SMERFS NOT ONLY ALLOWS  FOR THE INPUT AND STORAGE  OF THE THREE
C        ERROR TYPES, IT ALSO ALLOWS FOR THE EDITING OF THOSE TYPES. THE
C        OPTION  (CONTAINED IN THE SECOND MODULE)  IS SUPPLIED TO HANDLE
C        THE AGE-OLD PROBLEM OF USER-INPUT ERRORS AND THE SITUATION OF A
C        RECORDED ERROR(S)  SUBSEQUENTLY  BEING DETERMINED  NOT TO BE AN
C        ACTUAL SOFTWARE ERROR.
C
C        THE THIRD  SMERFS MODULE ALLOWS FOR UNIT TRANSFORMATIONS OF THE
C        STORED DATA.  THIS OPTION IS SUPPLIED  IN HOPES OF  OBTAINING A
C        BETTER MODEL FIT AND ONLY APPLIES TO THE TBF DATA TYPES (WC AND
C        CPU). ONCE THE TRANSFORMATION IS PERFORMED, THE DATA BASE FILES
C        ARE UPDATED UNDER THIS MODULE OPTION.
C
C        THE FOURTH  SMERFS MODULE ALLOWS  FOR OTHER  TRANSFORMATIONS OF
C        THE STORED DATA.  IT IS  SIMILAR IN  FUNCTIONALITY TO THE THIRD
C        MODULE; HOWEVER, THE DATA BASE FILES WILL NOT BE UPDATED.
C
C        ONCE THE DATA BASE IS INPUT  (AND POSSIBLY EDITED AND/OR TRANS-
C        FORMED), GENERAL STATISTICS AND PLOT(S)  OF THE DATA CAN BE OB-
C        TAINED THROUGH THE FIFTH AND SIXTH MODULES,  RESPECTIVELY.  THE
C        GENERATION OF  THE PLOTS  ARE PERFORMED  UTILIZING AN  INTERNAL
C        LINE PRINTER PLOTTER. (SEE MODULE NINE.)
C
C        THE SEVENTH SMERFS MODULE CONTAINS THE MODEL APPLICABILITY ANA-
C        LYSIS:  ACCURACY,  BIAS, NOISE, AND TREND.  CURRENTLY, THE ONLY
C        ANALYSIS AVAILABLE FOR  INTERVAL DATA ANALYSIS IS THE ACCURACY.
C
C        THE EIGHTH SMERFS MODULE  CONTAINS THE ACTUAL ERROR DATA ANALY-
C        SIS. SIX ROUTINES ARE SUPPLIED FOR EXECUTION TIME DATA AND FIVE
C        ARE SUPPLIED FOR INTERVAL DATA. THE SOFTWARE RELIABILTIY MODELS
C        WHICH OPERATE WITH TIME-TO-FAILURE (TTF) DATA DO NOT REQUIRE AN
C        EXTRA PROCESSING BY THE USERS. THE SMERFS PROGRAM AUTOMATICALLY
C        PERFORMS THE CONVERSION OF THE TBF DATA TO TTF DATA, BY SUMMING
C        THE TBF ENTRIES.
C
C        MODULE NINE TERMINATES THE EXECUTION OF THE SMERFS PROGRAM.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   WARNINGS
C        THE SMERFS  DRIVER  AND LIBRARY UTILIZE THE FORTRAN 77 IMPLICIT
C        STATEMENT TO SPECIFY EITHER SINGLE OR DOUBLE PRECISION COMPUTA-
C        TIONS.  THIS VOIDS THE STANDARD  INTEGER DEFAULT SPECIFICATIONS
C        (I.E., THE "I" TO "N" RULE); ALL INTEGERS MUST BE DECLARED.
C   FILES
C             INPUT  (I) = USER INPUT RESPONSES TO PROMPTS
C             OUTPUT (O) = PROGRAM PROMPTS
C             UNIT11 (I) = STANDARD SMERFS WC  TBF FILE
C             UNIT12 (I) = STANDARD SMERFS CPU TBF FILE
C             UNIT13 (I) = STANDARD SMERFS INTERVAL COUNT & LENGTH FILE
C             UNIT14 (O) = UNIT11 UPDATED
C             UNIT15 (O) = UNIT12 UPDATED
C             UNIT16 (O) = UNIT13 UPDATED
C             UNIT17 (O) = EXECUTION HISTORY FILE
C             UNIT18 (B) = TEMPORARY STORAGE FILE (BINARY)
C             UNIT20 (O) = EXECUTION PLOTTING FILE
C             UNIT21 (I) = OPTIONAL ASCII WC  TBF FILE
C             UNIT22 (I) = OPTIONAL ASCII CPU TBF FILE
C             UNIT23 (I) = OPTIONAL ASCII INTERVAL COUNT & LENGTH FILE
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             ESC    (I) = ESTIMATION SELECTION CODE TO INDICATE MODEL
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C             UPDFLG (I) = FLAG TO CONTROL FILE DATA INPUT AND UPDATE
C        YIELDED
C             PLTTYP (I) = ANALYSIS PLOT TYPE
C   LOCAL GLOSSARY
C             FLAG   (I) = OPTION FLAG
C   ERRORS
C        (NONE)
C   ASSOCIATED SUBPROGRAMS
C        SEE AUTOMATED CODE EXAMINER (ACE) OUTPUT
C   EXTERNAL FILES
C        SMERFS LIBRARY FILE
C   REFERENCES
C        "ALGORITHMS FOR MINIMIZATION WITHOUT DERIVATIVES,"  R.P. BRENT,
C        PRENTICE-HALL, 1972.  (FOR THE SUPPLIED DEKKER-BRENT  MINIMIZA-
C        TION ALGORITHM)
C
C        "APPLICATIONS, BASICS, AND COMPUTING OF EXPLORATORY DATA ANALY-
C        SIS," P. VELLEMEN & D. HOAGLIN,  DUXBURY PRESS,  BOSTON, MASS.,
C        1981. (FOR THE SUPPLIED SMOOTHING ROUTINE)
C
C        "A SURVEY OF  SOFTWARE  RELIABILITY  MODELING  AND ESTIMATION,"
C        WILLIAM H. FARR, DAHLGREN, VA., NSWC TR 83-171, 1983.
C
C        "A TRUST REGION METHOD FOR MAXIMUM LIKELIHOOD PARAMETER ESTIMA-
C        TION," JAMES VANDERGRAFT, LAUREL, MD., CEI BTS25-87-26, 1987.
C
C        "COMPUTER METHODS FOR MATHEMATICAL COMPUTATIONS," G.E. FORSYTHE
C        M.A. MALCOLM & C.B. MOLER,  PRENTICE-HALL,  1977.  (FOR DEKKER-
C        BRENT SUPPLIED MINIMIZATION ALGORITHM)
C
C        "COMPUTER PROGRAMMING  AND CODING STANDARDS FOR THE FORTRAN AND
C        SIMSCRIPT II.5 PROGRAMMING LANGUAGES," ROBERT BEVAN & JOHN REY-
C        NOLDS, DAHLGREN, VA., NSWC TR 3878, REVISED 1981.
C
C        "NUMERICAL METHODS FOR UNCONSTRAINED OPTIMIZATION AND NONLINEAR
C        EQUATIONS," J. DENNIS & B. SCHNABEL, PRENTICE-HALL, 1983.  (FOR
C        TRUST REGION SUPPLIED MINIMIZATION ALGORITHM)
C
C        "STATISTICAL MODELING  AND ESTIMATION OF  RELIABILITY FUNCTIONS
C        FOR SOFTWARE (SMERFS) LIBRARY ACCESS GUIDE,"  WILLIAM H. FARR &
C        OLIVER D. SMITH, DAHLGREN, VA., NSWCDD TR 84-371, REVISED 1993.
C
C        "STATISTICAL MODELING  AND ESTIMATION OF  RELIABILITY FUNCTIONS
C        FOR SOFTWARE (SMERFS) USER'S GUIDE,"  WILLIAM H. FARR  & OLIVER
C        D. SMITH, DAHLGREN, VA., NSWCDD TR 84-373, REVISED 1993.
C
C        "TOOLS FOR THE ANALYSIS OF THE ACCURACY OF SOFTWARE RELIABILITY
C        PREDICTIONS,"  B. LITTLEWOOD,  A. A. ABDEL  GHALY,  P. Y. CHAN,
C        SOFTWARE SYSTEM  DESIGN METHODS,  EDITED BY  J. K. SKWIRZYNSKI,
C        NATO ASI SERIES, VOL F22, SPRINGER-VERLOG, 1986, PP. 299-333.
C
C        WILLIAM H. FARR, PHD (B10, NSWCDD) FOR ALL STATISTICAL INFORMA-
C        TION NOT DIRECTLY ACCESSIBLE FROM NSWC TR 82-171.
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
C               /PLTCOM/      PLOTTING COMMON
         COMMON /PLTCOM/      MAKFIL        ,PLTTYP
         INTEGER              MAKFIL        ,PLTTYP
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
         INTEGER              FLAG          ,I             ,J
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'   ENTER MAIN MODULE OPTION, OR ZERO FOR A LIST.')
  305    FORMAT ('1')
  310    FORMAT ('   THE AVAILABLE MAIN MODULE OPTIONS ARE:'
     1        ,/,'    1 DATA INPUT            6 PLOT(S) OF THE RAW DATA'
     2        ,/,'    2 DATA EDIT             7 MODEL APPLICABILITY ANA'
     3          ,'LYSES'
     4        ,/,'    3 UNIT CONVERSIONS      8 EXECUTIONS OF THE MODEL'
     5          ,'S'
     6        ,/,'    4 DATA TRANSFORMATIONS  9 STOP EXECUTION OF SMERF'
     7          ,'S'
     8        ,/,'    5 DATA STATISTICS'
     9        ,/,'   ENTER MAIN MODULE OPTION.')
  315    FORMAT (' '
     1       ,//,'   THE SMERFS EXECUTION HAS ENDED.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        ESTABLISH THE SMERFS HISTORY, PLOT, AND INTERNAL SCRATCH FILES.
C        THE PROGRAM HEADER WILL ALSO BE OUTPUT BY THE ACCESSED ROUTINE.
         CALL FILOPN
C
C        SET GLOBAL VARIABLES TO INDICATE THE ANALYSIS DATA TYPE HAS NOT
C        BEEN ESTABLISHED, THE INPUT DATA FILES MAY BE ACCESSED, AND THE
C        PREDICTION VECTOR IS EMPTY.
         DATTYP =  0
         UPDFLG = -1
         ESC    =  0
C
C        PRESET THE PROCESSING, DATA, AND PREDICTION STORAGE ARRAYS.
         DO 1025 I = 1, 3
             DO 1000 J = 1, 3
                 PRC(I,J) = 0
 1000            CONTINUE
C            ENDDO
 1025        CONTINUE
C        ENDDO
C
         DO 1050 I = 1, END2
             DAT(I) = 0.0
 1050        CONTINUE
C        ENDDO
C
         DO 1075 I = 1, END1
             PRD(I) = 0.0
 1075        CONTINUE
C        ENDDO
C
         CALL INPDRV
C
C        DOWHILE (MODULE SELECTION .NE. STOP)
 1100        WRITE (   *,300)
             WRITE (HIST,305)
             WRITE (HIST,300)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
             IF ((FLAG.LT.1) .OR. (FLAG.GT.9)) THEN
                 WRITE (   *,310)
                 WRITE (HIST,310)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
             ENDIF
             IF ((FLAG.LT.1) .OR. (FLAG.GT.8)) GO TO 1125
C
             IF     (FLAG.EQ.1) THEN
                 CALL INPDRV
C
             ELSEIF (FLAG.EQ.2) THEN
                 CALL EDTDRV
C
             ELSEIF (FLAG.EQ.3) THEN
                 CALL CNVDRV
C
             ELSEIF (FLAG.EQ.4) THEN
                 CALL TRNDRV
C
             ELSEIF (FLAG.EQ.5) THEN
                 CALL STADRV
C
             ELSEIF (FLAG.EQ.6) THEN
                 PLTTYP = 1
                 CALL PLTDRV
C
             ELSEIF (FLAG.EQ.7) THEN
                 CALL MAADRV
C
             ELSE
                 CALL MDLDRV
             ENDIF
             GO TO 1100
 1125        CONTINUE
C        ENDWHILE
C
C        SCRIPT THE TERMINATION MESSAGE AND CLOSE ALL OPENED FILES.
         WRITE (   *,315)
         WRITE (HIST,315)
         CALL FILCLS
         END
         BLOCK DATA SMF5BD
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        (NONE)
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PRESET SMERFS5 COMMON BLOCKS.
C   DESCRIPTION
C        THIS ROUTINE SIMPLY DEFINES SPECIFIC VALUES OF THE SMERFS5 COM-
C        MON BLOCKS.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C        YIELDED
C             CNV    (R) = TBF DATA CONVERSION CONSTANTS
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PLOT   (I) = UNIT NUMBER FOR THE PLOT FILE
C             PMAX   (R) = MAXIMUM NUMBER OF ITERATIONS FOR PREDICTIONS
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C             TYPES  (C) = DATA TYPE LABELS
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C   ARGUMENT LIST
C             (NONE)
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
C
C               /CNVCOM/      TBF DATA CONVERSION CONSTANTS COMMON
         COMMON /CNVCOM/      CNV(6)
C
C               /LBLCOM/      DATA TYPES AND TBF UNITS LABELS COMMON
         COMMON /LBLCOM/      TYPES(3)      ,UNITS(7)
         CHARACTER* 7         UNITS
         CHARACTER*10         TYPES
C
C               /PMXCOM/      PREDICTION MAXIMUM ITERATION COMMON
         COMMON /PMXCOM/      PMAX
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
C
C        PRESET THE TBF DATA CONVERSION ARRAY.
         DATA CNV(1)   / 60.0 /
         DATA CNV(2)   / 60.0 /
         DATA CNV(3)   / 24.0 /
         DATA CNV(4)   /  7.0 /
         DATA CNV(5)   /  4.0 /
         DATA CNV(6)   / 12.0 /
C
C        PRESET THE DATA TYPE LABELS.
         DATA TYPES(1) /' WC  TBF  '/
         DATA TYPES(2) /' CPU TBF  '/
         DATA TYPES(3) /' INTERVAL '/
C
C        PRESET THE PREDICTION MAXIMUM ITERATION VALUE.
         DATA PMAX     / 1000.0 /
C
C        PRESET THE UNIT LABELS FOR THE TBF DATA.
         DATA UNITS(1) /'SECONDS'/
         DATA UNITS(2) /'MINUTES'/
         DATA UNITS(3) /'HOURS  '/
         DATA UNITS(4) /'DAYS   '/
         DATA UNITS(5) /'WEEKS  '/
         DATA UNITS(6) /'MONTHS '/
         DATA UNITS(7) /'YEARS  '/
C
C        PRESET THE UNIT NUMBERS FOR THE SMERFS5 FILES.
         DATA HIST     / 17 /
         DATA PLOT     / 20 /
         DATA TMPB     / 18 /
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         END
         FUNCTION CNV24H
     G                   (HR    )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        FUNCTION CNV24H
C    G                   (HR    )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO TRANSLATE THE GIVEN ARGUMENT HR TO THE SPECIFIED UNITS.
C   DESCRIPTION
C        THE CNV24H FUNCTION  CONVERTS THE  ARGUMENT HR INTO  MINUTES BY
C        EXTRACTING THE TOTAL  NUMBER OF HOURS  (HRSPRT) AS A  SUBSET OF
C        THE ARGUMENT HR, CONVERTING THE SUBSET INTO MINUTES, AND ADDING
C        THE REMAINDER (HR - HRSPRT).
C
C        THE ROUTINE THEN CONVERTS THE MINUTES INTO THE SPECIFIED UNITS,
C        FOLLOWING A STRATEGY SIMILAR TO THAT IN THE CNVDRV ROUTINE.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             CNV    (R) = TBF DATA CONVERSION CONSTANTS
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C   ARGUMENT LIST
C        GIVEN
C             HR     (R) = TIME MEASURED ON A 24 HOUR CLOCK
C        YIELDED
C             CNV24H (R) = TIME CONVERTED TO THE SPECIFIED UNITS
C   LOCAL GLOSSARY
C             CUNIT  (I) = CONVERSION UNIT NUMBER
C             HRSPRT (I) = HOURS PORTION OF HR
C             MNSPRT (R) = MINUTES PORTION OF HR
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
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CUNIT         ,HRSPRT
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         HRSPRT = INT(HR / 100.0)
         MNSPRT = HR - REAL(HRSPRT * 100)
         CNV24H = REAL(HRSPRT * 60) + MNSPRT
C
C        SET THE CURRENT UNIT NUMBER TO INDICATE MINUTES.
         CUNIT = 2
C
         IF (CUNIT.NE.PRC(1,3)) THEN
C            DOUNTIL (PROPER UNITS ACHIEVED)
 1000            IF (CUNIT.LT.PRC(1,3)) THEN
                     CNV24H = CNV24H / CNV(CUNIT)
                     CUNIT  = CUNIT  + 1
                 ELSE
                     CUNIT  = CUNIT  - 1
                     CNV24H = CNV24H * CNV(CUNIT)
                 ENDIF
                 IF (CUNIT.NE.PRC(1,3)) GO TO 1000
C            ENDUNTIL
         ENDIF
         RETURN
         END
         SUBROUTINE CNVDRV
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE CNVDRV
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM A SPECIALIZE DATA TRANSFORMATION OF THE UNITS OF TBF
C        DATA SETS.
C   DESCRIPTION
C        THE CNVDRV ROUTINE HOUSES NO ACTUAL  DATA TRANSFORMATION STEPS;
C        IT MERELY SERVES  AS AN INPUT VEHICLE.  THE SMFTRN ROUTINE PER-
C        FORMS THE DATA CONVERSION, THE DATLST ROUTINE GENERATES THE DA-
C        TA LISTINGS, AND THE OUTPRM ROUTINE GENERATES THE NEW DATA BASE
C        FILES.
C
C        THIS ROUTINE SIMPLY STEPS THROUGH THE VARIOUS CONVERSIONS.  FOR
C        EXAMPLE, IF THE DATA SET IS STORED IN SECONDS AND HOURS ARE DE-
C        SIRED,  A DIVISION BY CNV(2) WILL OCCUR  TO CONVERT THE SECONDS
C        TO MINUTES.  THEN A DIVISION  BY CNV(1) WILL  OCCUR  TO CONVERT
C        THE MINUTES TO HOURS.
C
C        GOING THE OTHER WAY (FROM LARGER TO SMALLER UNITS) THE SAME AC-
C        TION IS PERFORMED; HOWEVER, THE DATA ARE MULTIPLIED.
C
C        AFTER UNIT CONVERSION IS COMPLETED  THE USER WILL  BE GIVEN THE
C        OPPORTUNITY TO REPLACE THE  DATA BASE FILE(S), PROVIDED THE UP-
C        DATE FLAG INDICATES THAT THE UPDATE FILES WERE GENERATED.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             CNV    (R) = TBF DATA CONVERSION CONSTANTS
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             TYPES  (C) = DATA TYPE LABELS
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C             UPDFLG (I) = FLAG TO CONTROL FILE DATA INPUT AND UPDATE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             A      (R) = TRANSFORMATION VARIABLE FROM CNV
C             DUM    (R) = TRANSFORMATION VARIABLE 2 - PLACE HOLDER
C             ERRFLG (I) = TRANSFORMATION ERROR FLAG - N/A
C             IDXB   (I) = POINTER TO THE BEGINNING OF DATA
C             IU     (I) = FILE INPUT UNIT VARIABLE
C             NUNIT  (I) = NEW UNIT NUMBER (DESIRED)
C   ERRORS
C        "DATA TYPE ERROR; TRY AGAIN (AFTER THE PROMPT)."
C        "THIS OPTION IS NOT APPLICABLE FOR INTERVAL DATA."
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
C               /CNVCOM/      TBF DATA CONVERSION CONSTANTS COMMON
         COMMON /CNVCOM/      CNV(6)
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
         INTEGER              ERRFLG        ,FLAG          ,I
     1                       ,IDXB          ,IU            ,NUNIT
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR WC TBF OR TWO FOR CPU TBF.')
  305    FORMAT (' **DATA TYPE ERROR; TRY AGAIN (AFTER THE PROMPT).')
  310    FORMAT (' **THIS OPTION IS NOT APPLICABLE FOR INTERVAL DATA.')
  315    FORMAT (' '
     1        ,/,'   THE',A10,'DATA ARE CURRENTLY STORED IN ',A7,'.'
     2       ,//,'   THE DATA MAY BE CONVERTED TO THE FOLLOWING UNITS:'
     3        ,/,'     1 SECONDS   3 HOURS     5 WEEKS     7 YEARS'
     4        ,/,'     2 MINUTES   4 DAYS      6 MONTHS'
     5        ,/,'   ENTER NEW CODE NUMBER FOR THE DATA.')
  320    FORMAT (' '
     1        ,/,'   ENTER ONE TO LIST THE CURRENT DATA; ELSE ZERO.')
  325    FORMAT (' '
     1        ,/,'   ENTER ONE TO SAVE THE NEW',A10,'DATA; ELSE ZERO.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET POINTERS NEEDED FOR DATA TRANSFORMATION.
         I = 0
         IF (DATTYP.LE.2) THEN
             I  = DATTYP
         ELSEIF (DATTYP.EQ.3) THEN
C            DOWHILE (ERROR ON INPUT)
 1000            WRITE (   *,300)
                 WRITE (HIST,300)
                 READ  (   *,  *) I
                 WRITE (HIST,  *) I
                 IF ((I.EQ.1) .OR. (I.EQ.2)) GO TO 1025
                 WRITE (   *,305)
                 WRITE (HIST,305)
                 GO TO 1000
 1025            CONTINUE
C            ENDWHILE
         ELSE
             WRITE (   *,310)
             WRITE (HIST,310)
         ENDIF
C
         IF (I.NE.0) THEN
             WRITE (   *,315) TYPES(I), UNITS(PRC(I,3))
             WRITE (HIST,315) TYPES(I), UNITS(PRC(I,3))
             READ  (   *,  *) NUNIT
             WRITE (HIST,  *) NUNIT
C
C            DETERMINE THE BEGINNING ADDRESS OF THE DATA.
             IDXB = (I * END1) - (END1 - 1)
C
             IF (PRC(I,3).NE.NUNIT) THEN
C                DOUNTIL (PROPER UNITS ACHIEVED)
 1050                IF (PRC(I,3).GT.NUNIT) THEN
                         PRC(I,3) = PRC(I,3) - 1
                         A        = CNV(PRC(I,3))
                     ELSE
                         A        = 1.0 / CNV(PRC(I,3))
                         PRC(I,3) = PRC(I,3) + 1
                     ENDIF
C
                     CALL SMFTRN
     G                           (PRC(I,1) , A        , DUM      , 5
     B                           ,DAT(IDXB)
     Y                           ,ERRFLG   )
                     IF (PRC(I,3).NE.NUNIT) GO TO 1050
C                ENDUNTIL
             ENDIF
C
C            DETERMINE IF THE CURRENT DATA LISTING IS DESIRED.
             WRITE (   *,320)
             WRITE (HIST,320)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
             IF (FLAG.EQ.1) THEN
                 CALL DATLST
             ENDIF
C
             IF (UPDFLG.EQ.1) THEN
C                GENERATE THE NEW DATA BASE FILES, IF UPDATE IS DESIRED.
                 WRITE (   *,325) TYPES(I)
                 WRITE (HIST,325) TYPES(I)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
C                    OUTPUT THE DATA IN THE NEW SMERFS5 FORMAT.
                     IU = I + 13
                     CALL OUTPRM
     G                           (I      , IU     )
                 ENDIF
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE DATLST
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE DATLST
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO GENERATE A LISTING OF THE DATA WITHIN THE DATA VECTOR.
C   DESCRIPTION
C        THIS ROUTINE MERELY GENERATES A LISTING OF THE CURRENT DATA. IT
C        REQUIRES NO ADDITIONAL USER INPUTS,  AS THE DATA TYPE IS DETER-
C        MINED THROUGH THE GLOBAL VARIABLE DATTYP.
C
C        IT IS POINTED OUT THAT THE  APPEARANCE OF THE WORD "INCOMPLETE"
C        BY THE LAST LISTED TBF ENTRY  INDICATES THAT  TESTING CONTINUED
C        AFTER THE FINAL FAILURE (IN THE NEXT TO LAST ENTRY). (I.E., THE
C        THE TIME REFLECTED IN THE FINAL ENTRY IS FAILURE-FREE.)
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
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C   ARGUMENT LIST
C             (NONE)
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
         INTEGER              I             ,J             ,K
     1                       ,L             ,M
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,4X,' FAILURE    TIME-BETWEEN'
     2        ,/,4X,' NUMBER',7X,A7
     3        ,/,4X,'========   ==============')
  305    FORMAT (' '
     1        ,/,4X,' FAILURE   WALL CLOCK TBF         CPU TBF'
     2        ,/,4X,' NUMBER',7X,A7,13X,A7
     3        ,/,4X,'========   ==============      ==============')
  310    FORMAT (' '
     1        ,/,4X,'INTERVAL   NO. OF FAULTS       TESTING LENGTH'
     2        ,/,4X,'========   ==============      ==============')
  315    FORMAT (5X,I4,5X,E15.8,5X,E15.8)
  320    FORMAT (5X,I4,5X,E15.8,6X,'(FAILURE-FREE)')
  325    FORMAT (5X,I4,5X,E15.8,5X,E15.8,6X,'(FAILURE-FREE)')
C
C ********************** START OF EXECUTABLE CODE **********************
C
         IF (DATTYP.LE.2) THEN
C            EITHER THE WC OR CPU TBF LISTING IS DESIRED.
             WRITE (   *,300) UNITS(PRC(DATTYP,3))
             WRITE (HIST,300) UNITS(PRC(DATTYP,3))
             IF (DATTYP.EQ.1) THEN
                 J = 1
                 K = PRC(1,1) - 1
             ELSE
                 J = STR2
                 K = END1 + PRC(2,1) - 1
             ENDIF
             L = 0
             IF (K.GE.J) THEN
                 DO 1000 I = J, K
                     L = L + 1
                     WRITE (   *,315) L, DAT(I)
                     WRITE (HIST,315) L, DAT(I)
 1000                CONTINUE
C                ENDDO
             ENDIF
             L = L + 1
             K = K + 1
             IF (PRC(DATTYP,2).EQ.0) THEN
C                THE LAST ENTRY IS NOT A TBF YET.
                 WRITE (   *,320) L, DAT(K)
                 WRITE (HIST,320) L, DAT(K)
             ELSE
                 WRITE (   *,315) L, DAT(K)
                 WRITE (HIST,315) L, DAT(K)
             ENDIF
         ELSEIF (DATTYP.EQ.3) THEN
C            BOTH THE WC AND CPU TBF LISTINGS ARE DESIRED.
             WRITE (   *,305) UNITS(PRC(1,3)), UNITS(PRC(2,3))
             WRITE (HIST,305) UNITS(PRC(1,3)), UNITS(PRC(2,3))
             J = 1
             K = PRC(1,1) - 1
             L = END1
             M = 0
             IF (K.GE.J) THEN
                 DO 1025 I = J, K
                     L = L + 1
                     M = M + 1
                     WRITE (   *,315) M, DAT(I), DAT(L)
                     WRITE (HIST,315) M, DAT(I), DAT(L)
 1025                CONTINUE
C                ENDDO
             ENDIF
             K = K + 1
             L = L + 1
             M = M + 1
             IF (PRC(1,2).EQ.0) THEN
C                THE LAST ENTRIES ARE NOT TBFS YET.
                 WRITE (   *,325) M, DAT(K), DAT(L)
                 WRITE (HIST,325) M, DAT(K), DAT(L)
             ELSE
                 WRITE (   *,315) M, DAT(K), DAT(L)
                 WRITE (HIST,315) M, DAT(K), DAT(L)
             ENDIF
         ELSE
C            INTERVAL OUTPUT MUST BE DESIRED.
             WRITE (   *,310)
             WRITE (HIST,310)
             J = 1
             K = PRC(3,1)
             L = END1
             M = 0
             DO 1050 I = J, K
                 L = L + 1
                 M = M + 1
                 WRITE (   *,315) M, DAT(I), DAT(L)
                 WRITE (HIST,315) M, DAT(I), DAT(L)
 1050            CONTINUE
C            ENDDO
         ENDIF
         RETURN
         END
         SUBROUTINE EDTDRV
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE EDTDRV
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM USER PRINTING AND EDITING OF A SPECIFIED SET OF DATA
C        WITHIN THE DATA VECTOR.
C   DESCRIPTION
C        THE ROUTINE ALLOWS  THE USER TO MODIFY  THE DATA VIA INSERTION,
C        DELETION,  COMBINATION,  AND ELEMENT ALTERATION.  ADDITIONALLY,
C        THE FATALITY FLAG OF THE LAST  STORED EXECUTION TIME  ENTRY CAN
C        BE CHANGED.
C
C        THE DATA MANIPULATIONS  WILL BE PERFORMED  IN A SMFLIB ROUTINE.
C        THE PORTIONS ARE VERY SIMPLISTIC;  BUT EACH IS AN INTEGRAL PART
C        OF ANY SOFTWARE  RELIABILITY  STUDY AND;  THEREFORE,  HAVE BEEN
C        ADDED TO THE LIBRARY.
C
C        AFTER DATA EDITING IS COMPLETED  THE USER WILL BE GIVEN THE OP-
C        PORTUNITY TO REPLACE THE DATA BASE FILE(S), PROVIDED THE UPDATE
C        FLAG INDICATES THAT THE UPDATE FILES WERE GENERATED.
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
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             TYPES  (C) = DATA TYPE LABELS
C             UPDFLG (I) = FLAG TO CONTROL FILE DATA INPUT AND UPDATE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             ADA    (R) = ALTERATION DATA ARRAY
C             ASZ    (I) = NUMBER OF INSERTIONS FOR SAMPLES 1 & 2
C             CHG    (I) = VECTOR INDICATING DATA TYPE(S) EDITED
C             DP1    (I) = DATA POINTER FOR SAMPLE 1
C             DP2    (I) = DATA POINTER FOR SAMPLE 2
C             ERRFLG (I) = ERROR FLAG
C             FLAG   (I) = OPTION FLAG
C             OSZ    (I) = OLD NUMBER OF ASSIGNED ELEMENTS
C             PC     (I) = PROCESSING VECTOR FOR SMFEDT ROUTINE
C             USZ    (I) = USED (ASSIGNED) LOCATIONS IN SAMPLE 1 & 2
C   ERRORS
C        "THE EDIT EXCEEDS BOUNDS OF THE CURRENT DATA"
C        "THE INSERTION WILL EXCEED THE MAXIMUM ARRAY SIZE FOR SMERFS."
C        "THIS OPTION IS NOT APPLICABLE FOR INTERVAL DATA"
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
         INTEGER              ASZ           ,CHG(4)        ,DP1
     1                       ,DP2           ,ERRFLG        ,FLAG
     2                       ,I             ,J             ,OSZ
     3                       ,PC(4)         ,USZ
         DIMENSION            ADA(2,10)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  305    FORMAT (' '
     1        ,/,'   ENTER EDIT OPTION, OR ZERO FOR A LIST.')
  310    FORMAT ('   THE AVAILABLE EDIT OPTIONS ARE:'
     1        ,/,'    1 CHANGE SPECIFIED ELEMENT'
     2        ,/,'    2 DELETE SPECIFIED ELEMENT(S)'
     3        ,/,'    3 INSERT UP TO 10 ELEMENTS'
     4        ,/,'    4 COMBINE TWO OR MORE ADJACENT ELEMENTS'
     5        ,/,'    5 CHANGE THE TBF FATALITY FLAG'
     6        ,/,'    6 LIST THE CURRENT DATA'
     7        ,/,'    7 RETURN TO THE MAIN PROGRAM'
     8        ,/,'   ENTER EDIT OPTION.')
  315    FORMAT ('    ENTER LOCATION NUMBER AND NEW VALUE FOR DATA 1.')
  320    FORMAT ('    ENTER NEW VALUE FOR DATA 2.')
  325    FORMAT ('    ENTER STARTING AND ENDING LOCATIONS TO DELETE.')
  330    FORMAT ('    ENTER LOCATION NUMBER JUST PRIOR TO THE INSERTION'
     1          ,' AND NUMBER OF'
     2        ,/,'    NEW ENTRIES.')
  335    FORMAT ('    ENTER',I2,' ELEMENT(S) FOR DATA 1.')
  340    FORMAT ('    AND  ',I2,' ELEMENT(S) FOR DATA 2.')
  345    FORMAT ('    ENTER STARTING AND ENDING LOCATIONS TO MERGE.')
  350    FORMAT ('    ENSURE THE TBF FATALITY FLAG IS PROPERLY SET.')
  355    FORMAT ('  **THE EDIT EXCEEDS BOUNDS OF THE CURRENT DATA.')
  360    FORMAT ('  **THE INSERTION WILL EXCEED THE MAXIMUM ARRAY SIZE '
     1          ,'FOR SMERFS.')
  365    FORMAT ('    THE FATALITY FLAG HAS BEEN SET TO INDICATE  A FAI'
     1          ,'LURE  OCCURRED'
     2        ,/,'    AT THE END OF THE TESTING.')
  366    FORMAT ('    THE FATALITY FLAG FOR THE LAST DATA POINT HAS BEE'
     1          ,'N SET TO INDI-'
     2        ,/,'    CATE THAT A FAILURE OCCURRED FOR THE PREVIOUS DAT'
     3          ,'A POINT.')
  370    FORMAT ('  **THIS OPTION IS NOT APPLICABLE FOR INTERVAL DATA.')
  375    FORMAT (' '
     1        ,/,'   ENTER ONE TO SAVE THE NEW',A10,'DATA; ELSE ZERO.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE  ELEMENTS OF CHG  TO INDICATE THAT  NO EDITING HAS BEEN
C        PERFORMED.
         DO 1000 I = 1, 4
             CHG(I) = 0
 1000        CONTINUE
C        ENDDO
C
C        DOWHILE (EDIT SELECTION .NE. RETURN)
 1025        WRITE (   *,305)
             WRITE (HIST,305)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
             IF ((FLAG.LT.1) .OR. (FLAG.GT.7)) THEN
                 WRITE (   *,310)
                 WRITE (HIST,310)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
             ENDIF
             IF ((FLAG.LT.1) .OR. (FLAG.GT.6)) GO TO 1050
C            PROVIDED THIS IS  ACTUALLY A DATA EDIT, PERFORM THE INITIAL
C            SMFEDT ARGUMENT ASSIGNMENTS.
             IF (FLAG.LE.4) THEN
C                SET THE ARGUMENTS TO THEIR MOST COMMON VALUES.
                 PC(1) = FLAG
                 PC(2) = 1
                 PC(4) = 0
                 DP1   = 1
                 DP2   = STR2
                 USZ   = PRC(1,1)
                 ASZ   = 10
C                NOW UPDATE THESE ARGUMENTS, AS NECESSARY.
                 IF (DATTYP.EQ.2) THEN
                     DP1   = STR2
                     DP2   = 1
                     USZ   = PRC(2,1)
                 ELSEIF (DATTYP.EQ.3) THEN
                     PC(2) = 2
                 ELSEIF (DATTYP.EQ.4) THEN
                     PC(2) = 2
                     USZ   = PRC(3,1)
                 ENDIF
C                STORE THE NUMBER OF CURRENTLY USED ELEMENTS.
                 OSZ = USZ
C
C                WITH THE INITIAL DATA ITEMS SET, THE ACTUAL DATA ALTER-
C                ATION VALUES WILL NOW BE PROMPT AND INPUT.
                 IF     (FLAG.EQ.1) THEN
C                    ELEMENT CHANGE
                     WRITE (   *,315)
                     WRITE (HIST,315)
                     READ  (   *,  *) PC(3), ADA(1,1)
                     WRITE (HIST,  *) PC(3), ADA(1,1)
                     IF (PC(2).EQ.2) THEN
                         WRITE (   *,320)
                         WRITE (HIST,320)
                         READ  (   *,  *) ADA(2,1)
                         WRITE (HIST,  *) ADA(2,1)
                     ENDIF
C
                 ELSEIF (FLAG.EQ.2) THEN
C                    ELEMENT(S) DELETE
                     WRITE (   *,325)
                     WRITE (HIST,325)
                     READ  (   *,  *) PC(3), PC(4)
                     WRITE (HIST,  *) PC(3), PC(4)
C
                 ELSEIF (FLAG.EQ.3) THEN
C                    ELEMENT(S) INSERT
                     WRITE (   *,330)
                     WRITE (HIST,330)
                     READ  (   *,  *) PC(3), PC(4)
                     WRITE (HIST,  *) PC(3), PC(4)
                     I = PC(4)
                     WRITE (   *,335) I
                     WRITE (HIST,335) I
                     READ  (   *,  *) (ADA(1,J), J = 1, I)
                     WRITE (HIST,  *) (ADA(1,J), J = 1, I)
                     IF (PC(2).EQ.2) THEN
                         WRITE (   *,340) I
                         WRITE (HIST,340) I
                         READ  (   *,  *) (ADA(2,J), J = 1, I)
                         WRITE (HIST,  *) (ADA(2,J), J = 1, I)
                     ENDIF
C
                 ELSE
C                    ELEMENTS MERGE
                     WRITE (   *,345)
                     WRITE (HIST,345)
                     READ  (   *,  *) PC(3), PC(4)
                     WRITE (HIST,  *) PC(3), PC(4)
                 ENDIF
C
C                USING THE SMFEDT ROUTINE, UPDATE THE VECTOR(S).
                 CALL SMFEDT
     G                       (PC      , ADA     , ASZ     , END1
     B                       ,DAT(DP1), DAT(DP2), USZ
     Y                       ,ERRFLG  )
                 IF (ERRFLG.EQ.0) THEN
C                    ISSUE THE TBF FATALITY FLAG WARNING MESSAGE, IF THE
C                    EDIT TYPE  WAS NOT FOR   INTERVAL DATA AND THE LAST
C                    ELEMENT OF THE DATA VECTOR WAS EDITED.
                     IF ((DATTYP.NE.4) .AND.
     1                   ((((FLAG.EQ.1) .OR. (FLAG.EQ.3)) .AND.
     2                      (PC(3).EQ.OSZ)) .OR.
     3                    (((FLAG.EQ.2) .OR. (FLAG.EQ.4)) .AND.
     4                      (PC(4).EQ.OSZ))))                       THEN
                         WRITE (   *,350)
                         WRITE (HIST,350)
                     ENDIF
C
C                    SET THE VECTOR ELEMENT TO INDICATE THE TYPE EDITED.
                     CHG(DATTYP) = 1
C
C                    UPDATE THE PROCESSING  TABLE TO THE  NEW NUMBER  OF
C                    ENTRIES (RETURNED IN USZ).
                     IF (DATTYP.LE.2) THEN
                         PRC(DATTYP,1) = USZ
                     ELSEIF (DATTYP.EQ.3) THEN
                         PRC(1,1) = USZ
                         PRC(2,1) = USZ
                     ELSE
                         PRC(3,1) = USZ
                     ENDIF
                 ELSE
C                    AN ERROR TERMINATED THE SPECIFIED  DATA EDIT AND NO
C                    EDITING WAS PERFORMED;  OUTPUT AN ERROR MESSAGE IN-
C                    DICATING THE ERROR TYPE.
                     IF (ERRFLG.EQ.1) THEN
                         WRITE (   *,355)
                         WRITE (HIST,355)
                     ELSEIF (ERRFLG.EQ.2) THEN
                         WRITE (   *,360)
                         WRITE (HIST,360)
                     ENDIF
                 ENDIF
             ELSEIF (FLAG.EQ.5) THEN
                 IF (DATTYP.LE.3) THEN
                     IF ((DATTYP.EQ.1) .OR. (DATTYP.EQ.3)) THEN
C                        ALTER THE WC TBF FATALITY FLAG.
                         PRC(1,2) = 1 - PRC(1,2)
                         I        = PRC(1,2)
                     ENDIF
                     IF ((DATTYP.EQ.2) .OR. (DATTYP.EQ.3)) THEN
C                        ALTER THE CPU TBF FATALITY FLAG.
                         PRC(2,2) = 1 - PRC(2,2)
                         I        = PRC(2,2)
                     ENDIF
C                    OUTPUT THE MESSAGE THAT THE CHANGE HAS OCCURRED AND
C                    SET THE CHG ELEMENT TO INDICATE THE TYPE EDITED.
                     IF (I.EQ.1) THEN
                         WRITE (   *,365)
                         WRITE (HIST,365)
                     ELSE
                         WRITE (   *,366)
                         WRITE (HIST,366)
                     ENDIF
                     CHG(DATTYP) = 1
                 ELSE
                     WRITE (   *,370)
                     WRITE (HIST,370)
                 ENDIF
             ELSE
C                A DATA LISTING MUST BE DESIRED.
                 CALL DATLST
             ENDIF
             GO TO 1025
 1050        CONTINUE
C        ENDWHILE
C
         IF (UPDFLG.EQ.1) THEN
C            PREPARE FOR THE DATA BASE UPDATE PROMPTS.
             IF (CHG(3).EQ.1) THEN
                 CHG(1) = 1
                 CHG(2) = 1
             ENDIF
             CHG(3) = CHG(4)
             DO 1075 I = 1, 3
                 IF (CHG(I).EQ.1) THEN
C                    THE DATA WERE CHANGED FOR THIS TYPE.
                     WRITE (   *,375) TYPES(I)
                     WRITE (HIST,375) TYPES(I)
                     READ  (   *,  *) FLAG
                     WRITE (HIST,  *) FLAG
                     IF (FLAG.EQ.1) THEN
C                        OUTPUT THE DATA IN THE NEW SMERFS5 FORMAT.
                         J = I + 13
                         CALL OUTPRM
     G                               (I      , J      )
                     ENDIF
                 ENDIF
 1075            CONTINUE
C            ENDDO
         ENDIF
         RETURN
         END
         SUBROUTINE EXVBAM
     G                     (FLAG  , NS    , CDAT
     Y                     ,MDAT  , FDAT  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE EXVBAM
C    G                     (FLAG  , NS    , CDAT
C    Y                     ,MDAT  , FDAT  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO CREATE THE EXTRA  TWO DATA VECTORS (FDAT & MDAT) REQUIRED IN
C        THE BROOKS AND MOTLEY DISCRETE SOFTWARE RELIABILITY MODEL.  THE
C        2 VECTORS (PASSED THROUGH  THE CALL LINE OF THE EXVBAM ROUTINE)
C        CONTAIN THE INTERVAL  INFORMATION ON THE FRACTIONAL PART OF THE
C        CODE BEING TESTED AND THE  CUMULATIVE NUMBER OF FAULTS DETECTED
C        (IN PREVIOUS TESTING PERIODS)  FOR THAT  FRACTIONAL PART OF THE
C        PROGRAM.
C   DESCRIPTION
C        THE FIRST ARGUMENT  OF THE CALL LINE  (FLAG) INDICATES  WHETHER
C        THE TESTING WAS CONDUCTED OVER A FRACTION  OF THE CODE (FLAG=1)
C        OR THE ENTIRE PROGRAM (FLAG=0).
C
C        IF FLAG IS SET TO ZERO, THE ROUTINE INTERNALLY COMPUTES THE RE-
C        QUIRED MDAT VALUES AND ASSIGNS A 1.0 TO ALL NS ELEMENTS OF FDAT
C        (TO INDICATE THAT THE ENTIRE PROGRAM WAS UNDER TEST). OTHERWISE
C        (IF FLAG IS SET TO 1),  THE ROUTINE WILL PROMPT THE USER TO IN-
C        PUT THE TWO VALUES (FOR ALL NS ELEMENTS OF FDAT AND MDAT).
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
C             CDAT   (R) = INTERVAL FAULT COUNT DATA VECTOR
C             FLAG   (I) = FLAG INDICATING  FRACTIONAL OR  TOTAL PROGRAM
C                          TESTING
C             NS     (I) = SIZE OF CDAT, FDAT, AND MDAT
C        YIELDED
C             FDAT   (R) = FRACTIONAL PART OF CODE TEST DATA VECTOR
C             MDAT   (R) = PREVIOUS CUMULATIVE FAULT DATA VECTOR
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
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              FLAG          ,NS
         DIMENSION            CDAT(NS)      ,FDAT(NS)      ,MDAT(NS)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'    ENTER FRACTION OF CODE BEING TESTED IN PERIOD   1'
     2          ,'.')
  305    FORMAT ('    ENTER FRACTION OF CODE BEING TESTED IN PERIOD',I4
     1          ,'  AND THE NUM-'
     2        ,/,'    BER OF FAULTS DETECTED  IN THE FRACTIONAL PART  I'
     3          ,'N THE PREVIOUS'
     4        ,/,2X,I4,' TEST PERIODS.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        ASSIGN THE FDAT VALUE FOR THE FIRST TESTING PERIOD.  THIS VALUE
C        WILL EITHER BE SET OR INPUT, DEPENDING ON THE VALUE OF FLAG.
         IF (FLAG.EQ.0) THEN
             FDAT(1) = 1.0
         ELSE
             WRITE (   *,300)
             WRITE (HIST,300)
             READ  (   *,  *) FDAT(1)
             WRITE (HIST,  *) FDAT(1)
         ENDIF
C
C        ASSIGN THE MDAT VALUE FOR THE FIRST TESTING PERIOD.
         MDAT(1) = 0.0
C
C        ASSIGN THE REMAINING ELEMENTS OF FDAT AND MDAT.
         DO 1000 I = 2, NS
             J = I - 1
             IF (FLAG.EQ.0) THEN
                 FDAT(I) = 1.0
                 MDAT(I) = MDAT(J) + CDAT(J)
             ELSE
                 WRITE (   *,305) I, J
                 WRITE (HIST,305) I, J
                 READ  (   *,  *) FDAT(I), MDAT(I)
                 WRITE (HIST,  *) FDAT(I), MDAT(I)
             ENDIF
 1000        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE EXVGPO
     G                     (CDAT  , NS    , FLAG
     Y                     ,MDAT  , LCOR  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE EXVGPO
C    G                     (CDAT  , NS    , FLAG
C    Y                     ,MDAT  , LCOR  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO CREATE THE  EXTRA DATA VECTOR (MDAT)  AND THE EXTRA VARIABLE
C        (LCOR) REQUIRED IN THE GENERALIZED POISSON MODEL. THE MDAT VEC-
C        TOR CONTAINS THE  INTERVAL INFORMATION ON THE CUMULATIVE NUMBER
C        OF FAULTS CORRECTED IN  PREVIOUS TESTING PERIODS, AND LCOR CON-
C        TAINS THE NUMBER OF CORRECTIONS MADE AFTER THE LAST PERIOD.
C   DESCRIPTION
C        THE THIRD ARGUMENT OF THE CALL LINE (FLAG) INDICATES WHETHER OR
C        NOT ANY OF THE FAULTS  DETECTED IN ONE TESTING PERIOD WERE COR-
C        RECTED IN A SUBSEQUENT PERIOD.
C
C        IF FLAG IS SET TO ZERO  (INDICATING ALL  FAULT CORRECTIONS WERE
C        PERFORMED DIRECTLY  FOLLOWING THE TESTING  PERIOD IN WHICH THEY
C        WERE DETECTED)  THE ROUTINE  WILL INTERNALLY TRANSFER  THE DATA
C        FROM THE CDAT VECTOR TO THE MDAT VECTOR.  OTHERWISE (IF FLAG IS
C        SET TO 1), THE  ROUTINE WILL PROMPT THE USER  FOR THE NUMBER OF
C        CORRECTIONS FOR EACH TESTING PERIOD.
C
C        LCOR WILL BE SET  TO CONTAIN THE NUMBER OF  FAULTS CORRECTED AT
C        THE END OF THE LAST TESTING PERIOD.
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
C             FLAG   (I) = FLAG  INDICATING  JOINT  FAULT DETECTIONS AND
C                          CORRECTIONS
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             NS     (I) = SIZE OF CDAT
C        YIELDED
C             LCOR   (R) = FAULT CORRECTIONS AFTER LAST PERIOD
C             MDAT   (R) = CUMULATIVE FAULT CORRECTIONS DATA VECTOR
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
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              FLAG          ,NS
         DIMENSION            CDAT(NS)      ,MDAT(NS)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('    ENTER NUMBER CORRECTED AT THE END OF PERIOD',I4
     1          ,'.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        ASSIGN MDAT VALUE FOR THE FIRST TESTING PERIOD.
         MDAT(1) = 0.0
C
C        ASSIGN THE REMAINING ELEMENTS OF MDAT.
         DO 1000 I = 2, NS
             J = I - 1
             IF (FLAG.EQ.0) THEN
                 MDAT(I) = MDAT(J) + CDAT(J)
             ELSE
                 WRITE (   *,300) J
                 WRITE (HIST,300) J
                 READ  (   *,  *) TMP
                 WRITE (HIST,  *) TMP
                 MDAT(I) = MDAT(J) + TMP
             ENDIF
 1000        CONTINUE
C        ENDDO
C
C        ASSIGN THE NUMBER OF FAULTS CORRECTED AFTER THE LAST PERIOD.
         LCOR = CDAT(NS)
         IF (FLAG.EQ.1) THEN
             WRITE (   *,300) NS
             WRITE (HIST,300) NS
             READ  (   *,  *) LCOR
             WRITE (HIST,  *) LCOR
         ENDIF
         RETURN
         END
         SUBROUTINE FILCLS
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE FILCLS
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO CLOSE ALL OPENED FILES IN PREPARATION FOR TERMINATION.
C   DESCRIPTION
C        THIS ROUTINE SIMPLY CLOSES ALL THE FILES WHICH ARE OPENED.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             MAKFIL (I) = FLAG TO CONTROL OPTIONAL PLOT FILE CREATION
C             PLOT   (I) = UNIT NUMBER FOR THE PLOT FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C             UPDFLG (I) = FLAG TO CONTROL FILE DATA INPUT AND UPDATE
C   ARGUMENT LIST
C             (NONE)
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
C               /PLTCOM/      PLOTTING COMMON
         COMMON /PLTCOM/      MAKFIL        ,PLTTYP
         INTEGER              MAKFIL        ,PLTTYP
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
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
C        "CLOSE" THE POSSIBLY OPENED INPUT AND OUTPUT FILES.
         DO 1000 I = 1, 3
             IF ((UPDFLG.EQ.1) .AND. (PRC(I,1).NE.0)) THEN
                 J = I + 13
                 CLOSE (J)
             ENDIF
 1000        CONTINUE
C        ENDDO
C
         CLOSE (HIST)
         CLOSE (TMPB)
C
         IF (MAKFIL.EQ.1) THEN
             CLOSE (PLOT)
         ENDIF
         RETURN
         END
         SUBROUTINE FILOPN
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE FILOPN
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO OPEN THE SPECIAL OUTPUT AND SUPPORT FILES FOR THE PROGRAM.
C   DESCRIPTION
C        THIS ROUTINE OPENS TWO OPTIONAL AND ONE REQUIRED DATA FILES FOR
C        THE SMERFS PROGRAM. (THE PROMPTING FOR  FILE NAMES  IS REPEATED
C        UNTIL THE ENTERED VALUE IS  DETERMINED TO BE ACCEPTABLE FOR THE
C        UTILIZED COMPUTER SYSTEM.) A DESCRIPTION OF EACH FILE FOLLOWS.
C
C        THE SMERFS HISTORY FILE IS THE FIRST OPTIONAL OUTPUT FILE. THIS
C        FILE CONTAINS A COPY OF THE ENTIRE INTERACTIVE SESSION.  IF THE
C        FILE IS CREATED, A FILE OF THE PROGRAM PROMPTS, USER RESPONSES,
C        AND PROGRAM OUTPUTS WILL BE AVAILABLE WHEN THE PROGRAM ENDS.
C
C        THE SMERFS PLOT FILE IS THE  SECOND OPTIONAL OUTPUT FILE.  THIS
C        FILE CONTAINS THE DATA VALUES (SPECIFIED BY THE USER) TO CREATE
C        CREATE HIGH-QUALITY PLOTS OF THE (1) RAW DATA, (2) SMOOTHED RAW
C        DATA, (3) PREDICTION DATA, (4) RESIDUAL DATA,  AND (5) SMOOTHED
C        RESIDUAL DATA.  THE DATA FILE WILL ALSO  CONTAIN VARIOUS TITLES
C        AND AXIS LABELS.
C
C        THE FINAL FILE IS SIMPLY AN INTERNAL UNFORMATTED STORAGE FILE.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        THIS  SUBPROGRAM  CONTAINS  CODE WHICH DOES NOT COMPLY WITH THE
C        PROGRAMMING STANDARDS; GROUP LEADER APPROVAL HAS BEEN OBTAINED.
C       -SPECIFICALLY:
C        THE DOWHILES CONTAIN NON-STRUCTURED LOOP-BACKS AND BRANCHES.
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PLOT   (I) = UNIT NUMBER FOR THE PLOT FILE
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C        YIELDED
C             MAKFIL (I) = FLAG TO CONTROL OPTIONAL PLOT FILE CREATION
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             FILNM  (C) = INPUT FILE NAME OF FILE TO BE OPENED
C   ERRORS
C        "FILE NAME ERROR; TRY AGAIN (AFTER THE PROMPT)."
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
C               /PLTCOM/      PLOTTING COMMON
         COMMON /PLTCOM/      MAKFIL        ,PLTTYP
         INTEGER              MAKFIL        ,PLTTYP
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         CHARACTER*50         FILNM
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        INPUT FORMATS
  100    FORMAT (A50)
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,9X
     2          ,'SSSSSSS  M     M  EEEEEEE  RRRRRRR  FFFFFFF  SSSSSSS'
     3        ,/,9X
     4          ,'S        M M M M  E        R     R  F        S'
     5        ,/,9X
     6          ,'SSSSSSS  M  M  M  EEEE     RRRRRRR  FFFF     SSSSSSS'
     7        ,/,9X
     8          ,'      S  M     M  E        R   R    F              S'
     9        ,/,9X
     A          ,'SSSSSSS  M     M  EEEEEEE  R     R  F        SSSSSSS'
     B       ,//,11X
     C          ,'SOFTWARE REVISION NUMBER FIVE (21 SEPTEMBER 1993)'
     D        ,/)
  305    FORMAT (' '
     1        ,/,'   ENTER OUTPUT FILE NAME FOR THE HISTORY FILE; ZERO '
     2          ,'IF THE FILE IS'
     3        ,/,'   NOT DESIRED, OR ONE FOR DETAILS ON THE FILE.')
  310    FORMAT (' '
     1        ,/,'   THE HISTORY FILE IS A COPY OF THE ENTIRE INTERACTI'
     2          ,'VE SESSION. IT'
     3        ,/,'   CAN BE USED FOR LATER ANALYSIS AND/OR DOCUMENTATIO'
     4          ,'N.')
  315    FORMAT (' **FILE NAME ERROR; TRY AGAIN (AFTER THE PROMPT).')
  320    FORMAT (' '
     1        ,/,'   ENTER OUTPUT FILE NAME FOR THE  PLOT   FILE; ZERO '
     2          ,'IF THE FILE IS'
     3        ,/,'   NOT DESIRED, OR ONE FOR DETAILS ON THE FILE.')
  325    FORMAT (' '
     1        ,/,'   THE PLOT FILE CONTAINS SELECTED DATA AND LABELS TO'
     2          ,' ALLOW A USER-'
     3        ,/,'   SUPPLIED GRAPHICS PROGRAM TO GENERATE HIGH-QUALITY'
     4          ,' PLOTS.  SINCE'
     5        ,/,'   A CHARACTER PLOTTER IS IMPLEMENTED WITHIN THE SMER'
     6          ,'FS PROGRAM (TO'
     7        ,/,'   ENSURE MACHINE PORTABILITY OF THE PACKAGE),  THE U'
     8          ,'SE OF THIS OP-'
     9        ,/,'   TION IS HIGHLY RECOMMENDED.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        ESTABLISH THE SMERFS HISTORY FILE.  ONCE ESTABLISHED SCRIPT THE
C        PROGRAM PROMPTS AND FINAL USER RESPONSE TO THAT FILE.
         WRITE (   *,300)
C        DOWHILE (ERROR ON INPUT)                  <<SEE RESTRICTIONS <<
 1000        WRITE (   *,305)
             READ  (   *,100) FILNM
             IF (FILNM.EQ.'1') THEN
                 WRITE (   *,310)
                 GO TO 1000
             ELSE
                 IF (FILNM.NE.'0') THEN
                     OPEN  (HIST, FILE=FILNM, STATUS='NEW', ERR=1025)
                 ELSE
                     OPEN  (HIST, STATUS='SCRATCH')
                 ENDIF
             ENDIF
             GO TO 1050
 1025        WRITE (   *,315)
             GO TO 1000
 1050        CONTINUE
C        ENDWHILE
         WRITE (HIST,300)
         WRITE (HIST,305)
         WRITE (HIST,310)
         WRITE (HIST,  *) FILNM
C
C        ESTABLISH THE SMERFS  PLOT   FILE.  ONCE ESTABLISHED SCRIPT THE
C        PROGRAM PROMPTS AND FINAL USER RESPONSE TO THE HISTORY FILE.
C        DOWHILE (ERROR ON INPUT)                  <<SEE RESTRICTIONS <<
 1075        WRITE (   *,320)
             READ  (   *,100) FILNM
             IF (FILNM.EQ.'1') THEN
                 WRITE (   *,325)
                 GO TO 1075
             ELSE
                 IF (FILNM.NE.'0') THEN
                     OPEN  (PLOT, FILE=FILNM, STATUS='NEW', ERR=1100)
                     MAKFIL = 1
                 ELSE
                     MAKFIL = 0
                 ENDIF
             ENDIF
             GO TO 1125
 1100        WRITE (   *,315)
             GO TO 1075
 1125        CONTINUE
C        ENDWHILE
         WRITE (HIST,320)
         WRITE (HIST,325)
         WRITE (HIST,  *) FILNM
C
C        ESTABLISH THE INTERNAL DATA STORAGE FILE.
         OPEN  (TMPB, FORM='UNFORMATTED', STATUS='SCRATCH')
         RETURN
         END
         SUBROUTINE FITDRV
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE FITDRV
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE THE MENUS AND TRANSFERS FOR THE EXECUTION OF THE IN-
C        CLUDED ANALYSES OF MODEL FIT OPTIONS.
C   DESCRIPTION
C        THE ROUTINE ONLY PROMPTS  THE USER FOR THE  TYPE OF ANALYSIS OF
C        FIT TO BE PERFORMED, AND ACCESSES SECONDARY ROUTINES TO PERFORM
C        THAT PROCESSING.  THE ROUTINES WILL ONLY HANDLE PRELIMINARY IN-
C        PUT, OUTPUT, AND PLOTTING.  ALL  REQUIRED  COMPUTATIONS WILL BE
C        PERFORMED BY ROUTINES OF THE SMFLIB LIBRARY.
C
C        AFTER ALL DESIRED ANALYSES  ARE PERFORMED, THE ROUTINE WILL DE-
C        TERMINE IF THE PLOT FILE WAS REQUESTED AT THE ONSET OF THE EXE-
C        CUTION. IF SO, THE ROUTINE WILL PROMPT THE USER TO SEE IF THESE
C        DATA ARE TO BE RECORDED ON THE FILE.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             MAKFIL (I) = FLAG TO CONTROL OPTIONAL PLOT FILE CREATION
C        YIELDED
C             PLTTYP (I) = ANALYSIS PLOT TYPE
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             FLAG   (I) = OPTION FLAG
C             TITLE  (C) = TITLE FOR THE OPTIONAL PLOT FILE ENTRY
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
C               /PLTCOM/      PLOTTING COMMON
         COMMON /PLTCOM/      MAKFIL        ,PLTTYP
         INTEGER              MAKFIL        ,PLTTYP
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         CHARACTER*40         TITLE
         INTEGER              FLAG
C
C                             LOCAL ASSIGNMENTS
C        (NONE)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        INPUT FORMATS
  100    FORMAT (A40)
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'   ENTER ANALYSES OPTION, OR ZERO FOR A LIST.')
  305    FORMAT ('   THE AVAILABLE ANALYSES OPTIONS ARE:'
     1        ,/,'    1 GOODNESS-OF-FIT TESTS'
     2        ,/,'    2 ORIGINAL, PREDICTED, AND RESIDUAL DATA LISTINGS'
     3        ,/,'    3 PLOT OF ORIGINAL AND PREDICTED DATA'
     4        ,/,'    4 PLOT OF THE RESIDUAL DATA'
     5        ,/,'    5 RETURN TO THE MODEL EXECUTION MENU'
     6        ,/,'   ENTER ANALYSES OPTION.')
  310    FORMAT (' '
     1        ,/,'   ENTER ONE TO PLACE THE PLOT AND GOODNESS-OF-FIT ST'
     2          ,'ATISTIC ON THE'
     3        ,/,'   OPTIONAL SMERFS PLOT FILE; ELSE ZERO.')
  315    FORMAT ('   ENTER A PLOT TITLE FOR THE DATA (UP TO 40 CHARACTE'
     1          ,'RS).')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        DOWHILE (ANALYSIS SELECTION .NE. STOP)
 1000        WRITE (   *,300)
             WRITE (HIST,300)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
             IF ((FLAG.LT.1) .OR. (FLAG.GT.5)) THEN
                 WRITE (   *,305)
                 WRITE (HIST,305)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
             ENDIF
             IF ((FLAG.LT.1) .OR. (FLAG.GT.4)) GO TO 1025
C
             IF     (FLAG.EQ.1) THEN
                 CALL FITGOF
C
             ELSEIF (FLAG.EQ.2) THEN
                 CALL FITLST
C
             ELSEIF (FLAG.EQ.3) THEN
                 PLTTYP = 3
                 CALL PLTDRV
C
             ELSE
                 PLTTYP = 2
                 CALL PLTDRV
             ENDIF
             GO TO 1000
 1025        CONTINUE
C        ENDWHILE
C
         IF (MAKFIL.EQ.1) THEN
C            PROMPT THE USER TO SEE IF THE DATA  SHOULD BE  ADDED TO THE
C            TO THE OPTIONAL PLOT FILE.
             WRITE (   *,310)
             WRITE (HIST,310)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
C
             IF (FLAG.EQ.1) THEN
C                PROMPT USER FOR A TITLE AND ACCESS THE ROUTINE.
                 WRITE (   *,315)
                 WRITE (HIST,315)
                 READ  (   *,100) TITLE
                 WRITE (HIST,  *) TITLE
                 CALL PLTEXT
     G                       (TITLE )
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE FITGOF
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE FITGOF
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE THE INTERACTIVE PROMPTS AND INPUTS FOR THE GOODNESS-
C        OF-FIT ANALYSIS FOR THE INTERVAL DATA MODELS;  OR THE OUTPUT OF
C        THE PREVIOUSLY COMPUTED GOODNESS-OF-FIT VALUE FOR THE EXECUTION
C        TIME MODELS.
C   DESCRIPTION
C        IF THE ROUTINE IS ENTERED DURING AN EXECUTION TIME ANALYSIS, NO
C        ADDITIONAL PROCESSING IS REQUIRED. THE VALUES FOR D AND THE AS-
C        SOCIATED FLAG TO INDICATE THE  SIGNIFICANCE OF THE VALUE AT THE
C        0.05 LEVEL WERE  ALREADY COMPUTED WITHIN THE  SMFLIB PREDICTION
C        ROUTINES. THE FOLLOWING TWO FLAG VALUES ARE POSSIBLE:
C             1 - D IS LARGER  THAN THE ALLOWED DISTANCE; THEREFORE, THE
C                 THE MODEL MAY NOT PROVIDE AN ADEQUATE FIT.)
C             2 - D IS SMALLER THAN THE ALLOWED DISTANCE; THEREFORE, THE
C                 THE MODEL MAY PROVIDE AN ADEQUATE FIT.)
C
C        IF THE DATA TYPE WAS FOR INTERVAL DATA, THE ROUTINE WILL PROMPT
C        THE USER FOR A  CELL COMBINATION NUMBER (CCN),  AND THEN ACCESS
C        THE SMFGOF ROUTINE  TO COMPUTE  THE CHI-SQUARE  STATISTIC (CHI)
C        AND THE ASSOCIATED DEGREES-OF-FREEDOM (DOF).
C
C        SMFGOF REQUIRES ARGUMENTS CONTAINING THE ORIGINAL AND PREDICTED
C        DATA VECTORS (BOTH OF SIZE NS),  THE NUMBER OF PARAMETERS ESTI-
C        MATED WITHIN THE MODEL (EST), AND THE USER-SPECIFIED CCN.
C
C        ONCE THE  EXECUTION OF  SMFGOF IS PERFORMED,  THE RETURN STATUS
C        FLAG (RFLAG) WILL INDICATE  THE SUCCESSFULNESS OF THE GOODNESS-
C        OF-FIT PROCESSING. THE FOLLOWING THREE VALUES ARE POSSIBLE:
C             0 - SUCCESSFUL CHI AND DOF COMPUTATIONS
C             1 - SUCCESSFUL CHI AND DOF COMPUTATIONS, BUT THE SPECIFIED
C                 CELL COMBINATION  COULD  NOT BE ACHIEVED  FOR THE LAST
C                 INTERVAL.  THE ARGUMENT LIF  WILL CONTAIN THE LAST IN-
C                 TERVAL'S FREQUENCY.
C             2 - THE DEGREES-OF-FREEDOM WAS  CALCULATED TO BE LESS THAN
C                 ONE.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             D      (R) = CALCULATED KOLMOGOROV DISTANCE
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             EST    (I) = NUMBER OF ESTIMATIONS FROM THE MODEL
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             CCN    (R) = CELL COMBINATION FREQUENCY NUMBER
C             CHI    (R) = CHI-SQUARE STATISTIC
C             DOF    (I) = DEGREES-OF-FREEDOM
C             FLAG   (I) = OPTION SELECTION FLAG
C             IDX    (I) = ORIGINAL DATA INDEX POINTER
C             LIF    (R) = LAST INTERVAL'S FREQUENCY
C             NS     (I) = SIZE OF ORIGINAL AND PREDICTED VECTORS
C             RFLAG  (I) = RETURN STATUS FLAG
C   ERRORS
C        RFLAG  = 1 : CCN COULD NOT BE ACHIEVED FOR THE LAST INTERVAL
C        RFLAG  = 2 : THE CALCULATED DOF WAS LESS THAN ONE
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
         INTEGER              DOF           ,FLAG          ,IDX
     1                       ,NS            ,RFLAG
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'    THE KOLMOGOROV DISTANCE IS ',E15.8,', BASED'
     2          ,' UPON',I4,' DATA'
     3        ,/,'    POINTS.  THAT DISTANCE IS SIGNIFICANT AT THE 0.05'
     4          ,' LEVEL; THERE-'
     5        ,/,'    FORE, IT APPEARS THE MODEL MAY NOT PROVIDE AN ADE'
     6          ,'QUATE FIT.')
  305    FORMAT (' '
     1        ,/,'    THE KOLMOGOROV DISTANCE IS ',E15.8,', BASED'
     2          ,' UPON',I4,' DATA'
     3        ,/,'    POINTS.  THAT DISTANCE  IS NOT  SIGNIFICANT AT TH'
     4          ,'E 0.05  LEVEL;'
     5        ,/,'    THEREFORE, IT APPEARS THE MODEL MAY PROVIDE AN AD'
     6          ,'EQUATE FIT.')
  310    FORMAT (' '
     1        ,/,'    ENTER THE CELL COMBINATION FREQUENCY (THE STANDAR'
     2          ,'D IS FIVE), OR'
     3        ,/,'    A MINUS ONE TO INDICATE NO CELL COMBINATIONS.')
  315    FORMAT ('  **THE CHI-SQUARE STATISTIC CANNOT BE CALCULATED;  T'
     1          ,'HE DEGREES-OF-'
     2        ,/,'    FREEDOM WAS CALCULATED TO BE LESS THAN ONE.')
  320    FORMAT ('    THE CELL COMBINATION  WAS NOT ACHIEVED  FOR THE  '
     1          ,'LAST INTERVAL;'
     2        ,/,'    THE GREATEST POSSIBLE VALUE WAS',E15.8,'.')
  325    FORMAT ('    THE CHI-SQUARE STATISTIC  IS',E15.8,' WITH',I6
     1          ,' DEGREES-'
     2        ,/,'    OF-FREEDOM.')
  330    FORMAT (' '
     1        ,/,'    ENTER ONE TO TRY ANOTHER COMBINATION FREQUENCY; E'
     2          ,'LSE ZERO.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
         IF (DATTYP.LE.3) THEN
C            EXECUTION TIME  DATA TYPE IS PRESENT;  SCRIPT THE DISTANCE,
C            AND INTERPRET THE FLAG.
             NS  = (PRC(ESD,1) + PRC(ESD,2) - 1)
C
             IF     (DFLG.EQ.1) THEN
                 WRITE (   *,300) D, NS
                 WRITE (HIST,300) D, NS
             ELSEIF (DFLG.EQ.2) THEN
                 WRITE (   *,305) D, NS
                 WRITE (HIST,305) D, NS
             ENDIF
         ELSE
C            INTERVAL DATA IS PRESENT;  SET THE LOCAL  VARIABLES FOR THE
C            STARTING INDEX OF THE DATA AND THE SAMPLE SIZE.
             IDX = 1
             NS  = PRC(3,1)
C
C            DOWHILE (FLAG .EQ. 1)
 1000            WRITE (   *,310)
                 WRITE (HIST,310)
                 READ  (   *,  *) CCN
                 WRITE (HIST,  *) CCN
C
C                WITH THE INPUT CELL COMBINATION, ACCESS THE SMFGOF ROU-
C                TINE TO CALCULATE THE GOODNESS-OF-FIT STATISTICS.
                 CALL SMFGOF
     G                       (DAT   , PRD   , NS    , EST   , CCN
     Y                       ,CHI   , DOF   , LIF   , RFLAG )
C
                 IF (RFLAG.EQ.2) THEN
C                    THE CHI-SQUARE STATISTIC COULD NOT BE COMPUTED.
                     WRITE (   *,315)
                     WRITE (HIST,315)
                 ELSE
C                    OUTPUT THE LAST INTERVAL'S FREQUENCY, IF NEEDED.
                     IF (RFLAG.EQ.1) THEN
                         WRITE (   *,320) LIF
                         WRITE (HIST,320) LIF
                     ENDIF
C
C                    OUTPUT THE CHI-SQUARE  STATISTIC AND THE ASSOCIATED
C                    DEGREES-OF-FREEDOM.
                     WRITE (   *,325) CHI, DOF
                     WRITE (HIST,325) CHI, DOF
                 ENDIF
                 WRITE (   *,330)
                 WRITE (HIST,330)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) GO TO 1000
C            ENDWHILE
         ENDIF
         RETURN
         END
         SUBROUTINE FITLST
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE FITLST
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE A LISTING OF THE  ORIGINAL, PREDICTED, AND  RESIDUAL
C        VALUES FOR EITHER AN EXECUTION TIME OR INTERVAL DATA MODEL.
C   DESCRIPTION
C        THE ROUTINE SIMPLY SCRIPTS THE TABLE AND RETURNS CONTROL TO THE
C        ANALYSIS DRIVER.
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
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             IDX    (I) = ORIGINAL DATA INDEX POINTER
C             NS     (I) = SIZE OF ORIGINAL AND PREDICTED VECTORS
C             RES    (R) = RESIDUAL OF THE I-TH ELEMENTS
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
         INTEGER              I             ,IDX           ,NS
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,4X,'NUMBER     ORIGINAL DATA     PREDICTED DATA'
     2          ,'    RESIDUAL DATA'
     3        ,/,4X,'======     ==============    =============='
     4          ,'    ==============')
  305    FORMAT (4X,I4,6X,E15.8,3X,E15.8,3X,E15.8)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET THE LOCAL VARIABLES FOR THE  STARTING INDEX OF THE DATA AND
C        THE SAMPLE SIZE.
         IF (DATTYP.LE.3) THEN
             IDX = (END1 * (ESD - 1)) + 1
             NS  = (PRC(ESD,1) + PRC(ESD,2) - 1)
         ELSE
             IDX = 1
             NS  = PRC(3,1)
         ENDIF
C
C        SCRIPT THE ANALYSIS TABLE.
         WRITE (   *,300)
         WRITE (HIST,300)
         DO 1000 I = 1, NS
             RES = DAT(IDX) - PRD(I)
             WRITE (   *,305) I, DAT(IDX), PRD(I), RES
             WRITE (HIST,305) I, DAT(IDX), PRD(I), RES
             IDX = IDX + 1
 1000        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE INPASC
     G                     (ITYPE , IUNIT )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE INPASC
C    G                     (ITYPE , IUNIT )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO INPUT THE OPTIONAL ASCII DATA FILE.
C   DESCRIPTION
C        THIS ROUTINE ACCEPTS DATA IN  FREE-FORMAT TO  SATISFY THE INPUT
C        OF DATA FILES WHICH WERE NOT GENERATED BY THE SMERFS PROGRAM. A
C        FEW REQUIREMENTS  ARE IMPOSED ON THE FORMAT OF THE FILES (REFER
C        TO THE ASSUMPTIONS ENTRY OF THIS PROLOGUE).
C
C        THE ROUTINE  SIMPLY INITIALIZES  THE TEMPORARY  STORAGE ARRAYS,
C        INPUTS THE DATA, AND THEN DETERMINES WHEN THE TRIP VALUE OCCURS
C        TO DETERMINE THE SIZE OF THE DATA SET.  AN ERROR WILL RESULT IF
C        DURING THE INPUT  AN EOF WAS NOT  ENCOUNTERED,  SINCE THE UPPER
C        LIMIT IS DEFINED BY THE STORAGE SPACE.
C
C        THE ROUTINE THEN ENSURES THAT THE  ADDITION WILL NOT EXCEED THE
C        STORAGE SPACE.
C
C        THE INPUT DATA VALUES ARE THEN TRANSFERRED TO THE STORAGE SPACE
C        (CHECKING IF THE FIRST ENTERED VALUE SHOULD BE ADDED TO THE VA-
C        LUE IN THE PREVIOUS LAST LOCATION OR PLACED IN A NEW LOCATION),
C        AS CONTROLLED BY THE FATALITY FLAG.
C   ASSUMPTIONS
C        1. IF WC AND CPU ARE BOTH BEING ENTERED, THEY MUST BE ON SEPAR-
C           ATE INPUT FILES.
C        2. IF INTERVAL DATA ARE BEING ENTERED, THEY MUST BE ON ONE FILE
C           AND ENTERED AS PAIRS. THAT IS:
C            COUNT-1, LENGTH-1, COUNT-2, LENGTH-2, ...
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C             STR2   (I) = INDEX TO BEGINNING OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             TYPES  (C) = DATA TYPE LABELS
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C        YIELDED
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1
C             TM2C   (R) = TEMPORARY STORAGE AREA OF SIZE END1
C   ARGUMENT LIST
C             ITYPE  (I) = ARGUMENT SIMILAR IN FUNCTIONALITY TO DATTYP
C             IUNIT  (I) = UNIT NUMBER FROM WHICH DATA ARE TO BE READ
C   LOCAL GLOSSARY
C             CNTNEW (I) = NUMBER OF ELEMENTS OR PAIRS READ FROM FILE
C             CNTTMP (I) = INTERMEDIATE TO SEE IF DATA CAN BE ADDED
C             ERRFLG (I) = FLAG TO INDICATE DATA SET TOO LARGE
C             MRGFLG (I) = FLAG TO INDICATE FIRST MERGED WITH STORED
C             OS     (I) = OFFSET FOR SECOND DATA STORAGE SPACE
C   ERRORS
C        "THE NUMBER OF ENTRIES EXCEEDS THE MAXIMUM ARRAY SIZE FOR
C         SMERFS."
C        "THE ADDITION WILL EXCEED THE MAXIMUM ARRAY SIZE FOR SMERFS."
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
C               /LBLCOM/      DATA TYPES AND TBF UNITS LABELS COMMON
         COMMON /LBLCOM/      TYPES(3)      ,UNITS(7)
         CHARACTER* 7         UNITS
         CHARACTER*10         TYPES
C
C               /TMPCOM/      TEMPORARY STORAGE COMMON
         COMMON /TMPCOM/      TM1C(END1)    ,TM2C(END1)
C
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              ITYPE         ,IUNIT
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CNTNEW        ,CNTTMP        ,ERRFLG
     1                       ,I             ,MRGFLG        ,OS
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('  **THE NUMBER OF ENTRIES EXCEEDS THE MAXIMUM ARRAY S'
     1          ,'IZE FOR SMERFS.')
  305    FORMAT ('    THE INPUT OF',I4,A10,'ELEMENTS WAS PERFORMED.')
  310    FORMAT (' '
     1        ,/,'    THE FIRST VALUE FROM THE FILE WILL BE ADDED TO TH'
     2          ,'E LAST ELEMENT'
     3        ,/,'    OF THE STORED DATA, SINCE THE FATALITY FLAG INDIC'
     4          ,'ATES A FAILURE'
     5        ,/,'    HAD NOT HAPPENED ON THE FINAL ENTRY.'
     6        ,/,' ')
  315    FORMAT ('  **THE ADDITION WILL EXCEED THE MAXIMUM ARRAY SIZE F'
     1          ,'OR SMERFS.')
  320    FORMAT ('    THE',A10,'STORAGE SPACE NOW CONTAINS',I4,' ELEMEN'
     1          ,'TS.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
         REWIND IUNIT
C
C        INITIALIZE THE TEMPORARY STORAGE AREA, SO THE NUMBER OF ENTRIES
C        CAN BE EASILY DETERMINED.
         DO 1000 I = 1, END1
             TM1C(I) = -99.0
             IF (ITYPE.EQ.3) THEN
                 TM2C(I) = -99.0
             ENDIF
 1000        CONTINUE
C        ENDDO
C
C        ACCEPT THE DATA, SETTING THE ERROR FLAG IF MORE DATA EXISTS.
         ERRFLG = 0
         IF (ITYPE.LE.2) THEN
C            WALL CLOCK OR CPU TBF DATA ARE TO BE ENTERED.
             READ  (IUNIT,  *,END=1025) (TM1C(I), I = 1, END1)
                 ERRFLG = 1
 1025            CONTINUE
C            END-EOF-ENCOUNTERED
         ELSE
C            INTERVAL DATA PAIRS ARE TO BE ENTERED.
             READ  (IUNIT,  *,END=1050) (TM1C(I), TM2C(I), I = 1, END1)
                 ERRFLG = 1
 1050            CONTINUE
C            END-EOF-ENCOUNTERED
         ENDIF
C
         IF (ERRFLG.EQ.1) THEN
C            SCRIPT THE FIRST ERROR MESSAGE.
             WRITE (   *,300)
             WRITE (HIST,300)
         ELSE
C            DETERMINE THE NUMBER OF  INPUT ELEMENTS.  NOTE, SINCE PAIRS
C            ARE SPECIFIED FOR INTERVAL DATA,  ONLY THE FAULT COUNTS ARE
C            TESTED.
             CNTNEW = 0
C            DOUNTIL (TRIP VALUE LOCATED)
 1075            CNTNEW = CNTNEW + 1
                 IF (TM1C(CNTNEW).NE.-99.0) GO TO 1075
C            ENDUNTIL
             CNTNEW = CNTNEW - 1
C
C            SCRIPT MESSAGE INDICATING THE AMOUNT OF DATA READ.
             WRITE (   *,305) CNTNEW, TYPES(ITYPE)
             WRITE (HIST,305) CNTNEW, TYPES(ITYPE)
C
C            DETERMINE THE TOTAL NUMBER OF ELEMENTS.  NOTE, ONE LESS LO-
C            CATION IS NEEDED IF MERGING THE LAST STORED AND FIRST INPUT
C            TBF DATA.
             CNTTMP = PRC(ITYPE,1) + CNTNEW
             MRGFLG = 0
             IF ((ITYPE.LE.2) .AND. (PRC(ITYPE,2).EQ.0) .AND.
     1           (PRC(ITYPE,1).NE.0)) THEN
                 CNTTMP = CNTTMP - 1
                 MRGFLG = 1
                 WRITE (   *,310)
                 WRITE (HIST,310)
             ENDIF
C
C            ENSURE THE ADDITION WILL NOT EXCEED THE ARRAY LIMITS.
             IF (CNTTMP.GT.END1) THEN
C                SCRIPT THE SECOND ERROR MESSAGE.
                 WRITE (   *,315)
                 WRITE (HIST,315)
             ELSE
C                SET THE OFFSET FLAG TO EITHER ZERO  OR THE SIZE  OF THE
C                FIRST STORAGE SPACE.
                 OS = 0
                 IF (ITYPE.EQ.2) THEN
                     OS = END1
                 ENDIF
C
                 IF (ITYPE.LE.2) THEN
C                    STORE THE FIRST TBF ELEMENT AS EITHER A NEW ELEMENT
C                    OR ADDED TO THE PREVIOUS ENTRY.
                     IF (MRGFLG.EQ.0) THEN
                         PRC(ITYPE,1)         = PRC(ITYPE,1) + 1
                         DAT(PRC(ITYPE,1)+OS) = TM1C(1)
                     ELSE
                         DAT(PRC(ITYPE,1)+OS) = DAT(PRC(ITYPE,1)+OS)
     1                                        + TM1C(1)
                     ENDIF
C
C                    STORE THE REMAINING ELEMENTS.
                     DO 1100 I = 2, CNTNEW
                         PRC(ITYPE,1)         = PRC(ITYPE,1) + 1
                         DAT(PRC(ITYPE,1)+OS) = TM1C(I)
 1100                    CONTINUE
C                    ENDDO
                 ELSE
C                    STORE THE INTERVAL PAIRS.
                     DO 1150 I = 1, CNTNEW
                         PRC(3,1)           = PRC(3,1) + 1
                         DAT(PRC(3,1))      = TM1C(I)
                         DAT(PRC(3,1)+END1) = TM2C(I)
 1150                    CONTINUE
C                    ENDDO
                 ENDIF
C
C                SCRIPT TOTAL NUMBER AND TYPE OF THE DATA.
                 WRITE (   *,320) TYPES(ITYPE), PRC(ITYPE,1)
                 WRITE (HIST,320) TYPES(ITYPE), PRC(ITYPE,1)
             ENDIF
C
         ENDIF
         RETURN
         END
         SUBROUTINE INPDRV
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE INPDRV
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO RECORD THE DATA FOR ANALYSIS, PROMPT FOR THE FILE UPDATE VA-
C        LUE, AND GENERATE THE UPDATED DATA BASE FILE(S), IF INDICATED.
C   DESCRIPTION
C        DATA CAN BE INPUT FROM A PREVIOUS SMERFS-GENERATED DATA FILE, A
C        STANDARD ASCII  DATA FILE (CONSTRUCTED BY OTHER SOFTWARE),  AND
C        FROM THE KEYBOARD, THE INPUT OF A SMERFS-GENERATED DATA FILE IS
C        ONLY ALLOWED DURING THE FIRST ACCESS OF THIS MODULE. SUBSEQUENT
C        ENTRIES WILL AUTOMATICALLY FALL PAST THE OPTION.
C
C        ONCE DATA ARE STORED IN THE DATA VECTORS, ALL SUBSEQUENT INPUTS
C        WILL BE BASED ON THE TYPE ENTERED THE FIRST TIME  (WHETHER THAT
C        INPUT CAME FROM A DATA FILE OR A PREVIOUS KEYBOARD SESSION).
C
C        THE ROUTINE NOW CONTAINS A DATA LIST OPTION AND PRIOR TO RETURN
C        TO THE MAIN MODULE,  INPDRV WILL WRITE  ALL THE STORED  DATA TO
C        THE APPROPRIATE OUTPUT FILE (IF INDICATED).
C
C        THE PLACEMENT  OF DATA ITEMS  WILL BE AS  FOLLOWS ONCE THE DATA
C        ARE INPUT TO THE SMERFS PROGRAM:
C        1. THE FIRST COLUMN OF THE  INTEGER ARRAY PRC CONTAINS THE NUM-
C           BER OF ENTRIES FOR WC TBF DATA,  CPU TBF DATA,  AND INTERVAL
C           DATA, RESPECTIVELY.
C           NOTE IF ELEMENT 3 IS NONZERO, ELEMENTS 1 AND 2 MUST BE ZERO;
C           CONVERSELY, IF ELEMENT 3 IS ZERO,  ELEMENT 1 AND/OR 2 MAY BE
C           NONZERO.
C        2. THE FIRST TWO ELEMENTS OF THE SECOND COLUMN OF THE PRC ARRAY
C           CONTAIN  THE FATALITY FLAGS  FOR THE TWO  TBF DATA TYPES.  A
C           ZERO INDICATES THAT TESTING WAS CONDUCTED AFTER THE LAST EN-
C           TERED FAILURE, AND  ONE INDICATES THAT NO ADDITIONAL TESTING
C           WAS CONDUCTED AFTER THE FINAL FAILURE.
C           NOTE THE THIRD ELEMENT  IS NOT APPLICABLE;  IT IS USED AS AN
C           EXTRA ARGUMENT THROUGHOUT THE PROGRAM.
C        3. THE FIRST TWO ELEMENTS OF THE THIRD  COLUMN OF THE PRC ARRAY
C           CONTAIN THE MEASUREMENT UNIT (SECONDS, MINUTES, HOURS, DAYS,
C           WEEKS, MONTHS, OR YEARS).
C           NOTE THE THIRD ELEMENT IS  AGAIN NOT APPLICABLE;  IT IS USED
C           TO INDICATE THE NEW FILE FORMAT.
C        4. IF WC TBF DATA ARE  BEING  RECORDED THEN  ELEMENTS 1 THROUGH
C           END1 OF THE  REAL VECTOR DAT ARE AVAILABLE FOR STORING THOSE
C           DATA; ELEMENTS STR2 THROUGH END2 ARE SIMILARLY AVAILABLE FOR
C           CPU TBF DATA.
C        5. IF INTERVAL DATA ARE  BEING  RECORDED THEN  THE FAULT COUNTS
C           WILL BE PLACED IN  ELEMENTS 1 THROUGH END1 OF THE VECTOR DAT
C           AND THE  ASSOCIATED TESTING  LENGTHS WILL BE STORED IN LOCA-
C           TIONS STR2 THROUGH END2.
C
C        THIS IS THE MOST MACHINE DEPENDENT ROUTINE; CONSIDERATIONS THAT
C        MUST BE ADDRESSED (DURING EXECUTION) INCLUDE:
C        1. DOES THE TARGET  COMPUTER HAVE LOCAL FILES (LIKE THE CDC) OR
C           ARE ALL OPENED FILES MADE PERMANENT (LIKE THE VAX).
C        2. DOES THE TARGET COMPUTER HAVE MULT REVISIONS OF A FILE (LIKE
C           THE VAX) OR IS ONLY ONE REVISION ALLOWED (LIKE THE CDC).
C        3. DOES THE TARGET COMPUTER HAVE A LIMIT TO THE NUMBER OF CHAR-
C           ACTERS THAT A FILE NAME CAN HAVE.
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
C             TYPES  (C) = DATA TYPE LABELS
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             UPDFLG (I) = FLAG TO CONTROL FILE DATA INPUT AND UPDATE
C        YIELDED
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             ASFIL  (I) = VECTOR TO INDICATE OPTIONAL ASCII FILES
C             FILOPN (I) = FILE OPENED FLAG
C             FLAG   (I) = OPTION FLAG
C             IU     (I) = FILE I/O UNIT VARIABLE
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
         INTEGER              ASFIL(3)      ,FILOPN        ,FLAG
     1                       ,I             ,IU            ,J
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        INPUT FORMATS
  100    FORMAT (3I10)
  105    FORMAT (E15.8)
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'   ENTER DESIRED DATA TYPE, OR ZERO FOR A LIST.')
  305    FORMAT ('   THE AVAILABLE DATA TYPES ARE:'
     1        ,/,'    1 WALL CLOCK (WC) TIME-BETWEEN-FAILURES (TBF)'
     2        ,/,'    2 CENTRAL PROCESSING UNITS (CPU) TBF'
     3        ,/,'    3 WC TBF AND CPU TBF'
     4        ,/,'    4 INTERVAL FAULT COUNTS AND TESTING LENGTHS'
     5        ,/,'   ENTER DESIRED DATA TYPE.')
  310    FORMAT (' '
     1        ,/,'   ENTER ONE FOR A STANDARD SMERFS FILE INPUT; ELSE Z'
     2          ,'ERO.')
  315    FORMAT (' '
     1        ,/,'    THE AVAILABLE UNITS FOR TBF DATA PROCESSING ARE:'
     2        ,/,'     1 SECONDS   3 HOURS     5 WEEKS     7 YEARS'
     3        ,/,'     2 MINUTES   4 DAYS      6 MONTHS'
     4        ,/,'    ENTER CODE NUMBER FOR THE',A10,'DATA.')
  320    FORMAT ('    THE INPUT OF',I4,A10,'ELEMENTS WAS PERFORMED.')
  325    FORMAT (' '
     1        ,/,'   ENTER INPUT OPTION, OR ZERO FOR A LIST.')
  330    FORMAT ('   THE AVAILABLE INPUT OPTIONS ARE:'
     1        ,/,'    1 ASCII FILE INPUT'
     2        ,/,'    2 KEYBOARD INPUT'
     3        ,/,'    3 LIST THE CURRENT DATA'
     4        ,/,'    4 RETURN TO THE MAIN PROGRAM'
     5        ,/,'   ENTER INPUT OPTION.')
  335    FORMAT (' '
     1        ,/,'    THE FOLLOWING DATA ARE CURRENTLY RESIDENT.'
     2       ,//,'     DATA TYPE   NUMBER   UNITS'
     3        ,/,'     =========   ======   =======')
  340    FORMAT ('     WC  TBF      ',I4,4X,A7)
  345    FORMAT ('     CPU TBF      ',I4,4X,A7)
  350    FORMAT (' '
     1        ,/,'    ENTER ONE IF THE DATA TO BE ENTERED  ARE CONSISTE'
     2          ,'NT IN TYPE AND'
     3        ,/,'    UNITS WITH THESE; ELSE ZERO TO ABORT THIS PROCESS'
     4          ,'ING.')
  355    FORMAT (' '
     1        ,/,'    ENTER ONE IF A FAILURE HAPPENED  AT THE END OF TH'
     2          ,'E FINAL ENTRY,'
     3        ,/,'    OR ZERO IF A FAILURE HAD NOT HAPPENED.')
  360    FORMAT (' '
     1        ,/,'    NOTE WC TBF DATA ARE ENTERED IN 24 HOUR UNITS AND'
     2          ,' STORED IN THE'
     3        ,/,'    UNITS OF THE CURRENT DATA, ',A7,'.')
  365    FORMAT (' '
     1        ,/,'    NOTE WC TBF DATA ARE ENTERED IN 24 HOUR UNITS AND'
     2          ,' STORED IN THE'
     3        ,/,'    SPECIFIED UNITS, ',A7,'.'
     4        ,/)
  370    FORMAT (' '
     1        ,/,'   ENTER ONE FOR THE PROGRAM TO MAKE NEW DATA FILES; '
     2          ,'ELSE ZERO. THE'
     3        ,/,'   RESPONSE WILL BE USED THROUGHOUT THE EXECUTION. A '
     4          ,'ZERO WILL ALSO'
     5        ,/,'   VOID THE DATA RESTORE OPTION IN DATA TRANSFORMATIO'
     6          ,'NS.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
         IF (UPDFLG.EQ.-1) THEN
C            THIS IS THE FIRST ENTRY TO THE ROUTINE.  THE DATA TYPE MUST
C            BE SPECIFIED.
             WRITE (   *,300)
             WRITE (HIST,300)
             READ  (   *,  *) DATTYP
             WRITE (HIST,  *) DATTYP
             IF ((DATTYP.LT.1) .OR. (DATTYP.GT.4)) THEN
                 WRITE (   *,305)
                 WRITE (HIST,305)
                 READ  (   *,  *) DATTYP
                 WRITE (HIST,  *) DATTYP
             ENDIF
C
C            INITIALIZE THE UPDATE FILE OPENED FLAG TO ZERO.
             FILOPN = 0
C
C            DETERMINE IF STANDARD SMERFS FILE INPUT IS DESIRED.
             WRITE (   *,310)
             WRITE (HIST,310)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
C
             IF (FLAG.EQ.1) THEN
C                SET PRC TO REFLECT DESIRED FILE(S).
                 IF (DATTYP.LE.2) THEN
                     PRC(DATTYP,1) = 1
                 ELSEIF (DATTYP.EQ.3) THEN
                     PRC(1,1) = 1
                     PRC(2,1) = 1
                 ELSE
                     PRC(3,1) = 1
                 ENDIF
C
                 DO 1000 I = 1, 3
                     IF (PRC(I,1).EQ.1) THEN
C                        ESTABLISH THE PROGRAM UNIT NUMBER  AND OPEN THE
C                        INPUT FILE.
                         IU = I + 10
                         CALL INPOPN
     G                               (I     , IU    , 1     )
C
C                        ENTER THE FIRST LINE OF THE DATA FILE TO DETER-
C                        MINE IF THE FILE WAS CONSTRUCTED  UNDER THE OLD
C                        SMERFS4 FORMAT.
                         READ  (IU,100) (PRC(I,J), J = 1, 3)
C
                         IF (PRC(I,3).EQ.0) THEN
C                            OLD SMERFS DATA FILE FORMAT.
                             IF (I.LE.2) THEN
C                                PROMPT FOR THE UNITS OF THESE TBF DATA.
                                 WRITE (   *,315) TYPES(I)
                                 WRITE (HIST,315) TYPES(I)
                                 READ  (   *,  *) PRC(I,3)
                                 WRITE (HIST,  *) PRC(I,3)
                             ELSE
C                                SET UNIT LOCATION TO  INDICATE NEW FOR-
C                                MAT FOR INTERVAL DATA.
                                 PRC(I,3) = 1
                             ENDIF
C
C                            INPUT THE DATA.
                             IF     (I.EQ.1) THEN
                                 READ  (IU,105) (DAT(J), J =    1, END1)
                             ELSEIF (I.EQ.2) THEN
                                 READ  (IU,105) (DAT(J), J = STR2, END2)
                             ELSE
                                 READ  (IU,105) (DAT(J), J =    1, END2)
                             ENDIF
                             WRITE (   *,320) PRC(I,1), TYPES(I)
                             WRITE (HIST,320) PRC(I,1), TYPES(I)
                         ELSE
C                            THE DATA ARE STORED IN NEW SMERFS5 FORMAT.
                             CALL INPPRM
     G                                   (I     , IU    )
                         ENDIF
                         CLOSE (IU)
                     ENDIF
 1000                CONTINUE
C                ENDDO
             ENDIF
         ENDIF
C
C        DOWHILE (INPUT SELECTION .NE. RETURN)
 1025        WRITE (   *,325)
             WRITE (HIST,325)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
             IF ((FLAG.LT.1) .OR. (FLAG.GT.4)) THEN
                 WRITE (   *,330)
                 WRITE (HIST,330)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
             ENDIF
             IF ((FLAG.LT.1) .OR. (FLAG.GT.3)) GO TO 1075
C
             IF     (FLAG.EQ.1) THEN
C                SET OPTIONAL ASCII VECTOR TO REFLECT DESIRED FILE(S).
                 ASFIL(1) = 0
                 ASFIL(2) = 0
                 ASFIL(3) = 0
                 IF (DATTYP.LE.2) THEN
                     ASFIL(DATTYP) = 1
                 ELSEIF (DATTYP.EQ.3) THEN
                     ASFIL(1) = 1
                     ASFIL(2) = 1
                 ELSE
                     ASFIL(3) = 1
                 ENDIF
C
                 I = PRC(1,1) + PRC(2,1) + PRC(3,1)
                 IF (I.NE.0) THEN
C                    DATA CURRENTLY RESIDES IN THE STORAGE ARRAYS;  GIVE
C                    THE USER AN OPTION TO ABORT IF THE TBF DATA DOESN'T
C                    MATCH.
                     IF (PRC(3,1).EQ.0) THEN
                         WRITE (   *,335)
                         WRITE (HIST,335)
                         IF (PRC(1,1).NE.0) THEN
                             WRITE (   *,340) PRC(1,1)
     1                                      , UNITS(PRC(1,3))
                             WRITE (HIST,340) PRC(1,1)
     1                                      , UNITS(PRC(1,3))
                         ENDIF
                         IF (PRC(2,1).NE.0) THEN
                             WRITE (   *,345) PRC(2,1)
     1                                      , UNITS(PRC(2,3))
                             WRITE (HIST,345) PRC(2,1)
     1                                      , UNITS(PRC(2,3))
                         ENDIF
                         WRITE (   *,350)
                         WRITE (HIST,350)
                         READ  (   *,  *) FLAG
                         WRITE (HIST,  *) FLAG
                     ENDIF
                 ENDIF
C
                 IF (FLAG.EQ.1) THEN
                     DO 1050 I = 1, 3
                         IF (ASFIL(I).EQ.1) THEN
C                            ESTABLISH THE PROGRAM UNIT NUMBER  AND OPEN
C                            THE INPUT FILE.
                             IU = I + 20
                             CALL INPOPN
     G                                   (I     , IU    , 1     )
C
                             IF (I.LE.2) THEN
                                 IF (PRC(I,1).EQ.0) THEN
C                                    PROMPT FOR THE UNITS OF TBF DATA.
                                     WRITE (   *,315) TYPES(I)
                                     WRITE (HIST,315) TYPES(I)
                                     READ  (   *,  *) PRC(I,3)
                                     WRITE (HIST,  *) PRC(I,3)
                                 ENDIF
                             ELSE
C                                SET UNIT LOCATION TO INDICATE  NEW FOR-
C                                MAT FOR INTERVAL DATA.
                                 PRC(I,3) = 1
                             ENDIF
C
C                            ACCESS THE INPASC ROUTINE TO STORE DATA.
                             CALL INPASC
     G                                   (I     , IU    )
                             CLOSE (IU)
                         ENDIF
 1050                    CONTINUE
C                    ENDDO
C
C                    PROMPT FOR FATALITY FLAG OF THE LAST ELEMENT. SINCE
C                    THE VALUE IS NOT  USED UNLESS PRC(X,1) IS SET,  THE
C                    TWO ARE SIMPLY ASSIGNED TO THE INPUT VALUE.
                     IF ((ASFIL(1).EQ.1) .OR. (ASFIL(2).EQ.1)) THEN
                         WRITE (   *,355)
                         WRITE (HIST,355)
                         READ  (   *,  *) PRC(1,2)
                         WRITE (HIST,  *) PRC(1,2)
                         PRC(2,2) = PRC(1,2)
                     ENDIF
                 ENDIF
C
             ELSEIF (FLAG.EQ.2) THEN
C                PREPARE FOR KEYBOARD INPUT PORTION.
                 I = PRC(1,1) + PRC(2,1) + PRC(3,1)
                 IF (I.NE.0) THEN
C                    DATA CURRENTLY RESIDES IN THE STORAGE ARRAYS;  GIVE
C                    THE USER AN OPTION TO ABORT IF THE TBF DATA DOESN'T
C                    MATCH.
                     IF (PRC(3,1).EQ.0) THEN
                         WRITE (   *,335)
                         WRITE (HIST,335)
                         IF (PRC(1,1).NE.0) THEN
                             WRITE (   *,340) PRC(1,1)
     1                                      , UNITS(PRC(1,3))
                             WRITE (HIST,340) PRC(1,1)
     1                                      , UNITS(PRC(1,3))
                         ENDIF
                         IF (PRC(2,1).NE.0) THEN
                             WRITE (   *,345) PRC(2,1)
     1                                      , UNITS(PRC(2,3))
                             WRITE (HIST,345) PRC(2,1)
     1                                      , UNITS(PRC(2,3))
                         ENDIF
                         WRITE (   *,350)
                         WRITE (HIST,350)
                         IF (PRC(1,1).NE.0) THEN
                             WRITE (   *,360) UNITS(PRC(1,3))
                             WRITE (HIST,360) UNITS(PRC(1,3))
                         ENDIF
                         READ  (   *,  *) FLAG
                         WRITE (HIST,  *) FLAG
                     ELSE
                         FLAG = 1
                     ENDIF
                 ELSE
C                    THIS IS THE FIRST INPUT DATA FOR SMERFS; CHANGE THE
C                    FATALITY FLAG AND UNIT INDICATOR ELEMENT TO SIMPLI-
C                    FY THE PROCESSING OF INPKEY AND INPSTR.
                     IF     (DATTYP.EQ.1) THEN
C                        THE REQUESTED INPUT TYPE WAS FOR WC TBF.
                         PRC(1,2) = 1
                         WRITE (   *,315) TYPES(1)
                         WRITE (HIST,315) TYPES(1)
                         READ  (   *,  *) PRC(1,3)
                         WRITE (HIST,  *) PRC(1,3)
                         WRITE (   *,365) UNITS(PRC(1,3))
                         WRITE (HIST,365) UNITS(PRC(1,3))
                     ELSEIF (DATTYP.EQ.2) THEN
C                        THE REQUESTED INPUT TYPE WAS FOR CPU TBF.
                         PRC(2,2) = 1
                         WRITE (   *,315) TYPES(2)
                         WRITE (HIST,315) TYPES(2)
                         READ  (   *,  *) PRC(2,3)
                         WRITE (HIST,  *) PRC(2,3)
                     ELSEIF (DATTYP.EQ.3) THEN
C                        THE REQUESTED INPUT TYPE WAS FOR BOTH.
                         PRC(1,2) = 1
                         WRITE (   *,315) TYPES(1)
                         WRITE (HIST,315) TYPES(1)
                         READ  (   *,  *) PRC(1,3)
                         WRITE (HIST,  *) PRC(1,3)
                         PRC(2,2) = 1
                         WRITE (   *,315) TYPES(2)
                         WRITE (HIST,315) TYPES(2)
                         READ  (   *,  *) PRC(2,3)
                         WRITE (HIST,  *) PRC(2,3)
                         WRITE (   *,365) UNITS(PRC(1,3))
                         WRITE (HIST,365) UNITS(PRC(1,3))
                     ELSE
C                        THE REQUESTED INPUT TYPE WAS FOR INTERVAL DATA.
                         PRC(3,3) = 1
                     ENDIF
                     FLAG = 1
                 ENDIF
C
                 IF (FLAG.EQ.1) THEN
C                    ACCESS THE INPKEY ROUTINE TO STORE THE DATA.
                     CALL INPKEY
                 ENDIF
             ELSE
                 CALL DATLST
             ENDIF
             GO TO 1025
 1075        CONTINUE
C        ENDWHILE
C
         IF (UPDFLG.EQ.-1) THEN
C            DETERMINE IF DATA FILE UPDATE IS DESIRED.
             WRITE (   *,370)
             WRITE (HIST,370)
             READ  (   *,  *) UPDFLG
             WRITE (HIST,  *) UPDFLG
         ENDIF
C
         IF (UPDFLG.EQ.1) THEN
C            GENERATE THE NEW DATA BASE FILES, IF THERE IS CORRESPONDING
C            DATA WITHIN THE DATA VECTOR.
             DO 1100 I = 1, 3
                 IF (PRC(I,1).NE.0) THEN
C                    THIS SAMPLE IS TO BE OUTPUT;  ESTABLISH THE PROGRAM
C                    UNIT NUMBER AND OPEN OR REWIND THE OUTPUT FILE.
                     IU = I + 13
                     IF (FILOPN.EQ.0) THEN
                         CALL INPOPN
     G                               (I     , IU    , 2     )
                     ELSE
                         REWIND IU
                     ENDIF
C
C                    OUTPUT THE DATA IN THE NEW SMERFS5 FORMAT.
                     CALL OUTPRM
     G                           (I      , IU     )
                 ENDIF
 1100            CONTINUE
C            ENDDO
             FILOPN = 1
         ENDIF
         RETURN
         END
         SUBROUTINE INPKEY
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE INPKEY
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO SCRIPT THE  NECESSARY PROMPTS FOR  KEYBOARD INPUT, INPUT THE
C        USER RESPONSES, AND  ACCESS THE  INPSTR ROUTINE TO  PERFORM THE
C        ACTUAL DATA STORAGE.
C   DESCRIPTION
C        BASED ON THE VALUE OF FLAG,  ONE OF FOUR TYPES OF KEYBOARD DATA
C        WILL BE ACCEPTED.  WC TBF, CPU TBF,  WC & CPU TBF, AND INTERVAL
C        INPUTS ARE ALLOWED.
C
C        THE CNV24H FUNCTION WILL BE  ACCESSED TO CONVERT THE WALL CLOCK
C        TIMES (MEASURED ON A 24 HOUR CLOCK) TO THE SPECIFIED UNITS.
C
C        ONCE THE DATA ARE INPUT (AND CONVERTED IF NECESSARY) THE INPSTR
C        ROUTINE WILL BE CALLED TO STORE THE DATA.
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
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             CPE    (I) = CPU FAILURE STATUS (0 - NO, 1 - YES)
C             CPT    (R) = CPU EXECUTION TIME
C             DONE   (I) = COMPLETION FLAG
C             IC     (R) = INTERVAL FAULT COUNT
C             IL     (R) = INTERVAL TESTING LENGTH
C             TE     (R) = WC FAILURE TIME (24HR)
C             TF     (R) = WC FINISH  TIME (24HR)
C             TM     (R) = WC EXECUTION TIME (MINUTES)
C             TN     (I) = WC FAILURES IN THE SITTING
C             TS     (R) = WC STARTING TIME (24HR)
C   ERRORS
C        "THE SECOND VALUE MUST BE ZERO OR ONE; RE-ENTER THAT VALUE."
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
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CPE           ,DONE          ,I
     1                       ,TN
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('    A RESPONSE OF NEGATIVE VALUES FOR THE PROMPT:'
     1        ,/,'     "ENTER STARTING & ENDING TIMES; AND NUMBER OF FA'
     2          ,'ILURES."'
     3        ,/,'    WILL END THE PROCESSING.')
  305    FORMAT (' '
     1        ,/,'    ENTER STARTING & ENDING TIMES; AND NUMBER OF FAIL'
     2          ,'URES.')
  315    FORMAT ('     ENTER TIME OF FAILURE NUMBER',I4,'.')
  320    FORMAT ('     ENTER ASSOCIATED CPU EXPENDED.')
  325    FORMAT ('     ENTER LAST FAILURE-FREE CPU.')
  330    FORMAT ('    A RESPONSE OF NEGATIVE VALUES FOR THE PROMPT:'
     1        ,/,'     "ENTER CPU; AND ONE IF A FAILURE HAPPENED, OR ZE'
     2          ,'RO IF NOT."'
     3        ,/,'    WILL END THE PROCESSING.',/)
  335    FORMAT ('    ENTER CPU; AND ONE IF A FAILURE HAPPENED, OR ZERO'
     1          ,' IF NOT.')
  340    FORMAT ('  **THE SECOND VALUE MUST BE ZERO OR ONE; RE-ENTER TH'
     1          ,'AT VALUE.')
  345    FORMAT ('    A RESPONSE OF NEGATIVE VALUES FOR THE PROMPT:'
     1        ,/,'     "ENTER FAULT COUNT AND TESTING LENGTH."'
     2        ,/,'    WILL END THE PROCESSING.',/)
  350    FORMAT ('    ENTER FAULT COUNT AND TESTING LENGTH.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
         DONE = 0
         IF ((DATTYP.EQ.1) .OR. (DATTYP.EQ.3)) THEN
C            EITHER A WC TBF INPUT  OR A WC & CPU TBF DATA INPUT MODE IS
C            DESIRED, OUTPUT THE METHOD OF INPUT TERMINATION MESSAGE.
             WRITE (   *,300)
             WRITE (HIST,300)
C
C            DOWHILE (DONE .EQ. 0)
 1000            IF (DONE.EQ.0) THEN
C                PROMPT AND INPUT THE WC TIMES AND NUMBER OF FAILURES.
                 WRITE (   *,305)
                 WRITE (HIST,305)
                 READ  (   *,  *) TS, TF, TN
                 WRITE (HIST,  *) TS, TF, TN
                 IF ((TS.LT.0.0) .OR. (TF.LT.0.0) .OR. (TN.LT.0)) THEN
                     DONE = 1
                 ENDIF
C
                 IF (DONE.EQ.0) THEN
                     IF (TN.NE.0) THEN
C                        THE INPUT RESPONSE OF THE NUMBER OF FAILURES IS
C                        POSITIVE. A LOOP OF TN TIMES (PROVIDED THAT THE
C                        DONE VARIABLE IS NOT SET TO ONE, INDICATING THE
C                        ARRAY SIZE WAS  ABOUT TO BE  EXCEEDED)  WILL BE
C                        CONDUCTED TO INPUT THE FAILURE TIMES.
                         I = 0
C                        DOWHILE ((I .LE. TN) .AND. (DONE .EQ. 0))
 1025                        I = I + 1
                             IF ((I.LE.TN) .AND. (DONE.EQ.0)) THEN
C                            PROMPT AND INPUT THE TIME OF FAILURE I.
                             WRITE (   *,315) I
                             WRITE (HIST,315) I
                             READ  (   *,  *) TE
                             WRITE (HIST,  *) TE
                             IF (DATTYP.EQ.3) THEN
C                                PROMPT AND INPUT THE CPU EXPENDED.
                                 WRITE (   *,320)
                                 WRITE (HIST,320)
                                 READ  (   *,  *) CPT
                                 WRITE (HIST,  *) CPT
                             ENDIF
C                            USING THE  CNV24H FUNCTION, COMPUTE  THE WC
C                            EXECUTION TIME (IN THE SPECIFIED UNITS).
                             TM = CNV24H(TE) - CNV24H(TS)
C
C                            ACCESS THE INPSTR ROUTINE TO STORE DATA.
                             CALL INPSTR
     G                                   (1       , END1    , DATTYP
     G                                   ,TM      , CPT
     B                                   ,PRC(1,2), PRC(1,1), DAT
     B                                   ,DAT(STR2)
     Y                                   ,DONE    )
C                            UPDATE STARTING TIME TO THE FAILURE TIME.
                             TS = TE
                             GO TO 1025
                             ENDIF
C                        ENDWHILE
                     ENDIF
                     IF ((TS.NE.TF) .AND. (DONE.EQ.0)) THEN
C                        EITHER THE  INPUT NUMBER  OF FAILURES (TN)  WAS
C                        ZERO OR TESTING  CONTINUED  AFTER THE  LAST RE-
C                        CORDED FAILURE TIME.
                         IF (DATTYP.EQ.3) THEN
C                            PROMPT AND INPUT EITHER THE  ASSOCIATED CPU
C                            EXPENDED OR THE FAILURE-FREE CPU.
                             IF (TN.EQ.0) THEN
                                 WRITE (   *,320)
                                 WRITE (HIST,320)
                             ELSE
                                 WRITE (   *,325)
                                 WRITE (HIST,325)
                             ENDIF
                             READ  (   *,  *) CPT
                             WRITE (HIST,  *) CPT
                         ENDIF
C                        USING THE CNV24H FUNCTION,  COMPUTE THE WC EXE-
C                        CUTION TIME (IN THE SPECIFIED UNITS).
                         TM = CNV24H(TF) - CNV24H(TS)
C
C                        ACCESS THE INPSTR ROUTINE TO STORE THE DATA.
                         CALL INPSTR
     G                               (0       , END1    , DATTYP
     G                               ,TM      , CPT
     B                               ,PRC(1,2), PRC(1,1), DAT
     B                               ,DAT(STR2)
     Y                               ,DONE    )
                     ENDIF
                     IF (DATTYP.EQ.3) THEN
C                        SET THE CPU FAILURE COUNT AND FATALITY FLAG EL-
C                        EMENTS OF THE PRC ARRAY EQUAL TO THE WC VALUES,
C                        AS BOTH ARE BEING PERFORMED.
                         PRC(2,1) = PRC(1,1)
                         PRC(2,2) = PRC(1,2)
                     ENDIF
                 ENDIF
                 GO TO 1000
                 ENDIF
C            ENDWHILE
         ELSEIF (DATTYP.EQ.2) THEN
C            A CPU TBF INPUT MODE IS DESIRED, OUTPUT THE METHOD OF INPUT
C            TERMINATION MESSAGE.
             WRITE (   *,330)
             WRITE (HIST,330)
C            DOWHILE (DONE .EQ. 0)
 1050            IF (DONE.EQ.0) THEN
C                PROMPT AND INPUT THE EXPENDED CPU AND THE TEST TERMINA-
C                TION STATE FLAG, WHERE ONE INDICATES THE SESSION  ENDED
C                WITH THE DETECTION OF A FAILURE, AND ZERO INDICATES THE
C                TESTING WAS FAILURE-FREE.  THEN SET DONE  IF THE  INPUT
C                INDICATED INPUT TERMINATION.
                 WRITE (   *,335)
                 WRITE (HIST,335)
                 READ  (   *,  *) CPT, CPE
                 WRITE (HIST,  *) CPT, CPE
                 IF ((CPT.LT.0.0) .OR. (CPE.LT.0)) THEN
                     DONE = 1
                 ENDIF
                 IF (DONE.EQ.0) THEN
C                    ENSURE THAT THE FATALITY FLAG IS A ZERO OR ONE;  IF
C                    NOT RE-PROMPT FOR THE INPUT.
                     IF ((CPE.NE.0) .AND. (CPE.NE.1)) THEN
                          WRITE (   *,340)
                          WRITE (HIST,340)
                          READ  (   *,  *) CPE
                          WRITE (HIST,  *) CPE
                     ENDIF
C
C                    ACCESS THE INPSTR ROUTINE TO STORE THE DATA.
                     CALL INPSTR
     G                           (CPE     , END1    , DATTYP , TM
     G                           ,CPT
     B                           ,PRC(2,2), PRC(2,1), DAT    , DAT(STR2)
     Y                           ,DONE    )
                 ENDIF
                 GO TO 1050
                 ENDIF
C            ENDWHILE
         ELSE
C            AN INTERVAL INPUT MODE  MUST BE DESIRED,  OUTPUT THE METHOD
C            OF INPUT TERMINATION MESSAGE.
             WRITE (   *,345)
             WRITE (HIST,345)
C            DOWHILE (DONE .EQ. 0)
 1075            IF (DONE.EQ.0) THEN
C                PROMPT AND INPUT THE INTERVAL DATA (FAULT COUNT & TEST-
C                ING LENGTH). THEN SET DONE IF THE INPUT CONSTITUTED IN-
C                PUT TERMINATION.
                 WRITE (   *,350)
                 WRITE (HIST,350)
                 READ  (   *,  *) IC, IL
                 WRITE (HIST,  *) IC, IL
                 IF ((IC.LT.0.0) .OR. (IL.LT.0.0)) THEN
                     DONE = 1
                 ENDIF
                 IF (DONE.EQ.0) THEN
C                    ACCESS THE INPSTR ROUTINE TO STORE THE DATA.
                     CALL INPSTR
     G                           (0       , END1    , DATTYP , IC
     G                           ,IL
     B                           ,1       , PRC(3,1), DAT    , DAT(STR2)
     Y                           ,DONE    )
                 ENDIF
                 GO TO 1075
                 ENDIF
C            ENDWHILE
         ENDIF
         RETURN
         END
         SUBROUTINE INPOPN
     G                     (ITYPE , IUNIT , ISTAT )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE INPOPN
C    G                     (ITYPE , IUNIT , ISTAT )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO OPEN A SPECIFIED DATA INPUT OR OUTPUT FILE.
C   DESCRIPTION
C        THE ROUTINE SIMPLY PROMPTS FOR A FILE NAME  AND OPENS THE FILE.
C        THE LEGALITY OF THE FILE NAME AND AVAILABILITY OF THE FILE WILL
C        BE DETERMINED THROUGH THE ERROR ARGUMENT OF THE OPEN STATEMENT.
C   ASSUMPTIONS
C        1. A LEGAL FILENAME WILL EVENTUALLY BE ENTERED  (I.E., THE ROU-
C           TINE HAS NO ABORT PROCESSING).
C   RESTRICTIONS
C        THIS  SUBPROGRAM  CONTAINS  CODE WHICH DOES NOT COMPLY WITH THE
C        PROGRAMMING STANDARDS; GROUP LEADER APPROVAL HAS BEEN OBTAINED.
C       -SPECIFICALLY:
C        THE DOWHILES CONTAIN NON-STRUCTURED LOOP-BACKS AND BRANCHES.
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             TYPES  (C) = DATA TYPE LABELS
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C   ARGUMENT LIST
C             ISTAT  (I) = FILE STATUS (1=OLD, 2=NEW)
C             ITYPE  (I) = ARGUMENT SIMILAR IN FUNCTIONALITY TO DATTYP
C             IUNIT  (I) = UNIT NUMBER FROM WHICH DATA ARE TO BE READ
C   LOCAL GLOSSARY
C             FILNM  (C) = INPUT FILE NAME OF FILE TO BE OPENED
C   ERRORS
C        "FILE NAME ERROR; TRY AGAIN (AFTER THE PROMPT)."
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
         INTEGER              ISTAT         ,ITYPE         ,IUNIT
C
C                             LOCAL SPECIFICATIONS
         CHARACTER*50         FILNM
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        INPUT FORMATS
  100    FORMAT (A50)
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'    ENTER INPUT FILE NAME FOR',A10,'DATA.')
  305    FORMAT ('  **FILE NAME ERROR; TRY AGAIN (AFTER THE PROMPT).')
  310    FORMAT (' '
     1        ,/,'    ENTER OUTPUT FILE NAME FOR',A10,'DATA.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
         IF (ISTAT.EQ.1) THEN
C            AN INPUT FILE IS TO BE OPENED.
C            DOWHILE (ERROR ON INPUT)              <<SEE RESTRICTIONS <<
 1000            WRITE (   *,300) TYPES(ITYPE)
                 WRITE (HIST,300) TYPES(ITYPE)
                 READ  (   *,100) FILNM
                 WRITE (HIST,  *) FILNM
                 OPEN  (IUNIT, FILE = FILNM, STATUS = 'OLD', ERR = 1025)
                 GO TO 1050
 1025            WRITE (   *,305)
                 WRITE (HIST,305)
                 GO TO 1000
 1050            CONTINUE
C            ENDWHILE
         ELSE
C            AN OUTPUT FILE IS TO BE OPENED.
C            DOWHILE (ERROR ON INPUT)              <<SEE RESTRICTIONS <<
 1075            WRITE (   *,310) TYPES(ITYPE)
                 WRITE (HIST,310) TYPES(ITYPE)
                 READ  (   *,100) FILNM
                 WRITE (HIST,  *) FILNM
                 OPEN  (IUNIT, FILE = FILNM, STATUS = 'NEW', ERR = 1100)
                 GO TO 1125
 1100            WRITE (   *,305)
                 WRITE (HIST,305)
                 GO TO 1075
 1125            CONTINUE
C            ENDWHILE
         ENDIF
         RETURN
         END
         SUBROUTINE INPPRM
     G                     (ITYPE , IUNIT )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE INPPRM
C    G                     (ITYPE , IUNIT )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO INPUT THE PRIMARY SMERFS DATA FILE.
C   DESCRIPTION
C        SINCE THIS ROUTINE IS ACCESSED BY  BOTH THE  INPDRV AND  TRNDRV
C        ROUTINES, THE DATA TYPE AND UNIT NUMBER ARE BOTH PASSED IN FROM
C        THE CALLING ROUTINE.
C
C        THE ROUTINE THEN SIMPLY ACCEPTS THE DATA  AND  SCRIPT A MESSAGE
C        INDICATING THE AMOUNT AND TYPE OF DATA THAT WAS READ.
C   ASSUMPTIONS
C        NO ERROR TESTING FOR STORAGE OVER-FLOW IS REQUIRED  BECAUSE THE
C        FILE IS SMERFS GENERATED.
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             END1   (I) = INDEX TO END OF FIRST  SET OF DATA
C             END2   (I) = INDEX TO END OF SECOND SET OF DATA
C             STR2   (I) = INDEX TO BEGINNING OF SECOND SET OF DATA
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             TYPES  (C) = DATA TYPE LABELS
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C        YIELDED
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C   ARGUMENT LIST
C             ITYPE  (I) = ARGUMENT SIMILAR IN FUNCTIONALITY TO DATTYP
C             IUNIT  (I) = UNIT NUMBER FROM WHICH DATA ARE TO BE READ
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
         INTEGER              ITYPE         ,IUNIT
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        INPUT FORMATS
  100    FORMAT (3I10)
  105    FORMAT (E15.8)
C
C        OUTPUT FORMATS
  300    FORMAT ('    THE INPUT OF',I4,A10,'ELEMENTS WAS PERFORMED.')
  305    FORMAT ('    THESE TBF DATA WERE STORED IN ',A7)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         REWIND IUNIT
C
C        ENTER THE PROCESSING TABLE FOR THE DATA SET.
         READ  (IUNIT,100) (PRC(ITYPE,I), I = 1, 3)
C
         IF     (ITYPE.EQ.1) THEN
C            WALL CLOCK TBF DATA ARE TO BE ENTERED.
             READ  (IUNIT,105) (DAT(I), I =    1,    PRC(1,1))
C
         ELSEIF (ITYPE.EQ.2) THEN
C            CPU TBF DATA ARE TO BE ENTERED.
             J = STR2 + PRC(2,1) - 1
             READ  (IUNIT,105) (DAT(I), I = STR2,    J)
C
         ELSE
C            INTERVAL DATA ARE TO BE ENTERED.
             READ  (IUNIT,105) (DAT(I), I =    1,    PRC(3,1))
             J = STR2 + PRC(3,1) - 1
             READ  (IUNIT,105) (DAT(I), I = STR2,    J)
         ENDIF
C
C        ECHO MESSAGE INDICATING THE NUMBER, TYPE, AND UNITS OF THE DATA
C        SET.
         WRITE (   *,300) PRC(ITYPE,1), TYPES(ITYPE)
         WRITE (HIST,300) PRC(ITYPE,1), TYPES(ITYPE)
         IF (ITYPE .LE. 2) THEN
             WRITE (   *,305) UNITS(PRC(ITYPE,3))
             WRITE (HIST,305) UNITS(PRC(ITYPE,3))
         ENDIF
         RETURN
         END
         SUBROUTINE INPSTR
     G                     (FFC   , NS    , VN    , ENT1  , ENT2
     B                     ,FFO   , NU    , DAT1  , DAT2
     Y                     ,ERRFLG)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE INPSTR
C    G                     (FFC   , NS    , VN    , ENT1  , ENT2
C    B                     ,FFO   , NU    , DAT1  , DAT2
C    Y                     ,ERRFLG)
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO STORE THE GIVEN DATA (WITHIN ARGUMENTS ENT1 AND ENT2) IN THE
C        DATA VECTORS DAT1 AND DAT2.
C   DESCRIPTION
C        THE DATA VECTORS (DAT1 & DAT2) MUST BOTH BE OF SIZE NS AND CON-
C        TAIN NU USED (ASSIGNED) ELEMENTS, IF BOTH VECTORS ARE TO BE UP-
C        DATED (WITH THE VALUES CONTAINED IN ARGUMENTS ENT1 & ENT2).
C
C        THE ARGUMENT VN INDICATES WHICH DATA VECTORS ARE TO BE UPDATED.
C        [THIS ARGUMENT WAS  ASSIGNED IN THE  INPDRV ROUTINE, AND PASSED
C        TO THIS ROUTINE VIA THE INPKEY ROUTINE.] A VALUE OF A ONE INDI-
C        CATES THAT DAT1 IS TO BE UPDATED  WITH THE VALUE IN ENT1, A TWO
C        INDICATES THAT DAT2 IS TO BE UPDATED WITH THE VALUE ENT2, AND A
C        THREE OR FOUR  INDICATES  BOTH VECTORS ARE TO BE  UPDATED USING
C        BOTH ENT1 AND ENT2.
C
C        THE PLACEMENT OF THE DATA IN THE DESIGNATED STORAGE AREA IS DE-
C        PENDENT ON THE VALUE OF THE  OLD FATALITY FLAG  (ARGUMENT FFO).
C        IF THE VALUE OF FFO IS A ZERO, THEN THE  NEW DATA WILL BE ADDED
C        TO THE NU-TH ELEMENT OF THE DATA VECTOR; OTHERWISE IF THE ARGU-
C        MENT HAD THE VALUE ONE, THE NEW  DATA WILL BE ADDED TO THE NEXT
C        ELEMENT (NU+1) OF THE DATA VECTOR. [RECALL THAT THE DATA VECTOR
C        HAD PREVIOUSLY BEEN INITIALIZED TO ZERO. THIS CAUSES THE ADDING
C        TO HAVE THE SAME  EFFECT AS ASSIGNING, FOR THE NU+1 CONDITION.]
C
C        PRIOR TO RETURN THE FFO ARGUMENT WILL BE UPDATED TO THE CURRENT
C        FATALITY FLAG (ARGUMENT FFC), PROVIDED THAT THE VN ARGUMENT IN-
C        DICATES THAT THE GIVEN DATA  WAS NOT OF THE INTERVAL DATA TYPE.
C        [RECALL THERE IS NO FATALITY FLAG FOR THE INTERVAL DATA.]
C
C        IF THE VALUE OF NU EVER EXCEEDS  THE VALUE OF NS, THE UPDATE IS
C        NOT PERFORMED, AN ERROR  MESSAGE IS OUTPUT, AND THE  ERROR FLAG
C        IS SET. THEN CONTROL RETURNS TO THE CALLING ROUTINE.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C        GIVEN
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C        BOTH
C             (THE VECTOR DAT AND THE ARRAY PRC WILL BE MODIFIED BY THIS
C             ROUTINE, BUT THROUGH THE ARGUMENTS DAT1, DAT2, NU, AND FFO
C             OF THE CALL LINE.)
C   ARGUMENT LIST
C        GIVEN
C             ENT1   (R) = UPDATE VALUE FOR DAT1
C             ENT2   (R) = UPDATE VALUE FOR DAT2
C             FFC    (I) = CURRENT FATALITY CONDITION FLAG  (UNUSED DUR-
C                          ING INTERVAL DATA STORAGE)
C             NS     (I) = DIMENSION OF DAT1 AND DAT2
C             VN     (I) = VECTOR NUMBER MODIFICATION FLAG
C        BOTH
C             DAT1   (R) = DATA VECTOR 1
C             DAT2   (R) = DATA VECTOR 2
C             FFO    (I) = OLD FATALITY CONDITION FLAG; RETURNS FFC
C             NU     (I) = USED (ASSIGNED) ELEMENTS IN DAT1 & DAT2
C        YIELDED
C             ERRFLG (I) = ERROR FLAG
C   LOCAL GLOSSARY
C             (NONE)
C   ERRORS
C        "THE NUMBER OF ENTRIES EXCEEDS THE MAXIMUM ARRAY SIZE FOR
C         SMERFS."
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
         INTEGER              ERRFLG        ,FFC           ,FFO
     1                       ,NS            ,NU            ,VN
         DIMENSION            DAT1(NS)      ,DAT2(NS)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('  **THE NUMBER OF ENTRIES EXCEEDS THE MAXIMUM ARRAY S'
     1          ,'IZE FOR SMERFS.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
         I = NU + 1
         IF ((FFO.EQ.1) .AND. (I.GT.NS)) THEN
C            THERE IS INADEQUATE ROOM IN THE VECTOR FOR THE NEW DATA.
             WRITE (   *,300)
             WRITE (HIST,300)
             ERRFLG = 1
         ELSE
C            UPDATE THE ARGUMENT REFLECTING THE  NUMBER OF USED ELEMENTS
C            (NU), IF THE OLD FATALITY FLAG (FFO) INDICATES THE PREVIOUS
C            ENTRY TERMINATED IN A FAILURE BEING DETECTED.
             IF (FFO.EQ.1) THEN
                 NU = NU + 1
             ENDIF
C
C            UPDATE THE FIRST  VECTOR, IF THE VN  ARGUMENT SO INDICATES.
             IF (VN.NE.2) THEN
                 DAT1(NU) = DAT1(NU) + ENT1
             ENDIF
C
C            UPDATE THE SECOND VECTOR, IF THE VN  ARGUMENT SO INDICATES.
             IF (VN.NE.1) THEN
                 DAT2(NU) = DAT2(NU) + ENT2
             ENDIF
C
C            UPDATE THE FFO ARGUMENT  WITH THE VALUE  IN THE CURRENT FA-
C            TALITY FLAG  ARGUMENT (FFC),  IF THE VN ARGUMENT  INDICATES
C            THE UPDATE IS NOT FOR INTERVAL DATA.
             IF (VN.NE.4) THEN
                 FFO = FFC
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE OUTPRM
     G                     (ITYPE , IUNIT )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE OUTPRM
C    G                     (ITYPE , IUNIT )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO OUTPUT THE PRIMARY SMERFS DATA FILE.
C   DESCRIPTION
C        SINCE THE ROUTINE IS ACCESSED BY THE INPDRV, CNVDRV, AND EDTDRV
C        ROUTINES, THE DATA TYPE AND UNIT NUMBER ARE BOTH PASSED IN FROM
C        THE CALLING ROUTINE.
C
C        THE ROUTINE THEN SIMPLY SCRIPTS THE DATA AND A MESSAGE INDICAT-
C        ING THE AMOUNT AND TYPE OF DATA THAT WAS WRITTEN.
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
C             TYPES  (C) = DATA TYPE LABELS
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C   ARGUMENT LIST
C             ITYPE  (I) = ARGUMENT SIMILAR IN FUNCTIONALITY TO DATTYP
C             IUNIT  (I) = UNIT NUMBER FROM WHICH DATA ARE TO BE READ
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
         INTEGER              ITYPE         ,IUNIT
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (3I10)
  305    FORMAT (E15.8)
  310    FORMAT ('    THE FILE STORAGE OF',I4,A10,'ELEMENTS WAS PERFORM'
     1          ,'ED.')
  315    FORMAT ('    THESE TBF DATA ARE STORED IN ',A7)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         REWIND IUNIT
C
C        PRINT THE PROCESSING TABLE FOR THE DATA SET.
         WRITE (IUNIT,300) (PRC(ITYPE,I), I = 1, 3)
C
         IF     (ITYPE.EQ.1) THEN
C            WALL CLOCK TBF DATA ARE TO BE WRITTEN.
             WRITE (IUNIT,305) (DAT(I), I =    1,    PRC(1,1))
C
         ELSEIF (ITYPE.EQ.2) THEN
C            CPU TBF DATA ARE TO BE WRITTEN.
             J = STR2 + PRC(2,1) - 1
             WRITE (IUNIT,305) (DAT(I), I = STR2,    J)
C
         ELSE
C            INTERVAL DATA ARE TO BE WRITTEN.
             WRITE (IUNIT,305) (DAT(I), I =    1,    PRC(3,1))
             J = STR2 + PRC(3,1) - 1
             WRITE (IUNIT,305) (DAT(I), I = STR2,    J)
         ENDIF
C
C        ECHO MESSAGE INDICATING THE NUMBER, TYPE, AND UNITS OF THE DATA
C        SET.
         WRITE (   *,310) PRC(ITYPE,1), TYPES(ITYPE)
         WRITE (HIST,310) PRC(ITYPE,1), TYPES(ITYPE)
         IF (ITYPE .LE. 2) THEN
             WRITE (   *,315) UNITS(PRC(ITYPE,3))
             WRITE (HIST,315) UNITS(PRC(ITYPE,3))
         ENDIF
         RETURN
         END
         SUBROUTINE PLTDRV
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE PLTDRV
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PLOT THE SPECIFIED ERROR DATA  AGAINST THE FAILURE # (OR THE
C        INTERVAL #); OR TO PROVIDE A MODEL ANALYSIS PLOT.  THE ANALYSIS
C        PLOT CAN CONSIST OF  EITHER THE  ORIGINAL AND PREDICTED DATA OR
C        THEIR RESIDUALS PLOTTED AGAINST THE FAILURE # (OR INTERVAL #).
C   DESCRIPTION
C        THE PLOTTING  IS ACTUALLY PERFORMED  WITHIN THE PLTLIN ROUTINE.
C        WHEREAS THE LINE PRINTER GRAPHS ARE LOW IN QUALITY, THEY DO RE-
C        SULT IN COMPLETE MACHINE PORTABILITY OF THIS SMERFS DRIVER.
C
C        ROUTINES HAVE BEEN  IMPLEMENTED TO ALLOW  FOR SMOOTHING  OF THE
C        RAW WALL CLOCK, CPU, AND INTERVAL FAULT COUNT DATA. THE SMOOTH-
C        ING ROUTINE CAN ALSO BE APPLIED TO RESIDUAL DATA.  NO SMOOTHING
C        IS ALLOWED FOR THE PLOT OF THE OBSERVED AND PREDICTED.
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
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PLTTYP (I) = ANALYSIS PLOT TYPE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C                          (BUT BOTH ARE RETURNED TO ORIGINAL VALUES)
C        YIELDED
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          DATA SMOOTHING
C             TM2C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, USED FOR
C                          DATA SMOOTHING
C   ARGUMENT LIST
C        (NONE)
C   LOCAL GLOSSARY
C             FLAG   (I) = OPTION FLAG
C             IDX1   (I) = DATA POINTER INDEX
C             IDX2   (I) = DATA POINTER INDEX
C             IDXE   (I) = DATA POINTER INDEX
C             LAB    (C) = PROMPT LABELS
C             LENFLG (C) = FLAG AND PAUSE FOR LENGTH PLOT
C             NS     (I) = AMOUNT OF DATA TO PLOT
C             TITL1  (C) = TITLE LINE (USER-SPECIFIED)
C             TITL2  (C) = TITLE LINE (USER-SPECIFIED)
C             UNTLAB (C) = UNIT LABEL FOR THE PLOT
C             XL     (C) = X AXIS LABEL ARRAY
C             XLC    (I) = X AXIS LABEL POINTER
C             YL     (C) = Y AXIS LABEL ARRAY
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
C               /LBLCOM/      DATA TYPES AND TBF UNITS LABELS COMMON
         COMMON /LBLCOM/      TYPES(3)      ,UNITS(7)
         CHARACTER* 7         UNITS
         CHARACTER*10         TYPES
C
C               /PLTCOM/      PLOTTING COMMON
         COMMON /PLTCOM/      MAKFIL        ,PLTTYP
         INTEGER              MAKFIL        ,PLTTYP
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
         CHARACTER* 6         YL(4)
         CHARACTER* 7         UNTLAB
         CHARACTER*10         LAB(4)        ,XL(2)
         CHARACTER*30         TITL1         ,TITL2
         INTEGER              FLAG          ,I             ,IDX1
     1                       ,IDX2          ,IDXE          ,J
     2                       ,LENFLG        ,NS            ,XLC
C
C                             LOCAL ASSIGNMENTS
         DATA                 YL(1)   /'TIME  '/
     1                       ,YL(2)   /'CPU   '/
     2                       ,YL(3)   /'FAULTS'/
     3                       ,YL(4)   /'LENGTH'/
         DATA                 LAB(1)  /'  WC  DATA'/
     1                       ,LAB(2)  /' CPU  DATA'/
     2                       ,LAB(3)  /'COUNT DATA'/
     3                       ,LAB(4)  /' RESIDUALS'/
         DATA                 XL(1)   /' FAILURE  '/
     1                       ,XL(2)   /' INTERVAL '/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        INPUT FORMATS
  100    FORMAT (A30)
C
C        OUTPUT FORMATS
  300    FORMAT ('    ENTER ONE FOR WC TBF OR TWO FOR CPU TBF.')
  305    FORMAT ('  **DATA TYPE ERROR; TRY AGAIN (AFTER THE PROMPT).')
  310    FORMAT ('    ENTER A COUNT PLOT TITLE (UP TO 30 CHARACTERS).'
     1          )
  315    FORMAT ('    ENTER A PLOT TITLE (UP TO 30 CHARACTERS).')
  320    FORMAT ('    ENTER ONE TO SMOOTH THE ',A10,'; ELSE ZERO.')
  325    FORMAT ('    ENTER ONE FOR ASSOCIATED LENGTH PLOT; ELSE ZERO.')
  330    FORMAT ('    ENTER A LENGTH PLOT TITLE (UP TO 30 CHARACTERS).')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        DETERMINE DATA TYPE TO BE PLOTTED, AND THEN DETERMINE THE STAR-
C        TING INDEX NUMBER, THE SAMPLE SIZE, AND THE X AXIS LABEL.
         IF (PLTTYP.EQ.1) THEN
C            AN ORIGINAL DATA PLOT IS DESIRED.
             IF (DATTYP.LE.2) THEN
                 I = DATTYP
             ELSEIF (DATTYP.EQ.3) THEN
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
             ELSE
                 I = 3
             ENDIF
         ELSE
C            AN ANALYSIS PLOT IS DESIRED, BASE DATA TYPE ON THE VALUE OF
C            THE GLOBAL ESTIMATION DATA TYPE FLAG.
             I = ESD
         ENDIF
         IF (I.EQ.1) THEN
             IDX1   = 1
             NS     = PRC(1,1) + (PRC(1,2) - 1)
             XLC    = 1
             UNTLAB = UNITS(PRC(1,3))
         ELSEIF (I.EQ.2) THEN
             IDX1   = STR2
             NS     = PRC(2,1) + (PRC(2,2) - 1)
             XLC    = 1
             UNTLAB = UNITS(PRC(2,3))
         ELSE
             IDX1   = 1
             IDX2   = STR2
             NS     = PRC(3,1)
             XLC    = 2
             UNTLAB = '       '
         ENDIF
C
C        INPUT THE TITLE AS INDICATED BY THE ANALYSIS PLOT FLAG. (LENGTH
C        TITLE WILL ONLY BE PROMPTED IF THE PLOT IS TO BE GENERATED.
         IF ((PLTTYP.EQ.1) .AND. (I.EQ.3)) THEN
             WRITE (   *,310)
             WRITE (HIST,310)
             READ  (   *,100) TITL1
             WRITE (HIST,  *) TITL1
         ELSE
             WRITE (   *,315)
             WRITE (HIST,315)
             READ  (   *,100) TITL1
             WRITE (HIST,  *) TITL1
         ENDIF
C
         IF (PLTTYP.EQ.1) THEN
C            THE ORIGINAL  DATA PLOT  IS DESIRED,  DETERMINE IF THE USER
C            DESIRES SMOOTHING OF THE DATA  (PROVIDED ADEQUATE ENTRIES).
             IF (NS.GT.6) THEN
                 WRITE (   *,320) LAB(I)
                 WRITE (HIST,320) LAB(I)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
                     IDXE = IDX1 + NS - 1
                     REWIND TMPB
                     WRITE (TMPB) (DAT(J), J = IDX1, IDXE)
                     CALL SMTDRV
     G                           (NS
     B                           ,DAT(IDX1)
     Y                           ,TM1C     , TM2C     )
                 ENDIF
             ENDIF
C
             IF ((I.EQ.1) .OR. (I.EQ.2)) THEN
C                EITHER A WC OR CPU PLOT IS DESIRED; TM2C IS NOT USED.
                 CALL PLTLIN
     G                       (1        , NS       , DAT(IDX1),TM2C
     G                       ,TITL1    , XL(XLC)  , YL(I)    ,UNTLAB
     G                       ,1        )
             ELSE
C                INTERVAL DATA ARE TO BE PLOTTED.
                 CALL PLTLIN
     G                       (1        , NS       , DAT(IDX1),TM2C
     G                       ,TITL1    , XL(XLC)  , YL(3)    ,UNTLAB
     G                       ,1        )
C
                 WRITE (   *,325)
                 WRITE (HIST,325)
                 READ  (   *,  *) LENFLG
                 WRITE (HIST,  *) LENFLG
                 IF (LENFLG.EQ.1) THEN
                     WRITE (   *,330)
                     WRITE (HIST,330)
                     READ  (   *,100) TITL2
                     WRITE (HIST,  *) TITL2
                     CALL PLTLIN
     G                           (1        , NS       , DAT(IDX2),TM2C
     G                           ,TITL2    , XL(XLC)  , YL(4)    ,UNTLAB
     G                           ,1        )
                 ENDIF
             ENDIF
             IF (FLAG.EQ.1) THEN
                 REWIND TMPB
                 READ  (TMPB) (DAT(J), J = IDX1, IDXE)
             ENDIF
         ELSEIF (PLTTYP.EQ.2) THEN
C            A RESIDUAL PLOT IS DESIRED; CALCULATE THE RESIDUALS AND DE-
C            TERMINE IF SMOOTHING IS DESIRED.
             REWIND TMPB
             WRITE (TMPB) (PRD(J), J = 1   , NS  )
             DO 1050 J = 1, NS
                 PRD(J) = DAT(IDX1) - PRD(J)
                 IDX1   = IDX1 + 1
 1050            CONTINUE
C            ENDDO
             IF (NS.GT.6) THEN
                 WRITE (   *,320) LAB(4)
                 WRITE (HIST,320) LAB(4)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
                 IF (FLAG.EQ.1) THEN
                     CALL SMTDRV
     G                           (NS
     B                           ,PRD
     Y                           ,TM1C     , TM2C     )
                 ENDIF
             ENDIF
C
             CALL PLTLIN
     G                   (1        , NS       , PRD      ,TM2C
     G                   ,TITL1    , XL(XLC)  , YL(I)    ,UNTLAB
     G                   ,1        )
             REWIND TMPB
             READ  (TMPB) (PRD(J), J = 1   , NS  )
         ELSE
C            THE ORIGINAL AND PREDICTED DATA PLOT IS DESIRED.
             CALL PLTLIN
     G                   (2        , NS       , DAT(IDX1),PRD
     G                   ,TITL1    , XL(XLC)  , YL(I)    ,UNTLAB
     G                   ,1        )
         ENDIF
         RETURN
         END
         SUBROUTINE PLTEXT
     G                     (TITLE )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE PLTEXT
C                          (TITLE )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PLACE  (1) RAW DATA,  (2) SMOOTHED RAW DATA,  (3) PREDICTION
C        DATA, (4) RESIDUAL DATA, (5) SMOOTHED RESIDUAL DATA,  AND (6) A
C        GOODNESS-OF-FIT STATISTIC ON THE SMERFS PLOT FILE.
C   DESCRIPTION
C        THE FIRST ACTION IS TO COMPUTE THE  CHI-SQUARE  GOODNESS-OF-FIT
C        STATISTIC IF INTERVAL DATA ARE PRESENT.  THE KOLMOGOROV STATIS-
C        TIC FOR THE TBF DATA TYPES IS ALREADY COMPUTED AND REQUIRES ON-
C        LY TO BE PRINTED OUT. FOR THE MESSAGES, ALTHOUGH A FIXED LENGTH
C        OF TWO LINES,  MAY CONTAIN THE FOLLOWING DIFFERENT TYPES OF IN-
C        FORMATION:
C
C        IF EXECUTION MODEL ANALYSES ARE DONE
C            THE CALCULATED KOLMOGOROV STATISTIC
C            IF THE COMPUTED DISTANCE IS LESS THAN THE 0.05 SIG. VALUE
C                THE MODEL MAY PROVIDE AN ADEQUATE FIT MESSAGE
C            ELSE
C                THE MODEL MAY NOT PROVIDE AN ADEQUATE FIT MESSAGE
C            ENDIF
C        ELSE (INTERVAL MODEL ANALYSES ARE DONE)
C            IF SUCCESSFUL (DEGREES-OF-FREEDOM IS .GE. ONE)
C                THE CHI-SQUARE STATISTIC AND THE DEGREES-OF-FREEDOM
C                IF THE CELL COMBINATION NUMBER (5) WAS NOT REACHED FOR/
C                                                          THE LAST CELL
C                    THE LAST CELL'S FREQUENCY
C                ELSE (THE CELL COMBINATION NUMBER WAS REACHED)
C                    A BLANK LINE
C                ENDIF
C            ELSE (DEGREES-OF-FREEDOM IS LESS THAN ONE)
C                AN UNAVAILABILITY MESSAGE
C                A BLANK LINE
C            ENDIF
C        ENDIF
C
C        USERS OF THE PLOT FILE MUST DETERMINE WHICH  INFORMATION SHOULD
C        BE WRITTEN TO THE FINAL PLOT.
C
C        THE ROUTINE SIMPLY OUTPUTS  VARIOUS TITLES, LABELS, AND DATA TO
C        ALLOW THE USER TO HAVE SMERFS DATA FOR SUBSEQUENT PLOTTING. THE
C        ROUTINE DOES NOT PROMPT FOR ANY INFORMATION;  RATHER THE SINGLE
C        TITLE ARGUMENT IS USED FOR UNIQUE IDENTIFICATION.  THE INDIVID-
C        UAL TITLES AND LABELS ARE SIMPLE PROVIDED AS QUIDELINES FOR THE
C        USE OF THE DATA FILE.
C
C        AT TIMES, THE SMOOTHING ROUTINE  IS ACCESS, BUT THE DATA ARRAYS
C        ARE ALWAYS RETURNED TO THEIR ORIGINAL VALUES.
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
C             D      (R) = CALCULATED KOLMOGOROV DISTANCE
C             DFLG   (I) = FLAG INDICATING THE SIGNIFICANCE  OF THE CAL-
C                          CULATED DISTANCE AT THE 0.05 LEVEL
C             ESD    (I) = ESTIMATION SELECTION DATA TO INDICATE DATA
C             EST    (I) = NUMBER OF ESTIMATIONS FROM THE MODEL
C             PLOT   (I) = UNIT NUMBER FOR THE PLOT FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             PRD    (R) = ARRAY TO HOLD PREDICTED DATA OF SIZE END1
C                          (BUT BOTH ARE RETURNED TO ORIGINAL VALUES)
C        YIELDED
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1
C             TM2C   (R) = TEMPORARY STORAGE AREA OF SIZE END1
C   ARGUMENT LIST
C             TITLE  (C) = TITLE FOR THE OPTIONAL PLOT FILE ENTRY
C   LOCAL GLOSSARY
C             CCN    (R) = CELL COMBINATION FREQUENCY NUMBER
C             CHI    (R) = CHI-SQUARE STATISTIC
C             DOF    (I) = DEGREES-OF-FREEDOM
C             IDX    (I) = DATA POINTER INDEX
C             IDXE   (I) = DATA POINTER INDEX
C             LIF    (R) = LAST INTERVAL'S FREQUENCY
C             NS     (I) = AMOUNT OF DATA TO PLOT
C             RES    (R) = RESIDUAL OF THE I-TH ELEMENTS
C             RFLAG  (I) = RETURN STATUS FLAG
C             XLAB   (C) = X-AXIS LABELS
C             YLAB   (C) = Y-AXIS LABELS
C   ERRORS
C        RFLAG  = 1 : CCN COULD NOT BE ACHIEVED FOR THE LAST INTERVAL
C        RFLAG  = 2 : THE CALCULATED DOF WAS LESS THAN ONE
C                   --FOR CONSISTENCY TO THE FITGOF ROUTINE, THIS INFOR-
C                     MATION IS SUPPLIED; BUT THE VALUES ONLY CHANGE THE
C                     MESSAGE ON THE PLOT FILE .
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
C               /LBLCOM/      DATA TYPES AND TBF UNITS LABELS COMMON
         COMMON /LBLCOM/      TYPES(3)      ,UNITS(7)
         CHARACTER* 7         UNITS
         CHARACTER*10         TYPES
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
         CHARACTER*(*)        TITLE
C
C                             LOCAL SPECIFICATIONS
         CHARACTER*15         XLAB(3)       ,YLAB(4)
         INTEGER              DOF           ,I             ,IDX
     1                       ,IDXE          ,J             ,NS
     2                       ,RFLAG
C
C                             LOCAL ASSIGNMENTS
         DATA                 XLAB(1) /'FAILURE NUMBER '/
     1                       ,XLAB(2) /'FAILURE NUMBER '/
     2                       ,XLAB(3) /'INTERVAL NUMBER'/
         DATA                 YLAB(1) /'WC  EXPENDITURE'/
     1                       ,YLAB(2) /'CPU EXPENDITURE'/
     2                       ,YLAB(3) /'FAULT COUNT    '/
     3                       ,YLAB(4) /'TESTING LENGTH '/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (1X,A40,5X,I5,3X,A7)
  305    FORMAT (1X,A15,5X,A15,5X,A15)
  306    FORMAT (' THE CALCULATED KOLMOGOROV DISTANCE IS',E15.8)
  307    FORMAT (' THE MODEL MAY NOT PROVIDE AN ADEQUATE FIT AT THE 0.0'
     1          ,'5 LEVEL')
  308    FORMAT (' THE MODEL MAY PROVIDE AN ADEQUATE FIT AT THE 0.05 LE'
     1          ,'VEL')
  310    FORMAT (' CHI-SQUARE STATISTIC NOT AVAILABLE')
  312    FORMAT (' ')
  315    FORMAT (' CHI-SQUARE STATISTIC IS',E15.8,' WITH',I6,' DEGREES-'
     1          ,'OF-FREEDOM')
  320    FORMAT (' THE GREATEST VALUE FOR THE LAST CELL WAS', E15.8)
  325    FORMAT (' VALUES FOR RAW DATA PLOT')
  330    FORMAT (1X,E15.8)
  331    FORMAT (1X,2E15.8)
  335    FORMAT (' VALUES FOR SMOOTHED RAW DATA PLOT')
  340    FORMAT (' *** SAMPLE SIZE TOO SMALL TO SMOOTH ***')
  345    FORMAT (' VALUES FOR PREDICTED DATA PLOT')
  350    FORMAT (' VALUES FOR RESIDUAL DATA PLOT')
  355    FORMAT (' VALUES FOR SMOOTHED RESIDUAL DATA PLOT')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        DETERMINE STARTING INDEX, SAMPLE SIZE, AND ENDING INDEX.
         IF (ESD.EQ.1) THEN
             IDX  = 1
             NS   = PRC(1,1) + (PRC(1,2) - 1)
             IDXE = NS
         ELSEIF (ESD.EQ.2) THEN
             IDX  = STR2
             NS   = PRC(2,1) + (PRC(2,2) - 1)
             IDXE = IDX      +  NS       - 1
         ELSE
             IDX  = 1
             NS   = PRC(3,1)
             IDXE = NS
         ENDIF
C
C        OUTPUT THE MAJOR TITLE LINE FOR THE PLOT,  THE SAMPLE SIZE, THE
C        UNITS OF THE TBF DATA, AND THE AXIS LABELS FOR THE PLOTS.
         IF (ESD.LE.2) THEN
             WRITE (PLOT,300) TITLE    , NS       , UNITS(PRC(ESD,3))
             WRITE (PLOT,305) XLAB(ESD), YLAB(ESD)
         ELSE
             WRITE (PLOT,300) TITLE    , NS
             WRITE (PLOT,305) XLAB(ESD), YLAB(ESD), YLAB(4)
         ENDIF
C
         IF ((ESD.EQ.1) .OR. (ESD.EQ.2)) THEN
C            SCRIPT THE PREVIOUSLY COMPUTED KOLMOGOROV STATISTIC AND THE
C            INTERPRETATION MESSAGE.
             WRITE (PLOT,306) D
             IF     (DFLG.EQ.1) THEN
                 WRITE (PLOT,307)
             ELSEIF (DFLG.EQ.2) THEN
                 WRITE (PLOT,308)
             ENDIF
         ELSE
C            OBTAIN THE  CHI-SQUARE  GOODNESS-OF-FIT STATISTIC  WITH THE
C            STANDARD CELL COMBINATION FREQUENCY OF FIVE, IF POSSIBLE.
             CCN = 5
             CALL SMFGOF
     G                   (DAT   , PRD   , NS    , EST   , CCN
     Y                   ,CHI   , DOF   , LIF   , RFLAG )
             IF (RFLAG.EQ.2) THEN
C                THE CHI-SQUARE STATISTIC COULD NOT BE COMPUTED.
                 WRITE (PLOT,310)
                 WRITE (PLOT,312)
             ELSE
                 WRITE (PLOT,315) CHI, DOF
                 IF (RFLAG.EQ.1) THEN
                     WRITE (PLOT,320) LIF
                 ELSE
                     WRITE (PLOT,312)
                 ENDIF
             ENDIF
         ENDIF
C
C        OUTPUT SUB-HEADER,  AND TIME EXPENDITURES  OR  FAULT COUNTS AND
C        TESTING LENGTHS FOR A PLOT OF THE RAW DATA.
         WRITE (PLOT,325)
         IF (ESD.LE.2) THEN
             WRITE (PLOT,330) (DAT(I), I = IDX, IDXE)
         ELSE
             WRITE (PLOT,331) (DAT(I), DAT(I+END1), I = IDX, IDXE)
         ENDIF
C
         IF (NS.GT.6) THEN
C            STORE THE RAW DATA, AND THEN SMOOTH THE DATA.
             REWIND TMPB
             WRITE (TMPB) (DAT(I), I = IDX , IDXE)
             CALL SMTDRV
     G                   (NS
     B                   ,DAT(IDX )
     Y                   ,TM1C     , TM2C     )
C
C            OUTPUT SUB-HEADER, AND TIME EXPENDITURES OR FAULT COUNTS OF
C            THE PLOT OF THE SMOOTHED RAW DATA.
             WRITE (PLOT,335)
             WRITE (PLOT,330) (DAT(I), I = IDX, IDXE)
C
C            RESTORE THE RAW DATA TO ITS ORIGINAL FORM.
             REWIND TMPB
             READ  (TMPB) (DAT(I), I = IDX , IDXE)
         ELSE
C            OUTPUT MESSAGE THAT SAMPLE SIZE IS TOO SMALL FOR SMOOTHING.
             WRITE (PLOT,340)
         ENDIF
C
C        OUTPUT SUB-HEADER,  AND TIME EXPENDITURES OR FAULT COUNTS FOR A
C        PLOT OF THE PREDICTED  DATA.
         WRITE (PLOT,345)
         WRITE (PLOT,330) (PRD(I), I = 1, NS)
C
C        STORE THE PREDICTED DATA, AND THEN COMPUTE THE RESIDUALS.
         REWIND TMPB
         WRITE (TMPB) (PRD(I), I = 1, NS)
         J = IDX
         DO 1000 I = 1, NS
             PRD(I) = DAT(J) - PRD(I)
             J      = J      + 1
 1000        CONTINUE
C        ENDDO
C
C        OUTPUT SUB-HEADER,  AND TIME EXPENDITURES OR FAULT COUNTS FOR A
C        PLOT OF THE RESIDUAL DATA.
         WRITE (PLOT,350)
         WRITE (PLOT,330) (PRD(I), I = 1, NS)
C
         IF (NS.GT.6) THEN
C            SMOOTH THE RESIDUAL DATA.
             CALL SMTDRV
     G                   (NS
     B                   ,PRD
     Y                   ,TM1C     , TM2C     )
C
C            OUTPUT SUB-HEADER, AND TIME EXPENDITURES OR FAULT COUNTS OF
C            THE PLOT OF THE SMOOTHED RESIDUAL DATA.
             WRITE (PLOT,355)
             WRITE (PLOT,330) (PRD(I), I = 1, NS)
         ELSE
C            OUTPUT MESSAGE THAT SAMPLE SIZE IS TOO SMALL FOR SMOOTHING.
             WRITE (PLOT,340)
         ENDIF
C
C        RESTORE THE PREDICTION DATA TO ITS ORIGINAL FORM.
         REWIND TMPB
         READ  (TMPB) (PRD(I), I = 1, NS)
         RETURN
         END
         SUBROUTINE PLTLIN
     G                     (NPLT  , NS    , DATY  , DATZ  , TITLE
     G                     ,XL    , YL    , UNTLAB, HDRFLG)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE PLTLIN
C    G                     (NPLT  , NS    , DATY  , DATZ  , TITLE
C    G                     ,XL    , YL    , UNTLAB, HDRFLG)
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PRODUCE LOWER QUALITY GRAPHS OF THE REQUESTED DATA.
C   DESCRIPTION
C        THE ROUTINE IS ACTUALLY A VERY SPECIALIZED PLOTTING PACKAGE. IT
C        MAKES CERTAIN  ASSUMPTIONS ABOUT THE DATA.  FIRST BECAUSE THE Y
C        AXIS IS ALWAYS THE FAILURE NUMBER OR FAULT COUNT,  NON-POSITIVE
C        NUMBERS ARE NOT POSSIBLE AND THE NUMBERS MUST BE IN NUMERICALLY
C        ASCENDING ORDER (WITHOUT REPEATS). THE ROUTINE ADDITIONALLY AS-
C        SUMES THAT ONLY ONE X AXIS VALUE  WILL EQUATE TO A GIVEN Y AXIS
C        VALUE.
C
C        TITLE AND AXIS LABELS ARE ENTERED THROUGH THE CALL LINE.
C
C        FOR PLOTS WITH OVER FIFTY ELEMENTS, MULTIPLE PLOTS WILL BE GEN-
C        ERATED.  FOR BETTER COMPARISONS AND UNDERSTANDING OF THE PLOTS,
C        A SINGLE MAXIMUM/MINIMUM WILL BE USED FOR ALL PLOTS.
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
C             DATY   (R) = Y AXIS DATA VECTOR
C             DATZ   (R) = SECOND Y AXIS DATA VECTOR (OPTIONAL)
C             HDRFLG (I) = FLAG TO PRINT PLOT  TITLE, XL, AND PAUSE LINE
C                          1 = PRINT TITLE AND AXIS, NO PAUSE
C                          0 = PRINT PAUSE, NO TITLE OR AXIS
C             NPLT   (I) = NUMBER OF PLOTTED LINES
C             NS     (I) = SIZE OF X, Y, AND Z ARRAYS
C             TITLE  (C) = 30 CHARACTER TITLE
C             UNTLAB (C) = UNIT LABEL FOR THE PLOT
C             XL     (C) = X AXIS LABEL
C             YL     (C) = Y AXIS LABEL
C   LOCAL GLOSSARY
C             IWT    (I) = SCREEN PAUSE FLAG
C             LABELS (C) = THE FOUR POSSIBLE VALUES FOR AN "ELEMENT"
C             OPGRAF (C) = ARRAY TO HOLD THE "GRAPH" IN STORAGE
C             PLTE   (I) = ENDING DATA POINT TO BE PLOTTED
C             PLTS   (I) = STARTING DATA POINT TO BE PLOTTED
C             XSC    (I) = X AXIS SCALE VALUE FOR THE TIC MARKS
C             YMAX   (R) = Y (AND Z) AXIS MAXIMUM
C             YMIN   (R) = Y (AND Z) AXIS MINIMUM
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
         INTEGER              HDRFLG        ,NPLT          ,NS
         DIMENSION            DATY(NS)      ,DATZ(NS)
         CHARACTER*(*)        TITLE         ,UNTLAB        ,XL
     1                       ,YL
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,INDX          ,IWT
     1                       ,J             ,PLTE          ,PLTS
     2                       ,XSC(6)
         CHARACTER* 1         LABELS(4)     ,OPGRAF(15,50)
C
C                             LOCAL ASSIGNMENTS
         DATA                 LABELS(1) /' '/   ,  LABELS(2) /'*'/
     1                       ,LABELS(3) /'P'/   ,  LABELS(4) /'$'/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,28X,A30
     2        ,/)
  305    FORMAT (' ')
  310    FORMAT ( 1X,E15.8 ,' !',50A1)
  315    FORMAT (16X       ,' !',50A1)
  320    FORMAT ( 4X,A6,6X ,' !',50A1)
  325    FORMAT ( 4X,A7,5X ,' !',50A1)
  330    FORMAT (17X,'+---------+---------+---------+---------+--------'
     1          ,'-+'
     2        ,/, 8X,6I10)
  335    FORMAT (' '
     1        ,/,38X,A10)
  340    FORMAT (' '
     1        ,/,'    ENTER ONE TO VIEW THE NEXT SECTION; ELSE ZERO TO '
     2          ,'EXIT PLOTTER.')
  345    FORMAT (' '
     1        ,/,'    ENTER ONE TO CONTINUE THE PROCESSING.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        DETERMINE THE MINIMUM AND MAXIMUM Y AXIS VALUES.  THIS INCLUDES
C        THE OPTIONAL Z VALUES IF  NPLT IS ENTERED AS A VALUE OF TWO.
         YMIN = DATY(1)
         YMAX = DATY(1)
         DO 1000 I = 1, NS
             YMIN = MIN(YMIN, DATY(I))
             YMAX = MAX(YMAX, DATY(I))
             IF (NPLT.EQ.2) THEN
                 YMIN = MIN(YMIN, DATZ(I))
                 YMAX = MAX(YMAX, DATZ(I))
             ENDIF
 1000        CONTINUE
C        ENDDO
C
C        ESTABLISH A RANGE IF ALL THE VALUES ARE EQUAL.
         IF (YMIN.EQ.YMAX) THEN
             YMIN = YMIN - 1.0
             YMAX = YMAX + 1.0
         ENDIF
C
C        RE-SET THE MINIMUM TO THE NEXT LOWER WHOLE INTEGER.
         IF (YMIN .LT. 0.0) THEN
             IF (YMIN .NE. REAL(INT(YMIN))) THEN
                 YMIN = REAL(INT(YMIN)) - 1.0
             ENDIF
         ELSE
             YMIN = REAL(INT(YMIN))
         ENDIF
C
C        RE-SET THE MAXIMUM TO THE NEXT HIGHER WHOLE INTEGER.
         IF (YMAX .GT. 0.0) THEN
             IF (YMAX .NE. REAL(INT(YMAX))) THEN
                 YMAX = REAL(INT(YMAX)) + 1.0
             ENDIF
         ELSE
             YMAX = REAL(INT(YMAX))
         ENDIF
C
C        INITIALIZE THE STARTING POINT FOR THE PLOT.
         PLTS = -49
C
C        DOUNTIL (ALL DATA HAS BEEN PLOTTED)
C            DEFINE THE CURRENT SECTION LIMITS AND AXIS TIC VALUES.
 1025        PLTS   = PLTS   + 50
             PLTE   = MIN((PLTS+49), NS)
             XSC(1) = PLTS   -  1
             XSC(2) = XSC(1) + 10
             XSC(3) = XSC(2) + 10
             XSC(4) = XSC(3) + 10
             XSC(5) = XSC(4) + 10
             XSC(6) = XSC(5) + 10
C
C            INITIALIZE THE OUTPUT GRAPH "ELEMENTS" WITH BLANKS.
             DO 1075 I = 1, 15
                 DO 1050 J = 1, 50
                     OPGRAF(I,J) = LABELS(1)
 1050                CONTINUE
C                ENDDO
 1075            CONTINUE
C            ENDDO
C
C            FILL THE OPGRAF ARRAY WITH THE POINTS OF INTERSECTION.
             J = 0
             DO 1100 I = PLTS, PLTE
                 J    = J + 1
                 INDX = (DATY(I)-YMIN) / (YMAX-YMIN) * 14.0 + 1.0
                 OPGRAF(INDX,J) = LABELS(2)
                 IF (NPLT.EQ.2) THEN
                     INDX = (DATZ(I)-YMIN) / (YMAX-YMIN)  * 14.0 + 1.0
                     IF (OPGRAF(INDX,J).EQ.' ') THEN
                         OPGRAF(INDX,J) = LABELS(3)
                     ELSE
                         OPGRAF(INDX,J) = LABELS(4)
                     ENDIF
                 ENDIF
 1100            CONTINUE
C            ENDDO
C
C            OUTPUT THE PLOT HEADER.
             IF (HDRFLG.EQ.1) THEN
                 WRITE (   *,300) TITLE
                 WRITE (HIST,300) TITLE
             ELSE
                 WRITE (   *,305)
                 WRITE (HIST,305)
             ENDIF
C
C            OUTPUT THE FIRST LINE OF THE GRAPH, WITH MAXIMUM VALUE.
             WRITE (   *,310)     YMAX  , (OPGRAF(15,I), I = 1, 50)
             WRITE (HIST,310)     YMAX  , (OPGRAF(15,I), I = 1, 50)
C
C            OUTPUT THE NEXT SIX LINES OF THE GRAPH.
             DO 1125 J = 14, 9, -1
                 WRITE (   *,315)         (OPGRAF( J,I), I = 1, 50)
                 WRITE (HIST,315)         (OPGRAF( J,I), I = 1, 50)
 1125            CONTINUE
C            ENDDO
C
C            OUTPUT THE SEVENTH LINE OF THE GRAPH, WITH AXIS LABEL.
             WRITE (   *,320)     YL    , (OPGRAF( 8,I), I = 1, 50)
             WRITE (HIST,320)     YL    , (OPGRAF( 8,I), I = 1, 50)
C
C            OUTPUT THE EIGHTH LINE OF THE GRAPH, WITH UNITS LABEL.
             WRITE (   *,325)     UNTLAB, (OPGRAF( 7,I), I = 1, 50)
             WRITE (HIST,325)     UNTLAB, (OPGRAF( 7,I), I = 1, 50)
C
C            OUTPUT THE NEXT FIVE LINES OF THE GRAPH.
             DO 1150 J = 6, 2, -1
                 WRITE (   *,315)         (OPGRAF( J,I), I = 1, 50)
                 WRITE (HIST,315)         (OPGRAF( J,I), I = 1, 50)
 1150            CONTINUE
C            ENDDO
C
C            OUTPUT THE LAST LINE OF THE GRAPH, WITH MINIMUM VALUE.
             WRITE (   *,310)     YMIN  , (OPGRAF( 1,I), I = 1, 50)
             WRITE (HIST,310)     YMIN  , (OPGRAF( 1,I), I = 1, 50)
C
C            OUTPUT THE GRID LINE.
             WRITE (   *,330)     XSC
             WRITE (HIST,330)     XSC
C
C            OUTPUT THE X-AXIS LABEL.
             IF (HDRFLG.EQ.1) THEN
                 WRITE (   *,335) XL
                 WRITE (HIST,335) XL
             ELSE
                 WRITE (   *,305)
                 WRITE (HIST,305)
             ENDIF
C
             IWT = -1
             IF (PLTE.NE.NS) THEN
C                PROVIDE A PAUSE BETWEEN THE SECTIONS.
                 WRITE (   *,340)
                 WRITE (HIST,340)
                 READ  (   *,  *) IWT
                 WRITE (HIST,  *) IWT
             ENDIF
             IF ((IWT.EQ.1) .AND. (PLTE.NE.NS)) GO TO 1025
C        ENDUNTIL
C
         IF ((HDRFLG.EQ.0) .AND. (IWT.EQ.-1)) THEN
C            PROVIDE A PAUSE BEFORE RETURNING TO THE ACCESSING ROUTINE.
             WRITE (   *,345)
             WRITE (HIST,345)
             READ  (   *,  *) IWT
             WRITE (HIST,  *) IWT
         ENDIF
         RETURN
         END
         SUBROUTINE PLTSCT
     G                     (NS    , DATY  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE PLTSCT
C    G                     (NS    , DATY  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PLOT THE SPECIFIED ACCURACY OR BIAS DATA POINTS  AGAINST THE
C        ITERATION NUMBER IN NORMAL SCATTER PLOTS.
C   DESCRIPTION
C        THE PLOTTING  IS ACTUALLY PERFORMED  WITHIN THE PLTLIN ROUTINE.
C        WHEREAS THE LINE PRINTER GRAPHS ARE LOW IN QUALITY, THEY DO RE-
C        SULT IN COMPLETE MACHINE PORTABILITY OF THIS SMERFS DRIVER.
C
C        THIS ROUTINE EXISTS SOLELY BECAUSE THE PROCESSING OCCURES WITH-
C        IN THE MAAIAC, MAATAC, AND MAATBS ROUTINES.  TO ACCESS THE PLT-
C        LIN ROUTINE DIRECTLY WOULD MEAN THAT THE CHARACTER LABELS WOULD
C        APPEAR IN EACH OF THEM.
C   ASSUMPTIONS
C        (NONE)
C   RESTRICTIONS
C        (NONE)
C   PARAMETER GLOSSARY
C             (NONE)
C   GLOBAL GLOSSARY
C        GIVEN
C             TM1C   (R) = TEMPORARY STORAGE AREA OF SIZE END1, FOR FILL
C                          IN PLTLIN CALL LINE ONLY
C   ARGUMENT LIST
C        GIVEN
C             DATY   (R) = Y AXIS (ACCURACY OR UNSORTED BIAS) DATA
C             NS     (I) = SIZE OF THE ARRAY
C   LOCAL GLOSSARY
C             TITLE  (C) = TITLE LINE   (PLACE HOLDER ONLY)
C             UNTLAB (C) = UNIT LABEL   (PLACE HOLDER ONLY)
C             XL     (C) = X AXIS LABEL (PLACE HOLDER ONLY)
C             YL     (C) = Y AXIS LABEL (PLACE HOLDER ONLY)
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
C
C                             GLOBAL SPECIFICATIONS
C
C               /TMPCOM/      TEMPORARY STORAGE COMMON
         COMMON /TMPCOM/      TM1C(END1)    ,TM2C(END1)
C
C                             ARGUMENT SPECIFICATIONS
         INTEGER              NS
         DIMENSION            DATY(NS)
C
C                             LOCAL SPECIFICATIONS
         CHARACTER* 6         YL
         CHARACTER* 7         UNTLAB
         CHARACTER*10         XL
         CHARACTER*30         TITLE
C
C                             LOCAL ASSIGNMENTS
         DATA                 YL      /'      '/
         DATA                 UNTLAB  /'       '/
         DATA                 XL      /'          '/
         DATA                 TITLE   /'                              '/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         CALL PLTLIN
     G               (1     , NS    , DATY  , TM1C  , TITLE
     G               ,XL    , YL    , UNTLAB, 0     )
         RETURN
         END
         SUBROUTINE PLTUAY
     G                     (NS    , DATY  , UORY  )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE PLTUAY
C    G                     (NS    , DATY  , UORY  )
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PRODUCE LOWER QUALITY GRAPHS OF THE BIAS AND TREND DATA.
C   DESCRIPTION
C        THE ROUTINE IS ACTUALLY A VERY SPECIALIZED PLOTTING PACKAGE. IT
C        MAKES CERTAIN  ASSUMPTIONS ABOUT THE DATA.  BECAUSE THE X AND Y
C        AXES ARE ALWAYS REAL VALUES BETWEEN 0.0 AND 1.0, NO COMPLICATED
C        PRE-SCANNING OR ROUNDING OF THE DATA ARE REQUIRED.
C
C        THE ONLY POSSIBLE  CONFUSING AREAS  OF THIS ROUTINE ARE THE DE-
C        TERMINATION OF THE STEP AND THE SCRIPTING OF THE 0,0-1,1 LINE.
C
C        SINCE THE ROUTINE IS DESIGNED TO OPERATE ON A STANDARD TERMINAL
C        (RATHER THAN A HIGH RESOLUTION GRAPHICS TERMINAL  WITH PLOTTING
C        SOFTWARE), 14 ROWS AND 60 COLUMNS ARE ALL THAT CAN BE SHOWN. TO
C        PROVIDE A MEANINGFUL PLOT OF DATA SETS OF OVER 60 POINTS, A RE-
C        DUCTION IN POINTS IS DONE BY STEPPING THROUGH THE DATA. FOR EX-
C        AMPLE, IF 120 POINTS WERE TO BE PLOTTED, POINTS 1, 3, 5, .. ARE
C        PLOTTED.
C
C        IN ORDER TO RETAIN A FULL SCREEN FOR THE 0,0-1,1 LINE, IN ADDI-
C        TION TO STEPPING THROUGH THE DATA,  THE ROUTINE ALSO SKIPS COL-
C        UMNS. FOR EXAMPLE, IF 61 POINTS WERE TO BE PLOTTED, THE STEP IS
C        SET TO 2  (EVERY SECOND POINT).  BUT THAT ACCOUNTS  FOR ONLY 30
C        COLUMNS, SO EVERY OTHER COLUMN IS SKIPPED.
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
C             DATY   (R) = Y AXIS (U-PLOT OR Y-PLOT) DATA VECTOR
C             NS     (I) = SIZE OF U OR Y ARRAYS
C             UORY   (I) = INDEX TO PLOT CHARACTER (1=U-PLOT, 2=Y-PLOT)
C   LOCAL GLOSSARY
C             LABELS (C) = THE FIVE POSSIBLE VALUES FOR AN "ELEMENT"
C             OPGRAF (C) = ARRAY TO HOLD THE "GRAPH" IN STORAGE
C             STEP   (I) = DATA STEP, SO MORE THAN 60 CAN BE PLOTTED
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
         INTEGER              NS            ,UORY
         DIMENSION            DATY(NS)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,IWT           ,J
     1                       ,K             ,L             ,STEP
         CHARACTER* 1         LABELS(5)     ,OPGRAF(15,60)
C
C                             LOCAL ASSIGNMENTS
         DATA                 LABELS(1) /' '/   ,  LABELS(2) /'*'/
     1                       ,LABELS(3) /'U'/   ,  LABELS(4) /'Y'/
     2                       ,LABELS(5) /'$'/
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   1 !',60A1)
  305    FORMAT ('     !',60A1)
  310    FORMAT ('     !',60A1
     1        ,/,'   0 +---------+---------+---------+---------+-------'
     2          ,'--+---------+'
     3        ,/,'     0                   1/NS WITH A STEP OF',I2,'   '
     4          ,'                1')
  315    FORMAT (' '
     1        ,/,'    ENTER ONE TO CONTINUE THE PROCESSING.')
  320    FORMAT (' ')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        INITIALIZE THE OUTPUT GRAPH "ELEMENTS" WITH BLANKS.
         DO 1025 I = 1, 15
             DO 1000 J = 1, 60
                 OPGRAF(I,J) = LABELS(1)
 1000            CONTINUE
C            ENDDO
 1025        CONTINUE
C        ENDDO
C
C        DETERMINE THE STEP OF THE DATA  WHEN MORE THAN SIXTY POINTS ARE
C        ATTEMPTED TO BE REPRESENTED.
         STEP = 0
C        DOUNTIL (NS/STEP .LE. 60)
 1050        STEP = STEP + 1
             IF (NS.GT.STEP*60) GO TO 1050
C        ENDUNTIL
C
C        FILL THE OPGRAF ARRAY WITH THE POINTS OF INTERSECTION.
         DO 1075 I = 1, NS, STEP
             J = REAL(I) / REAL(NS) * 14.0 + 1.0
             K = REAL(I) / REAL(NS) * 59.0 + 1.0
             L = DATY(I)            * 14.0 + 1.0
             OPGRAF(J,K) = LABELS(2)
             IF (OPGRAF(L,K).EQ.LABELS(1)) THEN
                 OPGRAF(L,K) = LABELS(UORY+2)
             ELSE
                 OPGRAF(L,K) = LABELS(5)
             ENDIF
 1075        CONTINUE
C        ENDDO
C
C        OUTPUT THE FIRST LINE OF THE GRAPH, WITH MAXIMUM VALUE.
         WRITE (   *,300)     (OPGRAF(15,I), I = 1, 60)
         WRITE (HIST,300)     (OPGRAF(15,I), I = 1, 60)
C
C        OUTPUT THE CENTER LINES OF THE GRAPH.
         DO 1100 J = 14, 2, -1
             WRITE (   *,305) (OPGRAF(J, I), I = 1, 60)
             WRITE (HIST,305) (OPGRAF(J, I), I = 1, 60)
 1100        CONTINUE
C        ENDDO
C
C        OUTPUT THE LAST LINE  OF THE GRAPH, WITH MINIMUM VALUE, GRID, &
C        LABEL FOR THE X AXIS (INCLUDEING THE STEP).
         WRITE (   *,310)     (OPGRAF(1,I), I = 1, 60), STEP
         WRITE (HIST,310)     (OPGRAF(1,I), I = 1, 60), STEP
C
C        PROVIDE A PAUSE BEFORE RETURNING TO THE ACCESSING ROUTINE.
         WRITE (   *,315)
         WRITE (HIST,315)
         READ  (   *,  *) IWT
         WRITE (HIST,  *) IWT
         WRITE (   *,320)
         WRITE (HIST,320)
         RETURN
         END
         SUBROUTINE SMTDRV
     G                     (NS
     B                     ,Y
     Y                     ,ROUGH , SMOOTH)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMTDRV
C    G                     (NS
C    B                     ,Y
C    Y                     ,ROUGH , SMOOTH)
C
C   AUTHOR
C        PAUL VELLEMEN & DAVID HOAGLIN (CORNELL UNIVERSITY)
C   CODE STRUCTURING BY
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE SMOOTHING OF ONE DATA VECTOR, Y, OF A PLOT.
C   DESCRIPTION
C        THE SMOOTHING IS PERFORMED THROUGH A METHOD KNOWN AS TWICING.
C
C        FOR MORE DETAILS ON THE ACTUAL  SMOOTHING ROUTINES AND PRINCIP-
C        LES, SEE REFERENCE 2 IN THE DRIVER PROGRAM.
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
C             NS     (I) = NUMBER OF OBSERVATIONS
C        BOTH
C             Y      (R) = DATA VECTOR -- SMOOTHED ON RETURN
C        YIELDED
C             ROUGH  (R) = VECTOR OF ROUGHED DATA
C             SMOOTH (R) = VECTOR OF SMOOTHED DATA (FIRST PASS)
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
         INTEGER              NS
         DIMENSION            ROUGH(NS)     ,SMOOTH(NS)    ,Y(NS)
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
C        TRANSFER DATA TO SMOOTH VECTOR.
         DO 1000 I = 1, NS
             SMOOTH(I) = Y(I)
 1000        CONTINUE
C        ENDDO
         CALL SMTS3S
     G               (NS
     B               ,SMOOTH)
C
C        COMPUTE ROUGH FROM FIRST SMOOTHING.
         DO 1025 I = 1, NS
             ROUGH(I) = Y(I) - SMOOTH(I)
 1025        CONTINUE
C        ENDDO
C
C        RE-ROUGH SMOOTHERS ("TWICING").
         CALL SMTS3S
     G               (NS
     B               ,ROUGH )
C
C        COMPUTE THE FINAL SMOOTHED VECTOR.
         DO 1050 I = 1, NS
             Y(I) = SMOOTH(I) + ROUGH(I)
 1050        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE SMTMED
     G                     (X1    , X2    , X3
     Y                     ,XMED  , CHANGE)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMTMED
C    G                     (X1    , X2    , X3
C    Y                     ,XMED  , CHANGE)
C
C   AUTHOR
C        PAUL VELLEMEN & DAVID HOAGLIN (CORNELL UNIVERSITY)
C   CODE STRUCTURING BY
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PUT THE MEDIAN VALUE OF  X1, X2, & X3 INTO THE XMED ARGUMENT
C        OF THE CALL LINE, AND TO SET CHANGE TO .TRUE.  IF THE MEDIAN IS
C        NOT X2.
C   DESCRIPTION
C        SEE REFERENCE 2 OF DRIVER PROGRAM
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
C             X1     (R) = 1ST VALUE
C             X2     (R) = 2ND VALUE
C             X3     (R) = 3RD VALUE
C        YIELDED
C             CHANGE (L) = LOGICAL VARIABLE
C             XMED   (R) = MEDIAN VALUE
C   LOCAL GLOSSARY
C             TMP    (R) = TEMPORARY STORAGE
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
         LOGICAL              CHANGE
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
         XMED = X2
         TMP  = (X2 - X1) * (X3 - X2)
         IF (TMP.LT.0.0) THEN
             CHANGE = .TRUE.
             XMED   = X1
             TMP    = (X3 - X1) * (X3 - X2)
             IF (TMP.LE.0.0) THEN
                 XMED = X3
             ENDIF
         ENDIF
         RETURN
         END
         SUBROUTINE SMTS3H
     G                     (NS    , ITYPE
     B                     ,Y
     Y                     ,CHANGE)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMTS3H
C    G                     (NS    , ITYPE
C    B                     ,Y
C    Y                     ,CHANGE)
C
C   AUTHOR
C        PAUL VELLEMEN & DAVID HOAGLIN (CORNELL UNIVERSITY)
C   CODE STRUCTURING BY
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO COMPUTE THE  RUNNING MEDIANS OF 3 ON Y  (IF ITYPE=1) OR PRO-
C        VIDE HANNING -- THE 3-PT SMOOTHING BY  MOVING AVERAGES WEIGHTED
C        BY 1/4, 1/2, 1/4 (IF ITYPE=2).
C   DESCRIPTION
C        SEE REFERENCE 2 OF DRIVER PROGRAM
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
C             ITYPE  (I) = FLAG WHERE 1=S3  &  2=HANN
C             NS     (I) = NUMBER OF OBSERVATIONS
C        BOTH
C             Y      (R) = DATA VECTOR
C        YIELDED
C             CHANGE (L) = LOGICAL VARIABLE
C   LOCAL GLOSSARY
C             NM1    (I) = NS MINUS 1
C             Y1     (R) = TMP STORAGE VARIABLE
C             Y2     (R) = TMP STORAGE VARIABLE
C             Y3     (R) = TMP STORAGE VARIABLE
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
         INTEGER              ITYPE         ,NS
         DIMENSION            Y(NS)
         LOGICAL              CHANGE
C
C                             LOCAL SPECIFICATIONS
         INTEGER              I             ,J             ,NM1
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         NM1 = NS - 1
         Y2  = Y(1)
         Y3  = Y(2)
         DO 1000 I = 2, NM1
             Y1 = Y2
             Y2 = Y3
             J  = I + 1
             Y3 = Y(J)
             IF (ITYPE.EQ.1) THEN
                 CALL SMTMED
     G                       (Y1    , Y2    , Y3
     Y                       ,Y(I)  , CHANGE)
             ELSE
                 Y(I) = (Y1 + Y2 + Y2 + Y3) / 4.0
             ENDIF
 1000        CONTINUE
C        ENDDO
         RETURN
         END
         SUBROUTINE SMTS3R
     G                     (NS
     B                     ,Y     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMTS3R
C    G                     (NS
C    B                     ,Y     )
C
C   AUTHOR
C        PAUL VELLEMEN & DAVID HOAGLIN (CORNELL UNIVERSITY)
C   CODE STRUCTURING BY
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO COMPUTE THE  REPEATED  RUNNING MEDIANS  OF 3 ELEMENTS OF THE
C        VECTOR Y;  AND TO ESTIMATE THE  SMOOTHED VALUES FOR THE TWO END
C        POINTS OF THE SEQUENCE IN Y  (USING THE END POINT EXTRAPOLATION
C        RULE).
C   DESCRIPTION
C        SEE REFERENCE 2 OF DRIVER PROGRAM
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
C             NS     (I) = NUMBER OF OBSERVATIONS
C        BOTH
C             Y      (R) = DATA VECTOR
C   LOCAL GLOSSARY
C             CHANGE (L) = LOGICAL VARIABLE
C             NM1    (I) = NS MINUS 1
C             NM2    (I) = NS MINUS 2
C             YMED   (R) = RETURNED MEDIAN FROM SMTMED
C             Y0     (R) = COMBINATION OF OUTER TWO ELEMENTS
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
         DIMENSION            Y(NS)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              NM1           ,NM2
         LOGICAL              CHANGE
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        DOWHILE (CHANGE .NE. FALSE)
 1000        CHANGE = .FALSE.
             CALL SMTS3H
     G                   (NS    , 1
     B                   ,Y
     Y                   ,CHANGE)
             IF (CHANGE) GO TO 1000
C        ENDWHILE
C
         CHANGE = .FALSE.
C
C        COMPUTE THE LEFT END OF THE GRAPH.
         Y0 = (3.0 * Y(2)) - (2.0 * Y(3))
         CALL SMTMED
     G               (Y0    , Y(1)  , Y(2)
     Y               ,YMED  , CHANGE)
         Y(1) = YMED
C
C        COMPUTE THE RIGHT END OF THE GRAPH.
         NM1 = NS - 1
         NM2 = NS - 2
         Y0  = (3.0 * Y(NM1)) - (2.0 * Y(NM2))
         CALL SMTMED
     G               (Y0    , Y(NS) , Y(NM1)
     Y               ,YMED  , CHANGE)
         Y(NS) = YMED
         RETURN
         END
         SUBROUTINE SMTS3S
     G                     (NS
     B                     ,Y     )
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMTS3S
C    G                     (NS
C    B                     ,Y     )
C
C   AUTHOR
C        PAUL VELLEMEN & DAVID HOAGLIN (CORNELL UNIVERSITY)
C   CODE STRUCTURING BY
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO SMOOTH DATA VECTOR BY THE 3RSSH METHOD.
C   DESCRIPTION
C        SEE REFERENCE 2 OF DRIVER PROGRAM
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
C             NS     (I) = NUMBER OF OBSERVATIONS
C        BOTH
C             Y      (R) = DATA VECTOR
C   LOCAL GLOSSARY
C             CHANGE (L) = LOGICAL VARIABLE
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
         DIMENSION            Y(NS)
C
C                             LOCAL SPECIFICATIONS
         LOGICAL              CHANGE
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
         CALL SMTS3R
     G               (NS
     B               ,Y     )
         CHANGE = .FALSE.
         CALL SMTSPL
     G              (NS
     B              ,Y
     Y              ,CHANGE)
         IF (CHANGE) THEN
             CALL SMTS3R
     G                   (NS
     B                   ,Y     )
             CHANGE = .FALSE.
             CALL SMTSPL
     G                  (NS
     B                  ,Y
     Y                  ,CHANGE)
             IF (CHANGE) THEN
                 CALL SMTS3R
     G                       (NS
     B                       ,Y     )
             ENDIF
         ENDIF
         CALL SMTS3H
     G               (NS    , 2
     B               ,Y
     Y               ,CHANGE)
         RETURN
         END
         SUBROUTINE SMTSPL
     G                    (NS
     B                    ,Y
     Y                    ,CHANGE)
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE SMTSPL
C    G                    (NS
C    B                    ,Y
C    Y                    ,CHANGE)
C
C   AUTHOR
C        PAUL VELLEMEN & DAVID HOAGLIN (CORNELL UNIVERSITY)
C   CODE STRUCTURING BY
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO FIND TWO-FLATS IN Y, AND TO APPLY THE SPLITTING ALGORITHM.
C   DESCRIPTION
C        SEE REFERENCE 2 OF DRIVER PROGRAM
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
C             NS     (I) = NUMBER OF OBSERVATIONS
C        BOTH
C             Y      (R) = DATA VECTOR
C        YIELDED
C             CHANGE (L) = LOGICAL VARIABLE
C   LOCAL GLOSSARY
C             CNT    (I) = ITERATION COUNTER
C             NM2    (I) = NS MINUS 2
C             W      (R) = WINDOW VECTOR
C             Y1     (R) = TMP STORAGE FOR SCALED COMBINATIONS
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
         DIMENSION            Y(NS)
         LOGICAL              CHANGE
C
C                             LOCAL SPECIFICATIONS
         INTEGER              CNT           ,I             ,J
     1                       ,NM2
         DIMENSION            W(6)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        (NONE)
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        W IS A WINDOW OF 6 TS, WHICH IS SLID ALONG Y.
C
         NM2 = NS - 2
         DO 1000 I = 1, 4
             J    = I + 2
             W(J) = Y(I)
 1000        CONTINUE
C        ENDDO
C
C        IF Y(1).EQ.Y(2).NE.Y(3), TREAT 1ST 2 LIKE A 2-FLAT WITH END
C        POINT RULE.
C
         W(2) = Y(3)
         CNT  = 1
C
C        DOWHILE (CNT .LT. NS)
 1025        IF (W(3).EQ.W(4)) THEN
                 TMP = (W(3) - W(2)) * (W(5) - W(4))
                 IF (TMP.LT.0.0) THEN
C
C                    W(3) & W(4) FORM A 2-FLAT
                     IF (CNT.GE.3) THEN
                         Y1 = (3.0 * W(2)) - (2.0 * W(1))
                         CALL SMTMED
     G                               (Y1    , W(3)  , W(2)
     Y                               ,Y(CNT), CHANGE)
                     ENDIF
                     IF (CNT.LT.NM2) THEN
C                        APPLY LEFT END PT RULE AT CNT + 1
                         Y1 = (3.0 * W(5)) - (2.0 * W(6))
                         J  = CNT + 1
                         CALL SMTMED
     G                               (Y1    , W(4)  , W(5)
     Y                               ,Y(J)  , CHANGE)
                     ENDIF
                 ENDIF
             ENDIF
             DO 1050 I = 1, 5
                 J    = I + 1
                 W(I) = W(J)
 1050            CONTINUE
C            ENDDO
             CNT = CNT + 1
             IF (CNT.GE.NM2) THEN
                 W(6) = W(3)
             ELSE
                 J    = CNT + 3
                 W(6) = Y(J)
             ENDIF
             IF (CNT.LT.NS) GO TO 1025
C        ENDWHILE
         RETURN
         END
         SUBROUTINE STADRV
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE STADRV
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PROVIDE GENERAL STATISTICS OF THE DATA.
C   DESCRIPTION
C        TWO TYPES OF  REPORTS MAY BE  GENERATED VIA THIS ROUTINE EXECU-
C        TION.  THE GENERATION OF THE REPORTS IS DEPENDENT UPON THE TYPE
C        OF DATA THAT IS BEING ANALYZED.
C
C        FOR TBF DATA OR INTERVAL DATA (WITH EQUAL LENGTHS), THE STATIS-
C        TICS WILL CONSIST OF THE SUM, MEDIAN, HINGES, MINIMUM, MAXIMUM,
C        MEAN, STANDARD DEVIATION, VARIANCE, SKEWNESS, AND KURTOSIS OVER
C        A SAMPLE SIZE OF THE NUMBER OF FAILURES OR PERIODS  (WHEN EQUAL
C        LENGTH INTERVAL DATA ARE ANALYZED).  IT IS EMPHASIZED  THAT THE
C        TBF DATA WILL UTILIZE THE NUMBER OF FOUND FAILURES;  NOT MERELY
C        THE NUMBER OF ENTRIES  (WHICH MAY CONTAIN  A FAILURE-FREE ENTRY
C        FOR THE FINAL ELEMENT).
C
C        FOR INTERVAL DATA (WITH VARYING LENGTHS), THE OUTPUT STATISTICS
C        WILL BE REDUCED TO ONLY THE SUM,  MEDIAN, MINIMUM, MAXIMUM, AND
C        HINGES OF THE FAULT COUNTS AND TESTING LENGTHS;  AND A RATIO OF
C        THE TWO ABOVE COMPUTED SUMS.
C
C        THE COMPUTATIONS OF STATISTICS WILL BE  DONE BY THE SMFDST ROU-
C        TINE OF SMFLIB. THE DATA VECTOR(S)  WILL BE ORDERED BY THE ROU-
C        TINE;  THEREFORE, THOSE  VECTOR(S) MUST BE  STORED PRIOR TO THE
C        CALLING OF THE PROCEDURE AND RESTORED BEFORE RETURN.
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
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C             TMPB   (I) = UNIT NUMBER FOR THE TMP BINARY STORAGE FILE
C             UNITS  (C) = UNIT LABELS FOR THE TBF DATA TYPES
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C                          (BUT RETURNED TO ORIGINAL VALUES)
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             IDXB   (I) = POINTER TO THE BEGINNING OF DATA
C             IDXE   (I) = POINTER TO THE ENDING OF DATA
C             MAXL   (R) = MAXIMUM INTERVAL LENGTH
C             MINL   (R) = MINIMUM INTERVAL LENGTH
C             NS     (I) = NUMBER OF ENTRIES
C             RATIO  (R) = SUM1 / SUM2
C             STATS  (R) = VECTOR OF DATA STATISTICS
C             SUM1   (R) = SUMMATION VARIABLE
C             SUM2   (R) = SUMMATION VARIABLE
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
         INTEGER              I             ,IDXB          ,IDXE
     1                       ,J             ,NS
         DIMENSION            STATS(11)
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT (' '
     1        ,/,'                        WC  TIME-BETWEEN-FAILURES'
     2        ,/,'                         RECORDED IN ',A7)
  305    FORMAT (' '
     1        ,/,'                        CPU TIME-BETWEEN-FAILURES'
     2        ,/,'                         RECORDED IN ',A7)
  310    FORMAT ('                         WITH TOTAL TESTING TIME OF '
     1          ,E15.8
     2        ,/,'                         AND TIME OF LAST FAILURE OF'
     3          ,E15.8)
  315    FORMAT (' '
     1        ,/,'                        INTERVAL DATA WITH EQUAL LENG'
     2          ,'THS'
     3        ,/,'                         WITH FAULT COUNTS TOTALING '
     4          ,'TO',I12)
  320    FORMAT ('                        *****************************'
     1          ,'**************')
  325    FORMAT ('   MEDIAN OF THE DATA   *',13X,E15.8,13X,'*'
     1        ,/,'   LOWER & UPPER HINGES *',3X,E15.8,3X,E15.8,5X,'*'
     2        ,/,'   MINIMUM AND MAXIMUM  *',3X,E15.8,3X,E15.8,5X,'*')
  330    FORMAT ('   NUMBER OF ENTRIES    *',18X,I4,19X,'*'
     1        ,/,'   AVERAGE OF THE DATA  *',13X,E15.8,13X,'*'
     2        ,/,'   STD. DEV. & VARIANCE *',3X,E15.8,3X,E15.8,5X,'*'
     3        ,/,'   SKEWNESS & KURTOSIS  *',3X,E15.8,3X,E15.8,5X,'*')
  335    FORMAT (' '
     1        ,/,'                        INTERVAL DATA WITH VARYING LE'
     2          ,'NGTHS')
  340    FORMAT ('                        *------------- COMBINATION --'
     1          ,'-------------*'
     2        ,/,'   COUNT & LENGTH TOTALS*',3X,E15.8,3X,E15.8,5X,'*'
     3        ,/,'   RATIO OF THE PRIOR   *',13X,E15.8,13X,'*'
     4        ,/,'   NUMBER OF ENTRIES    *',18X,I4,19X,'*')
  345    FORMAT ('                        *------------- FAULT COUNTS -'
     1          ,'-------------*')
  350    FORMAT ('                        *----------- INTERVAL LENGTHS'
     1          ,' ------------*')
C
C ********************** START OF EXECUTABLE CODE **********************
C
         IF (DATTYP.LE.3) THEN
C            TBF STATISTICS ARE  TO BE GENERATED;  CHECK FOR BOTH TYPES.
             DO 1000 I = 1, 2
                 IF (PRC(I,1).NE.0) THEN
C                    A TBF DATA TYPE HAS BEEN FOUND;  SET THE TWO POINT-
C                    ERS & COUNTER AND SCRIPT APPROPRIATE TITLE.
                     IF (I.EQ.1) THEN
C                        WALL CLOCK TBF DATA ARE PRESENT.
                         IDXB = 1
                         IDXE = PRC(I,1) + (PRC(I,2) - 1)
                         NS   = IDXE - IDXB + 1
                         WRITE (   *,300) UNITS(PRC(1,3))
                         WRITE (HIST,300) UNITS(PRC(1,3))
                     ELSE
C                        CPU TBF DATA ARE PRESENT.
                         IDXB = STR2
                         IDXE = PRC(I,1) + (PRC(I,2) - 1) + END1
                         NS   = IDXE - IDXB + 1
                         WRITE (   *,305) UNITS(PRC(2,3))
                         WRITE (HIST,305) UNITS(PRC(2,3))
                     ENDIF
C
C                    USING THE SMFDST ROUTINE, COMPUTE THE STATISTICS OF
C                    THE DATA.
                     REWIND TMPB
                     WRITE (TMPB) (DAT(J), J = IDXB, IDXE)
                     CALL SMFDST
     G                           (NS        , 11
     B                           ,DAT(IDXB)
     Y                           ,STATS     )
                     REWIND TMPB
                     READ  (TMPB) (DAT(J), J = IDXB, IDXE)
C
                     SUM1 = STATS(1)
                     IF (PRC(I,1).NE.NS) THEN
C                        ADD ON THE FAILURE-FREE TESTING TIME.
                         J    = IDXE + 1
                         SUM1 = SUM1 + DAT(J)
                     ENDIF
                     WRITE (   *,310) SUM1, STATS(1)
                     WRITE (   *,320)
                     WRITE (   *,325) (STATS(J), J = 2, 6)
                     WRITE (   *,330) NS, (STATS(J), J = 7, 11)
                     WRITE (   *,320)
                     WRITE (HIST,310) SUM1, STATS(1)
                     WRITE (HIST,320)
                     WRITE (HIST,325) (STATS(J), J = 2, 6)
                     WRITE (HIST,330) NS, (STATS(J), J = 7, 11)
                     WRITE (HIST,320)
                 ENDIF
 1000            CONTINUE
C            ENDDO
         ELSE
C            INTERVAL STATISTICS ARE DESIRED; SET POINTERS AND STORE THE
C            COUNT AND LENGTH DATA.
             IDXB = STR2
             IDXE = PRC(3,1) + END1
             NS   = PRC(3,1)
             REWIND TMPB
             WRITE (TMPB) (DAT(J), J = 1   , NS  )
             WRITE (TMPB) (DAT(J), J = IDXB, IDXE)
C
C            CHECK FOR LENGTH EQUALITY.
             MAXL = DAT(IDXB)
             MINL = DAT(IDXB)
             DO 1025 I = IDXB, IDXE
                 MAXL = MAX(MAXL,DAT(I))
                 MINL = MIN(MINL,DAT(I))
 1025            CONTINUE
C            ENDDO
             IF (MAXL.EQ.MINL) THEN
C                THE LENGTHS OF ALL INTERVALS ARE EQUAL; PROCESS THE DA-
C                TA AS IF IT WERE TBF DATA.
                 CALL SMFDST
     G                       (NS    , 11
     B                       ,DAT
     Y                       ,STATS )
                 J = INT(STATS(1))
                 WRITE (   *,315) J
                 WRITE (   *,320)
                 WRITE (   *,325) (STATS(I), I = 2, 6)
                 WRITE (   *,330) NS, (STATS(I), I = 7, 11)
                 WRITE (   *,320)
                 WRITE (HIST,315) J
                 WRITE (HIST,320)
                 WRITE (HIST,325) (STATS(I), I = 2, 6)
                 WRITE (HIST,330) NS, (STATS(I), I = 7, 11)
                 WRITE (HIST,320)
             ELSE
C                THE INTERVALS WERE NOT OF THE SAME LENGTH;  THE REDUCED
C                TABLE WILL BE GENERATED.  THE SMFDST ROUTINE IS TOLD OF
C                THE REDUCTION BY THE SECOND  ARGUMENT OF THE CALL LINE.
                 WRITE (   *,335)
                 WRITE (   *,320)
                 WRITE (HIST,335)
                 WRITE (HIST,320)
                 CALL SMFDST
     G                       (NS        , 6
     B                       ,DAT
     Y                       ,STATS     )
                 SUM1 = STATS(1)
                 WRITE (   *,345)
                 WRITE (   *,325) (STATS(I), I = 2, 6)
                 WRITE (HIST,345)
                 WRITE (HIST,325) (STATS(I), I = 2, 6)
                 CALL SMFDST
     G                       (NS        , 6
     B                       ,DAT(STR2)
     Y                       ,STATS     )
                 SUM2 = STATS(1)
                 WRITE (   *,350)
                 WRITE (   *,325) (STATS(I), I = 2, 6)
                 WRITE (HIST,350)
                 WRITE (HIST,325) (STATS(I), I = 2, 6)
C
C                OUTPUT THE SUM AND RATIO LINES OF THE TABLE.
                 RATIO = SUM1 / SUM2
                 WRITE (   *,340) SUM1, SUM2, RATIO, NS
                 WRITE (   *,320)
                 WRITE (HIST,340) SUM1, SUM2, RATIO, NS
                 WRITE (HIST,320)
             ENDIF
             REWIND TMPB
             READ  (TMPB) (DAT(J), J = 1   , NS  )
             READ  (TMPB) (DAT(J), J = IDXB, IDXE)
         ENDIF
         RETURN
         END
         SUBROUTINE TRNDRV
C
C        ***************************************************************
C        *                        UNCLASSIFIED                         *
C        ***************************************************************
C
C   CALLING SEQUENCE
C        SUBROUTINE TRNDRV
C
C   AUTHOR
C        OLIVER D. SMITH (EG&G WASC)
C   CODE READ BY
C        WILLIAM H. FARR (B10, NSWCDD)
C   PURPOSE
C        TO PERFORM DATA RESTORATION AND  TRANSFORMATIONS OF A SPECIFIED
C        SET OF DATA WITHIN THE DATA VECTOR. (NOTE, THE DATA RESTORATION
C        OPTION WILL RESULT IN ERROR IF THE NEW DATA FILES WERE NOT GEN-
C        ERATED IN THE DATA INPUT ROUTINE.)
C   DESCRIPTION
C        THE PROCESSING OF THIS ROUTINE  REQUIRES  THE MODULE 1 INPUT OF
C        DATA SETS. THE ALLOWED TRANSFORMATIONS  INCLUDE LOG, EXP, X**P,
C        X+A, AND X*A.  ADDITIONALLY, A RESTORATION  OF DATA TO THE MOST
C        RECENT DATA BASE FILE CAN BE PERFORMED.
C
C        THE TRNDRV ROUTINE HOUSES NO ACTUAL  DATA TRANSFORMATION STEPS;
C        IT MERELY SERVES  AS AN INPUT VEHICLE.  THE SMFTRN ROUTINE PER-
C        FORMS THE DATA PROCESSING, AND THE DATLST ROUTINE GENERATES THE
C        DATA LISTINGS.
C
C        TWO POSSIBLE ERRORS CAN BE RETURNED BY THE  SMFTRN ROUTINE. THE
C        FIRST INDICATES THAT THE LOG COULD NOT BE PERFORMED DUE TO NEG-
C        ATIVE OR ZERO VALUES;  AND THE SECOND SHOWS THAT THE X**P COULD
C        NOT BE PERFORMED DUE TO  NEGATIVE VALUES FOR X RAISED TO A NON-
C        INTEGER POWER.
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
C             DATTYP (I) = FLAG INDICATING THE DATA TYPE BEING ANALYZED
C             HIST   (I) = UNIT NUMBER FOR THE HISTORY FILE
C             UPDFLG (I) = FLAG TO CONTROL FILE DATA INPUT AND UPDATE
C        BOTH
C             DAT    (R) = ARRAY TO HOLD OBSERVED DATA OF SIZE END2
C             PRC    (I) = DOUBLE DIMENSION ARRAY TO HOLD DATA INDICES
C   ARGUMENT LIST
C             (NONE)
C   LOCAL GLOSSARY
C             A      (R) = TRANSFORMATION VARIABLE 1
C             B      (R) = TRANSFORMATION VARIABLE 2
C             ERRFLG (I) = TRANSFORMATION ERROR FLAG
C             FLAG   (I) = OPTION FLAG
C             IDXB   (I) = POINTER TO THE BEGINNING OF DATA
C             IU     (I) = FILE INPUT UNIT VARIABLE
C             RF     (I) = RESTORATION FILE NUMBER FLAG
C   ERRORS
C        "DATA TYPE ERROR; TRY AGAIN (AFTER THE PROMPT)."
C        "TRANSFORMATION ILLEGAL - NEGATIVE X(I)"
C        "TRANSFORMATION ILLEGAL - NEGATIVE OR ZERO A * X(I) + B"
C        "RESTORATION NOT POSSIBLE; NEW DATA FILES WERE NOT CREATED."
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
C               /UNTCOM/      UNIT NUMBER - NAME COMMON
         COMMON /UNTCOM/      HIST          ,PLOT          ,TMPB
         INTEGER              HIST          ,PLOT          ,TMPB
C
C                             ARGUMENT SPECIFICATIONS
C        (NONE)
C
C                             LOCAL SPECIFICATIONS
         INTEGER              ERRFLG        ,FLAG          ,I
     1                       ,IDXB          ,IU            ,RF
C
C ********************* START OF FORMAT STATEMENTS *********************
C
C        OUTPUT FORMATS
  300    FORMAT ('   ENTER ONE FOR WC TBF OR TWO FOR CPU TBF.')
  305    FORMAT (' **DATA TYPE ERROR; TRY AGAIN (AFTER THE PROMPT).')
  310    FORMAT ('   ENTER ONE FOR COUNTS OR TWO FOR LENGTHS;  THE REST'
     1          ,'ORATION OPTION'
     2        ,/,'   (#6) WILL AFFECT BOTH THE COUNT AND LENGTH DATA.')
  315    FORMAT (' '
     1        ,/,'   ENTER TRANSFORMATION OPTION, OR ZERO FOR A LIST.')
  320    FORMAT ('   THE AVAILABLE TRANSFORMATION OPTIONS ARE:'
     1        ,/,'    1 LOG(A * X(I) + B)       5 X * A'
     2        ,/,'    2 EXP(A * X(I) + B)       6 RESTORE THE DATA'
     3        ,/,'    3 X ** A                  7 LIST THE CURRENT DATA'
     4        ,/,'    4 X + A                   8 RETURN TO THE MAIN PR'
     5          ,'OGRAM'
     6        ,/,'   ENTER TRANSFORMATION OPTION.')
  325    FORMAT ('    ENTER VALUE OF A.')
  330    FORMAT ('    ENTER VALUES OF A AND B.')
  335    FORMAT ('  **TRANSFORMATION ILLEGAL - NEGATIVE X(I).')
  340    FORMAT ('  **TRANSFORMATION ILLEGAL - NEGATIVE OR ZERO A * X(I'
     1          ,') + B.')
  345    FORMAT ('  **RESTORATION NOT POSSIBLE; NEW DATA FILES WERE NOT'
     1          ,' CREATED.')
C
C ********************** START OF EXECUTABLE CODE **********************
C
C        SET POINTERS NEEDED FOR DATA TRANSFORMATION AND RESTORATION.
         IF (DATTYP.LE.2) THEN
             I  = DATTYP
             RF = DATTYP
         ELSEIF (DATTYP.EQ.3) THEN
C            DOWHILE (ERROR ON INPUT)
 1000            WRITE (   *,300)
                 WRITE (HIST,300)
                 READ  (   *,  *) I
                 WRITE (HIST,  *) I
                 IF ((I.EQ.1) .OR. (I.EQ.2)) GO TO 1025
                 WRITE (   *,305)
                 WRITE (HIST,305)
                 GO TO 1000
 1025            CONTINUE
C            ENDWHILE
             RF = I
         ELSE
C            DOWHILE (ERROR ON INPUT)
 1050            WRITE (   *,310)
                 WRITE (HIST,310)
                 READ  (   *,  *) I
                 WRITE (HIST,  *) I
                 IF ((I.EQ.1) .OR. (I.EQ.2)) GO TO 1075
                 WRITE (   *,305)
                 WRITE (HIST,305)
                 GO TO 1050
 1075            CONTINUE
C            ENDWHILE
             RF = 3
         ENDIF
C
C        DOWHILE (TRANSFORMATION SELECTION .NE. RETURN)
 1100        WRITE (   *,315)
             WRITE (HIST,315)
             READ  (   *,  *) FLAG
             WRITE (HIST,  *) FLAG
             IF ((FLAG.LT.1) .OR. (FLAG.GT.8)) THEN
                 WRITE (   *,320)
                 WRITE (HIST,320)
                 READ  (   *,  *) FLAG
                 WRITE (HIST,  *) FLAG
             ENDIF
             IF ((FLAG.GE.1) .AND. (FLAG.LE.7)) THEN
C
             IF (FLAG.LE.5) THEN
C                THE SELECTION WAS  OTHER THAN THE  RESTORE DATA OPTION;
C                DETERMINE THE BEGINNING ADDRESS OF THE DATA.
                 IDXB = (I * END1) - (END1 - 1)
C
C                DETERMINE IF THE DESIRED TRANSFORMATION REQUIRES ONE OR
C                TWO INPUT VALUES.
                 IF (FLAG.GE.3) THEN
                     WRITE (   *,325)
                     WRITE (HIST,325)
                     READ  (   *,  *) A
                     WRITE (HIST,  *) A
                 ELSE
                     WRITE (   *,330)
                     WRITE (HIST,330)
                     READ  (   *,  *) A, B
                     WRITE (HIST,  *) A, B
                 ENDIF
C
C                ZERO THE ERROR FLAG,  ACCESS THE SMFTRN ROUTINE OF SMF-
C                LIB, AND OUTPUT AN ERROR MESSAGE IF APPLICABLE.
                 ERRFLG = 0
                 CALL SMFTRN
     G                       (PRC(RF,1) , A         , B         , FLAG
     B                       ,DAT(IDXB)
     Y                       ,ERRFLG    )
                 IF (ERRFLG.EQ.1) THEN
C                    THE FIRST ERROR TYPE WAS ENCOUNTERED.
                     WRITE (   *,335)
                     WRITE (HIST,335)
                 ELSEIF (ERRFLG.EQ.2) THEN
C                    THE SECOND ERROR TYPE WAS ENCOUNTERED.
                     WRITE (   *,340)
                     WRITE (HIST,340)
                 ENDIF
             ELSEIF (FLAG.EQ.6) THEN
C                THE DATA RESTORE OPTION WAS SELECTED.
                 IF (UPDFLG.EQ.1) THEN
                     IU = RF + 13
                     CALL INPPRM
     G                           (RF     , IU     )
                 ELSE
C                    RESTORATION CANNOT BE MADE.
                     WRITE (   *,345)
                     WRITE (HIST,345)
                 ENDIF
             ELSE
C                THE DATA LISTING OPTION WAS SELECTED.
                 CALL DATLST
             ENDIF
             GO TO 1100
             ENDIF
C        ENDWHILE
         RETURN
         END
