       IDENTIFICATION DIVISION.
       PROGRAM-ID. AEROSTEP-UI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL REPORT-FILE ASSIGN TO "aerostep.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE RECORD IS VARYING DEPENDING ON WS-REC-LEN.
       01  REPORT-RECORD               PIC X(120).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS              PIC XX.
       01 WS-REC-LEN                  PIC 9(3).
       01 WS-FAILED                   PIC X VALUE "N".

       *> Login Variables
       01 WS-OPERATOR-ID              PIC X(20).
       01 WS-ACCESS-CODE              PIC X(128).
       01 WS-EXPECTED-CODE            PIC X(128).
       01 WS-ENV-CODE                 PIC X(128).

       01 WS-PRESSURE                 PIC 9(4).
       01 MIN-PRESS                  PIC 9(4) VALUE 80.
       01 MAX-PRESS                  PIC 9(4) VALUE 120.
       01 WS-HEAT                    PIC 9(4).
       01 MIN-HEAT                   PIC 9(4) VALUE 200.
       01 MAX-HEAT                   PIC 9(4) VALUE 300.
       01 WS-QUALITY                 PIC 9(3).
       01 QUALITY-THRESH             PIC 9(3) VALUE 70.

       *> Optimization: Group item for timestamp to allow direct component access
       *> and avoid expensive STRING operations in the hot path.
       *> Reduced to 19 chars to remove trailing space in logs.
       01 WS-BASE-TIMESTAMP.
           05 WS-TS-DATE      PIC X(11).
           05 WS-TS-HH        PIC X(2).
           05 WS-TS-SEP1      PIC X VALUE ":".
           05 WS-TS-MM        PIC X(2).
           05 WS-TS-SEP2      PIC X VALUE ":".
           05 WS-TS-SS        PIC X(2).

       *> Optimization: Cache formatted date string to reduce system calls
       01 WS-FORMATTED-DATE          PIC X(11).
       01 WS-FIELD-NAME              PIC X(30).
       01 WS-FIELD-VALUE             PIC 9(4).
       01 WS-FIELD-VALUE-DISPLAY    PIC X(8).
       01 WS-NUM-EDIT               PIC ZZZ9.
       01 WS-DATE                   PIC 9(8).
       01 WS-TIME                   PIC 9(8).

       01 UI-LINE                   PIC 9(2) VALUE 0.

       01 WS-STATUS                 PIC X(10).
       *> Optimization: Integer status code for faster conditional checks (Strength Reduction)
       01 WS-STATUS-CODE            PIC 9 VALUE 0.
       01 WS-SLEEP-SEC              PIC 9(9) BINARY VALUE 1.
       01 WS-ESC                    PIC X VALUE X'1B'.

       *> Buffers for UI optimization
       01 WS-UI-ROW-BUFFER           PIC X(200).
       01 WS-PTR                     PIC 9(3).
       01 WS-LOG-PTR                 PIC 9(3).

       *> Pre-constructed Status Strings (Optimization)
       01 STR-PASSED.
           05 PIC X VALUE X'1B'.
           05 PIC X(4) VALUE "[32m".
           05 PIC X(10) VALUE "[+] PASSED".
           05 PIC X VALUE X'1B'.
           05 PIC X(3) VALUE "[0m".
       01 STR-FAILED.
           05 PIC X VALUE X'1B'.
           05 PIC X(4) VALUE "[31m".
           05 PIC X(10) VALUE "[X] FAILED".
           05 PIC X VALUE X'1B'.
           05 PIC X(3) VALUE "[0m".

       01 STR-SKIPPED.
           05 PIC X VALUE X'1B'.
           05 PIC X(4) VALUE "[37m".
           05 PIC X(11) VALUE "[-] SKIPPED".
           05 PIC X VALUE X'1B'.
           05 PIC X(3) VALUE "[0m".

       01 WS-CURSOR-HIDE.
           05 PIC X VALUE X'1B'.
           05 PIC X(6) VALUE "[?25l".
       01 WS-CURSOR-SHOW.
           05 PIC X VALUE X'1B'.
           05 PIC X(6) VALUE "[?25h".

       *> Unicode Box Drawing Characters (UTF-8 Hex)
       *> Only BOX-V is used in Procedure Division
       01 BOX-V                      PIC X(3) VALUE X'E29482'. *> │

       *> Pre-constructed Unicode Lines (calculated for 80 cols)
       *> Top Border: ┌ + 78*─ + ┐
       01 WS-BOX-TOP.
           05 BOX-TOP-L  PIC X(3) VALUE X'E2948C'.
           05 BOX-TOP-M  PIC X(234) VALUE ALL X'E29480'.
           05 BOX-TOP-R  PIC X(3) VALUE X'E29490'.

       *> Bottom Border: └ + 78*─ + ┘
       01 WS-BOX-BOTTOM.
           05 BOX-BOT-L  PIC X(3) VALUE X'E29494'.
           05 BOX-BOT-M  PIC X(234) VALUE ALL X'E29480'.
           05 BOX-BOT-R  PIC X(3) VALUE X'E29498'.

       *> Table Header Sep: ├ + 22*─ + ┼ + 11*─ + ┼ + 9*─ + ┼ + 33*─ + ┤
       01 WS-TABLE-DIV.
           05 T-DIV-L    PIC X(3) VALUE X'E2949C'.
           05 T-DIV-C1   PIC X(66) VALUE ALL X'E29480'.
           05 T-DIV-S1   PIC X(3) VALUE X'E294BC'.
           05 T-DIV-C2   PIC X(33) VALUE ALL X'E29480'.
           05 T-DIV-S2   PIC X(3) VALUE X'E294BC'.
           05 T-DIV-C3   PIC X(27) VALUE ALL X'E29480'.
           05 T-DIV-S3   PIC X(3) VALUE X'E294BC'.
           05 T-DIV-C4   PIC X(99) VALUE ALL X'E29480'.
           05 T-DIV-R    PIC X(3) VALUE X'E294A4'.

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           OPEN EXTEND REPORT-FILE
           IF WS-FILE-STATUS NOT = "00" AND WS-FILE-STATUS NOT = "05"
               DISPLAY "CRITICAL ERROR: CANNOT OPEN LOG FILE. STATUS: " WS-FILE-STATUS
               STOP RUN
           END-IF

           PERFORM SETUP-DATE

           PERFORM LOGIN-SEQUENCE

           DISPLAY WS-CURSOR-HIDE WITH NO ADVANCING

           PERFORM DRAW-UI-SHELL

           PERFORM INITIALIZATION
           IF WS-FAILED NOT = "Y"
               PERFORM PRESSURE-TEST
           ELSE
               PERFORM SKIP-PRESSURE
           END-IF
           IF WS-FAILED NOT = "Y"
               PERFORM HEAT-TREATMENT
           ELSE
               PERFORM SKIP-HEAT
           END-IF
           IF WS-FAILED NOT = "Y"
               PERFORM QUALITY-INSPECTION
           ELSE
               PERFORM SKIP-QUALITY
           END-IF

           PERFORM FINALIZE

           DISPLAY WS-CURSOR-SHOW WITH NO ADVANCING

           CLOSE REPORT-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "WARNING: ERROR CLOSING LOG FILE. STATUS: " WS-FILE-STATUS
           END-IF
           STOP RUN.

       LOGIN-SEQUENCE.
           ACCEPT WS-ENV-CODE FROM ENVIRONMENT "AERO_PASS"
           IF WS-ENV-CODE = SPACES THEN
               DISPLAY "CRITICAL SECURITY ERROR: SECURITY CONFIGURATION MISSING."
               DISPLAY "SYSTEM HALTED."
               STOP RUN
           ELSE
               MOVE WS-ENV-CODE TO WS-EXPECTED-CODE
           END-IF

           DISPLAY WS-ESC "[2J" WS-ESC "[H"
                   WS-BOX-TOP(1:240)
                   X'0A'
                   BOX-V "                        SECURITY ACCESS CONTROL                               " BOX-V
                   X'0A'
                   WS-BOX-BOTTOM(1:240)
                   X'0A'
                   " "
                   X'0A'
                   "   [i] Authorized Personnel Only. Activities Monitored."
                   X'0A'
                   " "
                   X'0A'
                   "   OPERATOR ID: "
                   WITH NO ADVANCING
           ACCEPT WS-OPERATOR-ID
           *> Sanitize input to prevent log injection
           INSPECT WS-OPERATOR-ID REPLACING ALL WS-ESC BY SPACE
                                            ALL "," BY SPACE

           *> Prevent CSV Injection (Formula Injection)
           *> Only sanitize if the FIRST character is a trigger to avoid
           *> breaking valid inputs like email addresses or hyphenated names.
           *> Replace with underscore instead of space to prevent TRIM from re-enabling the trigger.
           IF WS-OPERATOR-ID(1:1) = "=" OR
              WS-OPERATOR-ID(1:1) = "+" OR
              WS-OPERATOR-ID(1:1) = "-" OR
              WS-OPERATOR-ID(1:1) = "@"
               MOVE "_" TO WS-OPERATOR-ID(1:1)
           END-IF
           DISPLAY " "
           DISPLAY "   ACCESS CODE (Hidden): " WITH NO ADVANCING
           *> Use ANSI Hidden attribute to mask input
           DISPLAY WS-ESC "[8m" WITH NO ADVANCING
           ACCEPT WS-ACCESS-CODE
           DISPLAY WS-ESC "[0m"
           DISPLAY " "

           PERFORM GET-TIMESTAMP

           IF WS-ACCESS-CODE = WS-EXPECTED-CODE THEN
               DISPLAY "   " WS-ESC "[32m" "[+] ACCESS GRANTED." WS-ESC "[0m"
               CALL "C$SLEEP" USING WS-SLEEP-SEC
               MOVE "LOGIN" TO WS-FIELD-NAME
               MOVE "SUCCESS" TO WS-STATUS
               MOVE SPACES TO WS-FIELD-VALUE-DISPLAY
               PERFORM WRITE-LOG
           ELSE
               DISPLAY "   " WS-ESC "[31m" "[X] ACCESS DENIED." WS-ESC "[0m"
               MOVE "LOGIN" TO WS-FIELD-NAME
               MOVE "DENIED" TO WS-STATUS
               MOVE SPACES TO WS-FIELD-VALUE-DISPLAY
               PERFORM WRITE-LOG
               CLOSE REPORT-FILE
               STOP RUN
           END-IF.
       
       DRAW-UI-SHELL.
           *> Optimization: Consolidated DISPLAY calls (Clear + Shell + Header) to reduce I/O overhead
           DISPLAY WS-ESC "[2J" WS-ESC "[H"
                   WS-BOX-TOP(1:240)
                   X'0A'
                   BOX-V "                        AEROSTEP TESTING INTERFACE                            " BOX-V
                   X'0A'
                   WS-TABLE-DIV(1:240)
                   X'0A'
                   BOX-V " Step                 " BOX-V " Status    " BOX-V " Value   " BOX-V " Timestamp                       " BOX-V
                   X'0A'
                   WS-TABLE-DIV(1:240)
                   X'0A'
                   BOX-V " Initialization       " BOX-V "           " BOX-V "         " BOX-V "                                 " BOX-V
                   X'0A'
                   BOX-V " Pressure (80-120)    " BOX-V "           " BOX-V "         " BOX-V "                                 " BOX-V
                   X'0A'
                   BOX-V " Heat (200-300)       " BOX-V "           " BOX-V "         " BOX-V "                                 " BOX-V
                   X'0A'
                   BOX-V " Quality (Min 70)     " BOX-V "           " BOX-V "         " BOX-V "                                 " BOX-V
                   X'0A'
                   WS-TABLE-DIV(1:240)
                   X'0A'
                   BOX-V " Overall Status:                                                      " BOX-V
                   X'0A'
                   WS-BOX-BOTTOM(1:240)
                   WS-ESC "[2;55H"
                   "Op: "
                   FUNCTION TRIM(WS-OPERATOR-ID)
                   WITH NO ADVANCING.

       UPDATE-UI-ROW.
           IF UI-LINE > 0
               PERFORM GET-TIMESTAMP
               *> Optimized: Direct buffering to avoid intermediate copies and reduce STRING overhead
               MOVE 1 TO WS-PTR

               *> 1. Position and Write Field Name
               STRING WS-ESC "[" UI-LINE ";3H"
                      WS-FIELD-NAME(1:20)
                      WS-ESC "[" UI-LINE ";25H"
                      DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER
                      WITH POINTER WS-PTR

               *> 2. Write Status Directly
               IF WS-STATUS-CODE = 2
                   STRING STR-FAILED DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER WITH POINTER WS-PTR
               ELSE
                   IF WS-STATUS-CODE = 1
                       STRING STR-PASSED DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER WITH POINTER WS-PTR
                   ELSE
                       IF WS-STATUS-CODE = 3
                           STRING STR-SKIPPED DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER WITH POINTER WS-PTR
                       ELSE
                           STRING WS-ESC "[37m" WS-STATUS WS-ESC "[0m"
                               DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER WITH POINTER WS-PTR
                       END-IF
                   END-IF
               END-IF

               *> 3. Position and Write Value Directly
               STRING WS-ESC "[" UI-LINE ";38H"
                      DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER
                      WITH POINTER WS-PTR

               IF WS-STATUS-CODE = 2
                   STRING WS-ESC "[31m" WS-FIELD-VALUE-DISPLAY WS-ESC "[0m"
                       DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER WITH POINTER WS-PTR
               ELSE
                   IF WS-STATUS-CODE = 1
                       STRING WS-ESC "[32m" WS-FIELD-VALUE-DISPLAY WS-ESC "[0m"
                           DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER WITH POINTER WS-PTR
                   ELSE
                       IF WS-STATUS-CODE = 3
                           STRING WS-ESC "[37m" WS-FIELD-VALUE-DISPLAY WS-ESC "[0m"
                               DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER WITH POINTER WS-PTR
                       ELSE
                           STRING WS-FIELD-VALUE-DISPLAY
                               DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER WITH POINTER WS-PTR
                       END-IF
                   END-IF
               END-IF

               *> 4. Position and Write Timestamp
               STRING WS-ESC "[" UI-LINE ";48H"
                      WS-BASE-TIMESTAMP(1:19)
                      DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER
                      WITH POINTER WS-PTR

               DISPLAY WS-UI-ROW-BUFFER(1:WS-PTR - 1) WITH NO ADVANCING

               PERFORM WRITE-LOG
           END-IF
           .

       WRITE-LOG.
           *> Caller must ensure timestamp is set via GET-TIMESTAMP for efficiency
           MOVE 1 TO WS-LOG-PTR
           STRING WS-BASE-TIMESTAMP DELIMITED BY SIZE
                  ", " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-OPERATOR-ID) DELIMITED BY SIZE
                  ", " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-FIELD-NAME) DELIMITED BY SIZE
                  ", " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-STATUS) DELIMITED BY SIZE
                  ", " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-FIELD-VALUE-DISPLAY) DELIMITED BY SIZE
                  INTO REPORT-RECORD
                  WITH POINTER WS-LOG-PTR
           COMPUTE WS-REC-LEN = WS-LOG-PTR - 1
           WRITE REPORT-RECORD.

       INITIALIZATION.
           *> Set "Processing..." status for immediate feedback
           DISPLAY WS-ESC "[11;19H" WS-ESC "[33m"
                   "[*] PROCESSING...                         "
                   WS-ESC "[0m" WITH NO ADVANCING

           MOVE "Initialization" TO WS-FIELD-NAME
           MOVE 6 TO UI-LINE
           MOVE SPACES TO WS-FIELD-VALUE-DISPLAY
           MOVE "PASSED" TO WS-STATUS
           MOVE 1 TO WS-STATUS-CODE
           PERFORM UPDATE-UI-ROW
           CALL "C$SLEEP" USING WS-SLEEP-SEC.

       PRESSURE-TEST.
           COMPUTE WS-PRESSURE = FUNCTION RANDOM * (MAX-PRESS - MIN-PRESS + 1) + MIN-PRESS
           MOVE "Pressure (80-120)" TO WS-FIELD-NAME
           MOVE 7 TO UI-LINE
           MOVE WS-PRESSURE TO WS-FIELD-VALUE
           MOVE SPACES TO WS-FIELD-VALUE-DISPLAY
           MOVE WS-PRESSURE TO WS-NUM-EDIT
           STRING FUNCTION TRIM(WS-NUM-EDIT) " PSI"
               DELIMITED BY SIZE INTO WS-FIELD-VALUE-DISPLAY
           IF WS-PRESSURE < MIN-PRESS OR WS-PRESSURE > MAX-PRESS
               MOVE "FAILED" TO WS-STATUS
               MOVE 2 TO WS-STATUS-CODE
               MOVE "Y" TO WS-FAILED
           ELSE
               MOVE "PASSED" TO WS-STATUS
               MOVE 1 TO WS-STATUS-CODE
           END-IF
           PERFORM UPDATE-UI-ROW
           CALL "C$SLEEP" USING WS-SLEEP-SEC.

       HEAT-TREATMENT.
           COMPUTE WS-HEAT = FUNCTION RANDOM * (MAX-HEAT - MIN-HEAT + 1) + MIN-HEAT
           MOVE "Heat (200-300)" TO WS-FIELD-NAME
           MOVE 8 TO UI-LINE
           MOVE WS-HEAT TO WS-FIELD-VALUE
           MOVE SPACES TO WS-FIELD-VALUE-DISPLAY
           MOVE WS-HEAT TO WS-NUM-EDIT
           STRING FUNCTION TRIM(WS-NUM-EDIT) " C"
               DELIMITED BY SIZE INTO WS-FIELD-VALUE-DISPLAY
           IF WS-HEAT < MIN-HEAT OR WS-HEAT > MAX-HEAT
               MOVE "FAILED" TO WS-STATUS
               MOVE 2 TO WS-STATUS-CODE
               MOVE "Y" TO WS-FAILED
           ELSE
               MOVE "PASSED" TO WS-STATUS
               MOVE 1 TO WS-STATUS-CODE
           END-IF
           PERFORM UPDATE-UI-ROW
           CALL "C$SLEEP" USING WS-SLEEP-SEC.

       QUALITY-INSPECTION.
           COMPUTE WS-QUALITY = FUNCTION RANDOM * 100
           MOVE "Quality (Min 70)" TO WS-FIELD-NAME
           MOVE 9 TO UI-LINE
           MOVE WS-QUALITY TO WS-FIELD-VALUE
           MOVE SPACES TO WS-FIELD-VALUE-DISPLAY
           MOVE WS-QUALITY TO WS-NUM-EDIT
           STRING FUNCTION TRIM(WS-NUM-EDIT) " %"
               DELIMITED BY SIZE INTO WS-FIELD-VALUE-DISPLAY
           IF WS-QUALITY < QUALITY-THRESH
               MOVE "FAILED" TO WS-STATUS
               MOVE 2 TO WS-STATUS-CODE
               MOVE "Y" TO WS-FAILED
           ELSE
               MOVE "PASSED" TO WS-STATUS
               MOVE 1 TO WS-STATUS-CODE
           END-IF
           PERFORM UPDATE-UI-ROW
           CALL "C$SLEEP" USING WS-SLEEP-SEC.

       SKIP-PRESSURE.
           MOVE "Pressure (80-120)" TO WS-FIELD-NAME
           MOVE 7 TO UI-LINE
           MOVE "---" TO WS-FIELD-VALUE-DISPLAY
           MOVE "SKIPPED" TO WS-STATUS
           MOVE 3 TO WS-STATUS-CODE
           PERFORM UPDATE-UI-ROW.

       SKIP-HEAT.
           MOVE "Heat (200-300)" TO WS-FIELD-NAME
           MOVE 8 TO UI-LINE
           MOVE "---" TO WS-FIELD-VALUE-DISPLAY
           MOVE "SKIPPED" TO WS-STATUS
           MOVE 3 TO WS-STATUS-CODE
           PERFORM UPDATE-UI-ROW.

       SKIP-QUALITY.
           MOVE "Quality (Min 70)" TO WS-FIELD-NAME
           MOVE 9 TO UI-LINE
           MOVE "---" TO WS-FIELD-VALUE-DISPLAY
           MOVE "SKIPPED" TO WS-STATUS
           MOVE 3 TO WS-STATUS-CODE
           PERFORM UPDATE-UI-ROW.

       FINALIZE.
           *> Consolidated display for efficiency
           IF WS-FAILED = "Y"
               DISPLAY WS-ESC "[11;19H" WS-ESC "[31m"
                       "[X] PROCESS FAILED                            "
                       WS-ESC "[0m" WITH NO ADVANCING
           ELSE
               DISPLAY WS-ESC "[11;19H" WS-ESC "[32m"
                       "[+] PROCESS COMPLETED SUCCESSFULLY            "
                       WS-ESC "[0m" WITH NO ADVANCING
           END-IF
           DISPLAY WS-ESC "[13;1H".

       SETUP-DATE.
           ACCEPT WS-DATE FROM DATE YYYYMMDD
           STRING WS-DATE(1:4) "/" WS-DATE(5:2) "/" WS-DATE(7:2) " "
               DELIMITED BY SIZE INTO WS-FORMATTED-DATE.
           *> Optimization: Pre-fill static parts of timestamp buffer
           MOVE WS-FORMATTED-DATE TO WS-TS-DATE.

       GET-TIMESTAMP.
       *> Optimization: Direct MOVES are faster than STRING concatenation
       *> Date and separators are already set by SETUP-DATE.
       ACCEPT WS-TIME FROM TIME
       MOVE WS-TIME(1:2) TO WS-TS-HH
       MOVE WS-TIME(3:2) TO WS-TS-MM
       MOVE WS-TIME(5:2) TO WS-TS-SS.

       END PROGRAM AEROSTEP-UI.
