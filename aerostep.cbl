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

       01 WS-BASE-TIMESTAMP          PIC X(20).
       01 WS-FIELD-NAME              PIC X(30).
       01 WS-FIELD-VALUE             PIC 9(4).
       01 WS-FIELD-VALUE-DISPLAY    PIC X(4).
       01 WS-DATE                   PIC 9(8).
       01 WS-TIME                   PIC 9(8).

       01 UI-LINE                   PIC 9(2) VALUE 0.

       01 WS-STATUS                 PIC X(10).
       01 WS-ESC                    PIC X VALUE X'1B'.

       *> Buffers for UI optimization
       01 WS-UI-ROW-BUFFER           PIC X(200).
       01 WS-PTR                     PIC 9(3).
       01 WS-LOG-PTR                 PIC 9(3).

       *> Unicode Box Drawing Characters (UTF-8 Hex)
       01 BOX-H                      PIC X(3) VALUE X'E29480'. *> ─
       01 BOX-V                      PIC X(3) VALUE X'E29482'. *> │
       01 BOX-TL                     PIC X(3) VALUE X'E2948C'. *> ┌
       01 BOX-TR                     PIC X(3) VALUE X'E29490'. *> ┐
       01 BOX-BL                     PIC X(3) VALUE X'E29494'. *> └
       01 BOX-BR                     PIC X(3) VALUE X'E29498'. *> ┘
       01 BOX-T-DOWN                 PIC X(3) VALUE X'E294AC'. *> ┬
       01 BOX-T-UP                   PIC X(3) VALUE X'E294B4'. *> ┴
       01 BOX-T-RIGHT                PIC X(3) VALUE X'E2949C'. *> ├
       01 BOX-T-LEFT                 PIC X(3) VALUE X'E294A4'. *> ┤
       01 BOX-CROSS                  PIC X(3) VALUE X'E294BC'. *> ┼

       *> Pre-constructed Unicode Lines (calculated for 80 cols)
       *> Top Border: ┌ + 78*─ + ┐
       01 WS-BOX-TOP                 PIC X(240).
       *> Bottom Border: └ + 78*─ + ┘
       01 WS-BOX-BOTTOM              PIC X(240).
       *> Table Header Sep: ├ + 22*─ + ┼ + 11*─ + ┼ + 9*─ + ┼ + 33*─ + ┤
       01 WS-TABLE-DIV               PIC X(240).
       *> Temp Line for initialization
       01 WS-LINE-TEMP               PIC X(240).

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           PERFORM INITIALIZE-UI-RESOURCES
           OPEN EXTEND REPORT-FILE
           IF WS-FILE-STATUS NOT = "00" AND WS-FILE-STATUS NOT = "05"
               DISPLAY "CRITICAL ERROR: CANNOT OPEN LOG FILE. STATUS: " WS-FILE-STATUS
               STOP RUN
           END-IF

           PERFORM LOGIN-SEQUENCE

           PERFORM DRAW-UI-SHELL

           PERFORM INITIALIZATION
           IF WS-FAILED NOT = "Y"
               PERFORM PRESSURE-TEST
           END-IF
           IF WS-FAILED NOT = "Y"
               PERFORM HEAT-TREATMENT
           END-IF
           IF WS-FAILED NOT = "Y"
               PERFORM QUALITY-INSPECTION
           END-IF

           PERFORM FINALIZE

           CLOSE REPORT-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "WARNING: ERROR CLOSING LOG FILE. STATUS: " WS-FILE-STATUS
           END-IF
           STOP RUN.

       LOGIN-SEQUENCE.
           PERFORM CLEAR-SCREEN
           DISPLAY WS-BOX-TOP(1:240)
           DISPLAY BOX-V "                        SECURITY ACCESS CONTROL                               " BOX-V
           DISPLAY WS-BOX-BOTTOM(1:240)
           DISPLAY " "
           DISPLAY "   OPERATOR IDENTIFICATION REQUIRED"
           DISPLAY " "
           DISPLAY "   OPERATOR ID: " WITH NO ADVANCING
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

           ACCEPT WS-ENV-CODE FROM ENVIRONMENT "AERO_PASS"
           IF WS-ENV-CODE = SPACES THEN
               DISPLAY "CRITICAL SECURITY ERROR: SECURITY CONFIGURATION MISSING."
               DISPLAY "SYSTEM HALTED."
               STOP RUN
           ELSE
               MOVE WS-ENV-CODE TO WS-EXPECTED-CODE
           END-IF

           PERFORM GET-TIMESTAMP

           IF WS-ACCESS-CODE = WS-EXPECTED-CODE THEN
               DISPLAY "   " WS-ESC "[32m" "[+] ACCESS GRANTED." WS-ESC "[0m"
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
       
       CLEAR-SCREEN.
           *> Clear screen using ANSI escape sequence (GnuCOBOL compatible)
           DISPLAY WS-ESC "[2J" WS-ESC "[H" WITH NO ADVANCING.
       
       DRAW-UI-SHELL.
           PERFORM CLEAR-SCREEN
           *> Optimization: Consolidated DISPLAY calls to reduce I/O overhead
           DISPLAY WS-BOX-TOP(1:240)
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
                   WS-BOX-BOTTOM(1:240).

       UPDATE-UI-ROW.
           IF UI-LINE > 0
               PERFORM GET-TIMESTAMP
               *> Optimized: Direct buffering to avoid intermediate copies and reduce STRING overhead
               MOVE 1 TO WS-PTR

               *> 1. Name
               STRING WS-ESC "[" UI-LINE ";3H"
                      WS-FIELD-NAME(1:20)
                      WS-ESC "[" UI-LINE ";26H"
                      DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER
                      WITH POINTER WS-PTR

               *> 2. Status with Colors (Merged logic to avoid WS-STATUS-BUFFER)
               IF WS-STATUS(1:6) = "FAILED"
                   STRING WS-ESC "[31m[X] " WS-STATUS(1:6) WS-ESC "[0m"
                       DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER
                       WITH POINTER WS-PTR
               ELSE
                   IF WS-STATUS(1:6) = "PASSED"
                       STRING WS-ESC "[32m[+] " WS-STATUS(1:6) WS-ESC "[0m"
                           DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER
                           WITH POINTER WS-PTR
                   ELSE
                       STRING WS-ESC "[37m" WS-STATUS WS-ESC "[0m"
                           DELIMITED BY SIZE INTO WS-UI-ROW-BUFFER
                           WITH POINTER WS-PTR
                   END-IF
               END-IF

               *> 3. Value and Timestamp
               STRING WS-ESC "[" UI-LINE ";38H"
                      WS-FIELD-VALUE-DISPLAY
                      WS-ESC "[" UI-LINE ";48H"
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

       INITIALIZE-UI-RESOURCES.
           *> Initialize 78 chars of horizontal line
           MOVE ALL X'E29480' TO WS-LINE-TEMP

           *> Construct Top Box Line (80 cols)
           STRING BOX-TL DELIMITED BY SIZE
                  WS-LINE-TEMP(1:234) DELIMITED BY SIZE
                  BOX-TR DELIMITED BY SIZE
                  INTO WS-BOX-TOP

           *> Construct Bottom Box Line (80 cols)
           STRING BOX-BL DELIMITED BY SIZE
                  WS-LINE-TEMP(1:234) DELIMITED BY SIZE
                  BOX-BR DELIMITED BY SIZE
                  INTO WS-BOX-BOTTOM

           *> Construct Table Divider (80 cols)
           *> Layout: 22 | 11 | 9 | 33
           STRING BOX-T-RIGHT DELIMITED BY SIZE
                  WS-LINE-TEMP(1:66) DELIMITED BY SIZE
                  BOX-CROSS DELIMITED BY SIZE
                  WS-LINE-TEMP(1:33) DELIMITED BY SIZE
                  BOX-CROSS DELIMITED BY SIZE
                  WS-LINE-TEMP(1:27) DELIMITED BY SIZE
                  BOX-CROSS DELIMITED BY SIZE
                  WS-LINE-TEMP(1:99) DELIMITED BY SIZE
                  BOX-T-LEFT DELIMITED BY SIZE
                  INTO WS-TABLE-DIV.

       INITIALIZATION.
           MOVE "Initialization" TO WS-FIELD-NAME
           MOVE 6 TO UI-LINE
           MOVE SPACES TO WS-FIELD-VALUE-DISPLAY
           MOVE "PASSED" TO WS-STATUS
           PERFORM UPDATE-UI-ROW.

       PRESSURE-TEST.
           COMPUTE WS-PRESSURE = FUNCTION RANDOM * (MAX-PRESS - MIN-PRESS + 1) + MIN-PRESS
           MOVE "Pressure (80-120)" TO WS-FIELD-NAME
           MOVE 7 TO UI-LINE
           MOVE WS-PRESSURE TO WS-FIELD-VALUE
           MOVE WS-PRESSURE TO WS-FIELD-VALUE-DISPLAY
           IF WS-PRESSURE < MIN-PRESS OR WS-PRESSURE > MAX-PRESS
               MOVE "FAILED" TO WS-STATUS
               MOVE "Y" TO WS-FAILED
           ELSE
               MOVE "PASSED" TO WS-STATUS
           END-IF
           PERFORM UPDATE-UI-ROW.

       HEAT-TREATMENT.
           COMPUTE WS-HEAT = FUNCTION RANDOM * (MAX-HEAT - MIN-HEAT + 1) + MIN-HEAT
           MOVE "Heat (200-300)" TO WS-FIELD-NAME
           MOVE 8 TO UI-LINE
           MOVE WS-HEAT TO WS-FIELD-VALUE
           MOVE WS-HEAT TO WS-FIELD-VALUE-DISPLAY
           IF WS-HEAT < MIN-HEAT OR WS-HEAT > MAX-HEAT
               MOVE "FAILED" TO WS-STATUS
               MOVE "Y" TO WS-FAILED
           ELSE
               MOVE "PASSED" TO WS-STATUS
           END-IF
           PERFORM UPDATE-UI-ROW.

       QUALITY-INSPECTION.
           COMPUTE WS-QUALITY = FUNCTION RANDOM * 100
           MOVE "Quality (Min 70)" TO WS-FIELD-NAME
           MOVE 9 TO UI-LINE
           MOVE WS-QUALITY TO WS-FIELD-VALUE
           MOVE WS-QUALITY TO WS-FIELD-VALUE-DISPLAY
           IF WS-QUALITY < QUALITY-THRESH
               MOVE "FAILED" TO WS-STATUS
               MOVE "Y" TO WS-FAILED
           ELSE
               MOVE "PASSED" TO WS-STATUS
           END-IF
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

       GET-TIMESTAMP.
       ACCEPT WS-DATE FROM DATE YYYYMMDD
       ACCEPT WS-TIME FROM TIME
       STRING WS-DATE(1:4) "/" WS-DATE(5:2) "/" WS-DATE(7:2) " "
               WS-TIME(1:2) ":" WS-TIME(3:2) ":" WS-TIME(5:2)
           DELIMITED BY SIZE INTO WS-BASE-TIMESTAMP.

       END PROGRAM AEROSTEP-UI.
