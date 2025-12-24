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
       FD  REPORT-FILE.
       01  REPORT-RECORD               PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS              PIC XX.
       01 WS-FAILED                   PIC X VALUE "N".

       *> Login Variables
       01 WS-OPERATOR-ID              PIC X(20).
       01 WS-ACCESS-CODE              PIC X(20).
       01 WS-EXPECTED-CODE            PIC X(20).
       01 WS-ENV-CODE                 PIC X(20).

       01 WS-PRESSURE                 PIC 9(4).
       01 MIN-PRESS                  PIC 9(4) VALUE 80.
       01 MAX-PRESS                  PIC 9(4) VALUE 120.
       01 WS-HEAT                    PIC 9(4).
       01 MIN-HEAT                   PIC 9(4) VALUE 200.
       01 MAX-HEAT                   PIC 9(4) VALUE 300.
       01 WS-QUALITY                 PIC 9(3).
       01 QUALITY-THRESH             PIC 9(3) VALUE 70.

       01 WS-TIMESTAMP               PIC X(80).
       01 WS-BASE-TIMESTAMP          PIC X(20).
       01 WS-LOG-MESSAGE             PIC X(39).
       01 WS-FIELD-NAME              PIC X(30).
       01 WS-FIELD-VALUE             PIC 9(4).
       01 WS-FIELD-VALUE-DISPLAY    PIC X(4).
       01 WS-DATE                   PIC 9(8).
       01 WS-TIME                   PIC 9(6).

       01 UI-LINE                   PIC 9(2) VALUE 0.

       01 WS-STATUS                 PIC X(10).
       01 WS-ESC                    PIC X VALUE X'1B'.

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           OPEN EXTEND REPORT-FILE
           IF WS-FILE-STATUS NOT = "00"
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
           DISPLAY "+------------------------------------------------------------------------------+"
           DISPLAY "|                        SECURITY ACCESS CONTROL                               |"
           DISPLAY "+------------------------------------------------------------------------------+"
           DISPLAY " "
           DISPLAY "   OPERATOR IDENTIFICATION REQUIRED"
           DISPLAY " "
           DISPLAY "   OPERATOR ID: " WITH NO ADVANCING
           ACCEPT WS-OPERATOR-ID
           DISPLAY " "
           DISPLAY "   ACCESS CODE: " WITH NO ADVANCING
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

           IF WS-ACCESS-CODE = WS-EXPECTED-CODE THEN
               MOVE "LOGIN SUCCESSFUL" TO WS-LOG-MESSAGE
               PERFORM WRITE-LOG-ENTRY
               DISPLAY "   ACCESS GRANTED."
               CALL "C$SLEEP" USING 1
           ELSE
               MOVE "LOGIN FAILED - ACCESS DENIED" TO WS-LOG-MESSAGE
               PERFORM WRITE-LOG-ENTRY
               DISPLAY "   ACCESS DENIED."
               CLOSE REPORT-FILE
               STOP RUN
           END-IF.
       
       CLEAR-SCREEN.
           *> Clear screen using ANSI escape sequence (GnuCOBOL compatible)
           DISPLAY WS-ESC "[2J" WITH NO ADVANCING
           DISPLAY WS-ESC "[H" WITH NO ADVANCING.
       
       DRAW-UI-SHELL.
           PERFORM CLEAR-SCREEN
           DISPLAY "+------------------------------------------------------------------------------+"
           DISPLAY "|                        AEROSTEP TESTING INTERFACE                            |"
           DISPLAY "+------------------------------------------------------------------------------+"
           DISPLAY "| Step                 | Status    | Value   | Timestamp                         |"
           DISPLAY "+----------------------+-----------+---------+----------------------------------+"
           DISPLAY "| Initialization       |           |         |                                  |"
           DISPLAY "| Pressure Test        |           |         |                                  |"
           DISPLAY "| Heat Treatment       |           |         |                                  |"
           DISPLAY "| Quality Inspection   |           |         |                                  |"
           DISPLAY "+----------------------+-----------+---------+----------------------------------+"
           DISPLAY "| Overall Status:                                                       |"
           DISPLAY "+------------------------------------------------------------------------------+".

       UPDATE-UI-ROW.
           MOVE 0 TO UI-LINE

           *> Debugging with EVALUATE
           EVALUATE WS-FIELD-NAME
               WHEN "Initialization"
                   MOVE 6 TO UI-LINE
               WHEN "Pressure Test"
                   MOVE 7 TO UI-LINE
               WHEN "Heat Treatment"
                   MOVE 8 TO UI-LINE
               WHEN "Quality Inspection"
                   MOVE 9 TO UI-LINE
           END-EVALUATE

           IF UI-LINE > 0
               *> Position and print Step Name (Col 3)
               DISPLAY WS-ESC "[" UI-LINE ";3H" WITH NO ADVANCING
               DISPLAY WS-FIELD-NAME(1:20) WITH NO ADVANCING

               *> Position and print Status (Col 26)
               DISPLAY WS-ESC "[" UI-LINE ";26H" WITH NO ADVANCING
               DISPLAY WS-STATUS WITH NO ADVANCING

               *> Position and print Value (Col 38)
               DISPLAY WS-ESC "[" UI-LINE ";38H" WITH NO ADVANCING
               DISPLAY WS-FIELD-VALUE-DISPLAY WITH NO ADVANCING

               *> Position and print Timestamp (Col 48)
               DISPLAY WS-ESC "[" UI-LINE ";48H" WITH NO ADVANCING
               DISPLAY WS-BASE-TIMESTAMP WITH NO ADVANCING
           END-IF
           .

       INITIALIZATION.
           MOVE "Initialization" TO WS-FIELD-NAME
           PERFORM GET-TIMESTAMP
           MOVE WS-BASE-TIMESTAMP TO WS-TIMESTAMP
           MOVE SPACES TO WS-FIELD-VALUE-DISPLAY
           MOVE "PASSED" TO WS-STATUS
           PERFORM UPDATE-UI-ROW.

       PRESSURE-TEST.
           COMPUTE WS-PRESSURE = FUNCTION RANDOM * (MAX-PRESS - MIN-PRESS + 1) + MIN-PRESS
           MOVE "Pressure Test" TO WS-FIELD-NAME
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
           MOVE "Heat Treatment" TO WS-FIELD-NAME
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
           MOVE "Quality Inspection" TO WS-FIELD-NAME
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
           PERFORM GET-TIMESTAMP
           *> Position cursor to Overall Status line (Line 11), after label (Col 19)
           DISPLAY WS-ESC "[11;19H" WITH NO ADVANCING
           IF WS-FAILED = "Y"
               MOVE "PROCESS FAILED" TO WS-LOG-MESSAGE
               DISPLAY "PROCESS FAILED                                " WITH NO ADVANCING
           ELSE
               MOVE "PROCESS COMPLETED SUCCESSFULLY" TO WS-LOG-MESSAGE
               DISPLAY "PROCESS COMPLETED SUCCESSFULLY                " WITH NO ADVANCING
           END-IF
           PERFORM WRITE-LOG-ENTRY
           *> Move cursor below table (Line 13) to exit cleanly
           DISPLAY WS-ESC "[13;1H".

       WRITE-LOG-ENTRY.
           PERFORM GET-TIMESTAMP
           MOVE WS-BASE-TIMESTAMP TO REPORT-RECORD(1:19)
           MOVE " " TO REPORT-RECORD(20:1)
           MOVE WS-OPERATOR-ID TO REPORT-RECORD(21:20)
           MOVE " " TO REPORT-RECORD(41:1)
           MOVE WS-LOG-MESSAGE TO REPORT-RECORD(42:39)
           WRITE REPORT-RECORD.

       GET-TIMESTAMP.
       ACCEPT WS-DATE FROM DATE YYYYMMDD
       ACCEPT WS-TIME FROM TIME
       STRING WS-DATE(1:4) "/" WS-DATE(5:2) "/" WS-DATE(7:2) " "
               WS-TIME(1:2) ":" WS-TIME(3:2) ":" WS-TIME(5:2)
           DELIMITED BY SIZE INTO WS-BASE-TIMESTAMP.

       END PROGRAM AEROSTEP-UI.
