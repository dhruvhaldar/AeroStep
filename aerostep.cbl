IDENTIFICATION DIVISION.
       PROGRAM-ID. AEROSTEP-UI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE ASSIGN TO "aerostep.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE.
       01  REPORT-RECORD               PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-FAILED                   PIC X VALUE "N".
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
       01 WS-FIELD-NAME              PIC X(30).
       01 WS-FIELD-VALUE             PIC 9(4).
       01 WS-FIELD-VALUE-DISPLAY    PIC X(4).
       01 WS-DATE                   PIC 9(8).
       01 WS-TIME                   PIC 9(6).

       01 UI-LINE                   PIC 9(2).

       01 WS-STATUS                 PIC X(10).
       01 WS-ESC                    PIC X VALUE X'1B'.

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           OPEN OUTPUT REPORT-FILE
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
           STOP RUN.
       
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
           IF WS-FIELD-NAME = "Initialization"
               MOVE 6 TO UI-LINE
           ELSE IF WS-FIELD-NAME = "Pressure Test"
               MOVE 7 TO UI-LINE
           ELSE IF WS-FIELD-NAME = "Heat Treatment"
               MOVE 8 TO UI-LINE
           ELSE IF WS-FIELD-NAME = "Quality Inspection"
               MOVE 9 TO UI-LINE
           END-IF

           STRING
               WS-FIELD-NAME DELIMITED BY SIZE
               " | " WS-STATUS DELIMITED BY SIZE
               " | " WS-FIELD-VALUE-DISPLAY DELIMITED BY SIZE
               " | " WS-BASE-TIMESTAMP DELIMITED BY SIZE
               INTO WS-TIMESTAMP

           DISPLAY WS-TIMESTAMP
           .

       INITIALIZATION.
           MOVE "Initialization" TO WS-FIELD-NAME
           PERFORM GET-TIMESTAMP
           MOVE WS-BASE-TIMESTAMP TO WS-TIMESTAMP
           DISPLAY WS-TIMESTAMP
           DISPLAY " - Initialization started"
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
           IF WS-FAILED = "Y"
               DISPLAY "+------------------------------------------------------------------------------+"
               DISPLAY "| OVERALL STATUS: PROCESS FAILED                                              |"
               DISPLAY "+------------------------------------------------------------------------------+"
           ELSE
               DISPLAY "+------------------------------------------------------------------------------+"
               DISPLAY "| OVERALL STATUS: PROCESS COMPLETED SUCCESSFULLY                              |"
               DISPLAY "+------------------------------------------------------------------------------+"
           END-IF.

       GET-TIMESTAMP.
       ACCEPT WS-DATE FROM DATE YYYYMMDD
       ACCEPT WS-TIME FROM TIME
       DISPLAY "WS-TIME = " WS-TIME
       STRING WS-DATE(1:4) "/" WS-DATE(5:2) "/" WS-DATE(7:2) " "
               WS-TIME(1:2) ":" WS-TIME(3:2) ":" WS-TIME(5:2)
           DELIMITED BY SIZE INTO WS-BASE-TIMESTAMP.

       END PROGRAM AEROSTEP-UI.
