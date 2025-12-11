IDENTIFICATION DIVISION.
       PROGRAM-ID. AEROSTEP-UI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE ASSIGN TO "aerostep.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BATCH-FILE ASSIGN TO "batch_input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE.
       01  REPORT-RECORD               PIC X(120).

       FD  BATCH-FILE.
       01  BATCH-RECORD.
           05 COMPONENT-ID             PIC X(10).

       WORKING-STORAGE SECTION.
       01  WS-EOF                      PIC X VALUE "N".
       01  WS-FAILED                   PIC X VALUE "N".

       *> Test Variables
       01  WS-PRESSURE                 PIC 9(4).
       01  MIN-PRESS                   PIC 9(4) VALUE 80.
       01  MAX-PRESS                   PIC 9(4) VALUE 120.

       01  WS-HEAT                     PIC 9(4).
       01  MIN-HEAT                    PIC 9(4) VALUE 200.
       01  MAX-HEAT                    PIC 9(4) VALUE 300.

       01  WS-VIBRATION                PIC 99V99.
       01  MAX-VIBRATION               PIC 99V99 VALUE 20.00.
       01  WS-VIBRATION-DISP           PIC 99.99.

       01  WS-QUALITY                  PIC 9(3).
       01  QUALITY-THRESH              PIC 9(3) VALUE 70.

       *> Stats
       01  WS-STATS.
           05 STAT-TOTAL               PIC 9(4) VALUE 0.
           05 STAT-PASSED              PIC 9(4) VALUE 0.
           05 STAT-FAILED              PIC 9(4) VALUE 0.

       *> UI & Time
       01  WS-TIMESTAMP                PIC X(20).
       01  WS-DATE                     PIC 9(8).
       01  WS-TIME                     PIC 9(6).
       01  WS-SEED                     PIC 9(9).

       *> Display Helpers
       01  WS-FIELD-NAME               PIC X(30).
       01  WS-STATUS                   PIC X(10).
       01  WS-VAL-DISP                 PIC X(10).
       01  WS-ROW                      PIC 9(2).
       01  WS-COL                      PIC 9(2).

       *> ANSI Escape Codes
       01  WS-ESC                      PIC X VALUE X'1B'.
       01  WS-CSI                      PIC X(2).
       01  WS-POS-STR                  PIC X(10).

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           MOVE X'1B5B' TO WS-CSI.

           OPEN OUTPUT REPORT-FILE
           OPEN INPUT BATCH-FILE

           *> Seed random number generator
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME TO WS-SEED
           COMPUTE WS-PRESSURE = FUNCTION RANDOM(WS-SEED)

           PERFORM DRAW-UI-SHELL

           PERFORM UNTIL WS-EOF = "Y"
               READ BATCH-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       ADD 1 TO STAT-TOTAL
                       PERFORM PROCESS-COMPONENT
                       *> Sleep for visualization (approx 1 sec)
                       CALL "C$SLEEP" USING 1
               END-READ
           END-PERFORM

           PERFORM DISPLAY-SUMMARY

           CLOSE BATCH-FILE
           CLOSE REPORT-FILE

           *> Move cursor to bottom to exit cleanly
           DISPLAY WS-CSI "16;1H" WITH NO ADVANCING
           STOP RUN.

       PROCESS-COMPONENT.
           MOVE "N" TO WS-FAILED

           *> Clear previous values in UI
           PERFORM CLEAR-UI-VALUES

           *> Update Component Header
           PERFORM UPDATE-COMPONENT-HEADER

           *> Initialization
           PERFORM INITIALIZATION

           *> Tests
           IF WS-FAILED = "N" PERFORM PRESSURE-TEST END-IF
           IF WS-FAILED = "N" PERFORM HEAT-TREATMENT END-IF
           IF WS-FAILED = "N" PERFORM VIBRATION-TEST END-IF
           IF WS-FAILED = "N" PERFORM QUALITY-INSPECTION END-IF

           *> Final status for this component
           PERFORM UPDATE-OVERALL-STATUS.

       INITIALIZATION.
           MOVE "Initialization" TO WS-FIELD-NAME
           MOVE 6 TO WS-ROW
           PERFORM GET-TIMESTAMP
           MOVE "PASSED" TO WS-STATUS
           MOVE "   -   " TO WS-VAL-DISP
           PERFORM UPDATE-UI-ROW
           PERFORM LOG-RESULT.

       PRESSURE-TEST.
           MOVE "Pressure Test" TO WS-FIELD-NAME
           MOVE 7 TO WS-ROW
           COMPUTE WS-PRESSURE = FUNCTION RANDOM * (MAX-PRESS - MIN-PRESS + 1) + MIN-PRESS
           MOVE WS-PRESSURE TO WS-VAL-DISP

           PERFORM GET-TIMESTAMP
           IF WS-PRESSURE < MIN-PRESS OR WS-PRESSURE > MAX-PRESS
               MOVE "FAILED" TO WS-STATUS
               MOVE "Y" TO WS-FAILED
           ELSE
               MOVE "PASSED" TO WS-STATUS
           END-IF
           PERFORM UPDATE-UI-ROW
           PERFORM LOG-RESULT.

       HEAT-TREATMENT.
           MOVE "Heat Treatment" TO WS-FIELD-NAME
           MOVE 8 TO WS-ROW
           COMPUTE WS-HEAT = FUNCTION RANDOM * (MAX-HEAT - MIN-HEAT + 1) + MIN-HEAT
           MOVE WS-HEAT TO WS-VAL-DISP

           PERFORM GET-TIMESTAMP
           IF WS-HEAT < MIN-HEAT OR WS-HEAT > MAX-HEAT
               MOVE "FAILED" TO WS-STATUS
               MOVE "Y" TO WS-FAILED
           ELSE
               MOVE "PASSED" TO WS-STATUS
           END-IF
           PERFORM UPDATE-UI-ROW
           PERFORM LOG-RESULT.

       VIBRATION-TEST.
           MOVE "Vibration Test" TO WS-FIELD-NAME
           MOVE 9 TO WS-ROW
           *> Generate vibration 0.00 to 30.00
           COMPUTE WS-VIBRATION = FUNCTION RANDOM * 30
           MOVE WS-VIBRATION TO WS-VIBRATION-DISP
           MOVE WS-VIBRATION-DISP TO WS-VAL-DISP

           PERFORM GET-TIMESTAMP
           IF WS-VIBRATION > MAX-VIBRATION
               MOVE "FAILED" TO WS-STATUS
               MOVE "Y" TO WS-FAILED
           ELSE
               MOVE "PASSED" TO WS-STATUS
           END-IF
           PERFORM UPDATE-UI-ROW
           PERFORM LOG-RESULT.

       QUALITY-INSPECTION.
           MOVE "Quality Insp." TO WS-FIELD-NAME
           MOVE 10 TO WS-ROW
           COMPUTE WS-QUALITY = FUNCTION RANDOM * 100
           MOVE WS-QUALITY TO WS-VAL-DISP

           PERFORM GET-TIMESTAMP
           IF WS-QUALITY < QUALITY-THRESH
               MOVE "FAILED" TO WS-STATUS
               MOVE "Y" TO WS-FAILED
           ELSE
               MOVE "PASSED" TO WS-STATUS
           END-IF
           PERFORM UPDATE-UI-ROW
           PERFORM LOG-RESULT.

       UPDATE-OVERALL-STATUS.
           MOVE SPACES TO REPORT-RECORD
           IF WS-FAILED = "Y"
               ADD 1 TO STAT-FAILED
               MOVE "FAILED" TO WS-STATUS
               STRING "| OVERALL: " COMPONENT-ID " FAILED                                            |"
                   INTO REPORT-RECORD
               DISPLAY WS-CSI "13;1H" WITH NO ADVANCING
               DISPLAY REPORT-RECORD WITH NO ADVANCING
           ELSE
               ADD 1 TO STAT-PASSED
               MOVE "PASSED" TO WS-STATUS
               STRING "| OVERALL: " COMPONENT-ID " PASSED                                            |"
                   INTO REPORT-RECORD
               DISPLAY WS-CSI "13;1H" WITH NO ADVANCING
               DISPLAY REPORT-RECORD WITH NO ADVANCING
           END-IF.

       DRAW-UI-SHELL.
           DISPLAY WS-CSI "2J" WITH NO ADVANCING
           DISPLAY WS-CSI "H" WITH NO ADVANCING
           DISPLAY "+------------------------------------------------------------------------------+"
           DISPLAY "|                   AEROSTEP BATCH PROCESSING SYSTEM                           |"
           DISPLAY "+------------------------------------------------------------------------------+"
           DISPLAY "| Step                 | Status    | Value      | Timestamp                    |"
           DISPLAY "+----------------------+-----------+------------+------------------------------+"
           DISPLAY "| Initialization       |           |            |                              |"
           DISPLAY "| Pressure Test        |           |            |                              |"
           DISPLAY "| Heat Treatment       |           |            |                              |"
           DISPLAY "| Vibration Test       |           |            |                              |"
           DISPLAY "| Quality Insp.        |           |            |                              |"
           DISPLAY "+----------------------+-----------+------------+------------------------------+"
           DISPLAY "|                                                                              |"
           DISPLAY "+------------------------------------------------------------------------------+".

       UPDATE-COMPONENT-HEADER.
           DISPLAY WS-CSI "2;22H" WITH NO ADVANCING
           DISPLAY "COMPONENT: " COMPONENT-ID WITH NO ADVANCING.

       CLEAR-UI-VALUES.
           *> Clear Status, Value, Timestamp columns for rows 6-10
           PERFORM VARYING WS-ROW FROM 6 BY 1 UNTIL WS-ROW > 10
               DISPLAY WS-CSI WS-ROW ";26H" WITH NO ADVANCING
               DISPLAY "           |            |                              |" WITH NO ADVANCING
           END-PERFORM.

       UPDATE-UI-ROW.
           *> Move to Status Column (26)
           DISPLAY WS-CSI WS-ROW ";26H" WITH NO ADVANCING
           DISPLAY WS-STATUS WITH NO ADVANCING

           *> Move to Value Column (38)
           DISPLAY WS-CSI WS-ROW ";38H" WITH NO ADVANCING
           DISPLAY WS-VAL-DISP WITH NO ADVANCING

           *> Move to Timestamp Column (51)
           DISPLAY WS-CSI WS-ROW ";51H" WITH NO ADVANCING
           DISPLAY WS-TIMESTAMP WITH NO ADVANCING.

       GET-TIMESTAMP.
           ACCEPT WS-DATE FROM DATE YYYYMMDD
           ACCEPT WS-TIME FROM TIME
           STRING WS-DATE(1:4) "/" WS-DATE(5:2) "/" WS-DATE(7:2) " "
                  WS-TIME(1:2) ":" WS-TIME(3:2) ":" WS-TIME(5:2)
                  DELIMITED BY SIZE INTO WS-TIMESTAMP.

       LOG-RESULT.
           MOVE SPACES TO REPORT-RECORD
           STRING COMPONENT-ID ";" WS-FIELD-NAME ";" WS-STATUS ";" WS-VAL-DISP ";" WS-TIMESTAMP
                  DELIMITED BY SIZE INTO REPORT-RECORD
           WRITE REPORT-RECORD.

       DISPLAY-SUMMARY.
           DISPLAY WS-CSI "15;1H" WITH NO ADVANCING
           DISPLAY "BATCH SUMMARY: Total: " STAT-TOTAL
                   " | Passed: " STAT-PASSED
                   " | Failed: " STAT-FAILED.

       END PROGRAM AEROSTEP-UI.
