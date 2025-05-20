       IDENTIFICATION DIVISION.
       PROGRAM-ID. AEROSTEP.

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
       01  WS-FAILED                   PIC X VALUE "N".
       01  WS-PRESSURE                 PIC 9(4).
       01  MIN-PRESS                   PIC 9(4) VALUE 80.
       01  MAX-PRESS                   PIC 9(4) VALUE 120.
       01  WS-HEAT                     PIC 9(4).
       01  MIN-HEAT                    PIC 9(4) VALUE 200.
       01  MAX-HEAT                    PIC 9(4) VALUE 300.
       01  WS-QUALITY                  PIC 9(3).
       01  QUALITY-THRESH              PIC 9(3) VALUE 70.

       01  WS-CURRENT-DATE.
           05 WS-CURR-YEAR             PIC 9(4).
           05 WS-CURR-MONTH            PIC 9(2).
           05 WS-CURR-DAY              PIC 9(2).
           05 WS-CURR-HOUR             PIC 9(2).
           05 WS-CURR-MINUTE           PIC 9(2).
           05 WS-CURR-SECOND           PIC 9(2).
           05 FILLER                   PIC X(2).

       01  WS-TIMESTAMP                PIC X(80).
       01  WS-TIME                     PIC 9(6).
       01  WS-FIELD-NAME               PIC X(30).
       01  WS-FIELD-VALUE              PIC 9(4).
       01  WS-FIELD-VALUE-DISPLAY      PIC Z(4).
       01  WS-BASE-TIMESTAMP           PIC X(20).
       01  WS-DATE                     PIC 9(8).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN OUTPUT REPORT-FILE

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

       INITIALIZATION.
           PERFORM GET-TIMESTAMP
           STRING WS-BASE-TIMESTAMP " - Initialization started"
               DELIMITED BY SIZE INTO WS-TIMESTAMP
           DISPLAY WS-TIMESTAMP
           MOVE WS-TIMESTAMP TO REPORT-RECORD
           WRITE REPORT-RECORD.

       PRESSURE-TEST.
           COMPUTE WS-PRESSURE = FUNCTION RANDOM * (MAX-PRESS - MIN-PRESS + 1) + MIN-PRESS
           MOVE "Pressure Test" TO WS-FIELD-NAME
           MOVE WS-PRESSURE TO WS-FIELD-VALUE
           IF WS-PRESSURE < MIN-PRESS OR WS-PRESSURE > MAX-PRESS
               PERFORM LOG-FAILURE
               MOVE "Y" TO WS-FAILED
           ELSE
               PERFORM LOG-SUCCESS
           END-IF.

       HEAT-TREATMENT.
           COMPUTE WS-HEAT = FUNCTION RANDOM * (MAX-HEAT - MIN-HEAT + 1) + MIN-HEAT
           MOVE "Heat Treatment" TO WS-FIELD-NAME
           MOVE WS-HEAT TO WS-FIELD-VALUE
           IF WS-HEAT < MIN-HEAT OR WS-HEAT > MAX-HEAT
               PERFORM LOG-FAILURE
               MOVE "Y" TO WS-FAILED
           ELSE
               PERFORM LOG-SUCCESS
           END-IF.

       QUALITY-INSPECTION.
           COMPUTE WS-QUALITY = FUNCTION RANDOM * 100
           MOVE "Quality Inspection" TO WS-FIELD-NAME
           MOVE WS-QUALITY TO WS-FIELD-VALUE
           IF WS-QUALITY < QUALITY-THRESH
               PERFORM LOG-FAILURE
               MOVE "Y" TO WS-FAILED
           ELSE
               PERFORM LOG-SUCCESS
           END-IF.

       FINALIZE.
           PERFORM GET-TIMESTAMP
           IF WS-FAILED = "Y"
               STRING WS-TIMESTAMP " - PROCESS FAILED"
                   DELIMITED BY SIZE INTO WS-TIMESTAMP
           ELSE
               STRING WS-TIMESTAMP " - Process completed successfully"
                   DELIMITED BY SIZE INTO WS-TIMESTAMP
           END-IF
           DISPLAY WS-TIMESTAMP
           MOVE WS-TIMESTAMP TO REPORT-RECORD
           WRITE REPORT-RECORD.

       GET-TIMESTAMP.
           ACCEPT WS-DATE FROM DATE YYYYMMDD
           ACCEPT WS-TIME FROM TIME
           STRING
               WS-DATE(1:4) "/" WS-DATE(5:2) "/" WS-DATE(7:2) " "
               WS-TIME(1:2) ":" WS-TIME(3:2) ":" WS-TIME(5:2)
               DELIMITED BY SIZE INTO WS-BASE-TIMESTAMP.

       LOG-SUCCESS.
           PERFORM GET-TIMESTAMP
           MOVE WS-FIELD-VALUE TO WS-FIELD-VALUE-DISPLAY
           STRING
               WS-BASE-TIMESTAMP " - " WS-FIELD-NAME
               " PASSED (Value=" WS-FIELD-VALUE-DISPLAY ")"
               DELIMITED BY SIZE INTO WS-TIMESTAMP
           DISPLAY WS-TIMESTAMP
           MOVE WS-TIMESTAMP TO REPORT-RECORD
           WRITE REPORT-RECORD.

       LOG-FAILURE.
           PERFORM GET-TIMESTAMP
           MOVE WS-FIELD-VALUE TO WS-FIELD-VALUE-DISPLAY
           STRING
               WS-BASE-TIMESTAMP " - " WS-FIELD-NAME
               " FAILED (Value=" WS-FIELD-VALUE-DISPLAY ")"
               DELIMITED BY SIZE INTO WS-TIMESTAMP
           DISPLAY WS-TIMESTAMP
           MOVE WS-TIMESTAMP TO REPORT-RECORD
           WRITE REPORT-RECORD.
       END PROGRAM AEROSTEP.
