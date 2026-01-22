       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAP.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
          01 WS-YEAR        PIC 9(4).
          01 WS-RESULT      PIC 9 VALUE 0.
          01 WS-REMAINDER   PIC 9 VALUE 0.
          01 WS-TEMP        PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       LEAP.
           DIVIDE WS-YEAR BY 400 GIVING WS-TEMP REMAINDER WS-REMAINDER
           IF WS-REMAINDER = 0
               MOVE 1 TO WS-RESULT
           ELSE
               DIVIDE WS-YEAR BY 100 GIVING WS-TEMP REMAINDER WS-REMAINDER
               IF WS-REMAINDER = 0
                   MOVE 0 TO WS-RESULT
               ELSE
                   DIVIDE WS-YEAR BY 4 GIVING WS-TEMP REMAINDER WS-REMAINDER
                   IF WS-REMAINDER = 0
                       MOVE 1 TO WS-RESULT
                   ELSE
                       MOVE 0 TO WS-RESULT
                   END-IF
               END-IF
           END-IF
       STOP RUN.