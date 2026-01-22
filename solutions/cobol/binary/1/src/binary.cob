       IDENTIFICATION DIVISION.
       PROGRAM-ID. BINARY.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-BINARY    PIC X(60).
       01 WS-RESULT    PIC 9999.
       01 WS-ERROR     PIC X(60) VALUE SPACES. 
       01 WS-LENGTH    PIC 99 VALUE 0.
       01 WS-I         PIC 99 VALUE 0.
       01 WS-CHAR      PIC X VALUE SPACE.
       01 WS-DIGIT     PIC 9.
      
       
       PROCEDURE DIVISION.
       
       DECIMAL.
         MOVE 0 TO WS-RESULT
         MOVE SPACES TO WS-ERROR

         COMPUTE WS-LENGTH = FUNCTION LENGTH(FUNCTION TRIM(WS-BINARY TRAILING))

         IF WS-LENGTH = 0
            MOVE "error" TO WS-ERROR
            EXIT PARAGRAPH
         END-IF.

         PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LENGTH
            MOVE WS-BINARY(WS-I:1) TO WS-CHAR

            IF WS-CHAR = '0' or '1'
               MOVE WS-CHAR TO WS-DIGIT
               COMPUTE WS-RESULT = (WS-RESULT * 2) + WS-DIGIT
            ELSE
               IF WS-CHAR IS NUMERIC
                  MOVE "error: a number containing non-binary digits is invalid" TO WS-ERROR
               ELSE
                  MOVE "error: a number containing non-binary characters is invalid" TO WS-ERROR
               END-IF
               MOVE 0 TO WS-RESULT
            END-IF
         END-PERFORM.