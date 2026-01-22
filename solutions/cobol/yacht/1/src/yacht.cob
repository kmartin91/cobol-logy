       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT PIC 99 VALUE 0.
       01 WS-CATEGORY PIC X(15).
       01 WS-DICE PIC 9(5).
       
       01 WS-COUNTS.
          05 WS-COUNT OCCURS 6 TIMES PIC 9 VALUE 0.
       
       01 WS-TEMP-VALUES.
          05 WS-I PIC 9 VALUE 0.
          05 WS-DICE-VALUE PIC 9 VALUE 0.
          05 WS-SUM PIC 99 VALUE 0.
          05 WS-HAS-THREE PIC 9 VALUE 0.
          05 WS-HAS-TWO PIC 9 VALUE 0.
          05 WS-FOUR-VALUE PIC 9 VALUE 0.
          
       PROCEDURE DIVISION.
       YACHT.
           PERFORM INITIALIZE-COUNTS
           PERFORM COUNT-DICE
           
           EVALUATE WS-CATEGORY
               WHEN "ones"
                   COMPUTE WS-RESULT = WS-COUNT(1) * 1
               WHEN "twos"
                   COMPUTE WS-RESULT = WS-COUNT(2) * 2
               WHEN "threes"
                   COMPUTE WS-RESULT = WS-COUNT(3) * 3
               WHEN "fours"
                   COMPUTE WS-RESULT = WS-COUNT(4) * 4
               WHEN "fives"
                   COMPUTE WS-RESULT = WS-COUNT(5) * 5
               WHEN "sixes"
                   COMPUTE WS-RESULT = WS-COUNT(6) * 6
               WHEN "full house"
                   PERFORM CHECK-FULL-HOUSE
               WHEN "four of a kind"
                   PERFORM CHECK-FOUR-OF-KIND
               WHEN "little straight"
                   PERFORM CHECK-LITTLE-STRAIGHT
               WHEN "big straight"
                   PERFORM CHECK-BIG-STRAIGHT
               WHEN "choice"
                   PERFORM CALCULATE-CHOICE
               WHEN "yacht"
                   PERFORM CHECK-YACHT
           END-EVALUATE.
           
       INITIALIZE-COUNTS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE 0 TO WS-COUNT(WS-I)
           END-PERFORM.
           
       COUNT-DICE.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE WS-DICE(WS-I:1) TO WS-DICE-VALUE
               ADD 1 TO WS-COUNT(WS-DICE-VALUE)
           END-PERFORM.
           
       CHECK-FULL-HOUSE.
           MOVE 0 TO WS-HAS-THREE
           MOVE 0 TO WS-HAS-TWO
           MOVE 0 TO WS-RESULT
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               IF WS-COUNT(WS-I) = 3
                   MOVE 1 TO WS-HAS-THREE
               END-IF
               IF WS-COUNT(WS-I) = 2
                   MOVE 1 TO WS-HAS-TWO
               END-IF
           END-PERFORM
           
           IF WS-HAS-THREE = 1 AND WS-HAS-TWO = 1
               PERFORM CALCULATE-CHOICE
           END-IF.
           
       CHECK-FOUR-OF-KIND.
           MOVE 0 TO WS-FOUR-VALUE
           MOVE 0 TO WS-RESULT
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               IF WS-COUNT(WS-I) >= 4
                   MOVE WS-I TO WS-FOUR-VALUE
               END-IF
           END-PERFORM
           
           IF WS-FOUR-VALUE > 0
               COMPUTE WS-RESULT = WS-FOUR-VALUE * 4
           END-IF.
           
       CHECK-LITTLE-STRAIGHT.
           MOVE 0 TO WS-RESULT
           IF WS-COUNT(1) = 1 AND WS-COUNT(2) = 1 AND
              WS-COUNT(3) = 1 AND WS-COUNT(4) = 1 AND
              WS-COUNT(5) = 1
               MOVE 30 TO WS-RESULT
           END-IF.
           
       CHECK-BIG-STRAIGHT.
           MOVE 0 TO WS-RESULT
           IF WS-COUNT(2) = 1 AND WS-COUNT(3) = 1 AND
              WS-COUNT(4) = 1 AND WS-COUNT(5) = 1 AND
              WS-COUNT(6) = 1
               MOVE 30 TO WS-RESULT
           END-IF.
           
       CHECK-YACHT.
           MOVE 0 TO WS-RESULT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               IF WS-COUNT(WS-I) = 5
                   MOVE 50 TO WS-RESULT
               END-IF
           END-PERFORM.
           
       CALCULATE-CHOICE.
           MOVE 0 TO WS-SUM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE WS-DICE(WS-I:1) TO WS-DICE-VALUE
               ADD WS-DICE-VALUE TO WS-SUM
           END-PERFORM
           MOVE WS-SUM TO WS-RESULT.