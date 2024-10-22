      *-------------------------
       IDENTIFICATION DIVISION.
      *-------------------------
       PROGRAM-ID. ANNUITY.
      *-------------------------
       DATA DIVISION.
      *-------------------------
       WORKING-STORAGE SECTION.
       01 I                PIC 9(2).
       01 TOTAL-LOAN       PIC X(9).
       01 NUMTOTAL-LOAN    PIC 9(9).
       01 INTEREST-RATE  PIC   X(9).
       01 NUMINTEREST-RATE PIC 99V99.
       01 INTEREST-RATEP PIC 9(5).
       01 MONTHS           PIC 999     VALUE ZERO.
       01 YEARS            PIC X(5).
       01 NUMYEARS         PIC 9(5).
       01 CNT            PIC 999 VALUE ZERO.
       01 MONTHLY-PAYMENT  PIC $Z(9).99.
      *-------------------------
       PROCEDURE DIVISION.
      *-------------------------
           DISPLAY "WELCOME TO EMILY'S ANNUITY PROGRAM".
       INPUT-ONE SECTION.
           DISPLAY 'INPUT TOTAL LOAN.'
           ACCEPT TOTAL-LOAN.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF TOTAL-LOAN

             IF TOTAL-LOAN(I:1) IS NOT NUMERIC THEN
                 IF TOTAL-LOAN(I:1) = ' ' THEN
                    CONTINUE
                 ELSE
                    DISPLAY 'INVALID INPUT'
                    GO TO INPUT-ONE
                 END-IF
             END-IF
           END-PERFORM.

           COMPUTE NUMTOTAL-LOAN = FUNCTION NUMVAL(TOTAL-LOAN).

       INPUT-TWO SECTION.
           DISPLAY 'INPUT INTEREST RATE.'
           ACCEPT INTEREST-RATE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF INTEREST-RA
      -    TE
             IF INTEREST-RATE(I:1) IS NOT NUMERIC THEN
                 IF INTEREST-RATE(I:1) = ' ' OR '.' THEN
                    CONTINUE
                 ELSE
                    DISPLAY 'INVALID INPUT'
                    GO TO INPUT-TWO
                 END-IF
             END-IF
           END-PERFORM.

           COMPUTE NUMINTEREST-RATE = FUNCTION NUMVAL(INTEREST-RATE).

       INPUT-THREE SECTION.
           DISPLAY 'INPUT YEARS.'
           ACCEPT YEARS

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF YEARS
             IF YEARS(I:1) IS NOT NUMERIC THEN
                 IF YEARS(I:1) = ' ' THEN
                    CONTINUE
                 ELSE
                    DISPLAY 'INVALID INPUT'
                    GO TO INPUT-THREE
                 END-IF
             END-IF
           END-PERFORM.

           COMPUTE NUMYEARS = FUNCTION NUMVAL(YEARS).

       COMPUTING SECTION.
           COMPUTE INTEREST-RATEP = NUMINTEREST-RATE * 100

           DISPLAY 'TOTAL-LOAN: ' TOTAL-LOAN ' INTEREST-RATE: ' INTEREST
      -    -RATEP '%'.
           DISPLAY ' Y   M        AMOUNT '.
           DISPLAY '--- --- -------------'.
           PERFORM UNTIL CNT = NUMYEARS
             ADD 1 TO CNT
             ADD 12 TO MONTHS
             COMPUTE MONTHLY-PAYMENT = FUNCTION ANNUITY((NUMINTEREST-RAT
      -      E / 12), MONTHS) * NUMTOTAL-LOAN
      -
             DISPLAY CNT ' ' MONTHS ' ' MONTHLY-PAYMENT ' USD'
           END-PERFORM
           GOBACK.
