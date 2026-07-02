      *****************************************************************
      * TESTDTC - RUTLDTC 検証ドライバ (品質ゲート用, 本番資産外)     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTDTC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WK-PARM.
           05  WK-FUNC         PIC X(2).
           05  WK-GENGO        PIC X(1).
           05  WK-DATE-IN      PIC X(8).
           05  WK-DATE-OUT     PIC X(8).
           05  WK-SERIAL       PIC S9(7) COMP-3.
           05  WK-RC           PIC 9(2).
       77  WK-CASE             PIC 9(2) VALUE ZERO.
       77  WK-SER-ED           PIC -9(7).
       PROCEDURE DIVISION.
       MAIN-000.
      *    ---- VD ----
           MOVE 'VD' TO WK-FUNC.
           MOVE '20240229' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE '20230229' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE '21000229' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE '00000000' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE '99999999' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE '99991231' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE '20241301' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE '2024AB01' TO WK-DATE-IN. PERFORM CALL-RTN.
      *    ---- W6 ----
           MOVE 'W6' TO WK-FUNC.
           MOVE 'S' TO WK-GENGO.
           MOVE '631231  ' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE 'H' TO WK-GENGO.
           MOVE '310430  ' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE 'R' TO WK-GENGO.
           MOVE '060701  ' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE SPACE TO WK-GENGO.
           MOVE '700101  ' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE SPACE TO WK-GENGO.
           MOVE '380101  ' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE 'X' TO WK-GENGO.
           MOVE '010101  ' TO WK-DATE-IN. PERFORM CALL-RTN.
      *    ---- Y6 ----
           MOVE 'Y6' TO WK-FUNC.
           MOVE SPACE TO WK-GENGO.
           MOVE '491231  ' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE '500101  ' TO WK-DATE-IN. PERFORM CALL-RTN.
      *    ---- SN ----
           MOVE 'SN' TO WK-FUNC.
           MOVE '19800101' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE '19800301' TO WK-DATE-IN. PERFORM CALL-RTN.
           MOVE '20240701' TO WK-DATE-IN. PERFORM CALL-RTN.
      *    ---- NS ----
           MOVE 'NS' TO WK-FUNC.
           MOVE SPACES TO WK-DATE-IN.
           MOVE 16253 TO WK-SERIAL. PERFORM CALL-RTN.
           MOVE 0     TO WK-SERIAL. PERFORM CALL-RTN.
           MOVE 59    TO WK-SERIAL. PERFORM CALL-RTN.
           MOVE 365   TO WK-SERIAL. PERFORM CALL-RTN.
           MOVE 366   TO WK-SERIAL. PERFORM CALL-RTN.
           STOP RUN.
       CALL-RTN.
           ADD 1 TO WK-CASE.
           MOVE SPACES TO WK-DATE-OUT.
           IF WK-FUNC NOT = 'NS'
               MOVE ZERO TO WK-SERIAL.
           MOVE ZERO   TO WK-RC.
           CALL 'RUTLDTC' USING WK-PARM.
           MOVE WK-SERIAL TO WK-SER-ED.
           DISPLAY 'CASE=' WK-CASE ' F=' WK-FUNC ' G=' WK-GENGO
                   ' IN=' WK-DATE-IN ' OUT=' WK-DATE-OUT
                   ' SER=' WK-SER-ED ' RC=' WK-RC.
