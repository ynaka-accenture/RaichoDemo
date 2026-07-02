       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTSRT.
      * SORT出口ドライバ: D020 の E15/E35 呼出しを模擬
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KENF ASSIGN TO 'app/data/portable/KENCHK.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-K.
       DATA DIVISION.
       FILE SECTION.
       FD  KENF RECORD CONTAINS 128 CHARACTERS.
       01  K-REC               PIC X(128).
       WORKING-STORAGE SECTION.
       77  ST-K                PIC XX VALUE '00'.
       77  SRT-RC              PIC 9  VALUE ZERO.
       77  SRT-DEPT            PIC 9(2) VALUE ZERO.
       77  WK-IN               PIC 9(7) VALUE ZERO.
       77  WK-DEL              PIC 9(7) VALUE ZERO.
       77  WK-OUT              PIC 9(7) VALUE ZERO.
       01  DEPT-CNT-TBL.
           05  DEPT-CNT        PIC 9(7) OCCURS 10 VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN INPUT KENF.
       LOOP-RTN.
           READ KENF AT END GO TO OWARI.
           ADD 1 TO WK-IN.
           CALL 'RXSRT15C' USING K-REC SRT-RC.
           IF SRT-RC = 4
               ADD 1 TO WK-DEL
               GO TO LOOP-RTN.
           CALL 'RXSRT35C' USING K-REC SRT-DEPT.
           IF SRT-DEPT GREATER 0 AND SRT-DEPT LESS 11
               ADD 1 TO DEPT-CNT (SRT-DEPT).
           ADD 1 TO WK-OUT.
           GO TO LOOP-RTN.
       OWARI.
           CLOSE KENF.
           DISPLAY 'TESTSRT IN=' WK-IN ' DEL=' WK-DEL
                   ' OUT=' WK-OUT.
           DISPLAY 'DEPT01=' DEPT-CNT (1) ' DEPT10=' DEPT-CNT (10)
                   ' DEPT99CHI01=' DEPT-CNT (9).
           STOP RUN.
