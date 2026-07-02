       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTCICS.
      * 疑似端末ドライバ: 擬似会話 (RETURN TRANSID) と XCTL を再現
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-EIB.
       COPY RCEIBLK.
       01  WS-PARM.
       COPY RCCICSPM.
       01  WS-COMM             PIC X(200) VALUE SPACE.
       01  WS-NEXT             PIC X(212) VALUE SPACE.
       01  WS-NEXT-R REDEFINES WS-NEXT.
           05  NX-TRAN         PIC X(4).
           05  NX-PGM          PIC X(8).
           05  NX-END          PIC X(1).
           05  NX-COMM         PIC X(199).
       77  CUR-PGM             PIC X(8) VALUE 'RASGN00C'.
       77  WK-STEP             PIC 9(3) VALUE ZERO.
       77  WK-XCTL-MI          PIC 9(3) VALUE ZERO.
       77  WK-CAP              PIC 9(3) VALUE 30.
       77  WK-TM               PIC X    VALUE ' '.
       PROCEDURE DIVISION.
       MAIN-RTN.
           ACCEPT WK-TM FROM ENVIRONMENT 'RACS_TERM'.
           IF WK-TM = '1'
               MOVE 500 TO WK-CAP.
           MOVE ZERO           TO EIBCALEN.
           MOVE 'RSGN'         TO EIBTRNID.
           MOVE 'T001'         TO EIBTRMID.
       KURIKAESI.
           ADD 1               TO WK-STEP.
           IF WK-STEP GREATER WK-CAP
               DISPLAY 'TESTCICS E001 LOOP OVER'
               GO TO OWARI.
           EVALUATE CUR-PGM
               WHEN 'RASGN00C'
                   CALL 'RASGN00C' USING WS-EIB WS-COMM
               WHEN 'RAMEN01C'
                   CALL 'RAMEN01C' USING WS-EIB WS-COMM
               WHEN 'RAKYK01C'
                   CALL 'RAKYK01C' USING WS-EIB WS-COMM
               WHEN 'RAKYK02C'
                   CALL 'RAKYK02C' USING WS-EIB WS-COMM
               WHEN 'RAKEN01C'
                   CALL 'RAKEN01C' USING WS-EIB WS-COMM
               WHEN 'RARYO01C'
                   CALL 'RARYO01C' USING WS-EIB WS-COMM
               WHEN OTHER
                   ADD 1 TO WK-XCTL-MI
                   DISPLAY 'XCTL SAKI MIJISSOU: ' CUR-PGM
                   GO TO OWARI
           END-EVALUATE.
           MOVE 'GET-NEXT   '  TO CP-CMD.
           CALL 'RXCICSTB' USING WS-EIB WS-PARM WS-NEXT.
           IF NX-END = 'Y'
               GO TO OWARI.
           IF NX-PGM NOT = SPACES
      *        XCTL: 即時に遷移先を起動 (COMMAREA 引継ぎ)
               MOVE NX-PGM     TO CUR-PGM
               MOVE WS-NEXT (14:200) TO WS-COMM
               MOVE CP-DATA-LEN TO EIBCALEN
               GO TO KURIKAESI.
           IF NX-TRAN NOT = SPACES
      *        擬似会話: 次入力を伴って同トランを再起動
               MOVE NX-TRAN    TO EIBTRNID
               MOVE WS-NEXT (14:200) TO WS-COMM
               MOVE CP-DATA-LEN TO EIBCALEN
               EVALUATE NX-TRAN
                   WHEN 'RSGN' MOVE 'RASGN00C' TO CUR-PGM
                   WHEN 'RMEN' MOVE 'RAMEN01C' TO CUR-PGM
                   WHEN 'RKYK' MOVE 'RAKYK01C' TO CUR-PGM
                   WHEN 'RKY2' MOVE 'RAKYK02C' TO CUR-PGM
                   WHEN 'RKEN' MOVE 'RAKEN01C' TO CUR-PGM
                   WHEN 'RRYO' MOVE 'RARYO01C' TO CUR-PGM
                   WHEN OTHER  DISPLAY 'TRAN FUMEI: ' NX-TRAN
                               GO TO OWARI
               END-EVALUATE
               GO TO KURIKAESI.
           GO TO OWARI.
       OWARI.
           DISPLAY 'TESTCICS STEPS=' WK-STEP
                   ' XCTL-MI=' WK-XCTL-MI.
           STOP RUN.
