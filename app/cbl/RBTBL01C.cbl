      *****************************************************************
      * RBTBL01C  単価表整合点検 (随時)        東西電力 電算部        *
      *---------------------------------------------------------------*
      * H26.04 初版 (8%改定時の点検漏れ再発防止)                      *
      * 内蔵単価 (RBRYO01C と同値をここにも保持) が正であり           *
      * 単価ファイルの最新世代が内蔵と一致することを確かめる          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBTBL01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TANKAF  ASSIGN TO 'app/data/portable/TANKA.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-TAN.
           SELECT DATECTL ASSIGN TO 'app/data/portable/DATECTL.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-DTE.
           SELECT CHKOUT  ASSIGN TO 'app/data/portable/TBLCHK.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-CHK.
       DATA DIVISION.
       FILE SECTION.
       FD  TANKAF RECORD CONTAINS 80 CHARACTERS.
       COPY RCTANKA.
       FD  DATECTL RECORD CONTAINS 8 CHARACTERS.
       01  DTE-REC             PIC X(8).
       FD  CHKOUT RECORD CONTAINS 40 CHARACTERS.
       01  CHK-REC             PIC X(40).
       WORKING-STORAGE SECTION.
       77  ST-TAN              PIC XX VALUE '00'.
       77  ST-DTE              PIC XX VALUE '00'.
       77  ST-CHK              PIC XX VALUE '00'.
       77  W-KIJUN8            PIC 9(8) VALUE ZERO.
       77  WK-IN-CNT           PIC 9(7) VALUE ZERO.
       77  WK-RULE-NG          PIC 9(7) VALUE ZERO.
       77  WK-KYU-CNT          PIC 9(7) VALUE ZERO.
       77  WK-ICCHI            PIC 9(3) VALUE ZERO.
       77  WK-FUICCHI          PIC 9(3) VALUE ZERO.
       77  WS-I                PIC 9(3) VALUE ZERO.
       77  WS-S                PIC 9(1) VALUE ZERO.
       77  SEL-IX              PIC S9(3) VALUE ZERO.
       01  T-TBL-AREA.
           05  T-CNT           PIC S9(3) COMP-3 VALUE ZERO.
           05  T-E OCCURS 130.
               10  T-SYU       PIC X(2).
               10  T-KAISI     PIC 9(8).
               10  T-KIHON     PIC S9(5)V99 COMP-3.
               10  T-D1        PIC S9(3)V99 COMP-3.
               10  T-D2        PIC S9(3)V99 COMP-3.
               10  T-D3        PIC S9(3)V99 COMP-3.
               10  T-NEN       PIC S9(2)V99 COMP-3.
               10  T-SAI       PIC S9(2)V99 COMP-3.
               10  T-RITU      PIC 9(2)V9   COMP-3.
               10  T-KBN       PIC X.
      *    内蔵単価 (正). RBRYO01C の内蔵と同値を保つこと (運用 9.2)
       01  NZ-AREA.
           05  NZ-E OCCURS 3.
               10  NZ-SYU      PIC X(2).
               10  NZ-KIHON    PIC S9(5)V99 COMP-3.
               10  NZ-D1       PIC S9(3)V99 COMP-3.
               10  NZ-D2       PIC S9(3)V99 COMP-3.
               10  NZ-D3       PIC S9(3)V99 COMP-3.
               10  NZ-NEN      PIC S9(2)V99 COMP-3.
               10  NZ-SAI      PIC S9(2)V99 COMP-3.
               10  NZ-RITU     PIC 9(2)V9   COMP-3.
       PROCEDURE DIVISION.
       MAIN-SEC                SECTION.
       MAIN-000.
           PERFORM NZ-SETTEI   THRU NZ-SETTEI-EX.
           PERFORM KIJUN-GET   THRU KIJUN-GET-EX.
           PERFORM TAN-LOAD    THRU TAN-LOAD-EX.
           OPEN OUTPUT CHKOUT.
           MOVE 1              TO WS-S.
       SYU-LOOP.
           IF WS-S GREATER 3
               GO TO SYUKEI-RTN.
           PERFORM SAISHIN-SEL THRU SAISHIN-SEL-EX.
           IF SEL-IX = ZERO
               ADD 1 TO WK-FUICCHI
               MOVE 'SEDAI NASI' TO CHK-REC
               WRITE CHK-REC
               GO TO SYU-NEXT.
           PERFORM HIKAKU      THRU HIKAKU-EX.
       SYU-NEXT.
           ADD 1               TO WS-S.
           GO TO SYU-LOOP.
       SYUKEI-RTN.
           CLOSE CHKOUT.
           DISPLAY 'RBTBL01C IN=' WK-IN-CNT ' RULE-NG=' WK-RULE-NG
                   ' KYU=' WK-KYU-CNT.
           DISPLAY 'ICCHI=' WK-ICCHI ' FUICCHI=' WK-FUICCHI.
           IF WK-FUICCHI GREATER ZERO
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE.
           STOP RUN.
       ABEND-RTN.
           DISPLAY 'RBTBL01C ABEND'.
           MOVE 16             TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
       NZ-SETTEI               SECTION.
       NZS-010.
           MOVE '10' TO NZ-SYU (1).
           MOVE '11' TO NZ-SYU (2).
           MOVE '20' TO NZ-SYU (3).
           MOVE 1    TO WS-I.
       NZS-020.
           IF WS-I GREATER 3
               GO TO NZ-SETTEI-EX.
           MOVE 295.24 TO NZ-KIHON (WS-I).
           MOVE 30.00  TO NZ-D1 (WS-I).
           MOVE 36.60  TO NZ-D2 (WS-I).
           MOVE 40.69  TO NZ-D3 (WS-I).
           MOVE -8.07  TO NZ-NEN (WS-I).
           MOVE 3.49   TO NZ-SAI (WS-I).
           MOVE 10.0   TO NZ-RITU (WS-I).
           ADD 1       TO WS-I.
           GO TO NZS-020.
       NZ-SETTEI-EX.
           EXIT.
      *****************************************************************
       KIJUN-GET               SECTION.
       KJG-010.
           OPEN INPUT DATECTL.
           IF ST-DTE NOT = '00'
               GO TO ABEND-RTN.
           READ DATECTL
               AT END GO TO ABEND-RTN.
           IF DTE-REC NOT NUMERIC
               GO TO ABEND-RTN.
           MOVE DTE-REC        TO W-KIJUN8.
           CLOSE DATECTL.
       KIJUN-GET-EX.
           EXIT.
      *****************************************************************
      * 単価表読込 + 行規則点検
      *****************************************************************
       TAN-LOAD                SECTION.
       TNL-010.
           OPEN INPUT TANKAF.
       TNL-020.
           IF ST-TAN NOT = '00'
               GO TO TNL-090.
           READ TANKAF
               AT END GO TO TNL-090.
           ADD 1               TO WK-IN-CNT.
           IF TAN-SYUBETU (1:1) NOT = '1' AND
              TAN-SYUBETU (1:1) NOT = '2'
               ADD 1 TO WK-RULE-NG
               GO TO TNL-020.
           IF TAN-SYUBETU (2:1) NOT NUMERIC
               ADD 1 TO WK-RULE-NG
               GO TO TNL-020.
           IF TAN-SYUBETU NOT = '10' AND TAN-SYUBETU NOT = '11'
              AND TAN-SYUBETU NOT = '20'
               ADD 1 TO WK-RULE-NG
               GO TO TNL-020.
           IF TAN-TEKIYO-KAISI (5:2) LESS '01' OR
              TAN-TEKIYO-KAISI (5:2) GREATER '12'
               ADD 1 TO WK-RULE-NG
               GO TO TNL-020.
           IF TAN-KIHON-TANKA LESS 1 OR
              TAN-KIHON-TANKA GREATER 9999
               ADD 1 TO WK-RULE-NG
               GO TO TNL-020.
           IF TAN-ZEI-RITU GREATER 25
               ADD 1 TO WK-RULE-NG
               GO TO TNL-020.
           IF TAN-TEKIYO-KAISI LESS 19900101 OR
              TAN-TEKIYO-KAISI GREATER 20991231
               ADD 1 TO WK-RULE-NG
               GO TO TNL-020.
           IF TAN-DAN2-TANKA LESS TAN-DAN1-TANKA
               ADD 1 TO WK-RULE-NG
               GO TO TNL-020.
           IF TAN-DAN3-TANKA LESS TAN-DAN2-TANKA
               ADD 1 TO WK-RULE-NG
               GO TO TNL-020.
           IF TAN-ZEI-KBN NOT = 'U' AND TAN-ZEI-KBN NOT = 'S'
               ADD 1 TO WK-RULE-NG
               GO TO TNL-020.
           IF TAN-ZEI-KBN = 'S'
               IF TAN-TEKIYO-KAISI GREATER 20140401
                   ADD 1 TO WK-RULE-NG
                   GO TO TNL-020
               END-IF
           END-IF
           IF T-CNT NOT LESS 130
               GO TO TNL-090.
           ADD 1               TO T-CNT.
           MOVE TAN-SYUBETU      TO T-SYU (T-CNT).
           MOVE TAN-TEKIYO-KAISI TO T-KAISI (T-CNT).
           MOVE TAN-KIHON-TANKA  TO T-KIHON (T-CNT).
           MOVE TAN-DAN1-TANKA   TO T-D1 (T-CNT).
           MOVE TAN-DAN2-TANKA   TO T-D2 (T-CNT).
           MOVE TAN-DAN3-TANKA   TO T-D3 (T-CNT).
           MOVE TAN-NENCHO-TANKA TO T-NEN (T-CNT).
           MOVE TAN-SAIENE-TANKA TO T-SAI (T-CNT).
           MOVE TAN-ZEI-RITU     TO T-RITU (T-CNT).
           MOVE TAN-ZEI-KBN      TO T-KBN (T-CNT).
           GO TO TNL-020.
       TNL-090.
           CLOSE TANKAF.
       TAN-LOAD-EX.
           EXIT.
      *****************************************************************
      * 種別ごとの最新世代選択
      *****************************************************************
       SAISHIN-SEL             SECTION.
       SSL-010.
           MOVE ZERO           TO SEL-IX.
           MOVE 1              TO WS-I.
       SSL-020.
           IF WS-I GREATER T-CNT
               GO TO SAISHIN-SEL-EX.
           IF T-SYU (WS-I) NOT = NZ-SYU (WS-S)
               GO TO SSL-080.
           IF T-KAISI (WS-I) GREATER W-KIJUN8
               ADD 1 TO WK-KYU-CNT
               GO TO SSL-080.
           IF SEL-IX = ZERO
               GO TO SSL-070.
           IF T-KAISI (WS-I) NOT GREATER T-KAISI (SEL-IX)
               ADD 1 TO WK-KYU-CNT
               GO TO SSL-080.
           ADD 1               TO WK-KYU-CNT.
       SSL-070.
           MOVE WS-I           TO SEL-IX.
       SSL-080.
           ADD 1               TO WS-I.
           GO TO SSL-020.
       SAISHIN-SEL-EX.
           EXIT.
      *****************************************************************
      * 内蔵との突合 (8項目)
      *****************************************************************
       HIKAKU                  SECTION.
       HIK-010.
           IF T-KIHON (SEL-IX) NOT = NZ-KIHON (WS-S)
               GO TO HIK-NG.
           IF T-D1 (SEL-IX) NOT = NZ-D1 (WS-S)
               GO TO HIK-NG.
           IF T-D2 (SEL-IX) NOT = NZ-D2 (WS-S)
               GO TO HIK-NG.
           IF T-D3 (SEL-IX) NOT = NZ-D3 (WS-S)
               GO TO HIK-NG.
           IF T-NEN (SEL-IX) NOT = NZ-NEN (WS-S)
               GO TO HIK-NG.
           IF T-SAI (SEL-IX) NOT = NZ-SAI (WS-S)
               GO TO HIK-NG.
           IF T-RITU (SEL-IX) NOT = NZ-RITU (WS-S)
               GO TO HIK-NG.
           IF T-KBN (SEL-IX) NOT = 'U'
               GO TO HIK-NG.
           ADD 1               TO WK-ICCHI.
           MOVE SPACES         TO CHK-REC.
           STRING NZ-SYU (WS-S) ' ITCHI'
               DELIMITED BY SIZE INTO CHK-REC.
           WRITE CHK-REC.
           GO TO HIKAKU-EX.
       HIK-NG.
           ADD 1               TO WK-FUICCHI.
           MOVE SPACES         TO CHK-REC.
           STRING NZ-SYU (WS-S) ' FUICCHI'
               DELIMITED BY SIZE INTO CHK-REC.
           WRITE CHK-REC.
       HIKAKU-EX.
           EXIT.
