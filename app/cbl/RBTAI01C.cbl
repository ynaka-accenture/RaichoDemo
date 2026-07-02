      *****************************************************************
      * RBTAI01C  滞納判定・停止候補抽出 (日次D070)  東西電力 電算部  *
      *---------------------------------------------------------------*
      * H04.06 初版  H16.09 猶予日数変更  R02.04 消込連携             *
      * 復帰コード: 4=正常終了(候補あり) 0=正常終了(候補なし)         *
      *   8=入力エラー   ※後続 JCL の COND 設定に注意                *
      * 日付計算は共通サブと同等の式を当プログラム内に保持 (高速化)   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBTAI01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RYOIN   ASSIGN TO 'app/data/portable/RYOFILE.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-RYO.
           SELECT SHUMEI  ASSIGN TO 'app/data/portable/SHUMEI.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-SHU.
           SELECT TAIOUT  ASSIGN TO 'app/data/portable/TAILST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-TAI.
           SELECT DATECTL ASSIGN TO 'app/data/portable/DATECTL.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-DTE.
       DATA DIVISION.
       FILE SECTION.
       FD  RYOIN RECORD CONTAINS 256 CHARACTERS.
       COPY RCRYOREC.
       FD  SHUMEI RECORD CONTAINS 100 CHARACTERS.
       COPY RCSHUREC.
       FD  TAIOUT RECORD CONTAINS 64 CHARACTERS.
       01  TAI-REC.
           05  TAI-SPT         PIC X(22).
           05  TAI-YM          PIC 9(6).
           05  TAI-GOKEI       PIC S9(9) COMP-3.
           05  TAI-NISSU       PIC S9(5) COMP-3.
           05  TAI-RANK        PIC X(1).
           05  FILLER          PIC X(27).
       FD  DATECTL RECORD CONTAINS 8 CHARACTERS.
       01  DTE-REC             PIC X(8).
       WORKING-STORAGE SECTION.
       77  ST-RYO              PIC XX VALUE '00'.
       77  ST-SHU              PIC XX VALUE '00'.
       77  ST-TAI              PIC XX VALUE '00'.
       77  ST-DTE              PIC XX VALUE '00'.
       77  WK-IN-CNT           PIC 9(7)  VALUE ZERO.
       77  WK-PAID             PIC 9(7)  VALUE ZERO.
       77  WK-CAND             PIC 9(7)  VALUE ZERO.
       77  WK-RNK-A            PIC 9(7)  VALUE ZERO.
       77  WK-RNK-B            PIC 9(7)  VALUE ZERO.
       77  WK-VAL-NG           PIC 9(7)  VALUE ZERO.
       77  WS-CHIKU            PIC 9(2)  VALUE ZERO.
       01  TCHIKU-TBL.
           05  TCHIKU-CNT      PIC 9(5) OCCURS 47 VALUE ZERO.
       77  W-TODAY8            PIC 9(8)  VALUE ZERO.
       77  W-TODAY6            PIC 9(6)  VALUE ZERO.
       77  W-KIGEN8            PIC 9(8)  VALUE ZERO.
      *    通算日: 1980/1/1 起点 パック10進 (E-06)
       77  W-SER-KYO           PIC S9(7) COMP-3 VALUE ZERO.
       77  W-SER-KGN           PIC S9(7) COMP-3 VALUE ZERO.
       77  W-NISSU             PIC S9(5) VALUE ZERO.
       77  W-YY                PIC 9(4)  VALUE ZERO.
       77  W-MM                PIC 9(2)  VALUE ZERO.
       77  W-DD                PIC 9(2)  VALUE ZERO.
       77  W-WK                PIC S9(7) VALUE ZERO.
       77  WS-I                PIC 9(5)  VALUE ZERO.
       77  WS-J                PIC 9(5)  VALUE ZERO.
       77  WS-LO               PIC S9(5) VALUE ZERO.
       77  WS-HI               PIC S9(5) VALUE ZERO.
       77  WS-MD               PIC S9(5) VALUE ZERO.
       77  WS-HIT              PIC S9(5) VALUE ZERO.
       01  TUKI-TBL-V.
           05  FILLER PIC X(36) VALUE
               '000031059090120151181212243273304334'.
       01  TUKI-TBL REDEFINES TUKI-TBL-V.
           05  TUKI-RUI        PIC 9(3) OCCURS 12.
      *----------------------------------------------------------------
      *    消込テーブル
      *----------------------------------------------------------------
       01  SHT-AREA.
           05  SHT-CNT         PIC S9(5) COMP-3 VALUE ZERO.
           05  SHT-E OCCURS 6000.
               10  SHT-SPT     PIC X(22).
               10  SHT-FLG     PIC X.
       01  SHT-SWAP.
           05  SW-SPT          PIC X(22).
           05  SW-FLG          PIC X.
       PROCEDURE DIVISION.
       MAIN-SEC                SECTION.
       MAIN-000.
           PERFORM TODAY-GET   THRU TODAY-GET-EX.
           PERFORM SHT-LOAD    THRU SHT-LOAD-EX.
           PERFORM SHT-SORT    THRU SHT-SORT-EX.
           OPEN INPUT RYOIN.
           OPEN OUTPUT TAIOUT.
       MAIN-LOOP.
           READ RYOIN
               AT END GO TO SYUKEI-RTN.
           ADD 1               TO WK-IN-CNT.
           PERFORM RYO-VALID   THRU RYO-VALID-EX.
           PERFORM SHT-SRCH    THRU SHT-SRCH-EX.
           IF WS-HIT GREATER ZERO
               IF SHT-FLG (WS-HIT) = '1'
                   ADD 1       TO WK-PAID
                   GO TO MAIN-LOOP
               END-IF
           END-IF.
           PERFORM TAINO-HANTEI THRU TAINO-HANTEI-EX.
           GO TO MAIN-LOOP.
      *****************************************************************
       SYUKEI-RTN.
           CLOSE RYOIN TAIOUT.
           DISPLAY 'RBTAI01C TODAY=' W-TODAY8
                   ' IN=' WK-IN-CNT ' PAID=' WK-PAID.
           DISPLAY 'CAND=' WK-CAND ' RANK-A=' WK-RNK-A
                   ' RANK-B=' WK-RNK-B ' VALNG=' WK-VAL-NG
                   ' CHIKU01=' TCHIKU-CNT (1).
      *    復帰コード: 候補ありは 4 (正常). 0 は候補なし
           IF WK-CAND GREATER ZERO
               MOVE 4          TO RETURN-CODE
           ELSE
               MOVE 0          TO RETURN-CODE.
           STOP RUN.
       ABEND-RTN.
           DISPLAY 'RBTAI01C ABEND'.
           MOVE 16             TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * 当日日付取得: 運用日付ファイル優先. 無ければシステム日付
      *   (システム日付は 2桁年 YY>=80 を 19xx とみなす)
      *****************************************************************
       TODAY-GET               SECTION.
       TDG-010.
           OPEN INPUT DATECTL.
           IF ST-DTE NOT = '00'
               GO TO TDG-050.
           READ DATECTL
               AT END GO TO TDG-050.
           IF DTE-REC NOT NUMERIC
               GO TO TDG-050.
           MOVE DTE-REC        TO W-TODAY8.
           CLOSE DATECTL.
           GO TO TDG-090.
       TDG-050.
           ACCEPT W-TODAY6 FROM DATE.
           IF W-TODAY6 (1:2) NOT LESS '80'
               COMPUTE W-TODAY8 = 19000000 + W-TODAY6
           ELSE
               COMPUTE W-TODAY8 = 20000000 + W-TODAY6.
       TDG-090.
           MOVE W-TODAY8       TO W-WK.
           PERFORM SERIAL-CALC THRU SERIAL-CALC-EX.
           MOVE W-SER-KGN      TO W-SER-KYO.
       TODAY-GET-EX.
           EXIT.
      *****************************************************************
      * 通算日計算 (1980/1/1 起点. 共通サブ RUTLDTC と同式のはず)
      *   IN: W-WK (YYYYMMDD)  OUT: W-SER-KGN
      *****************************************************************
       SERIAL-CALC             SECTION.
       SRC-010.
           COMPUTE W-YY = W-WK / 10000.
           COMPUTE W-MM = ( W-WK / 100 ) - ( W-YY * 100 ).
           COMPUTE W-DD = W-WK - ( W-YY * 10000 ) - ( W-MM * 100 ).
           IF W-MM LESS 1 OR W-MM GREATER 12
               GO TO ABEND-RTN.
           COMPUTE W-SER-KGN = ( W-YY - 1980 ) * 365
               + ( W-YY - 1980 + 3 ) / 4
               + TUKI-RUI (W-MM) + W-DD.
      *    うるう年で 3月以降なら +1 (100年例外は考慮不要 -- H04)
           IF W-MM GREATER 2
               COMPUTE W-WK = W-YY - ( W-YY / 4 ) * 4
               IF W-WK = ZERO
                   ADD 1       TO W-SER-KGN
               END-IF
           END-IF.
       SERIAL-CALC-EX.
           EXIT.
      *****************************************************************
      * 滞納判定: 支払期限 = 検針日 + 30日. 期限超過日数でランク付け
      *   (SHIME-A は SHIME-B へ続く: 意図的な流れ落ち)
      *****************************************************************
       TAINO-HANTEI            SECTION.
       TAI-010.
           MOVE RYO-KENSHIN-BI TO W-WK.
           IF W-WK (1:2) NOT LESS '50'
               COMPUTE W-WK = 19000000 + W-WK
           ELSE
               COMPUTE W-WK = 20000000 + W-WK.
           PERFORM SERIAL-CALC THRU SERIAL-CALC-EX.
           COMPUTE W-NISSU = W-SER-KYO - ( W-SER-KGN + 30 ).
           IF W-NISSU NOT GREATER ZERO
               GO TO TAINO-HANTEI-EX.
           ADD 1               TO WK-CAND.
           IF RYO-SPT-NO (3:2) NUMERIC
               MOVE RYO-SPT-NO (3:2) TO WS-CHIKU
               IF WS-CHIKU GREATER ZERO AND WS-CHIKU LESS 48
                   ADD 1 TO TCHIKU-CNT (WS-CHIKU)
               END-IF
           END-IF.
       SHIME-A.
           MOVE SPACES         TO TAI-REC.
           MOVE RYO-SPT-NO     TO TAI-SPT.
           MOVE RYO-SEIKYU-YM  TO TAI-YM.
           MOVE RYO-GOKEI      TO TAI-GOKEI.
           MOVE W-NISSU        TO TAI-NISSU.
      *    (このまま SHIME-B へ続く)
       SHIME-B.
           IF W-NISSU GREATER 90
               MOVE 'A'        TO TAI-RANK
               ADD 1           TO WK-RNK-A
               GO TO SHIME-C.
           IF W-NISSU GREATER 30
               MOVE 'B'        TO TAI-RANK
               ADD 1           TO WK-RNK-B
               GO TO SHIME-C.
           MOVE 'C'            TO TAI-RANK.
       SHIME-C.
           WRITE TAI-REC.
       TAINO-HANTEI-EX.
           EXIT.
      *****************************************************************
      * 請求レコード検証 (R02 消込連携時に追加)
      *****************************************************************
       RYO-VALID               SECTION.
       RVA-010.
           IF RYO-SPT-NO (1:2) NOT = '03'
               GO TO RVA-NG.
           IF RYO-SEIKYU-YM (5:2) LESS '01' OR
              RYO-SEIKYU-YM (5:2) GREATER '12'
               GO TO RVA-NG.
           IF RYO-KENSHIN-BI NOT NUMERIC
               GO TO RVA-NG.
           IF RYO-GOKEI LESS -9999999 OR
              RYO-GOKEI GREATER 99999999
               GO TO RVA-NG.
           IF RYO-SEIKYU-KBN NOT = '1' AND
              RYO-SEIKYU-KBN NOT = '2'
               GO TO RVA-NG.
           IF RYO-NIWARI-NISSU LESS 01 OR
              RYO-NIWARI-NISSU GREATER 31
               GO TO RVA-NG.
           GO TO RYO-VALID-EX.
       RVA-NG.
           ADD 1               TO WK-VAL-NG.
           GO TO MAIN-LOOP.
       RYO-VALID-EX.
           EXIT.
      *****************************************************************
      * 消込テーブル展開・整列・探索
      *****************************************************************
       SHT-LOAD                SECTION.
       SHL-010.
           OPEN INPUT SHUMEI.
       SHL-020.
           IF ST-SHU NOT = '00'
               GO TO SHL-090.
           READ SHUMEI
               AT END GO TO SHL-090.
           IF SHT-CNT NOT LESS 6000
               GO TO SHL-090.
           ADD 1               TO SHT-CNT.
           MOVE SHU-SPT-NO     TO SHT-SPT (SHT-CNT).
           MOVE SHU-KESHIKOMI-FLG TO SHT-FLG (SHT-CNT).
           GO TO SHL-020.
       SHL-090.
           CLOSE SHUMEI.
       SHT-LOAD-EX.
           EXIT.
       SHT-SORT                SECTION.
       SHS-010.
           MOVE 2              TO WS-I.
       SHS-020.
           IF WS-I GREATER SHT-CNT
               GO TO SHT-SORT-EX.
           MOVE SHT-E (WS-I)   TO SHT-SWAP.
           COMPUTE WS-J = WS-I - 1.
       SHS-030.
           IF WS-J LESS 1
               GO TO SHS-040.
           IF SHT-SPT (WS-J) NOT GREATER SW-SPT
               GO TO SHS-040.
           MOVE SHT-E (WS-J)   TO SHT-E (WS-J + 1).
           SUBTRACT 1        FROM WS-J.
           GO TO SHS-030.
       SHS-040.
           MOVE SHT-SWAP       TO SHT-E (WS-J + 1).
           ADD 1               TO WS-I.
           GO TO SHS-020.
       SHT-SORT-EX.
           EXIT.
       SHT-SRCH                SECTION.
       SSR-010.
           MOVE ZERO           TO WS-HIT.
           MOVE 1              TO WS-LO.
           MOVE SHT-CNT        TO WS-HI.
       SSR-020.
           IF WS-LO GREATER WS-HI
               GO TO SHT-SRCH-EX.
           COMPUTE WS-MD = ( WS-LO + WS-HI ) / 2.
           IF SHT-SPT (WS-MD) = RYO-SPT-NO
               MOVE WS-MD      TO WS-HIT
               GO TO SHT-SRCH-EX.
           IF SHT-SPT (WS-MD) LESS RYO-SPT-NO
               COMPUTE WS-LO = WS-MD + 1
           ELSE
               COMPUTE WS-HI = WS-MD - 1.
           GO TO SSR-020.
       SHT-SRCH-EX.
           EXIT.
      *----------------------------------------------------------------
      *(H16.9 旧判定: 猶予20日. 顧客対応部と調整のうえ30日へ)
      *    COMPUTE W-NISSU = W-SER-KYO - ( W-SER-KGN + 20 ).
      *(R02 消込連携までは全件を候補にしていた)
      *----------------------------------------------------------------
