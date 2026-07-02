      *****************************************************************
      * RBSTM01C  検針票作成 (親)             (株)東西電力 電算部     *
      *---------------------------------------------------------------*
      * S61.02 初版  H11.10 西暦4桁  R01.05 新元号対応                *
      * 行編集は子 RBSTM02C. 入出力エラーは宣言部で記録し続行         *
      * 元号変換は当プログラム内蔵 (共通サブは重いため -- S61 判断)   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBSTM01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RYOIN   ASSIGN TO 'app/data/portable/RYOFILE.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-RYO.
           SELECT KYKMST  ASSIGN TO 'app/data/portable/KYKMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KYK.
           SELECT JYUMST  ASSIGN TO 'app/data/portable/JYUMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-JYU.
           SELECT STMLST  ASSIGN TO 'app/data/portable/STMLST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-STM.
       DATA DIVISION.
       FILE SECTION.
       FD  RYOIN RECORD CONTAINS 256 CHARACTERS.
       COPY RCRYOREC.
       FD  KYKMST RECORD CONTAINS 320 CHARACTERS.
       COPY RCKYKREC.
       FD  JYUMST RECORD CONTAINS 256 CHARACTERS.
       COPY RCJYUREC.
       FD  STMLST RECORD CONTAINS 132 CHARACTERS.
       01  LST-REC             PIC X(132).
       WORKING-STORAGE SECTION.
       77  ST-RYO              PIC XX VALUE '00'.
       77  ST-KYK              PIC XX VALUE '00'.
       77  ST-JYU              PIC XX VALUE '00'.
       77  ST-STM              PIC XX VALUE '00'.
       77  WK-IN-CNT           PIC 9(7)  VALUE ZERO.
       77  WK-LINE-CNT         PIC 9(7)  VALUE ZERO.
       77  WK-NASI             PIC 9(7)  VALUE ZERO.
       77  WK-IOERR            PIC 9(7)  VALUE ZERO.
       77  WK-VALNG            PIC 9(7)  VALUE ZERO.
       77  WK-SOKEI            PIC S9(13) COMP-3 VALUE ZERO.
       77  WK-SOKEN            PIC S9(9)  COMP-3 VALUE ZERO.
       77  WS-I                PIC 9(5)  VALUE ZERO.
       77  WS-J                PIC 9(5)  VALUE ZERO.
       77  WS-LO               PIC S9(5) VALUE ZERO.
       77  WS-HI               PIC S9(5) VALUE ZERO.
       77  WS-MD               PIC S9(5) VALUE ZERO.
       77  WS-HIT              PIC S9(5) VALUE ZERO.
       77  WS-JHIT             PIC S9(5) VALUE ZERO.
       77  W-YY                PIC 9(4)  VALUE ZERO.
       77  W-MM                PIC 9(2)  VALUE ZERO.
       77  W-YMD               PIC 9(8)  VALUE ZERO.
       77  W-WYY               PIC 9(2)  VALUE ZERO.
       01  STM-FUNC            PIC X     VALUE SPACE.
       01  STM-IN.
           05  IN-SEI          PIC X(12).
           05  IN-SIMEI        PIC X(40).
           05  IN-SPT          PIC X(22).
           05  IN-WAREKI       PIC X(10).
           05  IN-KENSU        PIC S9(6).
           05  IN-GOKEI        PIC S9(9).
           05  IN-SOKEI        PIC S9(13).
           05  IN-SOKEN        PIC S9(9).
       01  STM-LINE            PIC X(132).
       01  STM-MORE            PIC X     VALUE 'N'.
      *----------------------------------------------------------------
       01  KYT-AREA.
           05  KYT-CNT         PIC S9(5) COMP-3 VALUE ZERO.
           05  KYT-E OCCURS 6000.
               10  KYT-KEY     PIC X(22).
               10  KYT-JNO     PIC 9(10).
       01  KYT-SWAP.
           05  SWK-KEY         PIC X(22).
           05  SWK-JNO         PIC 9(10).
       01  JYT-AREA.
           05  JYT-CNT         PIC S9(5) COMP-3 VALUE ZERO.
           05  JYT-E OCCURS 5000.
               10  JYT-KEY     PIC 9(10).
               10  JYT-SEI     PIC X(12).
               10  JYT-NAME    PIC X(40).
       PROCEDURE DIVISION.
       DECLARATIVES.
       RYO-ERR-SEC             SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON RYOIN.
       RYO-ERR-P.
      *    入出力エラーは記録のみで続行 (S61 運用判断)
           ADD 1               TO WK-IOERR.
           DISPLAY 'RBSTM01C W101 RYOIN ST=' ST-RYO.
       END DECLARATIVES.
       MAIN-SEC                SECTION.
       MAIN-000.
           PERFORM KYT-LOAD    THRU KYT-LOAD-EX.
           PERFORM KYT-SORT    THRU KYT-SORT-EX.
           PERFORM JYT-LOAD    THRU JYT-LOAD-EX.
           OPEN INPUT RYOIN.
           OPEN OUTPUT STMLST.
       MAIN-LOOP.
           READ RYOIN
               AT END GO TO SYUKEI-RTN.
           ADD 1               TO WK-IN-CNT.
           PERFORM RYO-VALID   THRU RYO-VALID-EX.
           PERFORM KYT-SRCH    THRU KYT-SRCH-EX.
           IF WS-HIT = ZERO
               ADD 1           TO WK-NASI
               GO TO MAIN-LOOP.
           PERFORM JYT-SRCH    THRU JYT-SRCH-EX.
           IF WS-JHIT = ZERO
               ADD 1           TO WK-NASI
               GO TO MAIN-LOOP.
           PERFORM MEISAI-OUT  THRU MEISAI-OUT-EX.
           ADD RYO-GOKEI       TO WK-SOKEI.
           ADD 1               TO WK-SOKEN.
           GO TO MAIN-LOOP.
       SYUKEI-RTN.
           MOVE 'T'            TO STM-FUNC.
           MOVE WK-SOKEI       TO IN-SOKEI.
           MOVE WK-SOKEN       TO IN-SOKEN.
           CALL 'RBSTM02C' USING STM-FUNC STM-IN STM-LINE STM-MORE.
           MOVE STM-LINE       TO LST-REC.
           WRITE LST-REC.
           ADD 1               TO WK-LINE-CNT.
           CLOSE RYOIN STMLST.
           DISPLAY 'RBSTM01C IN=' WK-IN-CNT ' LINE=' WK-LINE-CNT
                   ' NASI=' WK-NASI ' IOERR=' WK-IOERR
                   ' VALNG=' WK-VALNG.
           DISPLAY 'SOKEN=' WK-SOKEN ' SOKEI=' WK-SOKEI.
           MOVE 0              TO RETURN-CODE.
           STOP RUN.
       ABEND-RTN.
           DISPLAY 'RBSTM01C ABEND'.
           MOVE 16             TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * 請求検証 (R01 消費税対応時に追加)
      *****************************************************************
       RYO-VALID               SECTION.
       RVA-010.
           IF RYO-SPT-NO (1:2) NOT = '03'
               GO TO RVA-NG.
           IF RYO-SPT-NO (3:2) LESS '01' OR
              RYO-SPT-NO (3:2) GREATER '47'
               GO TO RVA-NG.
           IF RYO-SEIKYU-YM (1:4) LESS '1990' OR
              RYO-SEIKYU-YM (1:4) GREATER '2099'
               GO TO RVA-NG.
           IF RYO-SEIKYU-YM (5:2) LESS '01' OR
              RYO-SEIKYU-YM (5:2) GREATER '12'
               GO TO RVA-NG.
           IF RYO-ZEI-SEDAI NOT = '01' AND
              RYO-ZEI-SEDAI NOT = '02' AND
              RYO-ZEI-SEDAI NOT = '03' AND
              RYO-ZEI-SEDAI NOT = '04'
               GO TO RVA-NG.
           IF RYO-GOKEI LESS -9999999 OR
              RYO-GOKEI GREATER 99999999
               GO TO RVA-NG.
           IF RYO-SEIKYU-KBN NOT = '1' AND RYO-SEIKYU-KBN NOT = '2'
               GO TO RVA-NG.
           IF RYO-KENSHIN-BI NOT NUMERIC
               GO TO RVA-NG.
           IF RYO-NIWARI-NISSU LESS 1 OR RYO-NIWARI-NISSU GREATER 31
               GO TO RVA-NG.
           IF RYO-NYUKIN-FLG NOT = '0' AND RYO-NYUKIN-FLG NOT = '1'
               GO TO RVA-NG.
           IF RYO-SIYORYO LESS ZERO
               IF RYO-GOKEI GREATER ZERO
                   IF RYO-SEIKYU-KBN = '1'
                       GO TO RVA-NG
                   END-IF
               END-IF
           END-IF.
           GO TO RYO-VALID-EX.
       RVA-NG.
           ADD 1               TO WK-VALNG.
           GO TO MAIN-LOOP.
       RYO-VALID-EX.
           EXIT.
      *****************************************************************
      * 明細出力: 子を MORE が消えるまで呼ぶ (見出し+明細)
      *****************************************************************
       MEISAI-OUT              SECTION.
       MEO-010.
           MOVE JYT-SEI  (WS-JHIT) TO IN-SEI.
           MOVE JYT-NAME (WS-JHIT) TO IN-SIMEI.
           MOVE RYO-SPT-NO     TO IN-SPT.
           PERFORM WAREKI-HENKAN THRU WAREKI-HENKAN-EX.
           MOVE RYO-SIYORYO    TO IN-KENSU.
           MOVE RYO-GOKEI      TO IN-GOKEI.
           MOVE 'D'            TO STM-FUNC.
       MEO-020.
           CALL 'RBSTM02C' USING STM-FUNC STM-IN STM-LINE STM-MORE.
           MOVE STM-LINE       TO LST-REC.
           WRITE LST-REC.
           ADD 1               TO WK-LINE-CNT.
           IF STM-MORE = 'Y'
               GO TO MEO-020.
       MEISAI-OUT-EX.
           EXIT.
      *****************************************************************
      * 和暦編集 (内蔵版: 令和は R01.05.01 開始として扱う)
      *****************************************************************
       WAREKI-HENKAN           SECTION.
       WAR-010.
           COMPUTE W-YY = RYO-SEIKYU-YM / 100.
           COMPUTE W-MM = RYO-SEIKYU-YM - W-YY * 100.
           COMPUTE W-YMD = RYO-SEIKYU-YM * 100 + 1.
           MOVE SPACES         TO IN-WAREKI.
           IF W-YMD NOT LESS 20190501
               COMPUTE W-WYY = W-YY - 2018
               MOVE 'R'        TO IN-WAREKI (1:1)
               GO TO WAR-020.
           IF W-YMD NOT LESS 19890108
               COMPUTE W-WYY = W-YY - 1988
               MOVE 'H'        TO IN-WAREKI (1:1)
               GO TO WAR-020.
           COMPUTE W-WYY = W-YY - 1925.
           MOVE 'S'            TO IN-WAREKI (1:1).
       WAR-020.
           MOVE W-WYY          TO IN-WAREKI (2:2).
           MOVE '.'            TO IN-WAREKI (4:1).
           MOVE W-MM           TO IN-WAREKI (5:2).
       WAREKI-HENKAN-EX.
           EXIT.
      *****************************************************************
      * 契約テーブル (spt -> 需要家番号)
      *****************************************************************
       KYT-LOAD                SECTION.
       KYL-010.
           OPEN INPUT KYKMST.
       KYL-020.
           READ KYKMST
               AT END GO TO KYL-090.
           IF KYT-CNT NOT LESS 6000
               GO TO KYL-090.
           ADD 1               TO KYT-CNT.
           MOVE KYK-SPT-NO     TO KYT-KEY (KYT-CNT).
           MOVE KYK-JYU-NO     TO KYT-JNO (KYT-CNT).
           GO TO KYL-020.
       KYL-090.
           CLOSE KYKMST.
       KYT-LOAD-EX.
           EXIT.
       KYT-SORT                SECTION.
       KYS-010.
           MOVE 2              TO WS-I.
       KYS-020.
           IF WS-I GREATER KYT-CNT
               GO TO KYT-SORT-EX.
           MOVE KYT-E (WS-I)   TO KYT-SWAP.
           COMPUTE WS-J = WS-I - 1.
       KYS-030.
           IF WS-J LESS 1
               GO TO KYS-040.
           IF KYT-KEY (WS-J) NOT GREATER SWK-KEY
               GO TO KYS-040.
           MOVE KYT-E (WS-J)   TO KYT-E (WS-J + 1).
           SUBTRACT 1        FROM WS-J.
           GO TO KYS-030.
       KYS-040.
           MOVE KYT-SWAP       TO KYT-E (WS-J + 1).
           ADD 1               TO WS-I.
           GO TO KYS-020.
       KYT-SORT-EX.
           EXIT.
       KYT-SRCH                SECTION.
       KYR-010.
           MOVE ZERO           TO WS-HIT.
           MOVE 1              TO WS-LO.
           MOVE KYT-CNT        TO WS-HI.
       KYR-020.
           IF WS-LO GREATER WS-HI
               GO TO KYT-SRCH-EX.
           COMPUTE WS-MD = ( WS-LO + WS-HI ) / 2.
           IF KYT-KEY (WS-MD) = RYO-SPT-NO
               MOVE WS-MD      TO WS-HIT
               GO TO KYT-SRCH-EX.
           IF KYT-KEY (WS-MD) LESS RYO-SPT-NO
               COMPUTE WS-LO = WS-MD + 1
           ELSE
               COMPUTE WS-HI = WS-MD - 1.
           GO TO KYR-020.
       KYT-SRCH-EX.
           EXIT.
      *****************************************************************
      * 需要家テーブル (番号順に生成されている前提で無整列 -- S61)
      *****************************************************************
       JYT-LOAD                SECTION.
       JYL-010.
           OPEN INPUT JYUMST.
       JYL-020.
           READ JYUMST
               AT END GO TO JYL-090.
           IF JYT-CNT NOT LESS 5000
               GO TO JYL-090.
           ADD 1               TO JYT-CNT.
           MOVE JYU-NO         TO JYT-KEY (JYT-CNT).
           MOVE JYU-SEI        TO JYT-SEI (JYT-CNT).
           MOVE JYU-SIMEI      TO JYT-NAME (JYT-CNT).
           GO TO JYL-020.
       JYL-090.
           CLOSE JYUMST.
       JYT-LOAD-EX.
           EXIT.
       JYT-SRCH                SECTION.
       JYR-010.
           MOVE ZERO           TO WS-JHIT.
           MOVE 1              TO WS-LO.
           MOVE JYT-CNT        TO WS-HI.
       JYR-020.
           IF WS-LO GREATER WS-HI
               GO TO JYT-SRCH-EX.
           COMPUTE WS-MD = ( WS-LO + WS-HI ) / 2.
           IF JYT-KEY (WS-MD) = KYT-JNO (WS-HIT)
               MOVE WS-MD      TO WS-JHIT
               GO TO JYT-SRCH-EX.
           IF JYT-KEY (WS-MD) LESS KYT-JNO (WS-HIT)
               COMPUTE WS-LO = WS-MD + 1
           ELSE
               COMPUTE WS-HI = WS-MD - 1.
           GO TO JYR-020.
       JYT-SRCH-EX.
           EXIT.
