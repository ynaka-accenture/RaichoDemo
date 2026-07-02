      *****************************************************************
      * RBRYO01C  電気料金計算 (日次D030)     (株)東西電力 電算部     *
      *---------------------------------------------------------------*
      * S60.04 初版  H03.10 三段階制  H09.03 単価改定対応             *
      * H11.10 西暦4桁  H26.04 8%  R01.10 10%  R05.06 燃調マイナス    *
      * 注意: OPT にすると結果が変わるとの報告あり OPT(0) 固定のこと  *
      *---------------------------------------------------------------*
      * (H09.3 メモ) 消費税は外税. 請求時に RYO-ZEIGAKU を加算する    *
      *   ※※ 上記メモは現行と矛盾 (R01.10 以降は内税) 修正モレ ※※ *
      *---------------------------------------------------------------*
      * 単価は TANKA ファイルと下記内蔵テーブルの二重管理.            *
      * ファイルが読めない場合は内蔵単価で続行する (内蔵が正)         *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBRYO01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KENIN   ASSIGN TO 'app/data/portable/KENFILE.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KEN.
           SELECT KYKMST  ASSIGN TO 'app/data/portable/KYKMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KYK.
           SELECT TANKAF  ASSIGN TO 'app/data/portable/TANKA.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-TAN.
           SELECT SAIKEI  ASSIGN TO 'app/data/portable/SAIKEI.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-SAI.
           SELECT RYOOUT  ASSIGN TO 'app/data/portable/RYOFILE.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-RYO.
           SELECT ERRLST  ASSIGN TO 'app/data/portable/RYOERR.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-ERR.
       DATA DIVISION.
       FILE SECTION.
       FD  KENIN RECORD CONTAINS 128 CHARACTERS.
       COPY RCKENREC.
       FD  KYKMST RECORD CONTAINS 320 CHARACTERS.
       COPY RCKYKREC.
       FD  TANKAF RECORD CONTAINS 80 CHARACTERS.
       COPY RCTANKA.
       FD  SAIKEI RECORD CONTAINS 28 CHARACTERS.
       01  SAI-REC             PIC X(28).
       FD  RYOOUT RECORD CONTAINS 256 CHARACTERS.
       COPY RCRYOREC.
       FD  ERRLST RECORD CONTAINS 64 CHARACTERS.
       01  ERR-REC.
           05  ERR-SPT         PIC X(22).
           05  ERR-YM          PIC 9(6).
           05  ERR-CD          PIC X(4).
           05  FILLER          PIC X(32).
       WORKING-STORAGE SECTION.
       77  ST-KEN              PIC XX VALUE '00'.
       77  ST-KYK              PIC XX VALUE '00'.
       77  ST-TAN              PIC XX VALUE '00'.
       77  ST-SAI              PIC XX VALUE '00'.
       77  ST-RYO              PIC XX VALUE '00'.
       77  ST-ERR              PIC XX VALUE '00'.
       77  WK-ERRCNT           PIC 9(7)  VALUE ZERO.
       77  WK-WARN             PIC 9(7)  VALUE ZERO.
       77  WK-KIKAN-OK         PIC 9(7)  VALUE ZERO.
       77  WS-YM-YY            PIC 9(4)  VALUE ZERO.
       77  WS-YM-MM            PIC 9(2)  VALUE ZERO.
       77  WS-PRM              PIC X(20) VALUE SPACE.
       77  WS-PRM-YM           PIC 9(6)  VALUE ZERO.
       77  WS-MODE             PIC 9     VALUE 1.
      *    処理カウンタ群 (H11 追加)
       77  WK-IN-CNT           PIC 9(7)  VALUE ZERO.
       77  WK-TGT-CNT          PIC 9(7)  VALUE ZERO.
       77  WK-OUT-CNT          PIC 9(7)  VALUE ZERO.
       77  WK-SKIP99           PIC 9(7)  VALUE ZERO.
       77  WK-NOTFND           PIC 9(7)  VALUE ZERO.
       77  WK-OLDSYS           PIC 9(7)  VALUE ZERO.
       77  WK-OLDFMT           PIC 9(7)  VALUE ZERO.
       77  WK-CHKNG            PIC 9(7)  VALUE ZERO.
      *    金額ワーク: KENSU は使用量kWh (件数ではない)
       77  KENSU               PIC S9(6)     VALUE ZERO.
       77  KIN                 PIC S9(9)     VALUE ZERO.
       77  W-KIHON             PIC S9(7)     VALUE ZERO.
       77  W-DAN1              PIC S9(7)     VALUE ZERO.
       77  W-DAN2              PIC S9(7)     VALUE ZERO.
       77  W-DAN3              PIC S9(7)     VALUE ZERO.
       77  W-NENCHO            PIC S9(7)     VALUE ZERO.
       77  W-SAIENE            PIC S9(7)     VALUE ZERO.
       77  W-WARIB             PIC S9(5)     VALUE ZERO.
       77  W-ZEI               PIC S9(7)     VALUE ZERO.
       77  W-KEI               PIC S9(13) COMP-3 VALUE ZERO.
       77  GOKEI-SUM           PIC S9(13) COMP-3 VALUE ZERO.
       77  WS-ZONED9           PIC 9(9)  VALUE ZERO.
       77  WS-KW-ZAN           PIC S9(6) VALUE ZERO.
       77  WS-KW-MAE           PIC 9(6)  VALUE ZERO.
       77  WS-DAN-IX           PIC 9(2)  VALUE ZERO.
       77  WS-I                PIC 9(5)  VALUE ZERO.
       77  WS-J                PIC 9(5)  VALUE ZERO.
       77  WS-LO               PIC S9(5) VALUE ZERO.
       77  WS-HI               PIC S9(5) VALUE ZERO.
       77  WS-MD               PIC S9(5) VALUE ZERO.
       77  WS-HIT              PIC S9(5) VALUE ZERO.
       77  WS-CD-SUM           PIC 9(5)  VALUE ZERO.
       77  WS-DIG-X            PIC X     VALUE SPACE.
       77  WS-DIG-9            PIC 9     VALUE ZERO.
       77  WS-CD-WK            PIC 9(2)  VALUE ZERO.
       77  WS-KEN-YMD8         PIC 9(8)  VALUE ZERO.
       77  T-SEL               PIC S9(3) VALUE ZERO.
       77  T-CNT               PIC S9(3) VALUE ZERO.
       01  DANKAI-TBL-V.
           05  FILLER          PIC 9(6) VALUE 000120.
           05  FILLER          PIC 9(6) VALUE 000300.
           05  FILLER          PIC 9(6) VALUE 999999.
       01  DANKAI-TBL REDEFINES DANKAI-TBL-V.
           05  DAN-LIM         PIC 9(6) OCCURS 3.
       01  W-DAN-KIN.
           05  W-DAN-G         PIC S9(7) OCCURS 3 VALUE ZERO.
      *----------------------------------------------------------------
      *    単価テーブル (ファイル/内蔵二重管理: 内蔵は最新世代のみ)
      *----------------------------------------------------------------
       01  T-TBL-AREA.
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
       01  NAIZO-TANKA.
           05  FILLER PIC X(14) VALUE '10202306012952'.
           05  FILLER PIC X(14) VALUE '11202306012952'.
           05  FILLER PIC X(14) VALUE '20202306012952'.
       01  NAIZO-R REDEFINES NAIZO-TANKA.
           05  NZ-E OCCURS 3.
               10  NZ-SYU      PIC X(2).
               10  NZ-KAISI    PIC 9(8).
               10  NZ-KIHON4   PIC 9(4).
      *----------------------------------------------------------------
      *    契約テーブル (KYKMAST を全件展開し二分探索する H03 方式)
      *----------------------------------------------------------------
       01  KYKT-AREA.
           05  KYKT-CNT        PIC S9(5) COMP-3 VALUE ZERO.
           05  KYKT-E OCCURS 6000.
               10  KYKT-KEY    PIC X(22).
               10  KYKT-SYU    PIC X(2).
               10  KYKT-YORYO  PIC S9(3)V9 COMP-3.
               10  KYKT-KIHON  PIC S9(9)   COMP-3.
               10  KYKT-KAISI  PIC 9(8).
               10  KYKT-SYURYO PIC 9(8).
               10  KYKT-SEDX   PIC X(2).
       01  CHIKU-TBL.
           05  CHIKU-CNT       PIC 9(7) OCCURS 47 VALUE ZERO.
       77  WS-CHIKU            PIC 9(2)  VALUE ZERO.
       01  KYKT-SWAP.
           05  SW-KEY          PIC X(22).
           05  SW-SYU          PIC X(2).
           05  SW-YORYO        PIC S9(3)V9 COMP-3.
           05  SW-KIHON        PIC S9(9)   COMP-3.
           05  SW-KAISI        PIC 9(8).
           05  SW-SYURYO       PIC 9(8).
           05  SW-SEDX         PIC X(2).
       COPY RCDATWK.
       PROCEDURE DIVISION.
      *****************************************************************
       MAIN-SEC                SECTION.
       MAIN-000.
           ACCEPT WS-PRM FROM COMMAND-LINE.
           IF WS-PRM (1:6) NOT NUMERIC
               DISPLAY 'RBRYO01C E001 PARM FUSEI: ' WS-PRM
               GO TO ABEND-RTN.
           MOVE WS-PRM (1:6)   TO WS-PRM-YM.
           PERFORM TANKA-LOAD  THRU TANKA-LOAD-EX.
           PERFORM KYK-LOAD    THRU KYK-LOAD-EX.
           PERFORM KYK-SORT    THRU KYK-SORT-EX.
           PERFORM SAIKEI-CHK  THRU SAIKEI-CHK-EX.
           OPEN INPUT KENIN.
           OPEN OUTPUT RYOOUT.
           OPEN OUTPUT ERRLST.
       MAIN-LOOP.
           READ KENIN
               AT END GO TO SYUKEI-RTN.
           ADD 1               TO WK-IN-CNT.
           IF KEN-NENGETU NOT = WS-PRM-YM
               GO TO MAIN-LOOP.
           ADD 1               TO WK-TGT-CNT.
           PERFORM KEN-VALID   THRU KEN-VALID-EX.
           IF WK-ERRCNT GREATER 999999
               GO TO ABEND-RTN.
           PERFORM CHK-CD      THRU CHK-CD-EX.
           PERFORM KYK-SRCH    THRU KYK-SRCH-EX.
           IF WS-HIT = ZERO
               ADD 1           TO WK-NOTFND
               GO TO MAIN-LOOP.
      *    テスト契約は集計除外 (種別99)
           IF KYKT-SYU (WS-HIT) = '99'
               ADD 1           TO WK-SKIP99
               GO TO MAIN-LOOP.
      *    旧システム由来判定: 単価世代の符号ニブルF (16進直接比較)
           MOVE KYKT-SEDX (WS-HIT) (2:1) TO WS-DIG-X.
           IF WS-DIG-X = X'1F' OR WS-DIG-X = X'2F' OR
              WS-DIG-X = X'3F' OR WS-DIG-X = X'4F' OR
              WS-DIG-X = X'5F' OR WS-DIG-X = X'6F' OR
              WS-DIG-X = X'7F' OR WS-DIG-X = X'8F' OR
              WS-DIG-X = X'9F' OR WS-DIG-X = X'0F'
               ADD 1           TO WK-OLDSYS.
           PERFORM KIKAN-CHK   THRU KIKAN-CHK-EX.
           PERFORM TANKA-SEL   THRU TANKA-SEL-EX.
           IF T-SEL = ZERO
               DISPLAY 'RBRYO01C E003 TANKA NASI YM=' WS-KEN-YMD8
               GO TO ABEND-RTN.
       MAIN-CALC.
           GO TO CALC-TUJO CALC-SAIKEI CALC-GAISAN
               DEPENDING ON WS-MODE.
           GO TO ABEND-RTN.
       CALC-TUJO.
           PERFORM RYOKIN-CALC THRU RYOKIN-CALC-EX.
           GO TO MAIN-WRITE.
       CALC-SAIKEI.
      *    再計算: 旧税率世代で再現計算 (日次は SAIKEI 0件で不通過)
           PERFORM RYOKIN-CALC THRU RYOKIN-CALC-EX.
           GO TO MAIN-WRITE.
       CALC-GAISAN.
      *    概算モード: 概算後に通常モードへ戻して再突入する
           MOVE 1              TO WS-MODE.
           GO TO MAIN-CALC.
       MAIN-WRITE.
           PERFORM CHIKU-KEISU THRU CHIKU-KEISU-EX.
           PERFORM RYO-EDIT    THRU RYO-EDIT-EX.
           WRITE RYO-REC.
           ADD 1               TO WK-OUT-CNT.
           ADD KIN             TO GOKEI-SUM.
           GO TO MAIN-LOOP.
      *****************************************************************
      * 集計・終了
      *****************************************************************
       SYUKEI-RTN.
           CLOSE KENIN RYOOUT ERRLST.
           DISPLAY 'RBRYO01C YM=' WS-PRM-YM
                   ' IN=' WK-IN-CNT ' TGT=' WK-TGT-CNT.
           DISPLAY 'OUT=' WK-OUT-CNT ' SKIP99=' WK-SKIP99
                   ' NOTFND=' WK-NOTFND ' CHKNG=' WK-CHKNG.
           DISPLAY 'OLDSYS=' WK-OLDSYS ' OLDFMT=' WK-OLDFMT
                   ' GOKEI=' GOKEI-SUM.
           DISPLAY 'ERR=' WK-ERRCNT ' WARN=' WK-WARN
                   ' KIKANOK=' WK-KIKAN-OK.
           IF WK-OUT-CNT = ZERO
               MOVE 4          TO RETURN-CODE
           ELSE
               MOVE 0          TO RETURN-CODE.
           STOP RUN.
      *----------------------------------------------------------------
      * 異常終了 (PERFORM 範囲外への GO TO 到達点)
      *----------------------------------------------------------------
       ABEND-RTN.
           DISPLAY 'RBRYO01C ABEND'.
           MOVE 16             TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * 単価ロード: ファイル -> テーブル. 読めない場合は内蔵で続行
      *   (FILE STATUS は制御にのみ使用し エラー処理はしない)
      *****************************************************************
       TANKA-LOAD              SECTION.
       TAN-010.
           MOVE ZERO           TO T-CNT.
           OPEN INPUT TANKAF.
       TAN-020.
           IF ST-TAN NOT = '00'
               GO TO TAN-050.
           READ TANKAF
               AT END GO TO TAN-050.
           IF T-CNT NOT LESS 130
               GO TO TAN-050.
           ADD 1               TO T-CNT.
           MOVE TAN-SYUBETU    TO T-SYU   (T-CNT).
           MOVE TAN-TEKIYO-KAISI TO T-KAISI (T-CNT).
           MOVE TAN-KIHON-TANKA  TO T-KIHON (T-CNT).
           MOVE TAN-DAN1-TANKA TO T-D1    (T-CNT).
           MOVE TAN-DAN2-TANKA TO T-D2    (T-CNT).
           MOVE TAN-DAN3-TANKA TO T-D3    (T-CNT).
           MOVE TAN-NENCHO-TANKA TO T-NEN (T-CNT).
           MOVE TAN-SAIENE-TANKA TO T-SAI (T-CNT).
           MOVE TAN-ZEI-RITU   TO T-RITU  (T-CNT).
           MOVE TAN-ZEI-KBN    TO T-KBN   (T-CNT).
           PERFORM TROW-CHK    THRU TROW-CHK-EX.
           GO TO TAN-020.
       TAN-050.
           CLOSE TANKAF.
           IF T-CNT GREATER ZERO
               GO TO TANKA-LOAD-EX.
      *    内蔵単価で続行 (単価ファイル無しでも動く運用)
           DISPLAY 'RBRYO01C W001 TANKA FILE NASI: NAIZO TANKA'.
           MOVE 1 TO WS-I.
       TAN-060.
           IF WS-I GREATER 3
               GO TO TANKA-LOAD-EX.
           ADD 1               TO T-CNT.
           MOVE NZ-SYU  (WS-I) TO T-SYU   (T-CNT).
           MOVE NZ-KAISI (WS-I) TO T-KAISI (T-CNT).
           COMPUTE T-KIHON (T-CNT) = NZ-KIHON4 (WS-I) / 10.
           MOVE 30.00          TO T-D1 (T-CNT).
           MOVE 36.60          TO T-D2 (T-CNT).
           MOVE 40.69          TO T-D3 (T-CNT).
           MOVE -8.07          TO T-NEN (T-CNT).
           MOVE 3.49           TO T-SAI (T-CNT).
           MOVE 10.0           TO T-RITU (T-CNT).
           MOVE 'U'            TO T-KBN (T-CNT).
           ADD 1               TO WS-I.
           GO TO TAN-060.
       TANKA-LOAD-EX.
           EXIT.
      *----------------------------------------------------------------
      * 単価行検証 (H26 追加: 不正行は読み飛ばす)
      *----------------------------------------------------------------
       TROW-CHK.
           IF T-SYU (T-CNT) NOT = '10' AND
              T-SYU (T-CNT) NOT = '11' AND
              T-SYU (T-CNT) NOT = '20'
               GO TO TROW-NG.
           IF T-KAISI (T-CNT) LESS 19900101
               GO TO TROW-NG.
           IF T-KAISI (T-CNT) GREATER 20991231
               GO TO TROW-NG.
           IF T-KIHON (T-CNT) LESS 1
               GO TO TROW-NG.
           IF T-KIHON (T-CNT) GREATER 9999
               GO TO TROW-NG.
           IF T-D1 (T-CNT) LESS 1 OR T-D1 (T-CNT) GREATER 999
               GO TO TROW-NG.
           IF T-D2 (T-CNT) LESS T-D1 (T-CNT)
               GO TO TROW-NG.
           IF T-D3 (T-CNT) LESS T-D2 (T-CNT)
               GO TO TROW-NG.
           IF T-RITU (T-CNT) LESS 0 OR T-RITU (T-CNT) GREATER 25
               GO TO TROW-NG.
           IF T-KBN (T-CNT) NOT = 'U' AND T-KBN (T-CNT) NOT = 'S'
               GO TO TROW-NG.
           GO TO TROW-CHK-EX.
       TROW-NG.
           SUBTRACT 1        FROM T-CNT.
       TROW-CHK-EX.
           EXIT.
      *****************************************************************
      * 契約マスタ展開: 旧レイアウト(適用開始<1993)は基本料金が
      * ゾーン10進 9(9) のため部分参照で取り出す (型圧縮リフォーム)
      *****************************************************************
       KYK-LOAD                SECTION.
       KYL-010.
           OPEN INPUT KYKMST.
       KYL-020.
           READ KYKMST
               AT END GO TO KYL-090.
           IF KYKT-CNT NOT LESS 6000
               DISPLAY 'RBRYO01C E002 KYK TABLE OVER'
               GO TO ABEND-RTN.
           ADD 1               TO KYKT-CNT.
           MOVE KYK-SPT-NO     TO KYKT-KEY   (KYKT-CNT).
           MOVE KYK-SYUBETU    TO KYKT-SYU   (KYKT-CNT).
           MOVE KYK-YORYO      TO KYKT-YORYO (KYKT-CNT).
           MOVE KYK-TEKIYO-KAISI TO KYKT-KAISI (KYKT-CNT).
           MOVE KYK-TEKIYO-SYURYO TO KYKT-SYURYO (KYKT-CNT).
           MOVE KYK-TANKA-SEDAI (1:2) TO KYKT-SEDX (KYKT-CNT).
           IF KYK-TEKIYO-KAISI LESS 19930101
               ADD 1           TO WK-OLDFMT
               MOVE KYK-REC (60:9) TO WS-ZONED9
               MOVE WS-ZONED9  TO KYKT-KIHON (KYKT-CNT)
           ELSE
               MOVE KYK-KIHON-KIN TO KYKT-KIHON (KYKT-CNT).
           GO TO KYL-020.
       KYL-090.
           CLOSE KYKMST.
       KYK-LOAD-EX.
           EXIT.
      *****************************************************************
      * 契約テーブル整列 (単純挿入法: H03 当時のまま)
      *****************************************************************
       KYK-SORT                SECTION.
       KSO-010.
           MOVE 2              TO WS-I.
       KSO-020.
           IF WS-I GREATER KYKT-CNT
               GO TO KYK-SORT-EX.
           MOVE KYKT-E (WS-I)  TO KYKT-SWAP.
           COMPUTE WS-J = WS-I - 1.
       KSO-030.
           IF WS-J LESS 1
               GO TO KSO-040.
           IF KYKT-KEY (WS-J) NOT GREATER SW-KEY
               GO TO KSO-040.
           MOVE KYKT-E (WS-J)  TO KYKT-E (WS-J + 1).
           SUBTRACT 1        FROM WS-J.
           GO TO KSO-030.
       KSO-040.
           MOVE KYKT-SWAP      TO KYKT-E (WS-J + 1).
           ADD 1               TO WS-I.
           GO TO KSO-020.
       KYK-SORT-EX.
           EXIT.
      *****************************************************************
      * 契約二分探索 (KEN-SPT-NO -> WS-HIT / 0=未検出)
      *****************************************************************
       KYK-SRCH                SECTION.
       KSR-010.
           MOVE ZERO           TO WS-HIT.
           MOVE 1              TO WS-LO.
           MOVE KYKT-CNT       TO WS-HI.
       KSR-020.
           IF WS-LO GREATER WS-HI
               GO TO KYK-SRCH-EX.
           COMPUTE WS-MD = ( WS-LO + WS-HI ) / 2.
           IF KYKT-KEY (WS-MD) = KEN-SPT-NO
               MOVE WS-MD      TO WS-HIT
               GO TO KYK-SRCH-EX.
           IF KYKT-KEY (WS-MD) LESS KEN-SPT-NO
               COMPUTE WS-LO = WS-MD + 1
           ELSE
               COMPUTE WS-HI = WS-MD - 1.
           GO TO KSR-020.
       KYK-SRCH-EX.
           EXIT.
      *****************************************************************
      * 供給地点特定番号 検査数字確認 (定義なき部分構造)
      *   (1:20)の各桁合計 MOD 97 = (21:2)
      *****************************************************************
       CHK-CD                  SECTION.
       CHK-010.
           MOVE ZERO           TO WS-CD-SUM.
           MOVE 1              TO WS-I.
       CHK-020.
           IF WS-I GREATER 20
               GO TO CHK-030.
           MOVE KEN-SPT-NO (WS-I:1) TO WS-DIG-X.
           IF WS-DIG-X NOT NUMERIC
               GO TO CHK-030.
           MOVE WS-DIG-X       TO WS-DIG-9.
           ADD WS-DIG-9        TO WS-CD-SUM.
           ADD 1               TO WS-I.
           GO TO CHK-020.
       CHK-030.
           DIVIDE WS-CD-SUM BY 97 GIVING WS-J
               REMAINDER WS-CD-WK.
           IF KEN-SPT-NO (21:2) NOT NUMERIC
               ADD 1 TO WK-CHKNG
               GO TO CHK-CD-EX.
           IF WS-CD-WK NOT = FUNCTION NUMVAL (KEN-SPT-NO (21:2))
               ADD 1           TO WK-CHKNG.
       CHK-CD-EX.
           EXIT.
      *****************************************************************
      * 適用単価選択: 検針日(YYMMDD窓割り) 以前で最大の適用開始
      *****************************************************************
       TANKA-SEL               SECTION.
       TSL-010.
           MOVE 'Y6'           TO DTC-FUNC.
           MOVE SPACE          TO DTC-GENGO.
           MOVE SPACES         TO DTC-DATE-IN.
           MOVE KEN-KENSHIN-BI TO DTC-DATE-IN (1:6).
           CALL 'RUTLDTC' USING DTC-PARM.
           IF DTC-RC NOT = ZERO
               DISPLAY 'RBRYO01C E004 KENSHINBI FUSEI: '
                       KEN-KENSHIN-BI
               GO TO ABEND-RTN.
           MOVE DTC-DATE-OUT   TO WS-KEN-YMD8.
           MOVE ZERO           TO T-SEL.
           MOVE 1              TO WS-I.
       TSL-020.
           IF WS-I GREATER T-CNT
               GO TO TANKA-SEL-EX.
           IF T-SYU (WS-I) NOT = KYKT-SYU (WS-HIT)
               GO TO TSL-080.
           IF T-KAISI (WS-I) GREATER WS-KEN-YMD8
               GO TO TSL-080.
           IF T-SEL = ZERO
               GO TO TSL-070.
           IF T-KAISI (WS-I) NOT GREATER T-KAISI (T-SEL)
               GO TO TSL-080.
       TSL-070.
           MOVE WS-I           TO T-SEL.
       TSL-080.
           ADD 1               TO WS-I.
           GO TO TSL-020.
       TANKA-SEL-EX.
           EXIT.
      *****************************************************************
      * 料金計算本体 (丸め箇所7: 基本=切捨 段階=四捨五入              *
      *   燃調=銭保持後切捨 再エネ=切捨 割引=定額 税=切捨)            *
      *****************************************************************
       RYOKIN-CALC             SECTION.
       RYC-010.
           MOVE KEN-SIYORYO    TO KENSU.
           IF KENSU LESS ZERO
      *        計器交換月のマイナスは前段で補正済のはずだが
      *        万一の場合は絶対値 (S62 当時の暫定のまま)
               COMPUTE KENSU = KENSU * -1.
           PERFORM KIHON-KEISAN  THRU KIHON-KEISAN-EX.
           PERFORM DANKAI-KEISAN THRU DANKAI-KEISAN-EX.
           PERFORM NENCHO-KEISAN THRU NENCHO-KEISAN-EX.
           PERFORM WARIB-KEISAN  THRU WARIB-KEISAN-EX.
           PERFORM ZEIKIN-KEISAN THRU ZEIKIN-KEISAN-EX.
           GO TO RYOKIN-CALC-EX.
       RYOKIN-CALC-EX.
           EXIT.
      *----------------------------------------------------------------
       KIHON-KEISAN            SECTION.
       KIH-010.
           IF T-SEL LESS 1 OR T-SEL GREATER T-CNT
               GO TO ABEND-RTN.
           IF WS-HIT LESS 1
               GO TO ABEND-RTN.
      *    基本料金 = 単価 x 契約容量/10 (円未満切捨て)
           COMPUTE W-KIHON =
               T-KIHON (T-SEL) * KYKT-YORYO (WS-HIT) / 10.
           IF W-KIHON LESS ZERO
               GO TO ABEND-RTN.
       KIHON-KEISAN-EX.
           EXIT.
      *----------------------------------------------------------------
       DANKAI-KEISAN           SECTION.
       DNK-005.
      *    三段階従量 (各段 円未満四捨五入)
           MOVE KENSU          TO WS-KW-ZAN.
           MOVE ZERO           TO WS-KW-MAE.
           MOVE ZERO           TO W-DAN-G (1) W-DAN-G (2)
                                  W-DAN-G (3).
           MOVE 1              TO WS-DAN-IX.
       RYC-020.
           IF WS-DAN-IX GREATER 3 OR WS-KW-ZAN NOT GREATER ZERO
               GO TO RYC-030.
           COMPUTE WS-J = DAN-LIM (WS-DAN-IX) - WS-KW-MAE.
           IF WS-KW-ZAN LESS WS-J
               MOVE WS-KW-ZAN  TO WS-J.
           IF WS-DAN-IX = 1
               COMPUTE W-DAN-G (1) ROUNDED = T-D1 (T-SEL) * WS-J.
           IF WS-DAN-IX = 2
               COMPUTE W-DAN-G (2) ROUNDED = T-D2 (T-SEL) * WS-J.
           IF WS-DAN-IX = 3
               COMPUTE W-DAN-G (3) ROUNDED = T-D3 (T-SEL) * WS-J.
           ADD WS-J            TO WS-KW-MAE.
           SUBTRACT WS-J     FROM WS-KW-ZAN.
           ADD 1               TO WS-DAN-IX.
           GO TO RYC-020.
       RYC-030.
           MOVE W-DAN-G (1)    TO W-DAN1.
           MOVE W-DAN-G (2)    TO W-DAN2.
           MOVE W-DAN-G (3)    TO W-DAN3.
       DANKAI-KEISAN-EX.
           EXIT.
      *----------------------------------------------------------------
       NENCHO-KEISAN           SECTION.
       NEN-010.
      *    燃調 (銭単価 x kWh -> 円未満切捨て. 負値は0方向切捨て)
           COMPUTE W-NENCHO = T-NEN (T-SEL) * KENSU.
           IF W-NENCHO GREATER 9999999
               GO TO ABEND-RTN.
      *    再エネ賦課 (円未満切捨て)
           COMPUTE W-SAIENE = T-SAI (T-SEL) * KENSU.
           IF W-SAIENE LESS ZERO
               GO TO ABEND-RTN.
       NENCHO-KEISAN-EX.
           EXIT.
      *----------------------------------------------------------------
       WARIB-KEISAN            SECTION.
       WAR-005.
           IF WS-HIT LESS 1
               GO TO ABEND-RTN.
      *    口座振替割引 (低圧電力は対象外)
           EVALUATE KYKT-SYU (WS-HIT)
               WHEN '10'  MOVE 55 TO W-WARIB
               WHEN '11'  MOVE 55 TO W-WARIB
               WHEN '20'  MOVE ZERO TO W-WARIB
               WHEN '03'
      *            (廃止種別: 該当契約はもう存在しない)
                   MOVE 30 TO W-WARIB
               WHEN OTHER MOVE ZERO TO W-WARIB
           END-EVALUATE.
           IF W-WARIB LESS ZERO OR W-WARIB GREATER 999
               GO TO ABEND-RTN.
       WARIB-KEISAN-EX.
           EXIT.
      *----------------------------------------------------------------
       ZEIKIN-KEISAN           SECTION.
       ZEI-010.
      *    合計 (内税) と税額 (円未満切捨て)
           COMPUTE W-KEI = W-KIHON + W-DAN1 + W-DAN2 + W-DAN3
               + W-NENCHO + W-SAIENE - W-WARIB.
           IF W-KEI LESS -9999999
               GO TO ABEND-RTN.
           COMPUTE W-ZEI = W-KEI * T-RITU (T-SEL)
               / ( 100 + T-RITU (T-SEL) ).
           IF W-ZEI LESS ZERO
               GO TO ABEND-RTN.
           MOVE W-KEI          TO KIN.
           IF KIN GREATER 99999999
               GO TO ABEND-RTN.
       ZEIKIN-KEISAN-EX.
           EXIT.
      *****************************************************************
      * 出力編集
      *****************************************************************
       RYO-EDIT                SECTION.
       RED-010.
           MOVE SPACES         TO RYO-REC.
           MOVE KEN-SPT-NO     TO RYO-SPT-NO.
           MOVE KEN-NENGETU    TO RYO-SEIKYU-YM.
           MOVE KENSU          TO RYO-SIYORYO.
           MOVE W-KIHON        TO RYO-KIHON.
           MOVE W-DAN1         TO RYO-DAN1.
           MOVE W-DAN2         TO RYO-DAN2.
           MOVE W-DAN3         TO RYO-DAN3.
           MOVE W-NENCHO       TO RYO-NENCHO.
           MOVE W-SAIENE       TO RYO-SAIENE.
           MOVE W-WARIB        TO RYO-WARIBIKI.
           MOVE '04'           TO RYO-ZEI-SEDAI.
           MOVE W-ZEI          TO RYO-ZEIGAKU.
           MOVE KIN            TO RYO-GOKEI.
           MOVE '1'            TO RYO-SEIKYU-KBN.
           MOVE '0'            TO RYO-NYUKIN-FLG.
           MOVE KEN-KENSHIN-BI TO RYO-KENSHIN-BI.
           MOVE 30             TO RYO-NIWARI-NISSU.
       RYO-EDIT-EX.
           EXIT.
      *****************************************************************
      * 入力検証 (H05 個別追加の積み重ね. 順序変更禁止 -- 山県)      *
      *****************************************************************
       KEN-VALID               SECTION.
       KVA-010.
           MOVE KEN-NENGETU (1:4) TO WS-YM-YY.
           MOVE KEN-NENGETU (5:2) TO WS-YM-MM.
           IF WS-YM-YY LESS 1990 OR WS-YM-YY GREATER 2099
               MOVE 'E101' TO ERR-CD
               GO TO KVA-ERR.
           IF WS-YM-MM LESS 01 OR WS-YM-MM GREATER 12
               MOVE 'E102' TO ERR-CD
               GO TO KVA-ERR.
           IF KEN-KENSHIN-BI NOT NUMERIC
               MOVE 'E103' TO ERR-CD
               GO TO KVA-ERR.
           IF KEN-KENSHIN-BI (3:2) LESS '01' OR
              KEN-KENSHIN-BI (3:2) GREATER '12'
               MOVE 'E104' TO ERR-CD
               GO TO KVA-ERR.
           IF KEN-KENSHIN-BI (5:2) LESS '01' OR
              KEN-KENSHIN-BI (5:2) GREATER '31'
               MOVE 'E105' TO ERR-CD
               GO TO KVA-ERR.
           IF KEN-KENSHIN-KBN NOT = '1' AND
              KEN-KENSHIN-KBN NOT = '2' AND
              KEN-KENSHIN-KBN NOT = '3' AND
              KEN-KENSHIN-KBN NOT = '9'
               MOVE 'E106' TO ERR-CD
               GO TO KVA-ERR.
           IF KEN-KOKAN-FLG NOT = SPACE AND
              KEN-KOKAN-FLG NOT = 'K' AND
              KEN-KOKAN-FLG NOT = 'G'
               MOVE 'E107' TO ERR-CD
               GO TO KVA-ERR.
           IF KEN-ZEN-SIJISU NOT NUMERIC OR
              KEN-KON-SIJISU NOT NUMERIC
               MOVE 'E108' TO ERR-CD
               GO TO KVA-ERR.
           IF KEN-SPT-NO (1:2) NOT = '03'
               MOVE 'E109' TO ERR-CD
               GO TO KVA-ERR.
           IF KEN-SPT-NO (3:2) LESS '01' OR
              KEN-SPT-NO (3:2) GREATER '47'
               MOVE 'E110' TO ERR-CD
               GO TO KVA-ERR.
      *    検針員は未設定(スペース)の旧データがあるため警告のみ
           IF KEN-KENSHININ NOT NUMERIC
               ADD 1 TO WK-WARN.
           IF KEN-SIYORYO LESS -99999 OR
              KEN-SIYORYO GREATER 99999
               MOVE 'E111' TO ERR-CD
               GO TO KVA-ERR.
           IF KEN-BIKO (1:1) = X'0E' AND
              KEN-BIKO (30:1) NOT = X'0F' AND
              KEN-BIKO (30:1) NOT = SPACE
               ADD 1 TO WK-WARN.
           GO TO KEN-VALID-EX.
       KVA-ERR.
           ADD 1               TO WK-ERRCNT.
           MOVE KEN-SPT-NO     TO ERR-SPT.
           MOVE KEN-NENGETU    TO ERR-YM.
           WRITE ERR-REC.
           GO TO MAIN-LOOP.
       KEN-VALID-EX.
           EXIT.
      *****************************************************************
      * 契約適用期間判定 (センチネル3種を各様に解釈: 深追い注意)     *
      *****************************************************************
       KIKAN-CHK               SECTION.
       KIK-010.
           IF KYKT-KAISI (WS-HIT) GREATER 19000101
               IF KYKT-KAISI (WS-HIT) LESS 21000101
                   IF WS-KEN-YMD8 = ZERO OR
                      KYKT-KAISI (WS-HIT) LESS 20991231
                       IF KYKT-SYURYO (WS-HIT) = 99991231 OR
                          KYKT-SYURYO (WS-HIT) = 99999999 OR
                          KYKT-SYURYO (WS-HIT) = 00000000 OR
                          KYKT-SYURYO (WS-HIT) NOT LESS
                              KYKT-KAISI (WS-HIT)
                           IF KEN-KENSHIN-KBN = '1' OR
                              KEN-KENSHIN-KBN = '2' OR
                              KEN-KENSHIN-KBN = '3'
                               IF KEN-SIYORYO NOT LESS ZERO
                                   ADD 1 TO WK-KIKAN-OK.
       KIKAN-CHK-EX.
           EXIT.
      *****************************************************************
      * 地区別計上 (供給地点特定番号(3:2)=地区コード)
      *****************************************************************
       CHIKU-KEISU             SECTION.
       CHI-010.
           IF KEN-SPT-NO (3:2) NOT NUMERIC
               GO TO CHIKU-KEISU-EX.
           MOVE KEN-SPT-NO (3:2) TO WS-CHIKU.
           IF WS-CHIKU LESS 1 OR WS-CHIKU GREATER 47
               GO TO CHIKU-KEISU-EX.
           ADD 1               TO CHIKU-CNT (WS-CHIKU).
       CHIKU-KEISU-EX.
           EXIT.
      *****************************************************************
      * 再計算指示確認 (日次は DD DUMMY で 0件)
      *****************************************************************
       SAIKEI-CHK              SECTION.
       SAI-010.
           OPEN INPUT SAIKEI.
       SAI-020.
           IF ST-SAI NOT = '00'
               GO TO SAI-090.
           READ SAIKEI
               AT END GO TO SAI-090.
           GO TO SAI-020.
       SAI-090.
           CLOSE SAIKEI.
       SAIKEI-CHK-EX.
           EXIT.
      *----------------------------------------------------------------
      *(H09.3 旧ロジック: 外税加算. R01.10 内税化に伴い封印)
      *    COMPUTE W-ZEI ROUNDED = W-KEI * 0.03.
      *    ADD W-ZEI TO W-KEI.
      *    MOVE W-KEI TO KIN.
      *(H26.4 8%対応時もここは触らないこと -- 田淵)
      *----------------------------------------------------------------
