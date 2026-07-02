      *****************************************************************
      * RBRYO02C  請求媒体データ作成 (日次D050)   東西電力 電算部     *
      *---------------------------------------------------------------*
      * H02.04 初版 (口振依頼媒体)  H07.11 対応表転記化  H26.10 P制度 *
      * 転記は MOVE CORRESPONDING による (項目名一致で決まるため      *
      *   コピー句の項目名変更時は本プログラムの確認必須.             *
      *   H07 障害 No.212 の再発防止事項)                             *
      * ポイントは下4桁のみ有効 (上位桁切捨てが仕様 -- H26 P制度要領) *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBRYO02C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RYOIN   ASSIGN TO 'app/data/portable/RYOFILE.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-RYO.
           SELECT KYKMST  ASSIGN TO 'app/data/portable/KYKMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KYK.
           SELECT SEIOUT  ASSIGN TO 'app/data/portable/SEIKYU.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-SEI.
       DATA DIVISION.
       FILE SECTION.
       FD  RYOIN RECORD CONTAINS 256 CHARACTERS.
       COPY RCRYOREC.
       FD  KYKMST RECORD CONTAINS 320 CHARACTERS.
       COPY RCKYKREC.
       FD  SEIOUT RECORD CONTAINS 120 CHARACTERS.
       01  SEI-REC             PIC X(120).
       WORKING-STORAGE SECTION.
       77  ST-RYO              PIC XX VALUE '00'.
       77  ST-KYK              PIC XX VALUE '00'.
       77  ST-SEI              PIC XX VALUE '00'.
       77  WK-IN-CNT           PIC 9(7)  VALUE ZERO.
       77  WK-OUT-CNT          PIC 9(7)  VALUE ZERO.
       77  WK-NASI             PIC 9(7)  VALUE ZERO.
       77  WK-OGUCHI           PIC 9(7)  VALUE ZERO.
       77  WK-MAILOK           PIC 9(7)  VALUE ZERO.
       77  IRAI-GOKEI          PIC S9(13) COMP-3 VALUE ZERO.
       77  POINT-GOKEI         PIC 9(9)  VALUE ZERO.
       77  WS-I                PIC 9(5)  VALUE ZERO.
       77  WS-J                PIC 9(5)  VALUE ZERO.
       77  WS-LO               PIC S9(5) VALUE ZERO.
       77  WS-HI               PIC S9(5) VALUE ZERO.
       77  WS-MD               PIC S9(5) VALUE ZERO.
       77  WS-HIT              PIC S9(5) VALUE ZERO.
      *    ポイント: 下4桁のみ有効 (B桁あふれは仕様)
       77  W-POINT             PIC 9(4)  VALUE ZERO.
       77  W-PKENSAN           PIC 9(9)  VALUE ZERO.
       77  WK-PKEN-NG          PIC 9(7)  VALUE ZERO.
       01  KYU-BUNPU.
           05  KYU-CNT         PIC 9(7) OCCURS 4 VALUE ZERO.
      *----------------------------------------------------------------
      *    転記元 (料金側): 名前一致で SEI 編集域へ転記される
      *----------------------------------------------------------------
       01  W-RYO-AREA.
           05  SPT-NO          PIC X(22).
           05  SEIKYU-YM       PIC 9(6).
           05  GOKEI           PIC S9(9).
           05  KENSHIN-BI      PIC 9(6).
       01  W-KZA-AREA.
           05  GINKO           PIC 9(4).
           05  SITEN           PIC 9(3).
           05  YOKIN-SYU       PIC 9(1).
           05  KOZA            PIC 9(7).
      *----------------------------------------------------------------
      *    媒体編集域: 上記と同名項目のみが CORRESPONDING で埋まる
      *    (SPT-NO は媒体仕様 20桁. 検査数字2桁は媒体では持たない)
      *----------------------------------------------------------------
       01  W-SEI-AREA.
           05  SEI-KBN         PIC X(1).
           05  GINKO           PIC 9(4).
           05  SITEN           PIC 9(3).
           05  YOKIN-SYU       PIC 9(1).
           05  KOZA            PIC 9(7).
           05  SPT-NO          PIC X(20).
           05  SEIKYU-YM       PIC 9(6).
           05  GOKEI           PIC S9(9).
           05  KENSHIN-BI      PIC 9(6).
           05  SEI-POINT       PIC 9(4).
           05  SEI-YOBI        PIC X(8).
           05  SEI-YOBI-P REDEFINES SEI-YOBI
                               PIC S9(13)V99 COMP-3.
           05  FILLER          PIC X(51).
      *----------------------------------------------------------------
       01  KZT-AREA.
           05  KZT-CNT         PIC S9(5) COMP-3 VALUE ZERO.
           05  KZT-E OCCURS 6000.
               10  KZT-KEY     PIC X(22).
               10  KZT-GINKO   PIC 9(4).
               10  KZT-SITEN   PIC 9(3).
               10  KZT-YOKIN   PIC 9(1).
               10  KZT-KOZA    PIC 9(7).
       01  KZT-SWAP.
           05  SW-KEY          PIC X(22).
           05  SW-GINKO        PIC 9(4).
           05  SW-SITEN        PIC 9(3).
           05  SW-YOKIN        PIC 9(1).
           05  SW-KOZA         PIC 9(7).
       PROCEDURE DIVISION.
       MAIN-SEC                SECTION.
       MAIN-000.
           PERFORM KZT-LOAD    THRU KZT-LOAD-EX.
           PERFORM KZT-SORT    THRU KZT-SORT-EX.
           OPEN INPUT RYOIN.
           OPEN OUTPUT SEIOUT.
       MAIN-LOOP.
           READ RYOIN
               AT END GO TO SYUKEI-RTN.
           ADD 1               TO WK-IN-CNT.
           PERFORM RYO-VALID   THRU RYO-VALID-EX.
           PERFORM KZT-SRCH    THRU KZT-SRCH-EX.
           IF WS-HIT = ZERO
               ADD 1           TO WK-NASI
               GO TO MAIN-LOOP.
           IF KZT-KOZA (WS-HIT) = ZERO
               IF KZT-GINKO (WS-HIT) = ZERO
                   IF RYO-SEIKYU-KBN = '1'
                       ADD 1 TO WK-NASI
                       GO TO MAIN-LOOP
                   END-IF
               END-IF
           END-IF.
           PERFORM SEI-EDIT    THRU SEI-EDIT-EX.
           MOVE W-SEI-AREA     TO SEI-REC.
           WRITE SEI-REC.
           ADD 1               TO WK-OUT-CNT.
           ADD GOKEI OF W-RYO-AREA TO IRAI-GOKEI.
           ADD W-POINT         TO POINT-GOKEI.
           GO TO MAIN-LOOP.
       SYUKEI-RTN.
           CLOSE RYOIN SEIOUT.
           DISPLAY 'RBRYO02C IN=' WK-IN-CNT ' OUT=' WK-OUT-CNT
                   ' NASI=' WK-NASI.
           DISPLAY 'PKENNG=' WK-PKEN-NG ' KYU=' KYU-CNT (1) '/'
                   KYU-CNT (2) '/' KYU-CNT (3) '/' KYU-CNT (4).
           DISPLAY 'OGUCHI=' WK-OGUCHI ' MAILOK=' WK-MAILOK
                   ' IRAI=' IRAI-GOKEI ' POINT=' POINT-GOKEI.
           MOVE 0              TO RETURN-CODE.
           STOP RUN.
       ABEND-RTN.
           DISPLAY 'RBRYO02C ABEND'.
           MOVE 16             TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * 媒体編集: 対応表転記 (名前一致) + 予備域二重利用
      *****************************************************************
       SEI-EDIT                SECTION.
       SED-010.
           MOVE SPACES         TO W-SEI-AREA.
           MOVE RYO-SPT-NO     TO SPT-NO OF W-RYO-AREA.
           MOVE RYO-SEIKYU-YM  TO SEIKYU-YM OF W-RYO-AREA.
           MOVE RYO-GOKEI      TO GOKEI OF W-RYO-AREA.
           MOVE RYO-KENSHIN-BI TO KENSHIN-BI OF W-RYO-AREA.
           MOVE KZT-GINKO (WS-HIT) TO GINKO OF W-KZA-AREA.
           MOVE KZT-SITEN (WS-HIT) TO SITEN OF W-KZA-AREA.
           MOVE KZT-YOKIN (WS-HIT) TO YOKIN-SYU OF W-KZA-AREA.
           MOVE KZT-KOZA  (WS-HIT) TO KOZA OF W-KZA-AREA.
      *    ここが対応表転記 (媒体側 SPT-NO は 20桁: 右2桁は落ちる)
           MOVE CORRESPONDING W-RYO-AREA TO W-SEI-AREA.
           MOVE CORRESPONDING W-KZA-AREA TO W-SEI-AREA.
           MOVE '2'            TO SEI-KBN.
      *    ポイント: 合計x3 の下4桁 (上位桁切捨てが仕様)
           COMPUTE W-POINT = FUNCTION MOD
               ( GOKEI OF W-RYO-AREA * 3  10000 ).
           MOVE W-POINT        TO SEI-POINT.
           PERFORM POINT-KENSAN THRU POINT-KENSAN-EX.
           PERFORM KYU-KEISU    THRU KYU-KEISU-EX.
      *    予備域: 大口は調整額(PACKED) 小口は郵送可否フラグ(文字)
           IF GOKEI OF W-RYO-AREA GREATER 20000
               COMPUTE SEI-YOBI-P =
                   GOKEI OF W-RYO-AREA * 0.05
               ADD 1           TO WK-OGUCHI
               GO TO SEI-EDIT-EX.
           IF GOKEI OF W-RYO-AREA LESS ZERO
               MOVE 'HENKIN  '  TO SEI-YOBI
               ADD 1           TO WK-MAILOK
               GO TO SEI-EDIT-EX.
           MOVE 'MAIL OK '     TO SEI-YOBI.
           ADD 1               TO WK-MAILOK.
       SEI-EDIT-EX.
           EXIT.
      *****************************************************************
      * ポイント検算: 10000 を引き続けて下4桁を得る
      *   (剰余関数を使わない旧作法の検算. 仕様の再表現)
      *****************************************************************
       POINT-KENSAN            SECTION.
       PKN-010.
           IF GOKEI OF W-RYO-AREA LESS ZERO
               GO TO POINT-KENSAN-EX.
           COMPUTE W-PKENSAN = GOKEI OF W-RYO-AREA * 3.
       PKN-020.
           IF W-PKENSAN LESS 10000
               GO TO PKN-030.
           SUBTRACT 10000    FROM W-PKENSAN.
           GO TO PKN-020.
       PKN-030.
           IF W-PKENSAN NOT = W-POINT
               ADD 1           TO WK-PKEN-NG
               GO TO POINT-KENSAN-EX.
           IF W-PKENSAN GREATER 9999
               ADD 1           TO WK-PKEN-NG.
       POINT-KENSAN-EX.
           EXIT.
      *****************************************************************
      * 金額階級計数
      *****************************************************************
       KYU-KEISU               SECTION.
       KYU-010.
           IF GOKEI OF W-RYO-AREA LESS 5000
               ADD 1 TO KYU-CNT (1)
               GO TO KYU-KEISU-EX.
           IF GOKEI OF W-RYO-AREA LESS 10000
               ADD 1 TO KYU-CNT (2)
               GO TO KYU-KEISU-EX.
           IF GOKEI OF W-RYO-AREA LESS 20000
               ADD 1 TO KYU-CNT (3)
               GO TO KYU-KEISU-EX.
           ADD 1               TO KYU-CNT (4).
       KYU-KEISU-EX.
           EXIT.
      *****************************************************************
      * 請求検証
      *****************************************************************
       RYO-VALID               SECTION.
       RVA-010.
           IF RYO-SPT-NO (1:2) NOT = '03'
               GO TO RVA-NG.
           IF RYO-SPT-NO (3:2) LESS '01' OR
              RYO-SPT-NO (3:2) GREATER '47'
               GO TO RVA-NG.
           IF RYO-SPT-NO (21:2) NOT NUMERIC
               GO TO RVA-NG.
           IF RYO-SEIKYU-YM (5:2) LESS '01' OR
              RYO-SEIKYU-YM (5:2) GREATER '12'
               GO TO RVA-NG.
           IF RYO-GOKEI LESS -9999999 OR
              RYO-GOKEI GREATER 99999999
               GO TO RVA-NG.
           IF RYO-KENSHIN-BI NOT NUMERIC
               GO TO RVA-NG.
           IF RYO-SEIKYU-KBN NOT = '1' AND RYO-SEIKYU-KBN NOT = '2'
               GO TO RVA-NG.
           IF RYO-NYUKIN-FLG NOT = '0' AND RYO-NYUKIN-FLG NOT = '1'
               GO TO RVA-NG.
           IF RYO-NIWARI-NISSU LESS 1 OR RYO-NIWARI-NISSU GREATER 31
               GO TO RVA-NG.
           GO TO RYO-VALID-EX.
       RVA-NG.
           ADD 1               TO WK-NASI.
           GO TO MAIN-LOOP.
       RYO-VALID-EX.
           EXIT.
      *****************************************************************
      * 口座テーブル (旧レイアウトは口座域 +4 バイト後方)
      *****************************************************************
       KZT-LOAD                SECTION.
       KZL-010.
           OPEN INPUT KYKMST.
       KZL-020.
           READ KYKMST
               AT END GO TO KZL-090.
           IF KZT-CNT NOT LESS 6000
               GO TO KZL-090.
           IF KYK-SPT-NO (1:1) = LOW-VALUE
               GO TO KZL-030.
           IF KYK-TEISI-FLG NOT = '0' AND KYK-TEISI-FLG NOT = '1'
               GO TO KZL-030.
           GO TO KZL-040.
       KZL-030.
           CONTINUE.
       KZL-040.
           ADD 1               TO KZT-CNT.
           MOVE KYK-SPT-NO     TO KZT-KEY (KZT-CNT).
           IF KYK-TEKIYO-KAISI LESS 19930101
               MOVE KYK-REC (69:4) TO KZT-GINKO (KZT-CNT)
               MOVE KYK-REC (73:3) TO KZT-SITEN (KZT-CNT)
               MOVE KYK-REC (76:1) TO KZT-YOKIN (KZT-CNT)
               MOVE KYK-REC (77:7) TO KZT-KOZA (KZT-CNT)
           ELSE
               MOVE KYK-GINKO      TO KZT-GINKO (KZT-CNT)
               MOVE KYK-SITEN      TO KZT-SITEN (KZT-CNT)
               MOVE KYK-YOKIN-SYU  TO KZT-YOKIN (KZT-CNT)
               MOVE KYK-KOZA       TO KZT-KOZA (KZT-CNT).
           GO TO KZL-020.
       KZL-090.
           CLOSE KYKMST.
       KZT-LOAD-EX.
           EXIT.
       KZT-SORT                SECTION.
       KZS-010.
           MOVE 2              TO WS-I.
       KZS-020.
           IF WS-I GREATER KZT-CNT
               GO TO KZT-SORT-EX.
           MOVE KZT-E (WS-I)   TO KZT-SWAP.
           COMPUTE WS-J = WS-I - 1.
       KZS-030.
           IF WS-J LESS 1
               GO TO KZS-040.
           IF KZT-KEY (WS-J) NOT GREATER SW-KEY
               GO TO KZS-040.
           MOVE KZT-E (WS-J)   TO KZT-E (WS-J + 1).
           SUBTRACT 1        FROM WS-J.
           GO TO KZS-030.
       KZS-040.
           MOVE KZT-SWAP       TO KZT-E (WS-J + 1).
           ADD 1               TO WS-I.
           GO TO KZS-020.
       KZT-SORT-EX.
           EXIT.
       KZT-SRCH                SECTION.
       KZR-010.
           MOVE ZERO           TO WS-HIT.
           MOVE 1              TO WS-LO.
           MOVE KZT-CNT        TO WS-HI.
       KZR-020.
           IF WS-LO GREATER WS-HI
               GO TO KZT-SRCH-EX.
           COMPUTE WS-MD = ( WS-LO + WS-HI ) / 2.
           IF KZT-KEY (WS-MD) = RYO-SPT-NO
               MOVE WS-MD      TO WS-HIT
               GO TO KZT-SRCH-EX.
           IF KZT-KEY (WS-MD) LESS RYO-SPT-NO
               COMPUTE WS-LO = WS-MD + 1
           ELSE
               COMPUTE WS-HI = WS-MD - 1.
           GO TO KZR-020.
       KZT-SRCH-EX.
           EXIT.
