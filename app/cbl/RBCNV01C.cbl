      *****************************************************************
      * RBCNV01C  契約マスタ 新旧レイアウト変換    東西電力 電算部    *
      *---------------------------------------------------------------*
      * H18.04 型圧縮リフォーム時に作成. 以後 移行・退避で随時使用    *
      * 旧 (適用開始<1993): 基本料金がゾーン9桁, 以降 +4 バイト後方   *
      * 新: 基本料金 PACKED 5バイト (現行コピー句 RCKYKREC)           *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBCNV01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KYKIN   ASSIGN TO 'app/data/portable/KYKMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-IN.
           SELECT KYKOUT  ASSIGN TO 'app/data/portable/KYKNEW.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-OUT.
           SELECT CNVERR  ASSIGN TO 'app/data/portable/CNVERR.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-ERR.
       DATA DIVISION.
       FILE SECTION.
       FD  KYKIN RECORD CONTAINS 320 CHARACTERS.
       01  IN-REC              PIC X(320).
       01  IN-REC-R REDEFINES IN-REC.
           05  IN-SPT          PIC X(22).
           05  IN-JYU          PIC 9(10).
           05  IN-SYU          PIC X(2).
           05  FILLER          PIC X(8).
           05  IN-KAISI        PIC 9(8).
           05  IN-SYURYO       PIC 9(8).
           05  IN-TEISI        PIC X(1).
           05  FILLER          PIC X(261).
       FD  KYKOUT RECORD CONTAINS 320 CHARACTERS.
       COPY RCKYKREC.
       FD  CNVERR RECORD CONTAINS 32 CHARACTERS.
       01  ERR-REC             PIC X(32).
       WORKING-STORAGE SECTION.
       77  ST-IN               PIC XX VALUE '00'.
       77  ST-OUT              PIC XX VALUE '00'.
       77  ST-ERR              PIC XX VALUE '00'.
       77  WK-IN-CNT           PIC 9(7) VALUE ZERO.
       77  WK-OLD-CNT          PIC 9(7) VALUE ZERO.
       77  WK-NEW-CNT          PIC 9(7) VALUE ZERO.
       77  WK-NG-CNT           PIC 9(7) VALUE ZERO.
       77  W-ZONED9            PIC 9(9) VALUE ZERO.
       77  WS-I                PIC 9(3) VALUE ZERO.
       77  WK-VAL2NG           PIC 9(7) VALUE ZERO.
       01  NENDAI-TBL.
           05  NENDAI-CNT      PIC 9(7) OCCURS 5 VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN INPUT KYKIN.
           OPEN OUTPUT KYKOUT.
           OPEN OUTPUT CNVERR.
       MAIN-LOOP.
           READ KYKIN
               AT END GO TO SYUKEI-RTN.
           ADD 1               TO WK-IN-CNT.
           PERFORM IN-VALID    THRU IN-VALID-EX.
           IF IN-KAISI LESS 19930101
               PERFORM OLD-CONV THRU OLD-CONV-EX
               ADD 1           TO WK-OLD-CNT
           ELSE
               PERFORM NEW-COPY THRU NEW-COPY-EX
               ADD 1           TO WK-NEW-CNT.
           PERFORM NEW-VALID   THRU NEW-VALID-EX.
           PERFORM NENDAI-KEISU THRU NENDAI-KEISU-EX.
           PERFORM KANA-SCAN    THRU KANA-SCAN-EX.
           WRITE KYK-REC.
           GO TO MAIN-LOOP.
       SYUKEI-RTN.
           CLOSE KYKIN KYKOUT CNVERR.
           DISPLAY 'RBCNV01C IN=' WK-IN-CNT ' OLD=' WK-OLD-CNT
                   ' NEW=' WK-NEW-CNT ' NG=' WK-NG-CNT
                   ' VAL2=' WK-VAL2NG.
           DISPLAY 'NENDAI=' NENDAI-CNT (1) '/' NENDAI-CNT (2)
                   '/' NENDAI-CNT (3) '/' NENDAI-CNT (4)
                   '/' NENDAI-CNT (5).
           IF WK-NG-CNT GREATER ZERO
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * 旧->新 変換: 前半共通部はそのまま, 基本料金をPACK,
      * 口座域以降を 4 バイト前へ詰める
      *****************************************************************
       OLD-CONV                SECTION.
       OLD-010.
           MOVE SPACES         TO KYK-REC.
           MOVE IN-REC (1:59)  TO KYK-REC (1:59).
           MOVE IN-REC (60:9)  TO W-ZONED9.
           MOVE W-ZONED9       TO KYK-KIHON-KIN.
           MOVE IN-REC (69:4)  TO KYK-REC (65:4).
           MOVE IN-REC (73:3)  TO KYK-REC (69:3).
           MOVE IN-REC (76:1)  TO KYK-REC (72:1).
           MOVE IN-REC (77:7)  TO KYK-REC (73:7).
           MOVE IN-REC (84:30) TO KYK-REC (80:30).
           MOVE IN-REC (114:12) TO KYK-REC (110:12).
           MOVE IN-REC (126:27) TO KYK-REC (122:27).
           MOVE IN-REC (153:4)  TO KYK-REC (149:4).
           MOVE IN-REC (157:16) TO KYK-REC (153:16).
           MOVE IN-REC (173:148) TO KYK-REC (169:148).
       OLD-CONV-EX.
           EXIT.
      *****************************************************************
      * 新レイアウトはそのまま転記 (将来の版判定拡張点)
      *****************************************************************
       NEW-COPY                SECTION.
       NCP-010.
           IF IN-KAISI GREATER 20991231
               ADD 1 TO WK-VAL2NG
               GO TO NCP-020.
       NCP-020.
           MOVE IN-REC         TO KYK-REC.
       NEW-COPY-EX.
           EXIT.
      *****************************************************************
      * 変換後検証: 新レイアウトとして全項目が妥当であること
      *****************************************************************
       NEW-VALID               SECTION.
       NVA-010.
           IF KYK-GINKO NOT NUMERIC
               GO TO NVA-NG.
           IF KYK-SITEN NOT NUMERIC
               GO TO NVA-NG.
           IF KYK-YOKIN-SYU NOT = '1' AND KYK-YOKIN-SYU NOT = '2'
               GO TO NVA-NG.
           IF KYK-KOZA NOT NUMERIC
               GO TO NVA-NG.
           IF KYK-SAISYU-KENSHIN NOT NUMERIC
               GO TO NVA-NG.
           IF KYK-ZEN-SIJISU NOT NUMERIC
               GO TO NVA-NG.
           IF KYK-SMART-FLG NOT = 'Y' AND KYK-SMART-FLG NOT = 'N'
               GO TO NVA-NG.
           IF KYK-DMD-TAISYO NOT = 'Y' AND KYK-DMD-TAISYO NOT = 'N'
               GO TO NVA-NG.
           IF KYK-NENCHO-KBN NOT = '1' AND KYK-NENCHO-KBN NOT = '0'
               GO TO NVA-NG.
           IF KYK-SAIENE-KBN NOT = '1' AND KYK-SAIENE-KBN NOT = '0'
               GO TO NVA-NG.
           IF KYK-KAIYAKU-YOTEI NOT NUMERIC
               GO TO NVA-NG.
           IF KYK-CHOKU-KBN NOT = '0' AND KYK-CHOKU-KBN NOT = '1'
               GO TO NVA-NG.
           IF KYK-DENKA-CD NOT = 'EL01' AND KYK-DENKA-CD NOT = SPACE
               GO TO NVA-NG.
           IF KYK-KOSHIN-BI NOT NUMERIC
               GO TO NVA-NG.
           GO TO NEW-VALID-EX.
       NVA-NG.
           ADD 1               TO WK-VAL2NG.
       NEW-VALID-EX.
           EXIT.
      *****************************************************************
      * 年代別計数 (旧レイアウト分布の把握用)
      *****************************************************************
       NENDAI-KEISU            SECTION.
       NEN-010.
           MOVE 1              TO WS-I.
       NEN-020.
           IF WS-I GREATER 5
               GO TO NENDAI-KEISU-EX.
           IF KYK-TEKIYO-KAISI LESS 19900101 + WS-I * 70000
               ADD 1 TO NENDAI-CNT (WS-I)
               GO TO NENDAI-KEISU-EX.
           ADD 1               TO WS-I.
           GO TO NEN-020.
       NENDAI-KEISU-EX.
           EXIT.
      *****************************************************************
      * 口座カナの粗検査 (数字開始・制御文字混入の検出)
      *****************************************************************
       KANA-SCAN               SECTION.
       KAN-010.
           IF KYK-KOZA-KANA = SPACES
               ADD 1 TO WK-VAL2NG
               GO TO KANA-SCAN-EX.
           IF KYK-KOZA-KANA (1:1) NUMERIC
               ADD 1 TO WK-VAL2NG
               GO TO KANA-SCAN-EX.
           IF KYK-KOZA-KANA (1:1) = '-'
               ADD 1 TO WK-VAL2NG
               GO TO KANA-SCAN-EX.
           IF KYK-POINT-RITU LESS ZERO
               ADD 1 TO WK-VAL2NG
               GO TO KANA-SCAN-EX.
           MOVE 1              TO WS-I.
       KAN-020.
           IF WS-I GREATER 30
               GO TO KANA-SCAN-EX.
           IF KYK-KOZA-KANA (WS-I:1) LESS SPACE
               ADD 1 TO WK-VAL2NG
               GO TO KANA-SCAN-EX.
           ADD 1               TO WS-I.
           GO TO KAN-020.
       KANA-SCAN-EX.
           EXIT.
      *****************************************************************
      * 入力検証
      *****************************************************************
       IN-VALID                SECTION.
       INV-010.
           IF IN-SPT (1:2) NOT = '03' AND
              IN-SPT (1:1) NOT = LOW-VALUE
               GO TO INV-NG.
           IF IN-JYU NOT NUMERIC
               GO TO INV-NG.
           IF IN-SYU NOT = '10' AND IN-SYU NOT = '11' AND
              IN-SYU NOT = '20' AND IN-SYU NOT = '99'
               GO TO INV-NG.
           IF IN-KAISI NOT NUMERIC
               GO TO INV-NG.
           IF IN-KAISI (5:2) LESS '01' OR
              IN-KAISI (5:2) GREATER '12'
               GO TO INV-NG.
           IF IN-SYURYO NOT NUMERIC
               GO TO INV-NG.
           IF IN-TEISI NOT = '0' AND IN-TEISI NOT = '1'
               GO TO INV-NG.
           IF IN-KAISI LESS 19930101
               IF IN-REC (60:9) NOT NUMERIC
                   GO TO INV-NG
               END-IF
           END-IF
           GO TO IN-VALID-EX.
       INV-NG.
           ADD 1               TO WK-NG-CNT.
           MOVE IN-SPT (1:22)  TO ERR-REC.
           WRITE ERR-REC.
           GO TO MAIN-LOOP.
       IN-VALID-EX.
           EXIT.
