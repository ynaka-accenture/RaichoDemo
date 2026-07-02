      *****************************************************************
      * RBKYK01C  契約棚卸し・期限反映 (日次D085)  東西電力 電算部    *
      *---------------------------------------------------------------*
      * H05.10 初版. 適用終了日の解釈は歴史的に三通りある:            *
      *   99991231 = 無期限 (H11 以降の標準)                          *
      *   99999999 = 無期限 (S60 旧様式. 意味同じだが由来が異なる)    *
      *   00000000 = 未設定 (登録時省略. 無期限とみなす)              *
      * 実日付なら基準日と比較し 到来していれば停止フラグを立てる     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBKYK01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KYKIN   ASSIGN TO 'app/data/portable/KYKMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-IN.
           SELECT DATECTL ASSIGN TO 'app/data/portable/DATECTL.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-DTE.
           SELECT KYKOUT  ASSIGN TO 'app/data/portable/KYKMST2.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-OUT.
       DATA DIVISION.
       FILE SECTION.
       FD  KYKIN RECORD CONTAINS 320 CHARACTERS.
       COPY RCKYKREC.
       FD  DATECTL RECORD CONTAINS 8 CHARACTERS.
       01  DTE-REC             PIC X(8).
       FD  KYKOUT RECORD CONTAINS 320 CHARACTERS.
       01  OUT-REC             PIC X(320).
       WORKING-STORAGE SECTION.
      *    命名は年代により 無印 / W- / WS- / WK- が混在 (H系)
       77  ST-IN               PIC XX VALUE '00'.
       77  ST-DTE              PIC XX VALUE '00'.
       77  ST-OUT              PIC XX VALUE '00'.
       77  KIJUN-BI            PIC 9(8) VALUE ZERO.
       77  YOMIKOMI            PIC 9(7) VALUE ZERO.
       77  W-KAKIDASI          PIC 9(7) VALUE ZERO.
       77  WS-TEISI-SET        PIC 9(7) VALUE ZERO.
       77  WK-NG-CNT           PIC 9(7) VALUE ZERO.
       77  SENT-A              PIC 9(7) VALUE ZERO.
       77  SENT-B              PIC 9(7) VALUE ZERO.
       77  SENT-C              PIC 9(7) VALUE ZERO.
       77  JITU-HIKAKU         PIC 9(7) VALUE ZERO.
       77  KYUSYS-KENSU        PIC 9(7) VALUE ZERO.
       77  W-BYTE              PIC X    VALUE SPACE.
       77  WK-LOWV             PIC 9(7) VALUE ZERO.
       77  KYU-FMT             PIC 9(7) VALUE ZERO.
       01  TANA-TBL.
           05  TANA-CNT        PIC 9(7) OCCURS 4 VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN-SEC                SECTION.
       MAIN-000.
           PERFORM KIJUN-GET   THRU KIJUN-GET-EX.
           OPEN INPUT KYKIN.
           OPEN OUTPUT KYKOUT.
       MAIN-LOOP.
           READ KYKIN
               AT END GO TO SYUKEI-RTN.
           ADD 1               TO YOMIKOMI.
           IF KYK-SPT-NO (1:1) = LOW-VALUE
               ADD 1           TO WK-LOWV
               GO TO KAKIDASI-P.
           PERFORM KYK-VALID   THRU KYK-VALID-EX.
           PERFORM KIGEN-HANTEI THRU KIGEN-HANTEI-EX.
           PERFORM KYUSYS-CHK  THRU KYUSYS-CHK-EX.
           PERFORM SYU-TANA    THRU SYU-TANA-EX.
           PERFORM TEISI-TENKEN THRU TEISI-TENKEN-EX.
       KAKIDASI-P.
           MOVE KYK-REC        TO OUT-REC.
           WRITE OUT-REC.
           ADD 1               TO W-KAKIDASI.
           GO TO MAIN-LOOP.
       SYUKEI-RTN.
           CLOSE KYKIN KYKOUT.
           DISPLAY 'RBKYK01C KIJUN=' KIJUN-BI ' IN=' YOMIKOMI
                   ' OUT=' W-KAKIDASI ' NG=' WK-NG-CNT.
           DISPLAY 'SENT-A=' SENT-A ' SENT-B=' SENT-B
                   ' SENT-C=' SENT-C ' JITU=' JITU-HIKAKU.
           DISPLAY 'TEISI-SET=' WS-TEISI-SET
                   ' KYUSYS=' KYUSYS-KENSU ' LOWV=' WK-LOWV
                   ' KYUFMT=' KYU-FMT.
           DISPLAY 'TANA=' TANA-CNT (1) '/' TANA-CNT (2) '/'
                   TANA-CNT (3) '/' TANA-CNT (4).
           MOVE 0              TO RETURN-CODE.
           STOP RUN.
       ABEND-RTN.
           DISPLAY 'RBKYK01C ABEND'.
           MOVE 16             TO RETURN-CODE.
           STOP RUN.
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
           MOVE DTE-REC        TO KIJUN-BI.
           CLOSE DATECTL.
       KIJUN-GET-EX.
           EXIT.
      *****************************************************************
      * 期限判定: センチネル三様の解釈 (由来の違いをコメントで保持)
      *****************************************************************
       KIGEN-HANTEI            SECTION.
       KGH-010.
           IF KYK-TEKIYO-SYURYO NOT NUMERIC
               ADD 1 TO WK-NG-CNT
               GO TO KIGEN-HANTEI-EX.
           IF KYK-TEKIYO-SYURYO = 99991231
      *        H11 標準の無期限
               ADD 1           TO SENT-A
               GO TO KIGEN-HANTEI-EX.
           IF KYK-TEKIYO-SYURYO = 99999999
      *        S60 旧様式の無期限 (置換未了のまま残存)
               ADD 1           TO SENT-B
               GO TO KIGEN-HANTEI-EX.
           IF KYK-TEKIYO-SYURYO = ZERO
      *        未設定 = 無期限扱い (H05 取決め)
               ADD 1           TO SENT-C
               GO TO KIGEN-HANTEI-EX.
           ADD 1               TO JITU-HIKAKU.
           IF KYK-TEKIYO-SYURYO (5:2) LESS '01' OR
              KYK-TEKIYO-SYURYO (5:2) GREATER '12'
               ADD 1 TO WK-NG-CNT
               GO TO KIGEN-HANTEI-EX.
           IF KYK-TEKIYO-SYURYO LESS KIJUN-BI
               IF KYK-TEISI-FLG = '0'
                   MOVE '1' TO KYK-TEISI-FLG
                   MOVE KIJUN-BI TO KYK-KOSHIN-BI
                   MOVE 'RBKYK01C' TO KYK-KOSHIN-PGM
                   ADD 1 TO WS-TEISI-SET
               END-IF
           END-IF.
       KIGEN-HANTEI-EX.
           EXIT.
      *****************************************************************
      * 種別別棚卸し計数
      *****************************************************************
       SYU-TANA                SECTION.
       STN-010.
           IF KYK-SYUBETU = '10'
               ADD 1 TO TANA-CNT (1)
               GO TO SYU-TANA-EX.
           IF KYK-SYUBETU = '11'
               ADD 1 TO TANA-CNT (2)
               GO TO SYU-TANA-EX.
           IF KYK-SYUBETU = '20'
               ADD 1 TO TANA-CNT (3)
               GO TO SYU-TANA-EX.
           IF KYK-SYUBETU = '99'
               ADD 1 TO TANA-CNT (4)
               GO TO SYU-TANA-EX.
           ADD 1               TO WK-NG-CNT.
       SYU-TANA-EX.
           EXIT.
      *****************************************************************
      * 旧システム由来判定 (符号ニブルF: 16進直接比較の第2箇所)
      *****************************************************************
       KYUSYS-CHK              SECTION.
       KSC-010.
           MOVE KYK-TANKA-SEDAI (2:1) TO W-BYTE.
           IF W-BYTE = X'1F' OR W-BYTE = X'2F' OR
              W-BYTE = X'3F' OR W-BYTE = X'4F' OR
              W-BYTE = X'5F' OR W-BYTE = X'6F' OR
              W-BYTE = X'7F' OR W-BYTE = X'8F' OR
              W-BYTE = X'9F' OR W-BYTE = X'0F'
               ADD 1           TO KYUSYS-KENSU.
       KYUSYS-CHK-EX.
           EXIT.
      *****************************************************************
      * 停止済み契約の点検 (フラグと終了日の整合)
      *****************************************************************
       TEISI-TENKEN            SECTION.
       TTK-010.
           IF KYK-TEISI-FLG NOT = '1'
               GO TO TEISI-TENKEN-EX.
           IF KYK-TEKIYO-SYURYO = 99991231
               ADD 1 TO WK-NG-CNT
               GO TO TEISI-TENKEN-EX.
           IF KYK-TEKIYO-SYURYO = 99999999
               ADD 1 TO WK-NG-CNT
               GO TO TEISI-TENKEN-EX.
       TEISI-TENKEN-EX.
           EXIT.
      *****************************************************************
      * 契約検証
      *****************************************************************
       KYK-VALID               SECTION.
       KVA-010.
           IF KYK-SPT-NO (1:2) NOT = '03'
               GO TO KVA-NG.
           IF KYK-SPT-NO (3:2) LESS '01' OR
              KYK-SPT-NO (3:2) GREATER '47'
               GO TO KVA-NG.
           IF KYK-SPT-NO (21:2) NOT NUMERIC
               GO TO KVA-NG.
           IF KYK-JYU-NO NOT NUMERIC
               GO TO KVA-NG.
           IF KYK-SYUBETU NOT = '10' AND KYK-SYUBETU NOT = '11'
              AND KYK-SYUBETU NOT = '20' AND KYK-SYUBETU NOT = '99'
               GO TO KVA-NG.
           IF KYK-TEKIYO-KAISI NOT NUMERIC
               GO TO KVA-NG.
           IF KYK-TEKIYO-KAISI (5:2) LESS '01' OR
              KYK-TEKIYO-KAISI (5:2) GREATER '12'
               GO TO KVA-NG.
           IF KYK-TEKIYO-SYURYO NOT NUMERIC
               GO TO KVA-NG.
           IF KYK-TEISI-FLG NOT = '0' AND KYK-TEISI-FLG NOT = '1'
               GO TO KVA-NG.
      *    型圧縮前の旧レイアウトは後続項目が 4バイト後方 (要判別)
           IF KYK-TEKIYO-KAISI LESS 19930101
               ADD 1           TO KYU-FMT
               GO TO KVA-KYU.
           IF KYK-SAISYU-KENSHIN NOT NUMERIC
               GO TO KVA-NG.
           IF KYK-ZEN-SIJISU NOT NUMERIC
               GO TO KVA-NG.
           IF KYK-KOSHIN-BI NOT NUMERIC
               GO TO KVA-NG.
           GO TO KYK-VALID-EX.
       KVA-KYU.
           IF KYK-REC (60:9) NOT NUMERIC
               GO TO KVA-NG.
           IF KYK-REC (114:6) NOT NUMERIC
               GO TO KVA-NG.
           IF KYK-REC (120:6) NOT NUMERIC
               GO TO KVA-NG.
           GO TO KYK-VALID-EX.
       KVA-NG.
           ADD 1               TO WK-NG-CNT.
       KYK-VALID-EX.
           EXIT.
