      *****************************************************************
      * RBMTR01C  計器検定期限抽出 (月次M020)     東西電力 電算部     *
      *---------------------------------------------------------------*
      * H02.10 初版  H31.04 新元号対応 (旧レコードは元号空白のまま)   *
      * 元号空白は YY>=64 を昭和 それ未満を平成とみなす (H02 取決め)  *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBMTR01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MTRMST  ASSIGN TO 'app/data/portable/MTRMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-MTR.
           SELECT DATECTL ASSIGN TO 'app/data/portable/DATECTL.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-DTE.
           SELECT KIGNOUT ASSIGN TO 'app/data/portable/MTRKIGN.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KGN.
       DATA DIVISION.
       FILE SECTION.
       FD  MTRMST RECORD CONTAINS 128 CHARACTERS.
       COPY RCMTRREC.
       FD  DATECTL RECORD CONTAINS 8 CHARACTERS.
       01  DTE-REC             PIC X(8).
       FD  KIGNOUT RECORD CONTAINS 48 CHARACTERS.
       01  KGN-REC.
           05  KGN-MTR         PIC X(10).
           05  KGN-SPT         PIC X(22).
           05  KGN-MANKI-YM    PIC 9(6).
           05  KGN-KBN         PIC X(1).
           05  FILLER          PIC X(9).
       WORKING-STORAGE SECTION.
       77  ST-MTR              PIC XX VALUE '00'.
       77  ST-DTE              PIC XX VALUE '00'.
       77  ST-KGN              PIC XX VALUE '00'.
       77  W-KIJUN8            PIC 9(8) VALUE ZERO.
       77  W-KIJUN-YM          PIC 9(6) VALUE ZERO.
       77  W-MANKI-YY          PIC 9(4) VALUE ZERO.
       77  W-MANKI-YM          PIC 9(6) VALUE ZERO.
       77  W-GENDO-YM          PIC 9(6) VALUE ZERO.
       77  WK-IN-CNT           PIC 9(7) VALUE ZERO.
       77  WK-NG-CNT           PIC 9(7) VALUE ZERO.
       77  WK-KIRE             PIC 9(7) VALUE ZERO.
       77  WK-MAJIKA           PIC 9(7) VALUE ZERO.
       77  WK-SEIJO            PIC 9(7) VALUE ZERO.
       77  WK-MADO-S           PIC 9(7) VALUE ZERO.
       77  WK-MADO-H           PIC 9(7) VALUE ZERO.
       77  WS-I                PIC 9(2) VALUE ZERO.
       77  W-KEIKA-Y           PIC S9(4) VALUE ZERO.
       01  KEIKA-TBL.
           05  KEIKA-CNT       PIC 9(7) OCCURS 5 VALUE ZERO.
       01  KISYU-TBL.
           05  KISYU-CNT       PIC 9(7) OCCURS 10 VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN-SEC                SECTION.
       MAIN-000.
           PERFORM KIJUN-GET   THRU KIJUN-GET-EX.
           OPEN INPUT MTRMST.
           OPEN OUTPUT KIGNOUT.
       MAIN-LOOP.
           READ MTRMST
               AT END GO TO SYUKEI-RTN.
           ADD 1               TO WK-IN-CNT.
           PERFORM MTR-VALID   THRU MTR-VALID-EX.
           PERFORM MANKI-KEISAN THRU MANKI-KEISAN-EX.
           PERFORM KIGEN-HANTEI THRU KIGEN-HANTEI-EX.
           PERFORM KISYU-KEISU THRU KISYU-KEISU-EX.
           PERFORM KEIKA-BUNPU THRU KEIKA-BUNPU-EX.
           GO TO MAIN-LOOP.
       SYUKEI-RTN.
           CLOSE MTRMST KIGNOUT.
           DISPLAY 'RBMTR01C KIJUN=' W-KIJUN-YM ' IN=' WK-IN-CNT
                   ' NG=' WK-NG-CNT.
           DISPLAY 'KIRE=' WK-KIRE ' MAJIKA=' WK-MAJIKA
                   ' SEIJO=' WK-SEIJO.
           DISPLAY 'KEIKA=' KEIKA-CNT (1) '/' KEIKA-CNT (2) '/'
                   KEIKA-CNT (3) '/' KEIKA-CNT (4) '/'
                   KEIKA-CNT (5).
           DISPLAY 'MADO-S=' WK-MADO-S ' MADO-H=' WK-MADO-H
                   ' KISYU0=' KISYU-CNT (1)
                   ' KISYU9=' KISYU-CNT (10).
           MOVE 0              TO RETURN-CODE.
           STOP RUN.
       ABEND-RTN.
           DISPLAY 'RBMTR01C ABEND'.
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
           MOVE DTE-REC        TO W-KIJUN8.
           COMPUTE W-KIJUN-YM = W-KIJUN8 / 100.
           CLOSE DATECTL.
       KIJUN-GET-EX.
           EXIT.
      *****************************************************************
      * 満期年月計算: 元号->西暦 + 検定有効10年
      *   元号空白は YY>=64 昭和 / それ未満 平成 (E-01 窓割り)
      *****************************************************************
       MANKI-KEISAN            SECTION.
       MNK-010.
           IF MTR-KENTEI-GENGO = 'R'
               COMPUTE W-MANKI-YY = 2018 + MTR-KENTEI-YY
               GO TO MNK-050.
           IF MTR-KENTEI-GENGO = 'H'
               COMPUTE W-MANKI-YY = 1988 + MTR-KENTEI-YY
               GO TO MNK-050.
           IF MTR-KENTEI-GENGO = 'S'
               COMPUTE W-MANKI-YY = 1925 + MTR-KENTEI-YY
               GO TO MNK-050.
           IF MTR-KENTEI-GENGO NOT = SPACE
               ADD 1 TO WK-NG-CNT
               MOVE 9999 TO W-MANKI-YY
               GO TO MNK-050.
           IF MTR-KENTEI-YY NOT LESS 64
               COMPUTE W-MANKI-YY = 1925 + MTR-KENTEI-YY
               ADD 1 TO WK-MADO-S
               GO TO MNK-050.
           COMPUTE W-MANKI-YY = 1988 + MTR-KENTEI-YY.
           ADD 1               TO WK-MADO-H.
       MNK-050.
           COMPUTE W-MANKI-YM = ( W-MANKI-YY + 10 ) * 100
                              + MTR-KENTEI-MM.
       MANKI-KEISAN-EX.
           EXIT.
      *****************************************************************
      * 期限判定: 基準日超過=切れ / 6月以内=間近
      *****************************************************************
       KIGEN-HANTEI            SECTION.
       KGH-010.
           IF W-MANKI-YM LESS W-KIJUN-YM
               ADD 1           TO WK-KIRE
               MOVE 'K'        TO KGN-KBN
               GO TO KGH-050.
           COMPUTE W-GENDO-YM = W-KIJUN-YM + 6.
           IF W-GENDO-YM (5:2) GREATER '12'
               COMPUTE W-GENDO-YM = W-GENDO-YM + 88.
           IF W-MANKI-YM NOT GREATER W-GENDO-YM
               ADD 1           TO WK-MAJIKA
               MOVE 'M'        TO KGN-KBN
               GO TO KGH-050.
           ADD 1               TO WK-SEIJO.
           GO TO KIGEN-HANTEI-EX.
       KGH-050.
           MOVE MTR-NO         TO KGN-MTR.
           MOVE MTR-SPT-NO     TO KGN-SPT.
           MOVE W-MANKI-YM     TO KGN-MANKI-YM.
           WRITE KGN-REC.
       KIGEN-HANTEI-EX.
           EXIT.
      *****************************************************************
       KISYU-KEISU             SECTION.
       KSK-010.
           IF MTR-KISYU-CD (1:1) NOT = 'K'
               GO TO KISYU-KEISU-EX.
           IF MTR-KISYU-CD (2:1) NOT NUMERIC
               GO TO KISYU-KEISU-EX.
           MOVE MTR-KISYU-CD (2:1) TO WS-I.
           ADD 1               TO KISYU-CNT (WS-I + 1).
       KISYU-KEISU-EX.
           EXIT.
      *****************************************************************
      * 設置経過年分布 (更改計画資料)
      *****************************************************************
       KEIKA-BUNPU             SECTION.
       KKB-010.
           COMPUTE W-KEIKA-Y = W-KIJUN8 / 10000
                             - MTR-SETTI-BI / 10000.
           IF W-KEIKA-Y LESS ZERO
               IF MTR-KOKAN-BI = ZERO
                   IF MTR-KENTEI-GENGO = SPACE
                       ADD 1 TO WK-NG-CNT
                   END-IF
               END-IF
               GO TO KEIKA-BUNPU-EX.
           IF W-KEIKA-Y LESS 3
               ADD 1 TO KEIKA-CNT (1)
               GO TO KEIKA-BUNPU-EX.
           IF W-KEIKA-Y LESS 6
               ADD 1 TO KEIKA-CNT (2)
               GO TO KEIKA-BUNPU-EX.
           IF W-KEIKA-Y LESS 10
               ADD 1 TO KEIKA-CNT (3)
               GO TO KEIKA-BUNPU-EX.
           IF W-KEIKA-Y LESS 15
               ADD 1 TO KEIKA-CNT (4)
               GO TO KEIKA-BUNPU-EX.
           ADD 1               TO KEIKA-CNT (5).
       KEIKA-BUNPU-EX.
           EXIT.
      *****************************************************************
       MTR-VALID               SECTION.
       MVA-010.
           IF MTR-NO (1:3) NOT = 'MTR'
               GO TO MVA-NG.
           IF MTR-SPT-NO (1:2) NOT = '03'
               GO TO MVA-NG.
           IF MTR-SPT-NO (3:2) LESS '01' OR
              MTR-SPT-NO (3:2) GREATER '47'
               GO TO MVA-NG.
           IF MTR-KENTEI-GENGO NOT = 'R' AND
              MTR-KENTEI-GENGO NOT = 'H' AND
              MTR-KENTEI-GENGO NOT = 'S' AND
              MTR-KENTEI-GENGO NOT = SPACE
               GO TO MVA-NG.
           IF MTR-KENTEI-YY NOT NUMERIC
               GO TO MVA-NG.
           IF MTR-KENTEI-MM LESS 01 OR MTR-KENTEI-MM GREATER 12
               GO TO MVA-NG.
           IF MTR-KETA-SU LESS 4 OR MTR-KETA-SU GREATER 8
               GO TO MVA-NG.
           IF MTR-KOKAN-BI NOT NUMERIC
               GO TO MVA-NG.
           IF MTR-SETTI-BI NOT NUMERIC
               GO TO MVA-NG.
           IF MTR-SETTI-BI (5:2) LESS '01' OR
              MTR-SETTI-BI (5:2) GREATER '12'
               GO TO MVA-NG.
           IF MTR-KISYU-CD = SPACES
               GO TO MVA-NG.
           IF MTR-SPT-NO (21:2) NOT NUMERIC
               GO TO MVA-NG.
           IF MTR-JORITU LESS 0.1 OR MTR-JORITU GREATER 99.9
               GO TO MVA-NG.
           IF MTR-KOKAN-BI NOT = ZERO
               IF MTR-KOKAN-BI LESS MTR-SETTI-BI
                   GO TO MVA-NG
               END-IF
           END-IF.
           GO TO MTR-VALID-EX.
       MVA-NG.
           ADD 1               TO WK-NG-CNT.
           GO TO MAIN-LOOP.
       MTR-VALID-EX.
           EXIT.
