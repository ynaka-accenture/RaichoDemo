      *****************************************************************
      * RBKEN01C  検針データ受信・編集 (日次D010)    東西電力 電算部  *
      *---------------------------------------------------------------*
      * H07.02 初版 (伝送受信対応)  H17.11 30分値対応(スマート先行)   *
      * 入力は RECFM=VB. レコード種別 1=H 2=D 3=K 9=T の順序厳守      *
      * 注意: 短いレコードの後方には前レコードの残像が残ることがある *
      *       (受信バッファ再利用のため. 種別と長さで読むこと)        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBKEN01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KENVB   ASSIGN TO 'app/data/portable/KENVB.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-VB.
           SELECT KENHEN  ASSIGN TO 'app/data/portable/KENHEN.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-HEN.
           SELECT KOKANF  ASSIGN TO 'app/data/portable/KOKANF.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KOK.
       DATA DIVISION.
       FILE SECTION.
       FD  KENVB RECORD IS VARYING IN SIZE FROM 9 TO 283
           CHARACTERS DEPENDING ON WS-RLEN.
       COPY RCKENIN.
       FD  KENHEN RECORD CONTAINS 128 CHARACTERS.
       COPY RCKENREC.
       FD  KOKANF RECORD CONTAINS 64 CHARACTERS.
       01  KOK-REC.
           05  KOK-SPT         PIC X(22).
           05  KOK-KYU-MTR     PIC X(10).
           05  KOK-SHIN-MTR    PIC X(10).
           05  KOK-BI          PIC 9(8).
           05  FILLER          PIC X(14).
       WORKING-STORAGE SECTION.
       77  ST-VB               PIC XX VALUE '00'.
       77  ST-HEN              PIC XX VALUE '00'.
       77  ST-KOK              PIC XX VALUE '00'.
       77  WS-RLEN             PIC 9(4) VALUE ZERO.
       77  WS-PRM              PIC X(20) VALUE SPACE.
       77  WS-PRM-YM           PIC 9(6)  VALUE ZERO.
       77  WK-H-CNT            PIC 9(7)  VALUE ZERO.
       77  WK-D-CNT            PIC 9(7)  VALUE ZERO.
       77  WK-K-CNT            PIC 9(7)  VALUE ZERO.
       77  WK-T-CNT            PIC 9(7)  VALUE ZERO.
       77  WK-JUN-NG           PIC 9(7)  VALUE ZERO.
       77  WK-LEN-NG           PIC 9(7)  VALUE ZERO.
       77  WK-VAL-NG           PIC 9(7)  VALUE ZERO.
       77  WK-SUM              PIC 9(10) VALUE ZERO.
       77  WK-30SUM            PIC S9(9) VALUE ZERO.
       77  W-30KEI             PIC S9(9) VALUE ZERO.
       77  WS-I                PIC 9(3)  VALUE ZERO.
       77  WS-YOKI             PIC 9(4)  VALUE ZERO.
      *    処理状態: 1=ヘッダ待ち 2=本文 3=トレーラ後
       77  JOTAI               PIC 9     VALUE 1.
       PROCEDURE DIVISION.
       MAIN-SEC                SECTION.
       MAIN-000.
           ACCEPT WS-PRM FROM COMMAND-LINE.
           IF WS-PRM (1:6) NOT NUMERIC
               DISPLAY 'RBKEN01C E001 PARM FUSEI'
               GO TO ABEND-RTN.
           MOVE WS-PRM (1:6)   TO WS-PRM-YM.
           OPEN INPUT KENVB.
           OPEN OUTPUT KENHEN.
           OPEN OUTPUT KOKANF.
       MAIN-LOOP.
           READ KENVB
               AT END GO TO SYUKEI-RTN.
      *    状態と種別で振り分け (順序を崩す伝送は受け付けない)
           GO TO JYOTAI-1 JYOTAI-2 JYOTAI-3
               DEPENDING ON JOTAI.
           GO TO ABEND-RTN.
       JYOTAI-1.
           IF KIN-KBN = '1'
               PERFORM HDR-SHORI  THRU HDR-SHORI-EX
               MOVE 2 TO JOTAI
               GO TO MAIN-LOOP.
           ADD 1               TO WK-JUN-NG.
           GO TO MAIN-LOOP.
       JYOTAI-2.
           IF KIN-KBN = '2'
               PERFORM DTL-SHORI  THRU DTL-SHORI-EX
               GO TO MAIN-LOOP.
           IF KIN-KBN = '3'
               PERFORM KOK-SHORI  THRU KOK-SHORI-EX
               GO TO MAIN-LOOP.
           IF KIN-KBN = '9'
               PERFORM TRL-SHORI  THRU TRL-SHORI-EX
               MOVE 3 TO JOTAI
               GO TO MAIN-LOOP.
           ADD 1               TO WK-JUN-NG.
           GO TO MAIN-LOOP.
       JYOTAI-3.
      *    トレーラ後は何が来ても順序エラー
           ADD 1               TO WK-JUN-NG.
           GO TO MAIN-LOOP.
      *****************************************************************
       SYUKEI-RTN.
           CLOSE KENVB KENHEN KOKANF.
           DISPLAY 'RBKEN01C YM=' WS-PRM-YM
                   ' H=' WK-H-CNT ' D=' WK-D-CNT
                   ' K=' WK-K-CNT ' T=' WK-T-CNT.
           DISPLAY 'VAL-NG=' WK-VAL-NG.
           DISPLAY 'JUN-NG=' WK-JUN-NG ' LEN-NG=' WK-LEN-NG
                   ' SUM=' WK-SUM ' SUM30=' WK-30SUM.
           IF JOTAI NOT = 3
               DISPLAY 'RBKEN01C E009 TRAILER NASI'
               MOVE 8 TO RETURN-CODE
               STOP RUN.
           IF WK-JUN-NG GREATER ZERO OR WK-LEN-NG GREATER ZERO
               MOVE 8          TO RETURN-CODE
           ELSE
               MOVE 0          TO RETURN-CODE.
           STOP RUN.
       ABEND-RTN.
           DISPLAY 'RBKEN01C ABEND'.
           MOVE 16             TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * ヘッダ処理
      *****************************************************************
       HDR-SHORI               SECTION.
       HDR-010.
           IF WS-RLEN NOT = 21
               ADD 1           TO WK-LEN-NG.
           IF KIN-H-SOSIN-CD NOT = 'SMT01' AND
              KIN-H-SOSIN-CD NOT = 'SMT02' AND
              KIN-H-SOSIN-CD NOT = 'HND01'
               ADD 1           TO WK-JUN-NG.
           PERFORM HDR-VALID   THRU HDR-VALID-EX.
           ADD 1               TO WK-H-CNT.
           MOVE KIN-H-KENSU    TO WS-YOKI.
       HDR-SHORI-EX.
           EXIT.
      *****************************************************************
      * 明細処理: 30分値は KIN-D-SU の範囲だけが有効 (以降は残像)
      *****************************************************************
       DTL-SHORI               SECTION.
       DTL-010.
           IF KIN-D-SU LESS 1 OR KIN-D-SU GREATER 48
               ADD 1           TO WK-LEN-NG
               GO TO DTL-SHORI-EX.
           IF WS-RLEN NOT = 43 + KIN-D-SU * 5
               ADD 1           TO WK-LEN-NG
               GO TO DTL-SHORI-EX.
           PERFORM DTL-VALID   THRU DTL-VALID-EX.
           MOVE ZERO           TO W-30KEI.
           MOVE 1              TO WS-I.
       DTL-020.
           IF WS-I GREATER KIN-D-SU
               GO TO DTL-030.
           ADD KIN-D-VAL (WS-I) TO W-30KEI.
           ADD 1               TO WS-I.
           GO TO DTL-020.
       DTL-030.
D          MOVE ZERO TO W-30KEI.
D          MOVE 1 TO WS-I.
D      DTL-021.
D          IF WS-I GREATER 48
D              GO TO DTL-031.
D          ADD KIN-D-VAL (WS-I) TO W-30KEI.
D          ADD 1 TO WS-I.
D          GO TO DTL-021.
D      DTL-031.
           ADD W-30KEI         TO WK-30SUM.
           MOVE SPACES         TO KEN-REC.
           MOVE KIN-D-SPT-NO   TO KEN-SPT-NO.
           MOVE WS-PRM-YM      TO KEN-NENGETU.
           MOVE KIN-D-KENSHIN-BI TO KEN-KENSHIN-BI.
           MOVE ZERO           TO KEN-ZEN-SIJISU.
           MOVE KIN-D-SIJISU   TO KEN-KON-SIJISU.
           MOVE ZERO           TO KEN-SIYORYO.
           MOVE KIN-D-KENSHININ TO KEN-KENSHININ.
           MOVE KIN-D-KBN      TO KEN-KENSHIN-KBN.
           MOVE SPACE          TO KEN-KOKAN-FLG.
           WRITE KEN-REC.
           ADD 1               TO WK-D-CNT.
           ADD KIN-D-SIJISU    TO WK-SUM.
       DTL-SHORI-EX.
           EXIT.
      *****************************************************************
      * ヘッダ検証
      *****************************************************************
       HDR-VALID               SECTION.
       HVA-010.
           IF KIN-H-SAKUSEI-BI NOT NUMERIC
               GO TO HVA-NG.
           IF KIN-H-SAKUSEI-BI (5:2) LESS '01' OR
              KIN-H-SAKUSEI-BI (5:2) GREATER '12'
               GO TO HVA-NG.
           IF KIN-H-KENSU NOT NUMERIC
               GO TO HVA-NG.
           IF KIN-H-KENSU = ZERO
               GO TO HVA-NG.
           GO TO HDR-VALID-EX.
       HVA-NG.
           ADD 1               TO WK-VAL-NG.
       HDR-VALID-EX.
           EXIT.
      *****************************************************************
      * 明細検証 (H12 伝送障害多発時に追加. E4xx 番号は欠番あり)
      *****************************************************************
       DTL-VALID               SECTION.
       DVA-010.
           IF KIN-D-SPT-NO (1:2) NOT = '03'
               GO TO DVA-NG.
           IF KIN-D-SPT-NO (3:2) LESS '01' OR
              KIN-D-SPT-NO (3:2) GREATER '47'
               GO TO DVA-NG.
           IF KIN-D-SPT-NO (21:2) NOT NUMERIC
               GO TO DVA-NG.
           IF KIN-D-KENSHIN-BI NOT NUMERIC
               GO TO DVA-NG.
           IF KIN-D-KENSHIN-BI (3:2) LESS '01' OR
              KIN-D-KENSHIN-BI (3:2) GREATER '12'
               GO TO DVA-NG.
           IF KIN-D-KENSHIN-BI (5:2) LESS '01' OR
              KIN-D-KENSHIN-BI (5:2) GREATER '31'
               GO TO DVA-NG.
           IF KIN-D-SIJISU NOT NUMERIC
               GO TO DVA-NG.
           IF KIN-D-KENSHININ NOT NUMERIC AND
              KIN-D-KENSHININ NOT = SPACES
               GO TO DVA-NG.
           IF KIN-D-KBN NOT = '1' AND KIN-D-KBN NOT = '2' AND
              KIN-D-KBN NOT = '3' AND KIN-D-KBN NOT = '9'
               GO TO DVA-NG.
           GO TO DTL-VALID-EX.
       DVA-NG.
           ADD 1               TO WK-VAL-NG.
       DTL-VALID-EX.
           EXIT.
      *****************************************************************
      * 交換処理
      *****************************************************************
       KOK-SHORI               SECTION.
       KOK-010.
           IF WS-RLEN NOT = 63
               ADD 1 TO WK-LEN-NG
               GO TO KOK-SHORI-EX.
           IF KIN-K-SPT-NO (1:2) NOT = '03'
               ADD 1 TO WK-VAL-NG
               GO TO KOK-SHORI-EX.
           IF KIN-K-KYU-MTR = KIN-K-SHIN-MTR
               ADD 1 TO WK-VAL-NG
               GO TO KOK-SHORI-EX.
           IF KIN-K-KOKAN-BI NOT NUMERIC
               ADD 1 TO WK-VAL-NG
               GO TO KOK-SHORI-EX.
           IF KIN-K-KYU-SIJI NOT NUMERIC OR
              KIN-K-SHIN-SIJI NOT NUMERIC
               ADD 1 TO WK-VAL-NG
               GO TO KOK-SHORI-EX.
           MOVE SPACES         TO KOK-REC.
           MOVE KIN-K-SPT-NO   TO KOK-SPT.
           MOVE KIN-K-KYU-MTR  TO KOK-KYU-MTR.
           MOVE KIN-K-SHIN-MTR TO KOK-SHIN-MTR.
           MOVE KIN-K-KOKAN-BI TO KOK-BI.
           WRITE KOK-REC.
           ADD 1               TO WK-K-CNT.
       KOK-SHORI-EX.
           EXIT.
      *****************************************************************
      * トレーラ処理: 件数・指示数合計を突合
      *****************************************************************
       TRL-SHORI               SECTION.
       TRL-010.
           ADD 1               TO WK-T-CNT.
           IF KIN-T-KENSU NOT = WK-D-CNT
               ADD 1 TO WK-JUN-NG
               DISPLAY 'RBKEN01C E005 KENSU FUICCHI T='
                       KIN-T-KENSU ' D=' WK-D-CNT
           END-IF
           IF KIN-T-SIJI-GOKEI NOT = WK-SUM
               ADD 1 TO WK-JUN-NG
               DISPLAY 'RBKEN01C E006 GOKEI FUICCHI'.
       TRL-SHORI-EX.
           EXIT.
      *----------------------------------------------------------------
      *(H17.11 メモ: 30分値の合計は指示数差と一致しない場合がある
      *  --- 乗率適用前の生値のため. 帳票側で補正すること)
      *----------------------------------------------------------------
