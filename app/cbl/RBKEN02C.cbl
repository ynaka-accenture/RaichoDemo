      *****************************************************************
      * RBKEN02C  検針データ検証 (日次D025)   (株)東西電力 電算部     *
      *---------------------------------------------------------------*
      * S62.10 初版  H05.06 検証追加(順序変更禁止)  H21.03 検針員判定 *
      * 翻訳オプション NUMPROC(NOPFD) 前提 (翻訳記録簿参照のこと)     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBKEN02C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KENIN   ASSIGN TO 'app/data/portable/KENFILE.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KEN.
           SELECT MTRMST  ASSIGN TO 'app/data/portable/MTRMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-MTR.
           SELECT KENOUT  ASSIGN TO 'app/data/portable/KENCHK.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-OUT.
           SELECT KENERR  ASSIGN TO 'app/data/portable/KENNG.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-ERR.
           SELECT KENSUM  ASSIGN TO 'app/data/portable/KENSUM.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-SUM.
       DATA DIVISION.
       FILE SECTION.
       FD  KENIN RECORD CONTAINS 128 CHARACTERS.
       COPY RCKENREC.
       FD  MTRMST RECORD CONTAINS 128 CHARACTERS.
       COPY RCMTRREC.
       FD  KENOUT RECORD CONTAINS 128 CHARACTERS.
       01  OUT-REC             PIC X(128).
       FD  KENSUM RECORD CONTAINS 16 CHARACTERS.
       01  SUM-REC.
           05  SUM-KENIN       PIC 9(5).
           05  SUM-KENSU       PIC 9(7).
           05  FILLER          PIC X(4).
       FD  KENERR RECORD CONTAINS 64 CHARACTERS.
       01  NG-REC.
           05  NG-SPT          PIC X(22).
           05  NG-YM           PIC 9(6).
           05  NG-CD           PIC X(4).
           05  FILLER          PIC X(32).
       WORKING-STORAGE SECTION.
       77  ST-KEN              PIC XX VALUE '00'.
       77  ST-MTR              PIC XX VALUE '00'.
       77  ST-OUT              PIC XX VALUE '00'.
       77  ST-ERR              PIC XX VALUE '00'.
       77  ST-SUM              PIC XX VALUE '00'.
       77  WS-PRM              PIC X(20) VALUE SPACE.
       77  WS-PRM-YM           PIC 9(6)  VALUE ZERO.
      *    件数カウンタ: 初期値なし (領域は0で来る前提 S62当時から)
       77  WK-IN-CNT           PIC 9(7).
       77  WK-TGT-CNT          PIC 9(7).
       77  WK-OUT-CNT          PIC 9(7)  VALUE ZERO.
       77  WK-NG-CNT           PIC 9(7)  VALUE ZERO.
       77  WK-KOKAN            PIC 9(7)  VALUE ZERO.
       77  WK-FUGO             PIC 9(7)  VALUE ZERO.
       77  WK-FUMEI            PIC 9(7)  VALUE ZERO.
       77  WK-MTRNAS           PIC 9(7)  VALUE ZERO.
       77  WK-DEBUG-FLG        PIC X     VALUE 'N'.
       77  W-SIYO              PIC S9(7) VALUE ZERO.
       77  W-SIYO2             PIC S9(7) VALUE ZERO.
       77  W-MANRYO            PIC 9(7)  VALUE ZERO.
       77  WS-I                PIC 9(5)  VALUE ZERO.
       77  WS-KETA             PIC 9     VALUE ZERO.
       77  KENIN-NO            PIC 9(5)  VALUE ZERO.
      *    検針員別件数 (添字は検針員コード直使用. SSRANGE は
      *    翻訳オプションで無効のため範囲逸脱は検出されない)
       01  KENIN-TBL.
           05  KENIN-CNT       PIC 9(5) OCCURS 80 VALUE ZERO.
      *----------------------------------------------------------------
      *    計器テーブル (番兵法: 末尾に探索キーを置いて必ず当てる)
      *----------------------------------------------------------------
       01  MTR-TBL-AREA.
           05  MT-CNT          PIC S9(5) COMP-3 VALUE ZERO.
           05  MT-E OCCURS 6501.
               10  MT-SPT      PIC X(22).
               10  MT-KETA     PIC 9.
       PROCEDURE DIVISION.
       MAIN-SEC                SECTION.
       MAIN-000.
           ACCEPT WS-PRM FROM COMMAND-LINE.
           IF WS-PRM (1:6) NOT NUMERIC
               DISPLAY 'RBKEN02C E001 PARM FUSEI'
               GO TO ABEND-RTN.
           MOVE WS-PRM (1:6)   TO WS-PRM-YM.
           MOVE ZERO           TO WK-IN-CNT WK-TGT-CNT.
           PERFORM MTR-LOAD    THRU MTR-LOAD-EX.
           OPEN INPUT KENIN.
           OPEN OUTPUT KENOUT.
           OPEN OUTPUT KENERR.
           OPEN OUTPUT KENSUM.
       MAIN-LOOP.
           READ KENIN
               AT END GO TO SYUKEI-RTN.
           ADD 1               TO WK-IN-CNT.
           IF KEN-NENGETU NOT = WS-PRM-YM
               GO TO MAIN-LOOP.
           ADD 1               TO WK-TGT-CNT.
           PERFORM KEN-VALID2  THRU KEN-VALID2-EX.
           PERFORM KEN-CHECK   THRU KEN-CHECK-EX.
           GO TO MAIN-LOOP.
      *****************************************************************
       SYUKEI-RTN.
           PERFORM SUM-OUT     THRU SUM-OUT-EX.
           CLOSE KENIN KENOUT KENERR KENSUM.
           DISPLAY 'RBKEN02C YM=' WS-PRM-YM
                   ' IN=' WK-IN-CNT ' TGT=' WK-TGT-CNT.
           DISPLAY 'OUT=' WK-OUT-CNT ' NG=' WK-NG-CNT
                   ' KOKAN=' WK-KOKAN ' FUGO=' WK-FUGO.
           DISPLAY 'FUMEI=' WK-FUMEI ' MTRNAS=' WK-MTRNAS
                   ' KENIN01=' KENIN-CNT (1)
                   ' KENIN80=' KENIN-CNT (80).
           IF WK-NG-CNT GREATER ZERO
               MOVE 8          TO RETURN-CODE
           ELSE
               MOVE 0          TO RETURN-CODE.
           STOP RUN.
       ABEND-RTN.
           DISPLAY 'RBKEN02C ABEND'.
           MOVE 16             TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * 計器マスタ展開 (満了桁数の参照用)
      *****************************************************************
       MTR-LOAD                SECTION.
       MTL-010.
           OPEN INPUT MTRMST.
       MTL-020.
           READ MTRMST
               AT END GO TO MTL-090.
           IF MT-CNT NOT LESS 6500
               GO TO MTL-090.
           ADD 1               TO MT-CNT.
           MOVE MTR-SPT-NO     TO MT-SPT (MT-CNT).
           MOVE MTR-KETA-SU    TO MT-KETA (MT-CNT).
           GO TO MTL-020.
       MTL-090.
           CLOSE MTRMST.
       MTR-LOAD-EX.
           EXIT.
      *****************************************************************
      * 計器探索 (番兵法) -> WS-KETA / 見つからねば 6 とする
      *****************************************************************
       MTR-SRCH                SECTION.
       MSR-010.
           MOVE KEN-SPT-NO     TO MT-SPT (MT-CNT + 1).
           MOVE 1              TO WS-I.
       MSR-020.
           IF MT-SPT (WS-I) = KEN-SPT-NO
               GO TO MSR-030.
           ADD 1               TO WS-I.
           GO TO MSR-020.
       MSR-030.
           IF WS-I GREATER MT-CNT
               ADD 1           TO WK-MTRNAS
               MOVE 6          TO WS-KETA
           ELSE
               MOVE MT-KETA (WS-I) TO WS-KETA.
       MTR-SRCH-EX.
           EXIT.
      *****************************************************************
      * 追加検証 (H05.06 個別障害対応の積み重ね. 番号順のまま)
      *****************************************************************
       KEN-VALID2              SECTION.
       KV2-010.
           IF KEN-NENGETU (5:2) LESS '01' OR
              KEN-NENGETU (5:2) GREATER '12'
               MOVE 'E211' TO NG-CD
               GO TO KV2-NG.
           IF KEN-KENSHIN-BI (1:2) NOT = WS-PRM (3:2) AND
              KEN-KENSHIN-KBN NOT = '9'
               MOVE 'E212' TO NG-CD
               GO TO KV2-NG.
           IF KEN-KENSHIN-BI (3:2) NOT = KEN-NENGETU (5:2) AND
              KEN-KENSHIN-KBN NOT = '3' AND
              KEN-KENSHIN-KBN NOT = '9'
               MOVE 'E213' TO NG-CD
               GO TO KV2-NG.
           IF KEN-KENSHIN-KBN NOT = '1' AND
              KEN-KENSHIN-KBN NOT = '2' AND
              KEN-KENSHIN-KBN NOT = '3' AND
              KEN-KENSHIN-KBN NOT = '9'
               MOVE 'E214' TO NG-CD
               GO TO KV2-NG.
           IF KEN-KOKAN-FLG NOT = SPACE AND
              KEN-KOKAN-FLG NOT = 'K' AND
              KEN-KOKAN-FLG NOT = 'G'
               MOVE 'E215' TO NG-CD
               GO TO KV2-NG.
           IF KEN-BIKO (1:1) = X'0E' AND KEN-BIKO (2:1) = SPACE
               MOVE 'E216' TO NG-CD
               GO TO KV2-NG.
           IF KEN-SPT-NO (21:2) NOT NUMERIC
               MOVE 'E217' TO NG-CD
               GO TO KV2-NG.
           GO TO KEN-VALID2-EX.
       KV2-NG.
           ADD 1               TO WK-NG-CNT.
           MOVE KEN-SPT-NO     TO NG-SPT.
           MOVE KEN-NENGETU    TO NG-YM.
           WRITE NG-REC.
           GO TO MAIN-LOOP.
       KEN-VALID2-EX.
           EXIT.
      *****************************************************************
      * 検針員別集計簿の書出し
      *****************************************************************
       SUM-OUT                 SECTION.
       SMO-010.
           MOVE 1              TO WS-I.
       SMO-020.
           IF WS-I GREATER 80
               GO TO SUM-OUT-EX.
           IF KENIN-CNT (WS-I) = ZERO
               GO TO SMO-030.
           MOVE WS-I           TO SUM-KENIN.
           MOVE KENIN-CNT (WS-I) TO SUM-KENSU.
           WRITE SUM-REC.
       SMO-030.
           ADD 1               TO WS-I.
           GO TO SMO-020.
       SUM-OUT-EX.
           EXIT.
      *****************************************************************
      * 検針検証本体 (H05 の並びを崩さぬこと)
      *****************************************************************
       KEN-CHECK               SECTION.
       KCH-010.
           IF KEN-SPT-NO (1:2) NOT = '03'
               MOVE 'E201' TO NG-CD
               GO TO KCH-NG.
           IF KEN-KENSHIN-BI NOT NUMERIC
               MOVE 'E202' TO NG-CD
               GO TO KCH-NG.
           IF KEN-ZEN-SIJISU NOT NUMERIC OR
              KEN-KON-SIJISU NOT NUMERIC
               MOVE 'E203' TO NG-CD
               GO TO KCH-NG.
      *    検針員判定 (H21 追加)
      *    ※本番は NUMPROC(NOPFD) のためスペースも数字扱いとなり
      *      この分岐は通らない. 移行先では通る可能性あり (G-08)
           IF KEN-KENSHININ NUMERIC
               MOVE KEN-KENSHININ TO KENIN-NO
               IF KENIN-NO GREATER ZERO AND
                  KENIN-NO NOT GREATER 80
                   ADD 1 TO KENIN-CNT (KENIN-NO)
               END-IF
           ELSE
               ADD 1 TO WK-FUMEI.
       KCH-020.
           PERFORM MTR-SRCH    THRU MTR-SRCH-EX.
           PERFORM SIYO-KEISAN THRU SIYO-KEISAN-EX.
           IF W-SIYO2 NOT = W-SIYO
               MOVE 'E205' TO NG-CD
               GO TO KCH-NG.
D          IF WK-DEBUG-FLG = 'Y'
D              DISPLAY 'KCH SPT=' KEN-SPT-NO ' SIYO=' W-SIYO.
           MOVE KEN-REC        TO OUT-REC.
           WRITE OUT-REC.
           ADD 1               TO WK-OUT-CNT.
           GO TO KEN-CHECK-EX.
       KCH-NG.
           ADD 1               TO WK-NG-CNT.
           MOVE KEN-SPT-NO     TO NG-SPT.
           MOVE KEN-NENGETU    TO NG-YM.
           WRITE NG-REC.
       KEN-CHECK-EX.
           EXIT.
      *****************************************************************
      * 使用量再計算 (満了巻き戻り補正: 交換フラグに頼らない)
      *****************************************************************
       SIYO-KEISAN             SECTION.
       SIK-010.
           MOVE KEN-SIYORYO    TO W-SIYO.
      *    概算検針(区分G)は再計算せず申告値のまま通す (S63 特例)
           IF KEN-KOKAN-FLG = 'G' OR KEN-KENSHIN-KBN = '9'
               MOVE W-SIYO     TO W-SIYO2
               GO TO SIK-030.
           IF KEN-ZEN-SIJISU GREATER 999999 OR
              KEN-KON-SIJISU GREATER 999999
               MOVE ZERO       TO W-SIYO2
               GO TO SIK-030.
           IF KEN-KON-SIJISU NOT LESS KEN-ZEN-SIJISU
               COMPUTE W-SIYO2 = KEN-KON-SIJISU - KEN-ZEN-SIJISU
               GO TO SIK-030.
      *    今回 < 前回 : 計器満了の巻き戻りとみなす
           ADD 1               TO WK-KOKAN.
           MOVE 10             TO W-MANRYO.
           MOVE 1              TO WS-I.
       SIK-020.
           IF WS-I NOT LESS WS-KETA
               GO TO SIK-025.
           COMPUTE W-MANRYO = W-MANRYO * 10.
           ADD 1               TO WS-I.
           GO TO SIK-020.
       SIK-025.
           COMPUTE W-SIYO2 = W-MANRYO - KEN-ZEN-SIJISU
                           + KEN-KON-SIJISU.
       SIK-030.
           IF W-SIYO2 LESS ZERO
               ADD 1           TO WK-FUGO
               COMPUTE W-SIYO2 = W-SIYO2 * -1.
       SIYO-KEISAN-EX.
           EXIT.
      *----------------------------------------------------------------
      *(S62 当時の検算式: 保存のため残置 -- 触るな)
      *    COMPUTE W-SIYO2 = KEN-KON-SIJISU - KEN-ZEN-SIJISU + 1000000.
      *    DIVIDE W-SIYO2 BY 1000000 GIVING WS-I REMAINDER W-SIYO2.
      *----------------------------------------------------------------
