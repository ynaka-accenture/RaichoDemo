      *****************************************************************
      * RBGET01C  月次集計 (月次M010)         (株)東西電力 電算部     *
      *---------------------------------------------------------------*
      * H08.05 初版  H24.07 割引原資按分  R03.02 構成比追加           *
      * 翻訳オプション TRUNC(OPT) 前提. STD へ変更禁止 (結果相違報告) *
      * 出力は世代データセット RACS.GETSUM(+1) へ (運用手順書 7.3)    *
      * 注意: 「6月分」は検針月 (5/20-6/19 検針) であり暦月ではない.  *
      *       暦月換算は行わない (H08 経営会議決定)                   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBGET01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RYOIN   ASSIGN TO 'app/data/portable/RYOFILE.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-RYO.
           SELECT SUMOUT  ASSIGN TO 'app/data/portable/GETSUM.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-SUM.
           SELECT ERRF    ASSIGN TO 'app/data/portable/GETERR.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-ERR.
       DATA DIVISION.
       FILE SECTION.
       FD  RYOIN RECORD CONTAINS 256 CHARACTERS.
       COPY RCRYOREC.
       FD  SUMOUT RECORD CONTAINS 80 CHARACTERS.
       01  SUM-REC.
           05  SUM-CHIKU       PIC 9(2).
           05  SUM-KENSU       PIC 9(7).
           05  SUM-KWH         PIC S9(9)  COMP-3.
           05  SUM-KIN         PIC S9(11) COMP-3.
           05  SUM-WARIATE     PIC S9(7)  COMP-3.
           05  SUM-KOSEI       PIC 9(3)V99.
           05  FILLER          PIC X(51).
       FD  ERRF RECORD CONTAINS 32 CHARACTERS.
       01  ERR-REC             PIC X(32).
       WORKING-STORAGE SECTION.
       77  ST-RYO              PIC XX VALUE '00'.
       77  ST-SUM              PIC XX VALUE '00'.
       77  ST-ERR              PIC XX VALUE '00'.
       77  WK-IN-CNT           PIC 9(7)  VALUE ZERO.
       77  WK-NG-CNT           PIC 9(7)  VALUE ZERO.
      *    総合計: 初期値なし (ロード時 0 の前提 -- H08 から)
       77  SOKEI-KIN           PIC S9(13) COMP-3.
       77  SOKEI-KWH           PIC S9(11) COMP-3.
       77  SOKEI-KEN           PIC 9(7).
      *    割引原資 (本社負担分. 地区へ件数比で按分する)
       77  GENSHI-KIN          PIC S9(7)  VALUE 100000.
       77  WARIATE-ZAN         PIC S9(7)  VALUE ZERO.
       77  W-WARIATE           PIC S9(7)  VALUE ZERO.
       77  HEIKIN-TANKA        PIC S9(5)V99 VALUE ZERO.
      *    構成比は倍精度浮動で計算 (R03 -- 性能都合)
       77  KOSEI-F             COMP-2     VALUE ZERO.
       77  SOKEI-F             COMP-2     VALUE ZERO.
       77  IJOU-CNT            PIC 9(3)   VALUE ZERO.
       77  SAIGO-CHIKU         PIC 9(2)   VALUE ZERO.
       77  WS-I                PIC 9(3)   VALUE ZERO.
       77  WS-CHIKU            PIC 9(2)   VALUE ZERO.
       77  WS-J                PIC 9(3)   VALUE ZERO.
       77  W-MAXKEN            PIC 9(7)   VALUE ZERO.
       77  W-MAXCHI            PIC 9(2)   VALUE ZERO.
       77  W-SEDKEI            PIC 9(7)   VALUE ZERO.
       01  KAIKYU-TBL.
           05  KAIKYU-CNT      PIC 9(7) OCCURS 6 VALUE ZERO.
       01  SEDAI-TBL.
           05  SEDAI-CNT       PIC 9(7) OCCURS 4 VALUE ZERO.
       01  CHK-TBL.
           05  CHK-E OCCURS 47.
               10  CHK-KEN     PIC 9(7).
               10  CHK-KWH     PIC S9(9)  COMP-3.
               10  CHK-KIN     PIC S9(11) COMP-3.
       PROCEDURE DIVISION.
       MAIN-SEC                SECTION.
       MAIN-000.
           MOVE ZERO           TO SOKEI-KIN SOKEI-KWH SOKEI-KEN.
           MOVE 1              TO WS-I.
       INIT-LOOP.
           IF WS-I GREATER 47
               GO TO INIT-END.
           MOVE ZERO           TO CHK-KEN (WS-I) CHK-KWH (WS-I)
                                  CHK-KIN (WS-I).
           ADD 1               TO WS-I.
           GO TO INIT-LOOP.
       INIT-END.
           OPEN INPUT RYOIN.
           OPEN OUTPUT SUMOUT.
           OPEN OUTPUT ERRF.
       MAIN-LOOP.
           READ RYOIN
               AT END GO TO ANBUN-RTN.
           ADD 1               TO WK-IN-CNT.
           PERFORM RYO-VALID   THRU RYO-VALID-EX.
           MOVE RYO-SPT-NO (3:2) TO WS-CHIKU.
           ADD 1               TO CHK-KEN (WS-CHIKU).
           ADD RYO-SIYORYO     TO CHK-KWH (WS-CHIKU).
           ADD RYO-GOKEI       TO CHK-KIN (WS-CHIKU).
           ADD 1               TO SOKEI-KEN.
           ADD RYO-SIYORYO     TO SOKEI-KWH.
           ADD RYO-GOKEI       TO SOKEI-KIN.
           PERFORM KAIKYU-KEISU THRU KAIKYU-KEISU-EX.
           EVALUATE RYO-ZEI-SEDAI
               WHEN '01' ADD 1 TO SEDAI-CNT (1)
               WHEN '02' ADD 1 TO SEDAI-CNT (2)
               WHEN '03' ADD 1 TO SEDAI-CNT (3)
               WHEN '04' ADD 1 TO SEDAI-CNT (4)
           END-EVALUATE.
           GO TO MAIN-LOOP.
      *****************************************************************
      * 金額階級別度数 (H15 営業資料用)
      *****************************************************************
       KAIKYU-KEISU            SECTION.
       KAI-010.
           IF RYO-GOKEI LESS ZERO
               ADD 1 TO KAIKYU-CNT (1)
               GO TO KAIKYU-KEISU-EX.
           IF RYO-GOKEI LESS 5000
               ADD 1 TO KAIKYU-CNT (2)
               GO TO KAIKYU-KEISU-EX.
           IF RYO-GOKEI LESS 10000
               ADD 1 TO KAIKYU-CNT (3)
               GO TO KAIKYU-KEISU-EX.
           IF RYO-GOKEI LESS 20000
               ADD 1 TO KAIKYU-CNT (4)
               GO TO KAIKYU-KEISU-EX.
           IF RYO-GOKEI LESS 50000
               ADD 1 TO KAIKYU-CNT (5)
               GO TO KAIKYU-KEISU-EX.
           ADD 1 TO KAIKYU-CNT (6).
       KAIKYU-KEISU-EX.
           EXIT.
      *****************************************************************
      * 入力検証 (H10 に一括追加. E5xx)
      *****************************************************************
       RYO-VALID               SECTION.
       RVL-010.
           IF RYO-SPT-NO (1:2) NOT = '03'
               GO TO RVL-NG.
           IF RYO-SPT-NO (3:2) NOT NUMERIC
               GO TO RVL-NG.
           IF RYO-SPT-NO (3:2) LESS '01' OR
              RYO-SPT-NO (3:2) GREATER '47'
               GO TO RVL-NG.
           IF RYO-SPT-NO (21:2) NOT NUMERIC
               GO TO RVL-NG.
           IF RYO-SEIKYU-YM (1:4) LESS '1990' OR
              RYO-SEIKYU-YM (1:4) GREATER '2099'
               GO TO RVL-NG.
           IF RYO-SEIKYU-YM (5:2) LESS '01' OR
              RYO-SEIKYU-YM (5:2) GREATER '12'
               GO TO RVL-NG.
           IF RYO-GOKEI LESS -9999999 OR
              RYO-GOKEI GREATER 99999999
               GO TO RVL-NG.
           IF RYO-SIYORYO LESS -99999 OR
              RYO-SIYORYO GREATER 999999
               GO TO RVL-NG.
           IF RYO-ZEI-SEDAI NOT = '01' AND
              RYO-ZEI-SEDAI NOT = '02' AND
              RYO-ZEI-SEDAI NOT = '03' AND
              RYO-ZEI-SEDAI NOT = '04'
               GO TO RVL-NG.
           IF RYO-SEIKYU-KBN NOT = '1' AND RYO-SEIKYU-KBN NOT = '2'
               GO TO RVL-NG.
           IF RYO-NYUKIN-FLG NOT = '0' AND RYO-NYUKIN-FLG NOT = '1'
               GO TO RVL-NG.
           IF RYO-KENSHIN-BI NOT NUMERIC
               GO TO RVL-NG.
           IF RYO-NIWARI-NISSU LESS 1 OR RYO-NIWARI-NISSU GREATER 31
               GO TO RVL-NG.
           GO TO RYO-VALID-EX.
       RVL-NG.
           ADD 1               TO WK-NG-CNT.
           MOVE RYO-SPT-NO (1:22) TO ERR-REC.
           WRITE ERR-REC.
           GO TO MAIN-LOOP.
       RYO-VALID-EX.
           EXIT.
      *****************************************************************
      * 割引原資の按分: 件数比・円未満切捨て. 端数は最終有効地区へ    *
      * 寄せる (合計を原資と一致させるため -- H24 経理要件)           *
      *****************************************************************
       ANBUN-RTN.
           IF SOKEI-KEN = ZERO
               GO TO SYUKEI-RTN.
           MOVE GENSHI-KIN     TO WARIATE-ZAN.
           MOVE ZERO           TO SAIGO-CHIKU.
           MOVE 1              TO WS-I.
       ANB-LOOP.
           IF WS-I GREATER 47
               GO TO ANB-YOSE.
           IF CHK-KEN (WS-I) = ZERO
               ADD 1 TO WS-I
               GO TO ANB-LOOP.
      *    按分額 = 原資 x 地区件数 / 総件数 (切捨て)
           COMPUTE W-WARIATE = GENSHI-KIN * CHK-KEN (WS-I)
                             / SOKEI-KEN.
           SUBTRACT W-WARIATE FROM WARIATE-ZAN.
           MOVE WS-I           TO SAIGO-CHIKU.
           PERFORM SUM-EDIT    THRU SUM-EDIT-EX.
           ADD 1               TO WS-I.
           GO TO ANB-LOOP.
       ANB-YOSE.
      *    残差寄せ: 最終有効地区の行を上書き出力はできないため
      *    残差行 (地区99) として追記する運用 (H24)
           IF WARIATE-ZAN NOT = ZERO
               MOVE SPACES     TO SUM-REC
               MOVE 99         TO SUM-CHIKU
               MOVE ZERO       TO SUM-KENSU
               MOVE ZERO       TO SUM-KWH
               MOVE ZERO       TO SUM-KIN
               MOVE WARIATE-ZAN TO SUM-WARIATE
               MOVE ZERO       TO SUM-KOSEI
               WRITE SUM-REC.
           GO TO SYUKEI-RTN.
      *****************************************************************
       SUM-EDIT                SECTION.
       SME-010.
           MOVE SPACES         TO SUM-REC.
           MOVE WS-I           TO SUM-CHIKU.
           MOVE CHK-KEN (WS-I) TO SUM-KENSU.
           MOVE CHK-KWH (WS-I) TO SUM-KWH.
           MOVE CHK-KIN (WS-I) TO SUM-KIN.
           MOVE W-WARIATE      TO SUM-WARIATE.
      *    構成比 (倍精度): 判定は 2%以上を「大口地区」とする
           MOVE CHK-KEN (WS-I) TO KOSEI-F.
           MOVE SOKEI-KEN      TO SOKEI-F.
           COMPUTE KOSEI-F = KOSEI-F / SOKEI-F.
           IF KOSEI-F NOT LESS 0.02
               IF CHK-KEN (WS-I) GREATER 50
                   IF CHK-KIN (WS-I) GREATER 100000
                       ADD 1   TO IJOU-CNT
                   END-IF
               END-IF
           END-IF.
           COMPUTE SUM-KOSEI ROUNDED = KOSEI-F * 100.
           WRITE SUM-REC.
       SUM-EDIT-EX.
           EXIT.
      *****************************************************************
       SYUKEI-RTN.
           PERFORM SAIDAI-CHIKU THRU SAIDAI-CHIKU-EX.
           PERFORM SEDAI-KENSAN THRU SEDAI-KENSAN-EX.
           CLOSE RYOIN SUMOUT ERRF.
      *    平均単価 (複合式: 中間桁は翻訳オプション依存に注意)
           IF SOKEI-KWH GREATER ZERO
               COMPUTE HEIKIN-TANKA = SOKEI-KIN * 100
                   / SOKEI-KWH / 100.
           DISPLAY 'RBGET01C IN=' WK-IN-CNT ' NG=' WK-NG-CNT
                   ' KEN=' SOKEI-KEN ' KWH=' SOKEI-KWH.
           DISPLAY 'KIN=' SOKEI-KIN ' HEIKIN=' HEIKIN-TANKA
                   ' OKUCHI=' IJOU-CNT ' ZAN=' WARIATE-ZAN.
           DISPLAY 'KAIKYU=' KAIKYU-CNT (1) '/' KAIKYU-CNT (2)
                   '/' KAIKYU-CNT (3) '/' KAIKYU-CNT (4)
                   '/' KAIKYU-CNT (5) '/' KAIKYU-CNT (6)
                   ' SEDAI04=' SEDAI-CNT (4)
                   ' MAXCHI=' W-MAXCHI '/' W-MAXKEN.
           MOVE 0              TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * 最大件数地区の抽出 (営業資料)
      *****************************************************************
       SAIDAI-CHIKU            SECTION.
       SAI-010.
           MOVE ZERO           TO W-MAXKEN W-MAXCHI.
           MOVE 1              TO WS-J.
       SAI-020.
           IF WS-J GREATER 47
               GO TO SAIDAI-CHIKU-EX.
           IF CHK-KEN (WS-J) GREATER W-MAXKEN
               MOVE CHK-KEN (WS-J) TO W-MAXKEN
               MOVE WS-J           TO W-MAXCHI.
           ADD 1               TO WS-J.
           GO TO SAI-020.
       SAIDAI-CHIKU-EX.
           EXIT.
      *****************************************************************
      * 税世代別件数の検算 (総件数と一致すること)
      *****************************************************************
       SEDAI-KENSAN            SECTION.
       SED-010.
           MOVE ZERO           TO W-SEDKEI.
           MOVE 1              TO WS-J.
       SED-020.
           IF WS-J GREATER 4
               GO TO SED-030.
           ADD SEDAI-CNT (WS-J) TO W-SEDKEI.
           ADD 1               TO WS-J.
           GO TO SED-020.
       SED-030.
           IF W-SEDKEI NOT = SOKEI-KEN
               IF SOKEI-KEN GREATER ZERO
                   IF WK-NG-CNT = ZERO
                       IF W-SEDKEI GREATER ZERO
                           DISPLAY 'RBGET01C W501 SEDAI FUICCHI'
                       END-IF
                   END-IF
               END-IF
           END-IF.
       SEDAI-KENSAN-EX.
           EXIT.
      *----------------------------------------------------------------
      *(H24 検討メモ: 残差は最終地区へ加算する案もあったが 帳票側の
      *  照合が地区99前提のため現行方式とした. 変更時は RBTBL01C も)
      *----------------------------------------------------------------
