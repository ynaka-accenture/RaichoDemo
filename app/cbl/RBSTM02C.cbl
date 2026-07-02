      *****************************************************************
      * RBSTM02C  検針票 行編集 (子)          (株)東西電力 電算部     *
      *---------------------------------------------------------------*
      * S61.02 初版. 頁制御は切替段落方式 (当時の標準作法)            *
      * 呼出: FUNC 'D'=明細(必要なら見出し) 'T'=合計行                *
      *       MORE='Y' の間は同一FUNCで再呼出しのこと                 *
      * 注意: 行編集ワークはクリアしない (全桁上書きのため不要)       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBSTM02C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CTLF ASSIGN TO 'app/data/portable/STMCTL.ctl'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-CTL.
       DATA DIVISION.
       FILE SECTION.
       FD  CTLF RECORD CONTAINS 8 CHARACTERS.
       01  CTL-REC.
           05  CTL-TYPE        PIC X(3).
           05  CTL-VAL         PIC X(5).
       WORKING-STORAGE SECTION.
       77  ST-CTL              PIC XX VALUE '00'.
       77  FIRST-FLG           PIC X  VALUE 'Y'.
       77  ALT-CODE            PIC X(2) VALUE X'4B4B'.
       77  WS-I                PIC 9(3) VALUE ZERO.
       77  WS-GAIJI            PIC 9(5) VALUE ZERO.
       01  W-NAME              PIC X(40).
      *    票見出し: 日本語直書き不可時代の16進定数
      *    (読み: トウザイデンリヨク ケンシンヒヨウ)
       01  HYODAI-C            PIC X(20) VALUE
           X'0E4541454245434544454545464547454845490F'.
       77  PAGE-NO             PIC 9(4)  VALUE ZERO.
       77  GYO-CNT             PIC 9(2)  VALUE ZERO.
       77  MEISAI-MAX          PIC 9(2)  VALUE 60.
       77  ED-KIN              PIC ---,---,--9.
       77  ED-KEN              PIC ---,--9.
       77  ED-PAGE             PIC ZZZ9.
      *    行編集ワーク (S61: クリア運用なし)
       01  W-LINE              PIC X(132).
       LINKAGE SECTION.
       01  STM-FUNC            PIC X.
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
       01  STM-MORE            PIC X.
       PROCEDURE DIVISION USING STM-FUNC STM-IN STM-LINE STM-MORE.
       MAIN-RTN.
           IF FIRST-FLG = 'Y'
               PERFORM CTL-LOAD THRU CTL-LOAD-EX
               MOVE 'N' TO FIRST-FLG.
           MOVE 'N'            TO STM-MORE.
           IF STM-FUNC = 'T'
               GO TO GOKEI-EDIT.
           IF STM-FUNC = 'I'
               GO TO RESET-RTN.
           IF STM-FUNC NOT = 'D'
               GO TO OWARI.
           IF PAGE-NO GREATER 9998
               MOVE '*PAGE OVER*' TO STM-LINE
               GO TO OWARI.
           IF IN-WAREKI (1:1) NOT = 'R' AND
              IN-WAREKI (1:1) NOT = 'H' AND
              IN-WAREKI (1:1) NOT = 'S'
               MOVE SPACES TO IN-WAREKI.
           GO TO EDIT-SW.
      *----------------------------------------------------------------
      * 切替段落: 初期は見出しへ. ALTER で行き先が変わる
      *----------------------------------------------------------------
       EDIT-SW.
           GO TO HEAD-EDIT.
      *----------------------------------------------------------------
       HEAD-EDIT.
           ADD 1               TO PAGE-NO.
           MOVE ZERO           TO GYO-CNT.
           MOVE SPACES         TO W-LINE.
           MOVE HYODAI-C       TO W-LINE (40:20).
           MOVE PAGE-NO        TO ED-PAGE.
           MOVE ED-PAGE        TO W-LINE (120:4).
           MOVE 'P.'           TO W-LINE (117:2).
           MOVE W-LINE         TO STM-LINE.
      *    見出しを返したので 次呼出は明細へ切替える
           ALTER EDIT-SW TO PROCEED TO MEISAI-EDIT.
           MOVE 'Y'            TO STM-MORE.
           GO TO OWARI.
      *----------------------------------------------------------------
       MEISAI-EDIT.
           MOVE IN-SPT         TO W-LINE (1:22).
      *    宛名は姓のみ (先頭12バイト固定切出し)
      *    外字は印字不能のため代替字へ置換 (H03 プリンタ更改対応)
           MOVE IN-SEI         TO W-NAME (1:12).
           MOVE 1              TO WS-I.
       GAIJI-SCAN.
           IF WS-I GREATER 11
               GO TO GAIJI-END.
           IF W-NAME (WS-I:1) = X'69' OR W-NAME (WS-I:1) = X'6A'
               MOVE ALT-CODE TO W-NAME (WS-I:2)
               ADD 1 TO WS-GAIJI
               ADD 2 TO WS-I
               GO TO GAIJI-SCAN.
           ADD 1               TO WS-I.
           GO TO GAIJI-SCAN.
       GAIJI-END.
           MOVE W-NAME (1:12)  TO W-LINE (25:12).
           MOVE 'SAMA'         TO W-LINE (38:4).
           MOVE IN-WAREKI      TO W-LINE (44:10).
           MOVE IN-KENSU       TO ED-KEN.
           MOVE ED-KEN         TO W-LINE (58:8).
           MOVE 'KWH'          TO W-LINE (67:3).
           IF IN-GOKEI LESS ZERO
               MOVE '*HENKIN*' TO W-LINE (72:8)
           ELSE
               MOVE '        ' TO W-LINE (72:8).
           IF IN-GOKEI = ZERO
               MOVE '     *ZERO* ' TO W-LINE (82:12)
               GO TO KIN-END.
           IF IN-GOKEI GREATER 9999999
               MOVE '  **KOGAKU**' TO W-LINE (82:12)
               GO TO KIN-END.
           IF IN-GOKEI LESS -999999
               MOVE ' **MINASI** ' TO W-LINE (82:12)
               GO TO KIN-END.
           MOVE IN-GOKEI       TO ED-KIN.
           MOVE ED-KIN         TO W-LINE (82:12).
       KIN-END.
           MOVE 'EN'           TO W-LINE (95:2).
           IF IN-KENSU GREATER 99999
               MOVE '*'        TO W-LINE (66:1).
           IF IN-KENSU LESS ZERO
               MOVE '-'        TO W-LINE (66:1).
           MOVE W-LINE         TO STM-LINE.
           ADD 1               TO GYO-CNT.
           IF GYO-CNT NOT LESS MEISAI-MAX
      *        頁満了: 次の明細呼出は見出しから
               ALTER EDIT-SW TO PROCEED TO HEAD-EDIT.
           GO TO OWARI.
      *----------------------------------------------------------------
       RESET-RTN.
      *    分冊切替時の初期化 ('I': 現行運用では未使用)
           MOVE ZERO           TO PAGE-NO GYO-CNT.
           ALTER EDIT-SW TO PROCEED TO HEAD-EDIT.
           GO TO OWARI.
       GOKEI-EDIT.
           MOVE SPACES         TO W-LINE.
           IF IN-SOKEN LESS ZERO
               MOVE '*KEN FUSEI*' TO W-LINE (40:11)
               GO TO GOKEI-050.
           IF IN-SOKEI LESS ZERO
               MOVE '*KIN FUSEI*' TO W-LINE (40:11).
       GOKEI-050.
           MOVE '*** GOKEI ***' TO W-LINE (1:14).
           MOVE IN-SOKEN       TO ED-KIN.
           MOVE ED-KIN         TO W-LINE (58:12).
           MOVE 'KEN'          TO W-LINE (71:3).
           MOVE IN-SOKEI       TO ED-KIN.
           MOVE ED-KIN         TO W-LINE (80:12).
           MOVE 'EN'           TO W-LINE (93:2).
           MOVE W-LINE         TO STM-LINE.
       OWARI.
           GOBACK.
      *****************************************************************
      * 帳票制御カード読込
      *****************************************************************
       CTL-LOAD                SECTION.
       CTL-010.
           OPEN INPUT CTLF.
       CTL-020.
           IF ST-CTL NOT = '00'
               GO TO CTL-090.
           READ CTLF
               AT END GO TO CTL-090.
           IF CTL-TYPE = 'MAX'
               IF CTL-VAL (1:2) NUMERIC
                   MOVE CTL-VAL (1:2) TO MEISAI-MAX
               END-IF
           END-IF
           IF CTL-TYPE = 'ALT'
               GO TO CTL-030.
           GO TO CTL-020.
       CTL-030.
           IF CTL-VAL (1:4) = '4B4B'
               MOVE X'4B4B' TO ALT-CODE
               GO TO CTL-020.
           IF CTL-VAL (1:4) = '4040'
               MOVE X'4040' TO ALT-CODE
               GO TO CTL-020.
           GO TO CTL-020.
       CTL-090.
           CLOSE CTLF.
       CTL-LOAD-EX.
           EXIT.
