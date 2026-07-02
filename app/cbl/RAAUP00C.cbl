      *****************************************************************
      * RAAUP00C  デマンド受付 (常駐)          東西電力 電算部        *
      *---------------------------------------------------------------*
      * H22.06 初版  H24.10 履歴 Db2 化                               *
      * 待ち行列から電文を取り 判定子へ渡し 応答と履歴を書く          *
      * MQ は共通サブ RUMQSUB 経由 (電算部標準 5.1)                   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RAAUP00C.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       77  MQ-FUNC             PIC X(4)  VALUE SPACE.
       01  MQ-BUFFER           PIC X(100) VALUE SPACE.
       77  MQ-LEN              PIC S9(4) COMP VALUE ZERO.
       77  MQ-CC               PIC S9(4) COMP VALUE ZERO.
       77  MQ-REASON           PIC S9(4) COMP VALUE ZERO.
       01  OTO-DENBUN          PIC X(100) VALUE SPACE.
       01  OTO-DENBUN-R REDEFINES OTO-DENBUN.
           05  OT-SYU          PIC X(2).
           05  OT-SPT          PIC X(22).
           05  OT-JIKOKU       PIC X(4).
           05  OT-HANTEI       PIC X(1).
           05  OT-RITU         PIC 9(3).99.
           05  FILLER          PIC X(65).
       77  DM-HANTEI           PIC X(1)  VALUE SPACE.
       77  DM-RITU             PIC S9(3)V99 COMP-3 VALUE ZERO.
       77  DM-RC               PIC S9(4) COMP VALUE ZERO.
       77  HV-SPT              PIC X(22) VALUE SPACE.
       77  HV-JIKOKU           PIC X(8)  VALUE SPACE.
       77  HV-HANTEI           PIC X(8)  VALUE SPACE.
       77  HV-RITU             PIC X(8)  VALUE SPACE.
       77  WK-GET              PIC 9(5)  VALUE ZERO.
       77  WK-N                PIC 9(5)  VALUE ZERO.
       77  WK-W                PIC 9(5)  VALUE ZERO.
       77  WK-C                PIC 9(5)  VALUE ZERO.
       77  WK-E                PIC 9(5)  VALUE ZERO.
       77  WK-J100             PIC 9(5)  VALUE ZERO.
       77  WK-PUT-NG           PIC 9(5)  VALUE ZERO.
       77  WK-SQL-NG           PIC 9(5)  VALUE ZERO.
       77  WK-RETRY            PIC 9(1)  VALUE ZERO.
       77  WK-ZEN-SPT          PIC X(26) VALUE SPACE.
       77  WK-RENZOKU          PIC 9(3)  VALUE ZERO.
       77  WS-I                PIC 9(3)  VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM KIDO-KENSA  THRU KIDO-KENSA-EX.
       MQ-LOOP.
           MOVE 'GET '         TO MQ-FUNC.
           CALL 'RUMQSUB' USING MQ-FUNC MQ-BUFFER MQ-LEN
                                MQ-CC MQ-REASON.
           IF MQ-REASON = 2033
               GO TO SYUKEI-RTN.
           IF MQ-REASON = 2009
      *        接続断: 1回だけ再試行 (H22 運用)
               IF WK-RETRY = ZERO
                   MOVE 1 TO WK-RETRY
                   GO TO MQ-LOOP
               END-IF
               DISPLAY 'RAAUP00C E102 SETSUZOKU DAN'
               GO TO SYUKEI-RTN.
           IF MQ-REASON = 2085
               DISPLAY 'RAAUP00C E103 QUEUE TEIGI NASI'
               GO TO SYUKEI-RTN.
           IF MQ-CC NOT = ZERO
               DISPLAY 'RAAUP00C E101 MQ CC=' MQ-CC
                       ' RS=' MQ-REASON
               GO TO SYUKEI-RTN.
           MOVE ZERO           TO WK-RETRY.
           ADD 1               TO WK-GET.
           PERFORM DENBUN-YOBO THRU DENBUN-YOBO-EX.
           PERFORM YOBI-SOSA   THRU YOBI-SOSA-EX.
           CALL 'RAAUP01C' USING MQ-BUFFER DM-HANTEI
                                 DM-RITU DM-RC.
           PERFORM OTO-HENSYU  THRU OTO-HENSYU-EX.
           PERFORM OTO-KENSA   THRU OTO-KENSA-EX.
           MOVE 'PUT '         TO MQ-FUNC.
           MOVE OTO-DENBUN     TO MQ-BUFFER.
           CALL 'RUMQSUB' USING MQ-FUNC MQ-BUFFER MQ-LEN
                                MQ-CC MQ-REASON.
           IF MQ-CC NOT = ZERO
               ADD 1 TO WK-PUT-NG.
           PERFORM RIREKI-KAKI THRU RIREKI-KAKI-EX.
           GO TO MQ-LOOP.
       SYUKEI-RTN.
           DISPLAY 'RAAUP00C GET=' WK-GET ' N=' WK-N ' W=' WK-W
                   ' C=' WK-C ' E=' WK-E ' J100=' WK-J100
                   ' PUTNG=' WK-PUT-NG ' SQLNG=' WK-SQL-NG.
           MOVE 0              TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * 起動時検査 (二重起動・環境の確認は運用が別途行うが最低限)
      *****************************************************************
       KIDO-KENSA              SECTION.
       KDK-010.
           IF WK-GET NOT = ZERO
               DISPLAY 'RAAUP00C E001 NIJU KIDO'
               MOVE 8 TO RETURN-CODE
               STOP RUN.
           IF WK-RETRY NOT = ZERO
               GO TO KDK-NG.
           IF WK-J100 NOT = ZERO
               GO TO KDK-NG.
           GO TO KIDO-KENSA-EX.
       KDK-NG.
           DISPLAY 'RAAUP00C E002 WORK FUSEI'.
           MOVE 8              TO RETURN-CODE.
           STOP RUN.
       KIDO-KENSA-EX.
           EXIT.
      *****************************************************************
      * 受信予防検査: 同一地点同一コマの連続受信を数える (H24)
      *****************************************************************
       DENBUN-YOBO             SECTION.
       DYB-010.
           IF MQ-BUFFER (1:2) NOT = 'D1'
               GO TO DENBUN-YOBO-EX.
           IF MQ-BUFFER (3:26) = WK-ZEN-SPT
               ADD 1 TO WK-RENZOKU
               GO TO DYB-020.
           MOVE ZERO           TO WK-RENZOKU.
           MOVE MQ-BUFFER (3:26) TO WK-ZEN-SPT.
           GO TO DENBUN-YOBO-EX.
       DYB-020.
           IF WK-RENZOKU GREATER 3
               DISPLAY 'RAAUP00C W301 RENZOKU JUSHIN'.
       DENBUN-YOBO-EX.
           EXIT.
      *****************************************************************
      * 予備域走査: 末尾に制御文字が混じる伝送障害の検出 (H22)
      *****************************************************************
       YOBI-SOSA               SECTION.
       YBS-010.
           MOVE 38             TO WS-I.
       YBS-020.
           IF WS-I GREATER 100
               GO TO YOBI-SOSA-EX.
           IF MQ-BUFFER (WS-I:1) LESS SPACE
               DISPLAY 'RAAUP00C W302 SEIGYO MOJI'
               GO TO YOBI-SOSA-EX.
           ADD 1               TO WS-I.
           GO TO YBS-020.
       YOBI-SOSA-EX.
           EXIT.
      *****************************************************************
      * 応答編集と計数 (100.00 ジャストは別掲計数 -- 取決め No.31)
      *****************************************************************
       OTO-HENSYU              SECTION.
       OTH-010.
           MOVE SPACES         TO OTO-DENBUN.
           MOVE 'R1'           TO OT-SYU.
           MOVE MQ-BUFFER (3:22) TO OT-SPT.
           MOVE MQ-BUFFER (25:4) TO OT-JIKOKU.
           MOVE DM-HANTEI      TO OT-HANTEI.
           MOVE DM-RITU        TO OT-RITU.
           IF DM-HANTEI = 'N'
               ADD 1 TO WK-N
               GO TO OTO-HENSYU-EX.
           IF DM-HANTEI = 'W'
               ADD 1 TO WK-W
               IF DM-RITU = 100.00
                   ADD 1 TO WK-J100
               END-IF
               GO TO OTO-HENSYU-EX.
           IF DM-HANTEI = 'C'
               ADD 1 TO WK-C
               IF DM-RITU = 999.99
                   DISPLAY 'RAAUP00C W303 RITU JOGEN'
               END-IF
               GO TO OTO-HENSYU-EX.
           IF DM-HANTEI NOT = 'E'
               DISPLAY 'RAAUP00C E104 HANTEI FUMEI: ' DM-HANTEI
               ADD 1 TO WK-E
               GO TO OTO-HENSYU-EX.
           ADD 1               TO WK-E.
       OTO-HENSYU-EX.
           EXIT.
      *****************************************************************
      * 応答自己検証: 送る前に自分の編集結果を確かめる (H24)
      *****************************************************************
       OTO-KENSA               SECTION.
       OTK-010.
           IF OT-SYU NOT = 'R1'
               GO TO OTK-NG.
           IF OT-HANTEI NOT = 'N' AND OT-HANTEI NOT = 'W' AND
              OT-HANTEI NOT = 'C' AND OT-HANTEI NOT = 'E'
               GO TO OTK-NG.
           IF OT-JIKOKU NOT NUMERIC
               IF OT-HANTEI NOT = 'E'
                   GO TO OTK-NG
               END-IF
           END-IF
           GO TO OTO-KENSA-EX.
       OTK-NG.
           DISPLAY 'RAAUP00C E105 OTO FUSEI'.
       OTO-KENSA-EX.
           EXIT.
      *****************************************************************
      * 履歴書込み (H24: Db2 化. 表 RACS.DMD_RIREKI)
      *****************************************************************
       RIREKI-KAKI             SECTION.
       RRK-010.
           IF DM-HANTEI = 'E'
               GO TO RIREKI-KAKI-EX.
           IF DM-HANTEI = 'N'
               GO TO RIREKI-KAKI-EX.
           MOVE MQ-BUFFER (3:22) TO HV-SPT.
           MOVE MQ-BUFFER (25:4) TO HV-JIKOKU.
           MOVE DM-HANTEI      TO HV-HANTEI.
           MOVE DM-RITU        TO HV-RITU (1:6).
           EXEC SQL
               INSERT INTO DMD_RIREKI
                   (SPT_NO, JIKOKU, HANTEI, RITU)
               VALUES (:HV-SPT, :HV-JIKOKU, :HV-HANTEI, :HV-RITU)
           END-EXEC.
           IF SQLCODE = ZERO
               GO TO RIREKI-KAKI-EX.
           IF SQLCODE = -803
      *        重複キー: 同一コマ再送とみなし読み飛ばす
               ADD 1 TO WK-SQL-NG
               GO TO RIREKI-KAKI-EX.
           IF SQLCODE = -911
               ADD 1 TO WK-SQL-NG
               DISPLAY 'RAAUP00C E202 DEADLOCK'
               GO TO RIREKI-KAKI-EX.
           ADD 1               TO WK-SQL-NG.
           DISPLAY 'RAAUP00C E201 SQLCODE=' SQLCODE.
       RIREKI-KAKI-EX.
           EXIT.
