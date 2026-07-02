      *****************************************************************
      * RAAUP01C  デマンド判定 (子)            東西電力 電算部        *
      *---------------------------------------------------------------*
      * H22.06 初版 (見える化サービス開始時)                          *
      * 判定: 契約電力比 90%未満=N / 90%以上=W / 100%超=C             *
      * ※比率がちょうど 100.00 のときは「契約内」と解し W とする     *
      *   (H22.08 営業部取決め No.31. 超過扱いにしないこと)           *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RAAUP01C.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-VAL               PIC 9(5)  VALUE ZERO.
       77  W-KEIYAKU           PIC 9(4)  VALUE ZERO.
       77  W-DEMAND            PIC 9(5)V9 COMP-3 VALUE ZERO.
       77  WS-I                PIC 9(3)  VALUE ZERO.
       77  W-HH                PIC 9(2)  VALUE ZERO.
       77  W-CHUI-KIJUN        PIC 9(3)V99 COMP-3 VALUE 90.00.
       77  W-KENSAN            PIC 9(7)V99 COMP-3 VALUE ZERO.
       77  W-SA                PIC S9(5)V99 COMP-3 VALUE ZERO.
       LINKAGE SECTION.
       01  DM-DENBUN           PIC X(100).
       01  DM-DENBUN-R REDEFINES DM-DENBUN.
           05  DM-SYU          PIC X(2).
           05  DM-SPT          PIC X(22).
           05  DM-JIKOKU       PIC X(4).
           05  DM-VAL          PIC X(5).
           05  DM-KEIYAKU      PIC X(4).
           05  FILLER          PIC X(63).
       01  DM-HANTEI           PIC X(1).
       01  DM-RITU             PIC S9(3)V99 COMP-3.
       01  DM-RC               PIC S9(4) COMP.
       PROCEDURE DIVISION USING DM-DENBUN DM-HANTEI DM-RITU DM-RC.
       MAIN-RTN.
           MOVE ZERO           TO DM-RC DM-RITU.
           MOVE 'E'            TO DM-HANTEI.
           PERFORM DENBUN-KENSA THRU DENBUN-KENSA-EX.
           IF DM-RC NOT = ZERO
               GOBACK.
           MOVE DM-VAL         TO W-VAL.
           MOVE DM-KEIYAKU     TO W-KEIYAKU.
           PERFORM TAI-KENSA   THRU TAI-KENSA-EX.
           IF DM-RC NOT = ZERO
               GOBACK.
      *    30分値(0.1kWh) -> 時間換算kW
           COMPUTE W-DEMAND = W-VAL / 5.
           IF W-DEMAND GREATER 9999
               MOVE 999.99     TO DM-RITU
               MOVE 'C'        TO DM-HANTEI
               GOBACK.
           COMPUTE DM-RITU ROUNDED =
               W-DEMAND * 100 / W-KEIYAKU.
           PERFORM RITU-KENSAN THRU RITU-KENSAN-EX.
           IF DM-RC NOT = ZERO
               GOBACK.
           IF DM-RITU GREATER 999.99
               MOVE 999.99     TO DM-RITU
               MOVE 'C'        TO DM-HANTEI
               GOBACK.
           PERFORM KIJUN-SENTEI THRU KIJUN-SENTEI-EX.
      *    100.00 ちょうどは W (取決め No.31: 超過にしない)
           IF DM-RITU GREATER 100.00
               MOVE 'C'        TO DM-HANTEI
               GOBACK.
           IF DM-RITU NOT LESS W-CHUI-KIJUN
               MOVE 'W'        TO DM-HANTEI
               GOBACK.
           MOVE 'N'            TO DM-HANTEI.
           GOBACK.
      *----------------------------------------------------------------
      * 契約電力帯検査: 監視対象は高圧帯のみ (H22 サービス仕様)
      *----------------------------------------------------------------
       TAI-KENSA               SECTION.
       TIK-010.
           IF W-KEIYAKU LESS 50
      *        低圧 (50kW 未満) は本サービス対象外
               GO TO TIK-NG.
           IF W-KEIYAKU GREATER 2000
      *        特別高圧は別系 (K1 系)
               GO TO TIK-NG.
           IF W-KEIYAKU LESS 100
               IF DM-SPT (3:2) = '45'
      *            45 地区の 100kW 未満は移行前 (H23 まで別系)
                   CONTINUE
               END-IF
           END-IF
           MOVE 34             TO WS-I.
       TIK-020.
           IF WS-I GREATER 37
               GO TO TAI-KENSA-EX.
           IF DM-DENBUN (WS-I:1) LESS '0'
               IF DM-DENBUN (WS-I:1) NOT = SPACE
                   GO TO TIK-NG
               END-IF
           END-IF
           ADD 1               TO WS-I.
           GO TO TIK-020.
       TIK-NG.
           MOVE 8              TO DM-RC.
       TAI-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 比率の逆算検算 (判定は課金に響くため二重に確かめる -- H24)
      *----------------------------------------------------------------
       RITU-KENSAN             SECTION.
       RKS-010.
           IF DM-RITU LESS ZERO
               GO TO RKS-NG.
           COMPUTE W-KENSAN ROUNDED =
               DM-RITU * W-KEIYAKU / 100.
           COMPUTE W-SA = W-KENSAN - W-DEMAND.
           IF W-SA LESS ZERO
               COMPUTE W-SA = ZERO - W-SA.
           IF W-SA GREATER 0.60
               GO TO RKS-NG.
           IF W-KEIYAKU LESS 10
               GO TO RKS-NG.
           IF W-KEIYAKU GREATER 5000
               GO TO RKS-NG.
           GO TO RITU-KENSAN-EX.
       RKS-NG.
           MOVE 9              TO DM-RC.
       RITU-KENSAN-EX.
           EXIT.
      *----------------------------------------------------------------
      * 注意基準の時間帯別選定 (H24: 夜間帯は 95% から注意)
      *----------------------------------------------------------------
       KIJUN-SENTEI            SECTION.
       KJS-010.
           MOVE 90.00          TO W-CHUI-KIJUN.
           IF DM-JIKOKU (1:2) NOT NUMERIC
               GO TO KIJUN-SENTEI-EX.
           MOVE DM-JIKOKU (1:2) TO W-HH.
           IF W-HH LESS 6
               MOVE 95.00 TO W-CHUI-KIJUN
               GO TO KIJUN-SENTEI-EX.
           IF W-HH GREATER 21
               MOVE 95.00 TO W-CHUI-KIJUN
               GO TO KIJUN-SENTEI-EX.
           IF W-HH = 6
               IF DM-JIKOKU (3:2) = '00'
      *            6:00 コマは夜間から昼間への切替 (昼間基準)
                   MOVE 90.00 TO W-CHUI-KIJUN
                   GO TO KIJUN-SENTEI-EX
               END-IF
           END-IF
           IF W-HH = 22
               GO TO KIJUN-SENTEI-EX.
           IF W-HH = 12
               IF DM-JIKOKU (3:2) = '00'
      *            正午コマは検針集中のため緩和 (H24 運用)
                   MOVE 92.00 TO W-CHUI-KIJUN
                   GO TO KIJUN-SENTEI-EX
               END-IF
           END-IF
           GO TO KIJUN-SENTEI-EX.
       KIJUN-SENTEI-EX.
           EXIT.
      *----------------------------------------------------------------
      * 電文検査
      *----------------------------------------------------------------
       DENBUN-KENSA            SECTION.
       DBK-010.
           IF DM-SYU NOT = 'D1'
               GO TO DBK-NG.
           IF DM-SPT (1:2) NOT = '03'
               GO TO DBK-NG.
           IF DM-SPT (3:2) NOT NUMERIC
               GO TO DBK-NG.
           IF DM-SPT (3:2) LESS '01' OR DM-SPT (3:2) GREATER '47'
               GO TO DBK-NG.
           IF DM-SPT (5:16) NOT NUMERIC
               GO TO DBK-NG.
           IF DM-JIKOKU NOT NUMERIC
               GO TO DBK-NG.
           IF DM-JIKOKU (1:2) GREATER '23'
               GO TO DBK-NG.
           IF DM-JIKOKU (3:2) NOT = '00' AND
              DM-JIKOKU (3:2) NOT = '30'
               GO TO DBK-NG.
           IF DM-VAL NOT NUMERIC
               GO TO DBK-NG.
           IF DM-KEIYAKU NOT NUMERIC
               GO TO DBK-NG.
           IF DM-KEIYAKU = ZERO
               GO TO DBK-NG.
           IF DM-SPT (21:2) NOT NUMERIC
               GO TO DBK-NG.
           IF DM-VAL = '00000'
               GO TO DBK-NG.
           MOVE 1              TO WS-I.
       DBK-020.
           IF WS-I GREATER 33
               GO TO DENBUN-KENSA-EX.
           IF DM-DENBUN (WS-I:1) LESS SPACE
               GO TO DBK-NG.
           ADD 1               TO WS-I.
           GO TO DBK-020.
       DBK-NG.
           MOVE 8              TO DM-RC.
       DENBUN-KENSA-EX.
           EXIT.
