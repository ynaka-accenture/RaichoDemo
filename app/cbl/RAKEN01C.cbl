      *****************************************************************
      * RAKEN01C  検針照会 (TRAN=RKEN)         東西電力 電算部        *
      *---------------------------------------------------------------*
      * H06.10 初版. 地点キーで最新月の検針を表示する                 *
      * 注意: 供給地点の検査は簡易版 (照会のみのため -- H06 判断.     *
      *       検査数字の検算は行わない. 更新系とは別物である)         *
      * 注意: 指示数は乗率適用前の生値を表示する (帳票と異なる)       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RAKEN01C.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-RESP             PIC S9(8) COMP VALUE ZERO.
       77  WS-J                PIC 9(2)  VALUE ZERO.
       77  WS-KEIKOKU          PIC 9(1)  VALUE ZERO.
       77  WS-I                PIC 9(2)  VALUE ZERO.
       77  WS-BIKO-NG          PIC 9(1)  VALUE ZERO.
       COPY RCKENMAP.
      *    検針実績は正本コピー句を用いる (写しは作らない)
       COPY RCKENREC.
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  CA-STATE        PIC X(1).
           05  CA-USER         PIC X(8).
           05  CA-TRIES        PIC 9(1).
           05  FILLER          PIC X(190).
       PROCEDURE DIVISION.
       MAIN-RTN.
           IF EIBCALEN = ZERO
               EXEC CICS ABEND ABCODE('KN01') END-EXEC.
           IF CA-STATE = 'N'
               GO TO SHOKAI-SYORI.
           GO TO SYOKI-GAMEN.
      *----------------------------------------------------------------
       SYOKI-GAMEN.
           MOVE LOW-VALUES     TO KEN01MI.
           MOVE 'KENSHIN: SPT WO IRETE KUDA' TO KNO-MSG.
           EXEC CICS SEND MAP('KEN01M') MAPSET('RKEN01M')
               FROM(KEN01MO) ERASE END-EXEC.
           MOVE 'N'            TO CA-STATE.
           EXEC CICS RETURN TRANSID('RKEN')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
       SHOKAI-SYORI.
           EXEC CICS RECEIVE MAP('KEN01M') MAPSET('RKEN01M')
               INTO(KEN01MI) RESP(WS-RESP) END-EXEC.
           IF WS-RESP NOT = ZERO
               EXEC CICS RETURN END-EXEC.
           IF KNI-SPT (1:1) = 'C'
               IF KNI-SPT (2:21) = SPACES
                   MOVE SPACES TO KEN01MO
                   MOVE 'CLEAR SHIMASHITA          ' TO KNO-MSG
                   GO TO OUTO-HENSHIN
               END-IF
           END-IF.
           IF KNI-SPT (1:1) = 'M'
               IF KNI-SPT (2:21) = SPACES
                   MOVE 'M' TO CA-STATE
                   EXEC CICS XCTL PROGRAM('RAMEN01C')
                       COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC
               END-IF
           END-IF.
           PERFORM SPT-KANI    THRU SPT-KANI-EX.
           EXEC CICS READ DATASET('KENFILE')
               INTO(KEN-REC) RIDFLD(KNI-SPT)
               RESP(WS-RESP) END-EXEC.
           IF WS-RESP = 13
               MOVE SPACES     TO KEN01MO
               MOVE KNI-SPT    TO KNO-SPT
               MOVE 'KENSHIN MIAJTORI          ' TO KNO-MSG
               GO TO OUTO-HENSHIN.
           IF WS-RESP NOT = ZERO
               EXEC CICS ABEND ABCODE('KN02') END-EXEC.
           PERFORM HYOJI-HENSYU THRU HYOJI-HENSYU-EX.
       OUTO-HENSHIN.
           EXEC CICS SEND MAP('KEN01M') MAPSET('RKEN01M')
               FROM(KEN01MO) END-EXEC.
           MOVE 'N'            TO CA-STATE.
           EXEC CICS RETURN TRANSID('RKEN')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
      * 表示編集: 交換月は前後指示数の意味が変わる旨を警告
      *----------------------------------------------------------------
       HYOJI-HENSYU            SECTION.
       HYH-010.
           MOVE SPACES         TO KEN01MO.
           MOVE KEN-SPT-NO         TO KNO-SPT.
           MOVE KEN-NENGETU     TO KNO-YM.
           MOVE KEN-KENSHIN-BI  TO KNO-BI.
           MOVE KEN-ZEN-SIJISU         TO KNO-ZEN.
           MOVE KEN-KON-SIJISU         TO KNO-KON.
           MOVE KEN-SIYORYO        TO KNO-SIYO (1:6).
           MOVE KEN-KENSHIN-KBN         TO KNO-KBN.
           PERFORM JOKYO-KENSA THRU JOKYO-KENSA-EX.
           IF WS-KEIKOKU = ZERO
               MOVE 'SHOKAI KANRYO             ' TO KNO-MSG
               GO TO HYOJI-HENSYU-EX.
           IF WS-KEIKOKU = 1
               MOVE 'KOKAN-TSUKI: SIJI CHUI    ' TO KNO-MSG
               GO TO HYOJI-HENSYU-EX.
           IF WS-KEIKOKU = 2
               MOVE 'GAISAN KENSHIN (KBN=9)    ' TO KNO-MSG
               GO TO HYOJI-HENSYU-EX.
           IF WS-KEIKOKU = 3
               MOVE 'SAI-KENSHIN (KBN=3)       ' TO KNO-MSG
               GO TO HYOJI-HENSYU-EX.
           MOVE 'DATA IJO: KANRI RENRAKU   '    TO KNO-MSG.
       HYOJI-HENSYU-EX.
           EXIT.
      *----------------------------------------------------------------
       JOKYO-KENSA             SECTION.
       JKK-010.
           MOVE ZERO           TO WS-KEIKOKU.
           IF KEN-NENGETU NOT NUMERIC
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-KENSHIN-BI NOT NUMERIC
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-KENSHIN-KBN NOT = '1' AND
              KEN-KENSHIN-KBN NOT = '2' AND
              KEN-KENSHIN-KBN NOT = '3' AND
              KEN-KENSHIN-KBN NOT = '9'
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-KOKAN-FLG = 'K' OR KEN-KOKAN-FLG = 'G'
               MOVE 1 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-KOKAN-FLG NOT = SPACE
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-ZEN-SIJISU NOT NUMERIC
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-KON-SIJISU LESS ZERO
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-SIYORYO LESS -99999
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-SIYORYO GREATER 999999
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-KENSHIN-KBN = '9'
               MOVE 2 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-KENSHIN-KBN = '3'
               MOVE 3 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-NENGETU (5:2) LESS '01' OR
              KEN-NENGETU (5:2) GREATER '12'
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-KENSHININ NOT NUMERIC
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF KEN-KENSHININ LESS '00001' OR
              KEN-KENSHININ GREATER '00080'
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           PERFORM BIKO-SOSA THRU BIKO-SOSA-EX.
           IF WS-BIKO-NG NOT = ZERO
               MOVE 9 TO WS-KEIKOKU.
       JOKYO-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 備考走査: 制御文字混入の検出 (伝送障害の名残 -- H12)
      *----------------------------------------------------------------
       BIKO-SOSA               SECTION.
       BKS-010.
           MOVE ZERO           TO WS-BIKO-NG.
           MOVE 1              TO WS-I.
       BKS-020.
           IF WS-I GREATER 30
               GO TO BIKO-SOSA-EX.
           IF KEN-BIKO (WS-I:1) LESS SPACE
               MOVE 1 TO WS-BIKO-NG
               GO TO BIKO-SOSA-EX.
           ADD 1               TO WS-I.
           GO TO BKS-020.
       BIKO-SOSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 供給地点 簡易検査 (検査数字は見ない -- 冒頭注意書き参照)
      *----------------------------------------------------------------
       SPT-KANI                SECTION.
       SPK-010.
           IF KNI-SPT = SPACES OR KNI-SPT = LOW-VALUES
               GO TO SPK-NG.
           IF KNI-SPT (1:2) NOT = '03'
               GO TO SPK-NG.
           IF KNI-SPT (3:2) NOT NUMERIC
               GO TO SPK-NG.
           IF KNI-SPT (3:2) LESS '01' OR KNI-SPT (3:2) GREATER '47'
               GO TO SPK-NG.
           IF KNI-SPT (5:16) NOT NUMERIC
               GO TO SPK-NG.
           IF KNI-SPT (21:2) = '  '
               GO TO SPK-NG.
           IF KNI-SPT (1:1) = LOW-VALUE
               GO TO SPK-NG.
           GO TO SPT-KANI-EX.
       SPK-NG.
           MOVE SPACES         TO KEN01MO.
           MOVE 'SPT KEISHIKI AYAMARI      '    TO KNO-MSG.
           GO TO OUTO-HENSHIN.
       SPT-KANI-EX.
           EXIT.
