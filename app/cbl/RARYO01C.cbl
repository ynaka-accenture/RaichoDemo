      *****************************************************************
      * RARYO01C  料金照会 (TRAN=RRYO)         東西電力 電算部        *
      *---------------------------------------------------------------*
      * H08.02 初版  H26.10 予備域対応                                *
      * 予備域は大口のとき調整額 (PACKED). それ以外は文字フラグ.      *
      *   同じ 8 バイトの解釈が金額条件で変わる (媒体作成と対の仕様)  *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RARYO01C.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-RESP             PIC S9(8) COMP VALUE ZERO.
       77  WS-KEIKOKU          PIC 9(1)  VALUE ZERO.
       77  ED-KIN              PIC ---,---,--9 VALUE ZERO.
       77  W-ARAKEI            PIC S9(9) VALUE ZERO.
       77  WS-I                PIC 9(2)  VALUE ZERO.
       COPY RCRYOMAP.
      *    請求は正本コピー句を用いる (写しは作らない)
       COPY RCRYOREC.
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  CA-STATE        PIC X(1).
           05  CA-USER         PIC X(8).
           05  CA-TRIES        PIC 9(1).
           05  FILLER          PIC X(190).
       PROCEDURE DIVISION.
       MAIN-RTN.
           IF EIBCALEN = ZERO
               EXEC CICS ABEND ABCODE('RY01') END-EXEC.
           IF CA-STATE = 'R'
               GO TO SHOKAI-SYORI.
           GO TO SYOKI-GAMEN.
      *----------------------------------------------------------------
       SYOKI-GAMEN.
           MOVE LOW-VALUES     TO RYO01MI.
           MOVE 'RYOKIN: SPT WO IRETE KUDA' TO RYO-O-MSG.
           EXEC CICS SEND MAP('RYO01M') MAPSET('RRYO01M')
               FROM(RYO01MO) ERASE END-EXEC.
           MOVE 'R'            TO CA-STATE.
           EXEC CICS RETURN TRANSID('RRYO')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
       SHOKAI-SYORI.
           EXEC CICS RECEIVE MAP('RYO01M') MAPSET('RRYO01M')
               INTO(RYO01MI) RESP(WS-RESP) END-EXEC.
           IF WS-RESP NOT = ZERO
               EXEC CICS RETURN END-EXEC.
           IF RYI-SPT (1:1) = 'C'
               IF RYI-SPT (2:21) = SPACES
                   MOVE SPACES TO RYO01MO
                   MOVE 'CLEAR SHIMASHITA        ' TO RYO-O-MSG
                   GO TO OUTO-HENSHIN
               END-IF
           END-IF.
           IF RYI-SPT (1:1) = 'M'
               IF RYI-SPT (2:21) = SPACES
                   MOVE 'M' TO CA-STATE
                   EXEC CICS XCTL PROGRAM('RAMEN01C')
                       COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC
               END-IF
           END-IF.
           PERFORM SPT-KENSA   THRU SPT-KENSA-EX.
           EXEC CICS READ DATASET('RYOFILE')
               INTO(RYO-REC) RIDFLD(RYI-SPT)
               RESP(WS-RESP) END-EXEC.
           IF WS-RESP = 13
               MOVE SPACES     TO RYO01MO
               MOVE RYI-SPT    TO RYO-O-SPT
               MOVE 'SEIKYU MIAJTORI         ' TO RYO-O-MSG
               GO TO OUTO-HENSHIN.
           IF WS-RESP NOT = ZERO
               EXEC CICS ABEND ABCODE('RY02') END-EXEC.
           PERFORM HYOJI-HENSYU THRU HYOJI-HENSYU-EX.
       OUTO-HENSHIN.
           EXEC CICS SEND MAP('RYO01M') MAPSET('RRYO01M')
               FROM(RYO01MO) END-EXEC.
           MOVE 'R'            TO CA-STATE.
           EXEC CICS RETURN TRANSID('RRYO')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
      * 表示編集: 予備域の二重解釈 (媒体作成 RBRYO02C と対)
      *----------------------------------------------------------------
       HYOJI-HENSYU            SECTION.
       HYH-010.
           MOVE SPACES         TO RYO01MO.
           MOVE RYO-SPT-NO         TO RYO-O-SPT.
           MOVE RYO-SEIKYU-YM          TO RYO-O-YM.
           MOVE RYO-GOKEI       TO ED-KIN.
           MOVE ED-KIN         TO RYO-O-GOKEI.
           MOVE RYO-KIHON       TO ED-KIN.
           MOVE ED-KIN (4:8)   TO RYO-O-KIHON.
           MOVE RYO-ZEIGAKU         TO ED-KIN.
           MOVE ED-KIN (4:8)   TO RYO-O-ZEI.
           MOVE RYO-NYUKIN-FLG  TO RYO-O-FLG.
           PERFORM JOKYO-KENSA THRU JOKYO-KENSA-EX.
           IF WS-KEIKOKU = ZERO
               MOVE 'SHOKAI KANRYO           ' TO RYO-O-MSG
               GO TO HYOJI-HENSYU-EX.
           IF WS-KEIKOKU = 1
      *        大口: 予備域を調整額として読む (通常は文字フラグ)
               MOVE 'OGUCHI: CHOSEI ARI      ' TO RYO-O-MSG
               GO TO HYOJI-HENSYU-EX.
           IF WS-KEIKOKU = 2
               MOVE 'NYUKIN-ZUMI             ' TO RYO-O-MSG
               GO TO HYOJI-HENSYU-EX.
           IF WS-KEIKOKU = 3
               MOVE 'HENKIN (MINUS SEIKYU)   ' TO RYO-O-MSG
               GO TO HYOJI-HENSYU-EX.
           MOVE 'DATA IJO: KANRI RENRAKU '     TO RYO-O-MSG.
       HYOJI-HENSYU-EX.
           EXIT.
      *----------------------------------------------------------------
       JOKYO-KENSA             SECTION.
       JKK-010.
           MOVE ZERO           TO WS-KEIKOKU.
           IF RYO-SEIKYU-YM NOT NUMERIC
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF RYO-ZEI-SEDAI NOT = '01' AND
              RYO-ZEI-SEDAI NOT = '02'
              AND RYO-ZEI-SEDAI NOT = '03' AND
              RYO-ZEI-SEDAI NOT = '04'
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF RYO-SEIKYU-KBN NOT = '1' AND
              RYO-SEIKYU-KBN NOT = '2'
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF RYO-GOKEI GREATER 20000
               IF RYO-YOBI NOT = SPACES
                   MOVE 1 TO WS-KEIKOKU
                   GO TO JOKYO-KENSA-EX
               END-IF
           END-IF
           IF RYO-NYUKIN-FLG NOT = '0' AND RYO-NYUKIN-FLG NOT = '1'
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF RYO-NYUKIN-FLG = '1'
               MOVE 2 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF RYO-KENSHIN-BI NOT NUMERIC
               MOVE 9 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           IF RYO-GOKEI LESS ZERO
               MOVE 3 TO WS-KEIKOKU
               GO TO JOKYO-KENSA-EX.
           PERFORM ARA-KENSAN THRU ARA-KENSAN-EX.
       JOKYO-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 画面側の粗検算: 基本+税が合計を超えないこと (H26)
      *----------------------------------------------------------------
       ARA-KENSAN              SECTION.
       ARK-010.
           IF RYO-KIHON LESS ZERO
               MOVE 9 TO WS-KEIKOKU
               GO TO ARA-KENSAN-EX.
           IF RYO-ZEIGAKU LESS ZERO
               MOVE 9 TO WS-KEIKOKU
               GO TO ARA-KENSAN-EX.
           COMPUTE W-ARAKEI = RYO-KIHON + RYO-ZEIGAKU.
           IF W-ARAKEI GREATER RYO-GOKEI
               IF RYO-SEIKYU-KBN = '1'
                   IF RYO-NYUKIN-FLG = '0'
                       MOVE 9 TO WS-KEIKOKU
                   END-IF
               END-IF
           END-IF
           IF RYO-NIWARI-NISSU LESS 1
               MOVE 9 TO WS-KEIKOKU
               GO TO ARA-KENSAN-EX.
           IF RYO-NIWARI-NISSU GREATER 31
               MOVE 9 TO WS-KEIKOKU.
       ARA-KENSAN-EX.
           EXIT.
      *----------------------------------------------------------------
      * 供給地点検査 (更新系と同等の完全版)
      *----------------------------------------------------------------
       SPT-KENSA               SECTION.
       SPK-010.
           IF RYI-SPT = SPACES OR RYI-SPT = LOW-VALUES
               GO TO SPK-NG.
           IF RYI-SPT (1:2) NOT = '03'
               GO TO SPK-NG.
           IF RYI-SPT (3:2) NOT NUMERIC
               GO TO SPK-NG.
           IF RYI-SPT (3:2) LESS '01' OR RYI-SPT (3:2) GREATER '47'
               GO TO SPK-NG.
           IF RYI-SPT (5:16) NOT NUMERIC
               GO TO SPK-NG.
           MOVE 5              TO WS-I.
       SPK-015.
           IF WS-I GREATER 20
               GO TO SPK-018.
           IF RYI-SPT (WS-I:1) LESS '0'
               GO TO SPK-NG.
           ADD 1               TO WS-I.
           GO TO SPK-015.
       SPK-018.
           IF RYI-SPT (21:2) NOT NUMERIC
               GO TO SPK-NG.
           IF RYI-SPT (1:1) = LOW-VALUE
               GO TO SPK-NG.
           GO TO SPT-KENSA-EX.
       SPK-NG.
           MOVE SPACES         TO RYO01MO.
           MOVE 'SPT KEISHIKI AYAMARI    '     TO RYO-O-MSG.
           GO TO OUTO-HENSHIN.
       SPT-KENSA-EX.
           EXIT.
